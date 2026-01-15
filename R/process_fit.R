
#' @import dplyr tidyr tidybayes stringr
#' @importFrom stats quantile
process_fit <- function(fit, parallel_chains = NULL,
                        add_aggregates  = FALSE,
                        save_eps = FALSE,
                        save_temporal = TRUE,
                        save_generated_quantities = FALSE,
                        save_nontemporal  = FALSE

                        ) {
  if(is.null(parallel_chains)) parallel_chains <- 1

  if (save_temporal){
    #
    # eta and epsilon summaries
    #
    if(fit$stan_data$smoothing == 1 & save_eps) {
      temporal_variables <- c("eta", "epsilon")
    }
    else {
        temporal_variables <- c("eta")

    }
    temporal <- fit$samples$summary(
      temporal_variables,
      ~ stats::quantile(.x, probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)),
      .cores = parallel_chains) %>%
      tidyr::separate(.data$variable, c("variable", "index"), "\\[") %>%
      dplyr::mutate(index = stringr::str_replace_all(.data$index, "\\]", "")) %>%
      tidyr::separate(.data$index, c("c", "t"), ",") %>%
      dplyr::mutate_at(vars(c, t), as.integer) %>%
      dplyr::left_join(fit$geo_unit_index, by = "c") %>%
      dplyr::left_join(fit$time_index, by = "t")
    ans <- list(
      temporal = temporal)
  } else {
    ans <- list()
  } # end temporal

  if (add_aggregates){
    temporal_agg <- fit$samples$summary(
      "aggr",
      ~ stats::quantile(.x, probs = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)),
      .cores = parallel_chains) %>%
      tidyr::separate(.data$variable, c("variable", "index"), "\\[") %>%
      dplyr::mutate(index = stringr::str_replace_all(.data$index, "\\]", "")) %>%
      #  tidyr::separate(.data$index, c("c", "t"), ",") %>%
      rename(t = index) %>%
      dplyr::mutate_at(vars(t), as.integer) %>%
      dplyr::left_join(fit$time_index, by = "t")

    ans$temporal <-
      bind_rows(ans$temporal,
                temporal_agg %>%
                  mutate(admin1 = "National", variable = "eta"))

  }

  if (save_nontemporal){
    #
    # Transition function summaries
    #
    transition <- list()
    transition_quantiles <- list()
    for(column in fit$hierarchical_splines) {
      transition[[column]] <- extract_rate_vs_level_subhierarchical(fit, fit$hierarchical_splines, column)
      transition_quantiles[[column]] <- transition[[column]] %>%
        dplyr::group_by(.data$name, .data$x) %>%
        tidybayes::median_qi(.data$Y, .width = c(0.5, 0.8, 0.95))
    }


    #
    # Hyperparameters
    #
    Ptilde <- list()
    for(column in fit$hierarchical_asymptote) {
      Ptilde[[column]] <- extract_parameter_subhierarchical(fit, fit$hierarchical_asymptote, column,
                                                              "Ptilde")
    }

    Omega <- list()
    for(column in fit$hierarchical_level) {
      Omega[[column]] <- extract_parameter_subhierarchical(fit, fit$hierarchical_level, column,
                                                          "Omega")
    }
    ans <- c(ans,
             list(transition = transition,
                  transition_quantiles = transition_quantiles,
                  Ptilde = Ptilde,
                  Omega = Omega
             ))

    ar <- NULL
    if(fit$stan_data$smoothing == 1) {
      ar <- fit$samples$draws(c("rho", "tau")) %>%
        tidybayes::spread_draws(rho[i], tau[i]) %>%
        ungroup() %>%
        dplyr::select(-.data$i)
    }

    #
    # Hierarchical distributions
    #
    Ptilde_sigma <- fit$samples$draws(c("Ptilde_sigma")) %>%
      tidybayes::spread_draws(Ptilde_sigma[i])
    Omega_sigma <- fit$samples$draws(c("Omega_sigma")) %>%
      tidybayes::spread_draws(Omega_sigma[i])
    a_sigma <- fit$samples$draws(c("a_sigma")) %>%
      tidybayes::spread_draws(a_sigma[i, j])

    #
    # Data model
    #
    nonse <- fit$samples$draws("nonse") %>%
      tidybayes::spread_draws(nonse[source]) %>%
      dplyr::left_join(fit$source_index, by = "source")


    ans <- c(ans,
             list(
                  ar = ar,
                  nonse = nonse,

                  Ptilde_sigma = Ptilde_sigma,
                  Omega_sigma = Omega_sigma,
                  a_sigma = a_sigma
             ))
  } # end non=temproal
  if (save_generated_quantities){
    # not yet updated
    #
    # Generated quantities
    #
    generated_quantities <- fit$samples$draws(c("pit", "resid", "y_pred")) %>%
      tidybayes::spread_draws(pit[i], resid[i], y_pred[i])
    ans <- c(ans,
             list(
               generated_quantities = generated_quantities
             ))
    if (!just_one_indicator ){
      d_generated_quantities <- fit$samples$draws(c("d_pit", "d_resid", "d_y_pred")) %>%
        tidybayes::spread_draws(d_pit[i], d_resid[i], d_y_pred[i]) %>%
        rename(pit = d_pit, resid = d_resid, y_pred = d_y_pred)
      ans <- c(ans,
               list(
                 d_generated_quantities = d_generated_quantities
               ))
    }

  }
  ans
}

#' @import dplyr
#' @importFrom tidybayes median_qi spread_draws
#' @importFrom stats plogis
#' @importFrom rlang .data
extract_rate_vs_level_subhierarchical <- function(fit, f, subhierarchy, constrain_zero = "after"

) {
  hierarchical_a <- hierarchical_data(fit$geo_unit_index, f)

  start <- hierarchical_a$model_matrix$index %>%
    dplyr::filter(.data$column == subhierarchy) %>%
    dplyr::pull(i) %>%
    min()

  end <- hierarchical_a$model_matrix$index %>%
    dplyr::filter(.data$column == subhierarchy) %>%
    dplyr::pull(i) %>%
    max()

  B <- fit$stan_data$B

  num_constrained_zero <- fit$stan_data$spline_degree + 1

  a_star <- fit$samples$draws(c("a_star")) %>%
      tidybayes::spread_draws(a_star[j, i])

  a_star <- a_star %>%
    dplyr::group_by(.data$i, .data$.chain, .data$.iteration, .data$.draw) %>%
    dplyr::summarize(a_star =  list(.data[["a_star"]]))

  # tidyr::nest() %>%
  # dplyr::mutate(a_star = map(.data$data, `[[`, "a_star")) %>%
  # dplyr::select(-.data$data)

  # example from 1 param function
  # tidyr::nest() %>%
  #   dplyr::mutate(star = map(.data$data, `[[`, glue::glue("{x}_star"))) %>%
  #   dplyr::select(-.data$data)
  #
  # dplyr::summarize(
  #   star = list(.data[[glue::glue("{x}_star")]])
  # )

  uniq <- unique(hierarchical_a$model_matrix$mat[, 1:end, drop = FALSE])

  titles <- c()
  for(i in 1:nrow(uniq)) {
    index <- rep(0, hierarchical_a$n_terms)
    index[1:end] <- uniq[i, 1:end]
    title <- hierarchical_a$model_matrix$index %>%
      dplyr::filter(i == last(which(index == 1))) %>%
      dplyr::pull(.data$level)
    titles <- c(titles, title)

    a_star[[title]] = purrr::map_dbl(a_star$a_star, function(a_star) {
      fit$stan_data$a_lower_bound + (fit$stan_data$a_upper_bound - fit$stan_data$a_lower_bound) * stats::plogis(index %*% a_star)
    })
  }

  tmp_function_summ <- function(data) {
    if(constrain_zero == "after") {
      tibble::tibble(x = fit$stan_data$grid, Y = t(c(data$value, rep(0, num_constrained_zero)) %*% B)[,1]) %>%
        dplyr::filter(.data$x >= 0 & .data$x <= 1)
    }
    else {
      tibble::tibble(x = fit$stan_data$grid, Y = t(c(rep(0, num_constrained_zero), data$value) %*% B)[,1]) %>%
        dplyr::filter(.data$x >= 0 & .data$x <= 1)
    }
  }

  a_star <- a_star %>%
    tidyr::pivot_longer(cols = all_of(titles)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$a_star) %>%
    dplyr::group_by(.data$.chain, .data$.iteration, .data$.draw, .data$name) %>%
    # tidyr::nest() %>%
    # dplyr::mutate(Y = map(.data$data, function(data) {
    #   if(constrain_zero == "after") {
    #     tibble::tibble(x = fit$stan_data$grid, Y = t(c(data$value, rep(0, num_constrained_zero)) %*% B)[,1]) %>%
    #       dplyr::filter(.data$x >= 0 & .data$x <= 1)
    #   }
    #   else {
    #     tibble::tibble(x = fit$stan_data$grid, Y = t(c(rep(0, num_constrained_zero), data$value) %*% B)[,1]) %>%
    #       dplyr::filter(.data$x >= 0 & .data$x <= 1)
    #   }
    # }))  %>%
  # dplyr::ungroup() %>%
  # dplyr::select(-.data$data, -.data$.draw) %>%
  # tidyr::unnest(.data$Y)

  dplyr::summarize(
    Y = list(tmp_function_summ(.data)))  %>%
    tidyr::unnest(.data$Y)
  # example from 1 param function
  # tidyr::nest() %>%
  #   dplyr::mutate(star = map(.data$data, `[[`, glue::glue("{x}_star"))) %>%
  #   dplyr::select(-.data$data)
  #
  # dplyr::summarize(
  #   star = list(.data[[glue::glue("{x}_star")]])
  # )

  return(a_star)
}

