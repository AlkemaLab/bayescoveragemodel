
get_eta_samples <- function(fit, year_select = 2023) {

  if (!"samples" %in% names(fit)) {
    stop("The input 'fit' must contain 'samples'.")
  }

  iso_codes <- fit$geo_unit_index %>%
    dplyr::rename(C = c)

  year_index <- fit$time_index %>%
    dplyr::rename(T = t)

  # Determine if validation run
  is_validation <- any(fit$stan_data$held_out == 1)
  if (is_validation){
    # exclude countries that have no data in train or validation
    # to do: reconsider where to do this
    y_df <- tibble::tibble(
      held_out = as.logical(fit$stan_data$held_out),
      iso = fit$data$iso
    ) %>%
      group_by(iso) %>%
      mutate(n_train = sum(held_out == 0),
             n_test = sum(held_out == 1)) %>%
      ungroup() %>%
      filter(n_train > 0, n_test > 0)
    iso_include <- unique(y_df$iso)
  } else {
    iso_include <- unique(fit$data$iso)
  }

  # Extract draws
  params <- c("eta")
  draws <- fit$samples$draws(params) %>%
    tidybayes::spread_draws(eta[C, T]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.chain, -.iteration) %>%
    dplyr::rename(draw = .draw) %>%
    dplyr::left_join(iso_codes, by = "C") %>%
    dplyr::left_join(year_index, by = "T") %>%
    dplyr::filter(year == year_select,
                  iso %in% iso_include)

  return(draws)
}
