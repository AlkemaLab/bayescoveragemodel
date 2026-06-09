#' get_standata_routine
#'
#' @param routine_data tibble with  iso, year, indicator_name, routine_value, countdownmean
#' routine_value is the value of the routine data (eg coverage) and
#' countdownmean is the data quality indicator for that iso-year-indicator combi
#' needs to be filtered to iso of interest
#'
#'
#' @param fit_routine_obj brm-fit object
#' @param time_index from the model fit
#' @param geo_unit_index from the model fit
#'
#' @returns list with dat_routine (for plotting) and routine_list (to pass to stan)
#' @keywords internal
#'
get_standata_routine <- function(routine_data,
                                 fit_routine_obj,
                                 time_index, geo_unit_index
){

  # check that there is only one indicator
  if (length(unique(routine_data$indicator_name)) > 1){
    stop("routine data should only contain one indicator, but multiple found: ",
         paste(unique(routine_data$indicator_name), collapse = ", "))
  }
  dat_roc <-
    routine_data |>
    bayescoveragemodel::routinedata_add_roc_info(
      dq_covariates_min = c("countdownmean"),
      dq_covariates_max = c(),
      dq_covariates_max_abs = c()
    )


  # info:  how used in stan model
  # N_routine
  # hierarchical_sigma  = country random effect sd
  # log_sigma_mean_routine_roc = mean on log scale
  # c and t routine
  # if (N_routine > 0) {
  #   log_sigma_delta ~ normal(0, hierarchical_sigma); // one term per geounit
  #   for (j in 1:N_routine){
  #     real roc_routine_totalsd_j = exp(log_sigma_mean_routine_roc[j] +
  #                                        log_sigma_delta[c_routine_j[j]]);
  #     real eta_jt = inv_tr_eta(tr_eta_obs[c_routine_j[j], t_routine_j[j]]);
  #     real eta_jtmin1 = inv_tr_eta(tr_eta_obs[c_routine_j[j], t_routine_j[j]-1]);
  #     routine_roc[j] ~ normal(eta_jt - eta_jtmin1, roc_routine_totalsd_j);
  #   }
  # }

  dat_roc$log_sigma_mean_routine_roc <-
    get_logsigma_mean(fit_routineglobal = fit_routine_obj,
                      indicator_name = dat_roc$indicator_name,
                      worst_countdownmean =  dat_roc$worst_countdownmean
    )
  # we need the country hierarchical sigma, which varies across indicators
  hierarchical_sigma <- fit_routine_obj$sd_country[routine_data$indicator_name[1]]

  dat_routine <-
    dat_roc |>
    dplyr::left_join(time_index, by = "year") |>
    dplyr::rename(t_routine_j = t) |>
    dplyr::left_join(geo_unit_index) |>
    dplyr::rename(c_routine_j = c)
  if (dim(dat_routine)[1] == 0){
    dat_routine <- NULL
    routine_list <- NULL
  } else {
    if (anyNA(dat_routine$c_routine_j)){
      stop("some mismatch between regions provided in routine data vs those in survey data.")
    }
    dat_model <- dat_routine |>
      dplyr::filter(!is.na(routine_roc))  |> # we don't filter yet for dat_routine as we do want start years in the plot
      dplyr::select(routine_roc, c_routine_j, t_routine_j, log_sigma_mean_routine_roc)

    routine_list <- c(
      list(N_routine = dim(dat_model)[1]),
      as.list(dat_model),
      list(mean_log_sigma = 0, # not used, but kept to avoid having to change the stan code
           hierarchical_sigma = hierarchical_sigma))
  }

  return(list(dat_routine = dat_routine, routine_list = routine_list))
}

#' Routine data mean uncertainty
#'
#' @param fit_routineglobal brms fit for error variance routine data
#' @param indicator_name name indicator
#' @param worst_countdownmean vector with value of data quality indicator,
#' worst of start and end year for annual change
#'
#' @returns vector with mean log sigma
#' @keywords internal
#'
get_logsigma_mean <- function(fit_routineglobal,
                              indicator_name,
                              worst_countdownmean){
  log(fitted(fit_routineglobal,
             newdata =
               tibble::tibble(
                 indicator_name = indicator_name,
                 worst_countdownmean = worst_countdownmean,
                 country = NA),
             dpar = "sigma",
             #             re_formula = ~ 0)[, "Estimate"]
             re_formula = ~ (1|indicator_name))[, "Estimate"]
  )
}
