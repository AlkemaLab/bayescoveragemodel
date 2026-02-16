
#' get_standata_routine
#'
#' @param service_statistic_df tibble with  iso; admin1 for subnat; routine; sd_routine; routine_roc; sd_routine_roc; year
#' filtered to pop (eg country)
#' @param hyper_param mean and sd hyperparameters for roc data model
#' @param time_index from the model fit
#' @param geo_unit_index from the model fit
#'
#' @returns list with dat_routine (for plotting) and routine_list (to pass to stan)
#' @export
#'
get_standata_routine <- function(service_statistic_df, # filtered to pop (eg country)
                                 hyper_param,
                                           time_index, geo_unit_index
){
  dat_routine <-
    service_statistic_df %>%
    dplyr::mutate(routine_lower = pmax(0, routine_value - qnorm(0.975)*sd_routine),
                  routine_upper = pmin(1, routine_value + qnorm(0.975)*sd_routine),
                  sd2_routine_roc = sd_routine_roc^2) %>%
    left_join(time_index, by = "year") %>%
    # exclude data prior to the most recent survey? right now we don't
    rename(t_routine_j = t) %>%
    left_join(geo_unit_index) %>%
    rename(c_routine_j = c)
  if (dim(dat_routine)[1] == 0){
    dat_routine <- NULL
    routine_list <- NULL
  } else {
    if (anyNA(dat_routine$c_routine_j)){
      stop("some mismatch between regions provided in routine data vs those in survey data.")
    }

    dat_model <- dat_routine %>%
      filter(!is.na(routine_roc))  %>% # we don't filter yet for dat_routine as we do want start years in the plot
      dplyr::select(routine_roc, sd2_routine_roc,  c_routine_j, t_routine_j)

    routine_list <- c(list(N_routine = dim(dat_model)[1]),
                  as.list(dat_model),
                  list(mean_log_sigma = c(hyper_param$mean_log_sigma),
                        hierarchical_sigma = c(hyper_param$hierarchical_sigma)))
  #                as.list(hyper_param))
  }

  return(list(dat_routine = dat_routine, routine_list = routine_list))
}
