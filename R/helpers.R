
make_val_table_region <- function(valresults){
  valresults %>%
    dplyr::mutate(dplyr::across(-c("n_left_out", "name_region"), ~ round(100*.x, 0))) %>%
    rename(nobs = n_left_out, MedE = med_error, MedAE = med_abs_error,
           CI95width = avg_interval_width) %>%
    dplyr::select(-med_abs_rel_error, -med_rel_error) %>%
    rename(data_subset = name_region) %>%
    dplyr::select(data_subset, everything())
}

make_val_table <- function(valresults){
  valresults %>%
    dplyr::mutate(dplyr::across(-c("n_left_out"), ~ round(100*.x, 0))) %>%
    rename(nobs = n_left_out, MedE = med_error, MedAE = med_abs_error,
           CI95width = avg_interval_width) %>%
    dplyr::select(-med_abs_rel_error, -med_rel_error) %>%
    mutate(data_subset = "All") %>%
    dplyr::select(data_subset, everything())
}

make_val_table_datatype <- function(valresults){
  valresults %>%
    dplyr::mutate(dplyr::across(-c("n_left_out", "data_series_type"), ~ round(100*.x, 0))) %>%
    rename(nobs = n_left_out, MedE = med_error, MedAE = med_abs_error,
           CI95width = avg_interval_width) %>%
    dplyr::select(-med_abs_rel_error, -med_rel_error) %>%
    rename(data_subset = data_series_type) %>%
    dplyr::select(data_subset, everything())
}

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) 1/(1+exp(-x))


pluralize <- function(x, singular, plural) {
  ifelse(x > 1, plural, singular)
}

check_nas_or_pops <- function(data, column, year, population_data) {
  if(!(column %in% names(data))) {
    stop(glue::glue("Could not find column {column}."))
  }
  if(sum(is.na(data[[column]]))) {
    if (is.null(population_data) || !is.data.frame(population_data)) {
      stop(glue::glue("If there are NAs in column {column}, `population_data` must be provided."))
    }

    if (!all(c(year, column, "population") %in% colnames(population_data))) {
      stop(glue::glue("If there are NAs in column {column}, `population_data` must include columns {column}, {year}, and population."))
    }

    missing_years <- data[[year]][is.na(data[[column]])]
    non_missing_areas <- unique(data[[column]][!is.na(data[[column]])])
    required_pop_rows <- tidyr::expand_grid(
      year = missing_years,
      area = non_missing_areas)
    colnames(required_pop_rows) <- c(year, column)
    required_pop_rows <- required_pop_rows |>
      dplyr::left_join(population_data, by = c(year, column))
    missing_pop_rows <- required_pop_rows |>
      dplyr::filter(is.na(population))
    if (nrow(missing_pop_rows) > 0) {
      stop(glue::glue("If there are NAs in column {column}, `population_data` must include population data for all areas for the relevant years."))
    }
  }
}


#' Load a saved model fit
#'
#' This helper function loads a previously saved model fit based on the indicator name,
#' run step, and folder suffix.
#'
#' @param indicator Character string specifying the indicator name (e.g., `"ideliv"`, `"anc4"`).
#' @param runstep Character string indicating the model step (e.g., `"step1a"`, `"step1b"`).
#' @param folder_suffix Character or numeric. The suffix at the end of the folder name.
#'
#' @return A model fit object read from an RDS file.
#' @export
get_fit <- function(indicator, runstep, folder_suffix) {
  runname <- paste0(indicator, "_", runstep, "_", folder_suffix)
  file_path <- file.path(get_output_dir(runname), paste0(indicator, "_fit_wpostsumm.rds"))

  if (!file.exists(file_path)) {
    stop("The specified fit file does not exist: ", file_path)
  }

  fit <- readRDS(file_path)
  fit$output_dir <- file.path(here::here() %>% dirname(), "bayestransition_output", runname)

  return(fit)
}
