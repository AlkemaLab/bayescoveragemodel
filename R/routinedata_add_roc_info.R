#' Calculate Rate of Change and DQ Indicators
#'
#' This function calculates rate of change metrics and data quality (DQ) indicators
#' for routine coverage data. It computes year-over-year changes and creates
#' "start" and "worst" versions of DQ covariates for consecutive year pairs.
#'
#' @param data A data frame containing routine data with columns: iso (country code),
#'   year, indicator_name, routine_value, and optionally median_estimate. Must also
#'   contain the DQ covariate columns specified in the dq_covariates_* parameters.
#' @param add_est_roc Logical. If TRUE, calculates rate of change for median_estimate
#'   in addition to routine_value. Default is FALSE.
#' @param dq_covariates_min Character vector of DQ covariate names where "worst"
#'   means minimum value between current and start. Defaults to c("countdownmean").
#' @param dq_covariates_max Character vector of DQ covariate names where "worst"
#'   means maximum value between current and start. Default is empty vector.
#' @param dq_covariates_max_abs Character vector of DQ covariate names where "worst"
#'   means maximum absolute value between current and start. Default is c().
#'
#' @return A data frame with the original columns plus:
#'   \itemize{
#'     \item routine_roc: Year-over-year change in routine_value (always added)
#'     \item est_roc: Year-over-year change in median_estimate (if add_est_roc = TRUE)
#'     \item [var]_start: Lagged value of each DQ covariate for consecutive years
#'     \item worst_[var]: Worst (min or max) value between current and start for each DQ covariate
#'     \item worst_abs_[var]: Worst (max absolute) value for covariates in dq_covariates_max_abs
#'   }
#'   Note: Rate of change and DQ values are only calculated for consecutive years
#'   (i.e., when year - lag(year) == 1), otherwise NA.
#'
#' @examples
#' library(dplyr)
#'
#' # Create toy data
#' toy_data <- data.frame(
#'   iso = rep(c("CD1", "CD2"), each = 3),
#'   year = rep(2020:2022, 2),
#'   indicator_name = "dtp3",
#'   routine_value = c(0.85, 0.87, 0.90, 0.80, 0.82, 0.85),
#'   median_estimate = c(88, 89, 91, 83, 84, 87),
#'   notoutliers = c(100, 95, 90, 100, 100, 95),
#'   notmissing = c(100, 100, 95, 100, 95, 90),
#'   rr = c(95, 90, 85, 90, 88, 85),
#'   diff_level = c(3, 2, 1, 3, 2, 2)
#' )
#'
#' # Apply function
#' result <- routinedata_add_roc_info(
#'   toy_data,
#'   add_est_roc = TRUE,
#'   dq_covariates_min = c("notoutliers", "notmissing", "rr"),
#'   dq_covariates_max_abs = c("diff_level")
#' )
#'
#' # View results for CD1
#' result %>% filter(iso == "CD1") %>% select(year, routine_value, routine_roc, rr, worst_rr)
#'
#' @export
routinedata_add_roc_info <- function(data,
                                     add_est_roc = FALSE,
                                     dq_covariates_min = c("countdownmean"),
                                     dq_covariates_max = c(),
                                     dq_covariates_max_abs = c()) {

  # Start with the data
  result <- data |>
    arrange(iso, year) |>
    group_by(iso, indicator_name)

  # Build expressions for mutate
  mutate_exprs <- list()

  mutate_exprs$routine_roc <- quote(ifelse((year - lag(year)) == 1, routine_value - lag(routine_value), NA))
  # Add est_roc if requested
  if (add_est_roc) {
    mutate_exprs$est_roc <- quote(ifelse((year - lag(year)) == 1, median_estimate - lag(median_estimate), NA))
  }

  # Combine all DQ covariates
  all_dq_covariates <- c(dq_covariates_min, dq_covariates_max, dq_covariates_max_abs)

  # Add _start versions for all DQ covariates
  for (var in all_dq_covariates) {
    var_sym <- as.symbol(var)
    start_name <- paste0(var, "_start")
    mutate_exprs[[start_name]] <- bquote(ifelse((year - lag(year)) == 1, lag(.(var_sym)), NA))
  }

  # Add worst versions using pmin
  for (var in dq_covariates_min) {
    var_sym <- as.symbol(var)
    start_sym <- as.symbol(paste0(var, "_start"))
    worst_name <- paste0("worst_", var)
    mutate_exprs[[worst_name]] <- bquote(pmin(.(var_sym), .(start_sym), na.rm = FALSE))
  }

  # Add worst versions using pmax
  for (var in dq_covariates_max) {
    var_sym <- as.symbol(var)
    start_sym <- as.symbol(paste0(var, "_start"))
    worst_name <- paste0("worst_", var)
    mutate_exprs[[worst_name]] <- bquote(pmax(.(var_sym), .(start_sym), na.rm = FALSE))
  }

  # Add worst versions using pmax with abs()
  for (var in dq_covariates_max_abs) {
    var_sym <- as.symbol(var)
    start_sym <- as.symbol(paste0(var, "_start"))
    worst_name <- paste0("worst_abs_", var)
    mutate_exprs[[worst_name]] <- bquote(pmax(abs(.(var_sym)), abs(.(start_sym)), na.rm = FALSE))
  }

  # Apply all mutations
  result <-
    result |>
    mutate(!!!mutate_exprs) |>
    ungroup()

  return(result)
}
