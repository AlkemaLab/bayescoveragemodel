#' Extract posterior samples of eta (latent coverage) for a specific year
#'
#' Extracts posterior draws of the latent coverage parameter eta from a fitted
#' model object for a specified year. For validation runs, only includes
#' countries that have both training and test data.
#'
#' @param fit A fitted model object containing `samples` (cmdstanr or rstan fit object),
#'   `geo_unit_index`, `time_index`, `stan_data`, and `data`.
#' @param year_select The year for which to extract eta samples. Default is 2023.
#'   Ignored if `countryyear_select` is not NULL.
#' @param countryyear_select A tibble with columns `iso` and `year` specifying
#'   country-year combinations to extract. If not NULL, takes precedence over
#'   `year_select`. Default is NULL.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{iso}{Country ISO code}
#'     \item{year}{Selected year}
#'     \item{eta}{Posterior draw of eta (probit-scale coverage)}
#'     \item{draw}{Draw number}
#'     \item{cluster, subcluster, name_region}{Additional geographic identifiers if present}
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- load_fit("my_model_run")
#' eta_2023 <- get_eta_samples(fit, year_select = 2023)
#'
#' # Extract specific country-year combinations
#' cy_select <- tibble::tibble(iso = c("USA", "CAN"), year = c(2023, 2022))
#' eta_subset <- get_eta_samples(fit, countryyear_select = cy_select)
#' }
get_eta_samples <- function(fit, year_select = 2023, countryyear_select = NULL) {

  if (!"samples" %in% names(fit)) {
    stop("The input 'fit' must contain 'samples'.")
  }

  iso_codes <- fit$geo_unit_index |>
    dplyr::rename(C = c)

  year_index <- fit$time_index |>
    dplyr::rename(T = t)

  # Determine if validation run
  is_validation <- any(fit$stan_data$held_out == 1)
  if (is_validation){
    # exclude countries that have no data in train or validation
    # general code improvement: reconsider where to do this
    y_df <- tibble::tibble(
      held_out = as.logical(fit$stan_data$held_out),
      iso = fit$data$iso
    ) |>
      dplyr::group_by(iso) |>
      dplyr::mutate(n_train = sum(held_out == 0),
             n_test = sum(held_out == 1)) |>
      dplyr::ungroup() |>
      dplyr::filter(n_train > 0, n_test > 0)
    iso_include <- unique(y_df$iso)
  } else {
    iso_include <- unique(fit$data$iso)
  }

  # Extract draws
  params <- c("eta")
  draws <- extract_draws(fit, params) |>
    tidybayes::spread_draws(eta[C, T]) |>
    dplyr::ungroup() |>
    dplyr::select(-.chain, -.iteration) |>
    dplyr::rename(draw = .draw) |>
    dplyr::left_join(iso_codes, by = "C") |>
    dplyr::left_join(year_index, by = "T")

  # Filter by country-year combinations or year
  if (!is.null(countryyear_select)) {
    # countryyear_select takes precedence
    draws <- draws |>
      dplyr::inner_join(countryyear_select, by = c("iso", "year")) |>
      dplyr::filter(iso %in% iso_include)
  } else {
    # Use year_select
    draws <- draws |>
      dplyr::filter(year == year_select,
                    iso %in% iso_include)
  }

  draws <- draws |>
    dplyr::select(iso, year, eta, draw,
                  dplyr::any_of(c("cluster", "subcluster", "name_region")))

  return(draws)
}
