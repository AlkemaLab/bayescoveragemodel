#' Extract posterior samples of eta (latent coverage) for a specific year
#'
#' Extracts posterior draws of the latent coverage parameter eta from a fitted
#' model object for a specified year. For validation runs, only includes
#' countries that have both training and test data.
#'
#' @param fit A fitted model object containing `samples` (cmdstanr draws),
#'   `geo_unit_index`, `time_index`, `stan_data`, and `data`.
#' @param year_select The year for which to extract eta samples. Default is 2023.
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
#' }
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
    # to consider: reconsider where to do this
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
                  iso %in% iso_include) %>%
    dplyr::select(iso, year, eta, draw,
                  dplyr::any_of(c("cluster", "subcluster", "name_region")))

  return(draws)
}
