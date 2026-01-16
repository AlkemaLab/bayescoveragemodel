#' Extract Innovation Samples from Fitted Model
#'
#' Extracts epsilon_innovation (AR1 process innovations) and eta from posterior
#' samples. These represent the temporal deviations from the expected coverage
#' trajectory along with the underlying fitted values.
#'
#' @param fit Fitted model object containing:
#'   \item{samples}{cmdstan samples with epsilon_innovation CT and eta CT}
#'   \item{data}{Data frame with iso, country, name_region, year}
#'   \item{time_index}{Time index mapping with columns t and year}
#'
#' @return Nested tibble with one row per (iso, year) containing:
#'   \item{iso}{ISO country code}
#'   \item{year}{Year}
#'   \item{name_region}{Region name}
#'   \item{draws}{Nested tibble with draw-specific columns:
#'     \itemize{
#'       \item draw: Posterior draw number
#'       \item eta: Eta parameter on probit scale
#'       \item sd_y: Scale parameter (always 1 for standardized innovations)
#'       \item residual: Innovation value (epsilon_innovation)
#'       \item level: Fitted coverage on inv-probit scale
#'       \item level_prop: Eta on proportion scale
#'       \item yhat: Fitted value on inv-probit scale (same as level)
#'       \item y: Reconstructed observation (residual + yhat)
#'       \item sd_y_prop: Standard deviation on proportion scale
#'       \item y_prop: Reconstructed observation on proportion scale
#'     }}
#'
#' @details Only returns innovations for years within the observation range
#' for each country (min_obs_yr to max_obs_yr).
#'
#' @importFrom tidybayes spread_draws
#' @importFrom tidyr nest
#' @export
get_inno_samples <- function(fit) {

  if (!"samples" %in% names(fit) || !"data" %in% names(fit)) {
    stop("The input 'fit' must contain 'samples' and 'data'.")
  }

  # fit$data corresponds with standata$y
  model_data <- fit$data %>%
    tibble::as_tibble()

  obs_yrs <- model_data %>%
    dplyr::group_by(iso) %>%
    dplyr::summarise(
      min_obs_yr = min(year, na.rm = TRUE),
      max_obs_yr = max(year, na.rm = TRUE)
    )

  n_countries <- length(unique(model_data$iso))



  iso_codes <- fit$data %>%
    dplyr::select(iso, name_region) %>%
    unique() %>%
    dplyr::mutate(C = 1:n_countries)
  # to do: use this code but then change into cluster or subcluster or
  # get name_region later!
  # iso_codes <- fit$geo_unit_index %>%
  #   dplyr::rename(C = c)

  year_index <- fit$time_index %>%
    dplyr::rename(T = t)

  # Extract both epsilon_innovation and eta in a single call (much faster)
  draws <- fit$samples$draws(c("epsilon_innovation", "eta")) %>%
    tidybayes::spread_draws(epsilon_innovation[C, T], eta[C, T]) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.chain, -.iteration) %>%
    dplyr::rename(draw = .draw) %>%
    dplyr::left_join(iso_codes, by = "C") %>%
    dplyr::left_join(year_index, by = "T") %>%
    dplyr::left_join(obs_yrs, by = "iso") %>%
    dplyr::filter(year >= min_obs_yr, year <= max_obs_yr)

  # Add scale transformations (matches add_probit_scale_columns logic with sd_value = 1)
  draws <- draws %>%
    dplyr::mutate(
      residual = epsilon_innovation,
      level_prop = eta,
      level = inv_probit(level_prop),
      yhat = level,
      sd_y = 1,
      y = residual + yhat,
      sd_y_prop = get_se_probitofinvprobitprop(level, sd_y),
      y_prop = probit(y)
    ) %>%
    dplyr::select(iso, year, name_region, draw, eta, sd_y, residual,
                  level, level_prop, yhat, y, sd_y_prop, y_prop)

  # Nest draws by (iso, year)
  draws_nested <- draws %>%
    tidyr::nest(draws = c(draw, eta, sd_y, residual, level, level_prop,
                          yhat, y, sd_y_prop, y_prop))

  return(draws_nested)
}
