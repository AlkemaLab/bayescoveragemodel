#' Extract Residuals from Posterior Samples
#'
#' Extracts residuals from a fitted model object by comparing observed data
#' to posterior draws of eta_i (predicted coverage).
#'
#' @param fit Fitted model object containing:
#'   \item{samples}{cmdstan samples object with eta_i and scale parameters}
#'   \item{stan_data}{List with y (observations) and held_out flag}
#'
#' @return Nested tibble with one row per observation containing:
#'   \item{obs_index}{Observation index}
#'   \item{y}{Observed value on modeled scale}
#'   \item{y_prop}{Observed value on proportion scale}
#'   \item{held_out}{Logical indicating if observation was held out}
#'   \item{iso}{Country ISO code}
#'   \item{year}{Year of observation}
#'   \item{name_region}{Region name}
#'   \item{draws}{Nested tibble with draw-specific columns:
#'     \itemize{
#'       \item draw: Posterior draw number
#'       \item yhat: Predicted mean on modeled scale (= level)
#'       \item sd_y: Standard deviation for observation on modeled scale
#'       \item level: Level on modeled (inv-probit) scale
#'       \item level_prop: Level on proportion scale
#'       \item sd_y_prop: Standard deviation on proportion scale
#'       \item y_sim: Simulated observation (validation runs only)
#'     }}
#'
#' @importFrom tidybayes spread_draws
#' @importFrom dplyr mutate select ungroup rename left_join filter pull all_of
#' @importFrom tidyr nest
#' @export
get_residuals_samples <- function(fit) {

  if (!"samples" %in% names(fit) || !"stan_data" %in% names(fit)) {
    stop("The input 'fit' must contain 'samples' and 'stan_data'.")
  }

  # Check if fit$data matches stan_data
  if (nrow(fit$data) != length(fit$stan_data$y)) {
    stop("Number of rows in fit$data does not match number of observations used")
  }

  # Create observation-level data with metadata
  y_df <- tibble::tibble(
    obs_index = seq_along(fit$stan_data$y),
    y = fit$stan_data$y,
    held_out = as.logical(fit$stan_data$held_out),
    iso = fit$data$iso,
    year = fit$data$year,
    name_region = fit$data$name_region,
    data_series_type = fit$data$data_series_type
  ) %>%
    dplyr::mutate(y_prop = probit(y))

  # Determine if validation run
  is_validation <- any(y_df$held_out)
  if (is_validation) message("Validation run")

  # Extract draws - include y_sim for validation runs
  params <- if (is_validation) c("eta_i", "scale", "y_sim") else c("eta_i", "scale")
  draws <- fit$samples$draws(params)

  if (is_validation) {
    draws <- draws %>%
      tidybayes::spread_draws(eta_i[i], scale[i], y_sim[i])
  } else {
    draws <- draws %>%
      tidybayes::spread_draws(eta_i[i], scale[i])
  }

  draws <- draws %>%
    dplyr::ungroup() %>%
    dplyr::select(-.chain, -.iteration) %>%
    dplyr::rename(draw = .draw, obs_index = i)

  # Filter to held_out observations for validation runs
  if (is_validation) {
    held_out_indices <- y_df %>%
      dplyr::filter(held_out) %>%
      dplyr::pull(obs_index)
    # also remove countries without data in training set
    y_df <- y_df %>%
      dplyr::group_by(iso) %>%
      dplyr::mutate(n_train = sum(!held_out)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_train > 0, held_out,  data_series_type == "DHS")
    draws <- draws %>%
      dplyr::filter(obs_index %in% y_df$obs_index)
  }

  # Add scale transformations
  draws <- draws %>%
    dplyr::mutate(
      level_prop = eta_i,
      level = inv_probit(level_prop),
      yhat = level,
      sd_y = scale,
      sd_y_prop = get_se_probitofinvprobitprop(level, sd_y)
    )

  # Select draw columns - include y_sim for validation runs
  draw_cols <- c("obs_index", "draw", "yhat", "sd_y", "level", "level_prop", "sd_y_prop")
  if (is_validation) draw_cols <- c(draw_cols, "y_sim")
  draws <- draws %>%
    dplyr::select(dplyr::all_of(draw_cols))

  # Nest draws by observation and join with observation data
  draws_nested <- draws %>%
    tidyr::nest(draws = -obs_index)

  df <- y_df %>%
    dplyr::left_join(draws_nested, by = "obs_index") %>%
    dplyr::select(obs_index, y, y_prop, held_out, iso, year, name_region, data_series_type,
                  draws)

  return(df)
}
