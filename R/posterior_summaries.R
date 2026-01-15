get_posterior_summaries_andfindpar <- function(
    fit,
    process_indicator_prefixes = c(""),
    dm_indicator_prefixes = c(""),
    process_params = c("Betas_raw", "Betas_sigma", "Ptilde_raw", "Ptilde_sigma",
                       "Omega_raw", "Omega_sigma"),
    process_smoothing_params = c("rho", "tau"),
    process_subnat_smoothing_params = c("rho_correlationeps"),
    dm_params = c("nonse", "sdbias"),
    dm_outlier_params = c(#"local_shrinkage_dm",# no use case to re-use
                  "global_shrinkage_dm",
                          "caux_dm")) {
  if (fit$smoothing) {
    process_params <- c(process_params, process_smoothing_params)
  }
  if (fit$correlated_smoothing) {
    process_params <- c(process_params, process_subnat_smoothing_params)
  }
  if (fit$add_dataoutliers) {
    dm_params <- c(dm_params, dm_outlier_params)
  }

  params_to_collect <- dplyr::bind_rows(
    expand.grid(
      prefix = process_indicator_prefixes,
      param = process_params),
    expand.grid(
      prefix = dm_indicator_prefixes,
      param = dm_params)
  ) |>
    dplyr::mutate(prefixed_param_name = paste0(prefix, param))

  fixable_params_to_collect <- params_to_collect |>
    dplyr::pull(prefixed_param_name)

  res <- localhierarchy::get_posterior_summaries_localhierarchy(
    fit, params = fixable_params_to_collect)

  return(res)
}


