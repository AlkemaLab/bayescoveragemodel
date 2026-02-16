#' Get convergence diagnostics for a fitted model
#'
#' Computes and saves convergence diagnostics (Rhat, ESS) for hyperparameters
#' and creates density overlay plots.
#'
#' @param fit A fitted model object from \code{\link{fit_model}}
#'
#' @return NULL (invisibly). Saves diagnostics.csv and diagnostics.pdf to the output directory.
#'
#' @export
get_convergence_diagnostics <- function(fit){
  # just the Rhats and some plots for hyperparameters

  #### Rhats
  # Rhats etc for all parameters may be too many
  # do Rhats for hyperparameters for sure
  # here are some
  # to add param_raw, eg [1: min(30, length(fit$samples$draws("param_raw")))]

  parnames <- c("Ptilde_sigma_estimate",
                "Omega_sigma_estimate",
                "Betas_sigma_estimate",
                "nonse_estimate",
                "global_shrinkage_dm_estimate", "sqrt_caux_dm_estimate")
  # add ar parameters
  summ <- fit$samples$summary(parnames) %>%
    arrange(desc(rhat))
  write_csv(summ, file = file.path(fit$output_dir, "diagnostics.csv"))

  # Check Rhat convergence
  if (max(summ$rhat, na.rm = TRUE) > 1.05){
    warning("Some parameters have Rhat > 1.05")
  } else {
    message("All parameters have Rhat < 1.05")
  }

  # Check effective sample size (ESS)
  min_ess_bulk <- min(summ$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(summ$ess_tail, na.rm = TRUE)
  if (min_ess_bulk < 400) {
    warning(sprintf("Low bulk ESS detected (min = %.0f). Consider running more iterations.", min_ess_bulk))
  } else {
    message(sprintf("Bulk ESS adequate (min = %.0f)", min_ess_bulk))
  }
  if (min_ess_tail < 400) {
    warning(sprintf("Low tail ESS detected (min = %.0f). Consider running more iterations.", min_ess_tail))
  } else {
    message(sprintf("Tail ESS adequate (min = %.0f)", min_ess_tail))
  }

  print("saving some densities")
  plot_dirandnameb <- file.path(fit$output_dir, "diagnostics.pdf")
  pdf(plot_dirandnameb, width = 11, height = 11)
  for (i in 1:length(parnames)){
    parname <- parnames[i]
    samp <- fit$samples$draws(parname )
    # can produce trace plots too
    #mcmc_trace(samp)
    p <- mcmc_dens_overlay(samp)
    # for more than 1 parameter
    #mcmc_pairs(samp)
    print(p)
  }
  dev.off()

  return(NULL)
}
