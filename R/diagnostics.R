
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
  # to do: add ess too
  if (max(summ$rhat) > 1.05){
    warning("Some parameters have Rhat > 1.05")
  } else {
    message("All parameters have Rhat < 1.05")
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
