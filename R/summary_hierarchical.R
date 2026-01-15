
summary_hierarchical <- function(fit){
  print("adding some plots with info on hierarchical parameters")
  print("warnings about multiple groupings can be ignored")

  # info on splines coefficients a: mu-raws, sigmas, total parameter estimates
  plots <- plot_mu_raw(fit = fit, parname = "Betas", morethan1param = TRUE)
  pdf(file.path(fit$output_dir, paste0("splinesa.pdf")), width = 12, height = 8)
  for (k in 1:7){
    p <- plots[["summary_plots"]][[k]][[1]] +
      ggtitle(paste0("Betas mu_raw ", k)) +
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }
  print(plot_prior_post_sigmas(fit = fit, parname = "a"))
  dev.off()

  # info on Ptilde
  plots <- plot_mu_raw(fit = fit, parname = "Ptilde")
  pdf(file.path(fit$output_dir, paste0("Ptilde.pdf")), width = 12, height = 8)
  p <- plots[["summary_plots"]][[1]]
  print(p)
  print(plot_prior_post_sigmas(fit = fit, parname = "Ptilde"))
  res_global3 <- posterior_summary_hierparam(fit = fit, parname = "Ptilde",
                                             hierarchical_levels = fit$hierarchical_asymptote)
  p <- plot_posterior_summaries(res = res_global3)
  print(p)
  dev.off()

  # info on Omega
  plots <- plot_mu_raw(fit = fit, parname = "Omega")
  pdf(file.path(fit$output_dir, paste0("Omega.pdf")), width = 12, height = 8)
  p <- plots[["summary_plots"]][[1]]
  print(p)
  print(plot_prior_post_sigmas(fit = fit, parname = "Omega"))
  dev.off()
  return(invisible(NULL))
}
