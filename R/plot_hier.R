
plot_hierchecks <- function(fit, is_married){
  pdf(file = file.path(fit$output_dir, "hierchecks.pdf"), width = 12, height = 8)

  ### post and prior sigma
  for (parname in c("Omega", "Ptilde", "Betas")){
    print(plot_prior_post_sigmas(fit = fit, parname = parname))
  }

  #### mu_raws
  muraw_omega1a <- plot_mu_raw(fit = fit, parname = "Omega")
  muraw_ptilde1a <- plot_mu_raw(fit = fit, parname = "Ptilde")
  muraw_splines1a <- plot_mu_raw(fit = fit, parname = "Betas", morethan1param = TRUE)

  # for Omega (1param)
  print(muraw_omega1a[["summary_plots"]][[1]])
  for (i in 1:4){
    print(muraw_omega1a[["plots_allmuraw"]][[i]])
  }

  # Ptilde
  print(muraw_ptilde1a[["summary_plots"]][[1]])
  for (i in 1:4){
    print(muraw_ptilde1a[["plots_allmuraw"]][[i]])
  }

  # for splines: a list with plots
  # summary plots is a list of dimension k, for each k, it gives summaries, 30 at a time
  for (k in 1:7){
    print(muraw_splines1a[["summary_plots"]][[k]][[1]])
  }
  # specific plots for one parameter are in plots[["plots_allmuraw"]]
  # muraws suggest not much updating... slightly negative in intercept
  for (k in 1:7){
    p1 = muraw_splines1a[["plots_allmuraw"]][[k]][[1]] +
      ggtitle(paste("k = ", k)) +
      theme(legend.position = "none")
    p2 = muraw_splines1a[["plots_allmuraw"]][[k]][[2]]
    p3 = muraw_splines1a[["plots_allmuraw"]][[k]][[3]]
    p4 = muraw_splines1a[["plots_allmuraw"]][[k]][[4]]
    ggall = ggarrange(p1, p2, p3, p4,
                      ncol = 2, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")
    print(ggall)
  }


  ### hierarchical parameters
  omega1a <- posterior_summary_hierparam(fit = fit, parname = "Omega",
                                         hierarchical_levels = fit$hierarchical_level)
  ptilde1a <- posterior_summary_hierparam(fit = fit, parname = "Ptilde",
                                          hierarchical_levels = fit$hierarchical_asymptote)
  splines1a <- posterior_summary_hierparam(fit = fit, parname = "Betas",
                                           hierarchical_levels = fit$hierarchical_splines,
                                           morethan1param =  TRUE)

  p <- plot_posterior_summaries(res = omega1a)
  for (i in 1:length(p)){
    print(p[[i]] +  ggtitle(paste0("Omega, ", names(p)[i])))
  }
  p <- plot_posterior_summaries(res = ptilde1a)
  for (i in 1:length(p)){
    print(p[[i]] +  ggtitle(paste0("Ptilde, ", names(p)[i])))
  }
  p <- plot_posterior_summaries(res = splines1a)
  for (i in 1:length(p)){
    print(p[[i]] +  ggtitle(paste0("Betas, ", names(p)[i])))
  }

  dev.off()
  return(NULL)
}
