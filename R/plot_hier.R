#' Plot hierarchical model checks
#'
#' Creates diagnostic plots for hierarchical model parameters including
#' prior-posterior comparisons and mu_raw summaries.
#'
#' @param fit A fitted model object from \code{\link{fit_model}}
#' @param model_name Character string specifying the model type. Either "spline"
#'   (checks Omega, Ptilde, Betas) or "rw2" (checks Omega, gamma). Default is "spline".
#'
#' @return NULL (invisibly). Saves hierchecks.pdf to the output directory.
#'
#' @export
plot_hierchecks <- function(fit, model_name = "spline"){

  # Determine parameters based on model type
  if (model_name == "spline") {
    parnames <- c("Omega", "Ptilde", "Betas")
  } else if (model_name == "rw2") {
    parnames <- c("Omega", "gamma")
  } else {
    stop("model_name must be either 'spline' or 'rw2'")
  }

  pdf(file = file.path(fit$output_dir, "hierchecks.pdf"), width = 12, height = 8)

  ### post and prior sigma
  for (parname in parnames){
    print(localhierarchy::plot_prior_post_sigmas_localhierarchy(fit = fit, parname = parname))
  }

  # Omega, for both models
  #### mu_raws
  muraw_omega <- localhierarchy::plot_muraw_localhierarchy(fit = fit,
                                                           parname = "Omega")
  # for Omega (1param)
  print(muraw_omega[["summary_plots"]][[1]])
  for (i in 1:4){
    print(muraw_omega[["plots_allmuraw"]][[i]])
  }

  if (model_name == "spline") {
    # Ptilde and Betas for spline model
    muraw_ptilde <- localhierarchy::plot_muraw_localhierarchy(fit = fit, parname = "Ptilde")
    muraw_betas <- localhierarchy::plot_muraw_localhierarchy(fit = fit, parname = "Betas", morethan1param = TRUE)

    # Ptilde
    print(muraw_ptilde[["summary_plots"]][[1]])
    for (i in 1:4){
      print(muraw_ptilde[["plots_allmuraw"]][[i]])
    }

    # for splines: a list with plots
    # summary plots is a list of dimension k, for each k, it gives summaries, 30 at a time
    for (k in 1:7){
      print(muraw_betas[["summary_plots"]][[k]][[1]])
    }
    # specific plots for one parameter are in plots[["plots_allmuraw"]]
    # muraws suggest not much updating... slightly negative in intercept
    for (k in 1:7){
      p1 = muraw_betas[["plots_allmuraw"]][[k]][[1]] +
        ggtitle(paste("k = ", k)) +
        theme(legend.position = "none")
      p2 = muraw_betas[["plots_allmuraw"]][[k]][[2]]
      p3 = muraw_betas[["plots_allmuraw"]][[k]][[3]]
      p4 = muraw_betas[["plots_allmuraw"]][[k]][[4]]
      ggall = ggarrange(p1, p2, p3, p4,
                        ncol = 2, nrow = 2,
                        common.legend = TRUE,
                        legend = "bottom")
      print(ggall)
    }
  } else {
    # gamma for rw2 model
    muraw_gamma <- localhierarchy::plot_muraw_localhierarchy(
      fit = fit,
      parname = "gamma")

    # gamma
    print(muraw_gamma[["summary_plots"]][[1]])
    for (i in 1:4){
      print(muraw_gamma[["plots_allmuraw"]][[i]])
    }
  }

  ### hierarchical parameters
  omega <- localhierarchy::posterior_summary_hierparam_localhierarchy(fit = fit, parname = "Omega",
                                         hierarchical_levels = fit$hierarchical_level)

  p <- localhierarchy::plot_posterior_summaries_localhierarchy(res = omega)
  for (i in 1:length(p)){
    print(p[[i]] +  ggtitle(paste0("Omega, ", names(p)[i])))
  }

  if (model_name == "spline") {
    ptilde <- localhierarchy::posterior_summary_hierparam_localhierarchy(fit = fit, parname = "Ptilde",
                                            hierarchical_levels = fit$hierarchical_asymptote)
    betas <- localhierarchy::posterior_summary_hierparam_localhierarchy(fit = fit, parname = "Betas",
                                             hierarchical_levels = fit$hierarchical_splines,
                                             morethan1param =  TRUE)

    p <- localhierarchy::plot_posterior_summaries_localhierarchy(res = ptilde)
    for (i in 1:length(p)){
      print(p[[i]] +  ggtitle(paste0("Ptilde, ", names(p)[i])))
    }
    p <- localhierarchy::plot_posterior_summaries_localhierarchy(res = betas)
    for (i in 1:length(p)){
      print(p[[i]] +  ggtitle(paste0("Betas, ", names(p)[i])))
    }
  } else {
    gamma <- localhierarchy::posterior_summary_hierparam_localhierarchy(fit = fit, parname = "gamma",
                                             hierarchical_levels = fit$hierarchical_level)

    p <- localhierarchy::plot_posterior_summaries_localhierarchy(res = gamma)
    for (i in 1:length(p)){
      print(p[[i]] +  ggtitle(paste0("gamma, ", names(p)[i])))
    }
  }

  dev.off()
  return(NULL)
}
