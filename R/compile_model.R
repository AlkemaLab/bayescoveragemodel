
compile_model <- function(force_recompile, #mypackage = "fpet2",
                          stan_file_path, backend = "cmdstanr"){

  # Check that requested backend is available
  if (backend == "cmdstanr" && !requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr is not installed. Install it with: install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))")
  }
  if (backend == "rstan" && !requireNamespace("rstan", quietly = TRUE)) {
    stop("rstan is not installed. Install it with: install.packages('rstan')")
  }

  if (backend == "cmdstanr") {
    #stan_file_path <- system.file("stan/fpem_buildingblocks_complete.stan", package = mypackage)

    stan_model <- cmdstanr::cmdstan_model(
      stan_file = stan_file_path,
      ## related to pre-instantiate code
      ##         include_paths = include_paths,
      # quiet = FALSE,
      force_recompile = force_recompile
    )

    ### note april 28, 2025
    ### functions are not found when higher up folders have spaces in it!!!
    # error with "fp packages and analyses/fpet2" but runs fine for "fp_packages/fpet2"
    ##### older code/comments related to using instantiate yes/no ####
    # before using instantiate, we have functions in an include folder
    # instantiate doesn't like that so we moved these functions in the model fit itself
    # in addition, the folder itself had to be renamed??
    # code that may or may not be helpful :)
    # include_paths <- system.file("include", package = "BayesCoverageIndicators")
    # include_paths <- here::here("inst/include/")
    #stan_file_path <- here::here("inst/stan/fpem.stan")
    ##stan_model <- instantiate::stan_package_model(name = "fpem_allwomen", package = "fpet2")

    #### extra code to consider for passing priors as strings ####
    # what does this code do:
    # allows the user to use TAGS in the stan model file and replace those by strings that are passed in as arguments
    # example:
    # use argument rho_prior = "dnorm(0, 0.5)" in fit_model call
    # {{RHO_PRIOR}} in stan model is replaced by this value, ie this is what code in stan model looks like
    #// rho_estimate[1] ~ {{RHO_PRIOR}} T[0, 1];
    #// tau_estimate[1] ~ {{TAU_PRIOR}} T[0, positive_infinity()];
    # Replace tags with correct values
    #      stan_code <- readr::read_file(stan_file_path) %>%
    #     stringr::str_replace_all("\\{\\{RHO_PRIOR\\}\\}", rho_prior) %>%
    #     stringr::str_replace_all("\\{\\{TAU_PRIOR\\}\\}", tau_prior)
    #     temp_stan_file_path <- cmdstanr::write_stan_file(stan_code)

  } else if (backend == "rstan") {
    # rstan compilation
    stan_model <- rstan::stan_model(
      file = stan_file_path,
      # Note: rstan doesn't have direct equivalent of force_recompile
      # It will recompile if the model code has changed
      auto_write = TRUE,  # cache compiled models
      verbose = FALSE
    )

  } else {
    stop("backend must be either 'cmdstanr' or 'rstan'")
  }

  return(stan_model)
}
