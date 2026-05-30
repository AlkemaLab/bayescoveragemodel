# inits.R

# # function to increase elements in each column in decreasing order
# # not bullet proof, just use for inits
# sort_columns_decrease <- function(a # matrix or vector
#                                   ){
#   if (length(dim(a)) ==2){
#     for (i in 1:ncol(a)){
#       a[,i] <- rev(sort(a[,i]))
#     }
#   } else {
#     a <- rev(sort(a))
#   }
#   return(a)
# }
# #a <- matrix(rnorm(4), 2,2)
# #a
# #sort_columns(a)


init_fun <- function(chain_id, stan_data){

  #k <- stan_data$num_basis - stan_data$num_constrained_zero
  inits <- list()

  set.seed(chain_id*2)

  # smoothing
  if (stan_data$smoothing ==1){
    inits <- c(inits,
               list(
    epsilon_innovation = matrix(0, stan_data$n_geounit* stan_data$smoothing, stan_data$T * stan_data$smoothing)
                ))


    if (!stan_data$fix_smoothing){
      inits <- c(inits,
                 list(
      rho_estimate = 0.1, #runif(1,0,0.3),
      tau_estimate = runif(1,0.01,0.02)
                 ))
    }
  } # end smoothing

  # dm
  if (!stan_data$fix_nonse){
    inits <- c(inits,
               list(
    nonse_estimate = abs(rnorm(stan_data$S, 0, 0.1))
               ))
    if (stan_data$add_dataoutliers){
      inits <- c(inits,
                 list(
                   local_shrinkage_dm = rep(0.001, stan_data$N),

                   global_shrinkage_dm_estimate = abs(rnorm(1, 0, 0.01)),

                   # Note: Stan parameter is sqrt_caux_dm_estimate (not caux_dm_estimate)
                   # Prior is half-normal(0, 1), so initialize near 1
                   sqrt_caux_dm_estimate = abs(rnorm(1, 0, 0.5))

                 ))
    }
  } # end dm


  # # transition model parameters
  if (stan_data$Betas_raw_n_terms_estimate > 0){
    inits <- c(inits,
               list(
    Betas_raw_estimate = matrix(rnorm(stan_data$Betas_raw_n_terms_estimate*stan_data$k,0, 0.01),
                                stan_data$Betas_raw_n_terms_estimate, stan_data$k)
               ))
  }
  if (stan_data$Betas_n_sigma_estimate > 0){
    inits <- c(inits,
               list(
      # Betas_sigma_estimate =
      #sort_columns_decrease(matrix(abs(rnorm(stan_data$Betas_n_sigma_estimate*k, 0, 0.01)),
       #          stan_data$Betas_n_sigma_estimate, k)),
                 Betas_sigma_estimate_reverse_1 = sort(abs(rnorm(stan_data$Betas_n_sigma_estimate, 0, 0.01))),
                 Betas_sigma_estimate_reverse_2 = sort(abs(rnorm(stan_data$Betas_n_sigma_estimate, 0, 0.01))),
                 Betas_sigma_estimate_reverse_3 = sort(abs(rnorm(stan_data$Betas_n_sigma_estimate, 0, 0.01))),
                 Betas_sigma_estimate_reverse_4 = sort(abs(rnorm(stan_data$Betas_n_sigma_estimate, 0, 0.01)))
               ))
  }

  if (stan_data$Ptilde_raw_n_terms_estimate > 0){
    inits <- c(inits,
               list(
                 # to update inits
                 Ptilde_raw_estimate = (rnorm(stan_data$Ptilde_raw_n_terms_estimate, 1, 0.01))
               ))
  }
  if (stan_data$Ptilde_n_sigma_estimate > 0){
    inits <- c(inits,
               list(
                 Ptilde_sigma_estimate_reverse = sort(abs(rnorm(stan_data$Ptilde_n_sigma_estimate, 0, 0.01)))
               ))
  }

  if (stan_data$Omega_raw_n_terms_estimate > 0){
    inits <- c(inits,
               list(
                 Omega_raw_estimate = (rnorm(stan_data$Omega_raw_n_terms_estimate, 0, 0.01))
               ))
  }

  if (stan_data$Omega_n_sigma_estimate > 0){
    inits <- c(inits,
               list(
                 Omega_sigma_estimate = (abs(rnorm(stan_data$Omega_n_sigma_estimate, 0, 0.01)))
               ))
  }

  if ("gamma_n_sigma_estimate" %in% names(stan_data)){
    if (stan_data$gamma_raw_n_terms_estimate > 0){
      inits <- c(inits,
                 list(
                   gamma_raw_estimate = (rnorm(stan_data$gamma_raw_n_terms_estimate, 0, 0.01))
                 ))
    }
     if (stan_data$gamma_n_sigma_estimate > 0){
       inits <- c(inits,
                  list(
                    gamma_sigma_estimate_reverse = sort(abs(rnorm(stan_data$gamma_n_sigma_estimate, 0, 0.01)))
                  ))
    }
  }

  if (!stan_data$fix_subnat_corr){
    inits <- c(inits,
               list(
                 rho_correlationeps_estimate = runif(1,0,0.3)
               ))
  }
  return(inits)
}

#' Fix initialization dimensions for rstan backend
#'
#' RStan (using older Stan 2.21) requires explicit array dimensions, while
#' cmdstanr (using newer Stan 2.35+) is more flexible. This function ensures
#' that all vector parameters in the initialization have proper dimensions
#' for rstan compatibility.
#'
#' The function uses a systematic approach to classify parameters:
#' - Known scalars (single values that should stay as scalars)
#' - Known matrices (2D arrays)
#' - Everything else is treated as a vector and gets explicit dimensions
#'
#' @param init_list List returned by init_fun
#' @param backend Character string: "cmdstanr" or "rstan"
#'
#' @return Modified init_list with proper dimensions for rstan
#' @keywords internal
fix_init_dims_for_rstan <- function(init_list, backend = "cmdstanr") {
  if (backend != "rstan") {
    return(init_list)
  }

  # Parameters that should remain as scalars (no dimension attribute)
  # IMPORTANT: There are NO true scalars in the Stan model!
  # All parameters that look like scalars are actually array[1] in Stan:
  # - rho_estimate, tau_estimate: array[smoothing * (1 - fix_smoothing)]
  # - global_shrinkage_dm_estimate, caux_dm_estimate: array[add_dataoutliers * (1-fix_nonse)]
  # - rho_correlationeps_estimate: array[fix_subnat_corr ? 0 : 1]
  # Therefore, this list should remain EMPTY - all are treated as vectors
  known_scalars <- c()

  # Parameters that are matrices (already have 2D structure)
  known_matrices <- c(
    "Betas_raw_estimate",
    "epsilon_innovation"
  )

  # Process each parameter in the init list
  for (param_name in names(init_list)) {
    val <- init_list[[param_name]]

    if (is.null(val) || length(val) == 0) {
      next  # Skip NULL or empty values
    }

    # Handle known scalars - ensure they have NO dimensions
    if (param_name %in% known_scalars) {
      if (length(val) != 1) {
        warning(sprintf("Expected scalar for %s but got length %d", param_name, length(val)))
      }
      dim(init_list[[param_name]]) <- NULL
      next
    }

    # Handle known matrices - verify they have proper 2D structure
    if (param_name %in% known_matrices) {
      if (!is.matrix(val)) {
        warning(sprintf("%s should be a matrix but isn't - attempting to fix", param_name))
        # Try to convert to matrix (may need additional logic based on expected dimensions)
        dim(init_list[[param_name]]) <- c(length(val), 1)
      }
      # If already a matrix, leave as is (dimensions already set)
      next
    }

    # All other parameters are assumed to be vectors/arrays
    # This includes:
    # - All *_estimate variables (Ptilde_sigma_estimate_reverse, Omega_raw_estimate, etc.)
    # - All *_sigma_estimate_reverse_* variables (Betas_sigma_estimate_reverse_1, etc.)
    # - Vector parameters (nonse_estimate, local_shrinkage_dm, etc.)
    if (!is.matrix(val)) {
      # Set explicit dimension for vectors
      # This is critical for rstan which distinguishes between scalar and vector[1]
      dim(init_list[[param_name]]) <- length(val)
    }
  }

  return(init_list)
}
