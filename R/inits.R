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

                   caux_dm_estimate = 1 + abs(rnorm(1, 0, 0.25))

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
