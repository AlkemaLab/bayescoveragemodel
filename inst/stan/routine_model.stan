  // routine data model
  if (N_routine > 0) {
   log_sigma_delta ~ normal(mean_log_sigma, hierarchical_sigma); // one term per geounit
   for (j in 1:N_routine){
      real roc_routine_totalsd_j = sqrt(sd2_routine_roc[j] + exp(log_sigma_delta[c_routine_j[j]])^2);
      real eta_jt = inv_tr_eta(tr_eta_obs[c_routine_j[j], t_routine_j[j]]);
      real eta_jtmin1 = inv_tr_eta(tr_eta_obs[c_routine_j[j], t_routine_j[j]-1]);
      routine_roc[j] ~ normal(eta_jt - eta_jtmin1, roc_routine_totalsd_j);
   }
  }


