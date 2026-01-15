
// transition model for all populations considered
matrix process_model_returns_etatr(
    int n_geounit, int T, array[] int t_star,
    array[] int t_min, array[] int t_max,
    int smoothing, matrix epsilon,
    vector Omega,
    vector Ptilde,
    matrix tr_a_nonzero,
     int k, int num_basis,  vector ext_knots, int spline_degree
   ){

  matrix[n_geounit, num_basis] a;
  // set last num_constrained_zero coefficients to 0
  for(c in 1:n_geounit) {
     a[c, (k + 1):num_basis] = rep_row_vector(0, num_basis - k);
  }
  a[ , 1:k] = tr_a_nonzero;
  matrix[n_geounit, T] tr_eta;
  for(c in 1:n_geounit) {
    tr_eta[c, 1:T] = get_transition_1pop_returns_etatr(a[c,], Omega[c], Ptilde[c],
            smoothing,  epsilon[c,],
            //  int t_star, int t_min, int t_max, int T,
            t_star[c], t_min[c], t_max[c], T,
            ext_knots, num_basis, spline_degree);
  }
  return(tr_eta);
}


// transition model for 1 population
row_vector get_transition_1pop_returns_etatr(
    row_vector a,  real Omega, real P_tilde,
     int smoothing,
    row_vector epsilon,
    // matrix epsilon,
    int t_star, int t_min, int t_max, int T,
    vector ext_knots, int num_basis, int spline_degree){
  row_vector[T] tr_eta;
  real transition_function;
  tr_eta[t_star] =  Omega;
  // to do: add requirement somewhere that tmax >= tstar+1
  for(t in (t_star + 1):t_max) {
    transition_function =
      rate_spline(
       inv_tr_eta(tr_eta[t - 1]),
       P_tilde,
       a, ext_knots, num_basis, spline_degree);
    tr_eta[t] = tr_eta[t - 1] + transition_function + smoothing*epsilon[t];
  }
  // to do: add requirement somewhere that tmin <= tstar-1
  for(q in 1:(t_star - t_min)) {
    int t = t_star - q;
    transition_function =
    rate_spline(
        //0;
    //     rate_spline_noasymptote(
    // //    rate_constant(
      inv_tr_eta(tr_eta[t + 1]),
      P_tilde,
      a, ext_knots, num_basis, spline_degree);
    tr_eta[t] = tr_eta[t + 1] - transition_function - smoothing*epsilon[ t + 1];
  }
  // to do: fix
  // right now, added nonsense value for eta outside observation period
  // else stan crashes (can't apply the function to NAs)
  if (t_min > 1)
    tr_eta[1:(t_min-1)] = rep_row_vector(0,(t_min-1)) ;
  if (t_max < T)
    tr_eta[(t_max+1):T] = rep_row_vector(0,T - t_max) ;
  return(tr_eta);
}


// transition model for all populations considered outside obs period
matrix process_model_outsideobs(
  matrix tr_eta, // transformed eta of dime geo_unit x T with estimates given inside obs period
    int n_geounit, int T,
    array[] int t_min, array[] int t_max,
    int smoothing, matrix epsilon,
    vector Ptilde,
    matrix tr_a_nonzero,
     int k, int num_basis,  vector ext_knots, int spline_degree
   ){

  matrix[n_geounit, num_basis] a;
  // set last num_constrained_zero coefficients to 0
  for(c in 1:n_geounit) {
     a[c, (k + 1):num_basis] = rep_row_vector(0, num_basis - k);
  }
  a[ , 1:k] = tr_a_nonzero;
  matrix[n_geounit, T] shock = rep_matrix(0, n_geounit, T);
  matrix[n_geounit, T] tr_eta_again = tr_eta;
  for(c in 1:n_geounit) {
    tr_eta_again[c, 1:T] = get_transition_1pop_outsideobs(
            tr_eta[c, 1:T],
            a[c,],  Ptilde[c],
            smoothing,  epsilon[c,],
            //  int t_star, int t_min, int t_max, int T,
            t_min[c], t_max[c], T,
            ext_knots, num_basis, spline_degree);
  }
  return(tr_eta_again);
}

// transition model for 1 population outside obs period
row_vector get_transition_1pop_outsideobs(
  row_vector tr_eta_obs, // transformed eta of length T with estimates given inside obs period
    row_vector a,  real P_tilde,
     int smoothing,
    row_vector epsilon,
    // matrix epsilon,
    int t_min, int t_max, int T,
    vector ext_knots, int num_basis, int spline_degree){
  real transition_function;
  row_vector[T] tr_eta;
  tr_eta[t_min:t_max] = tr_eta_obs[t_min:t_max];
  if (t_max < T){
    for(t in (t_max + 1):T) {
      transition_function = rate_spline(inv_tr_eta(tr_eta[t - 1]),
       P_tilde,
       a, ext_knots, num_basis, spline_degree);
      tr_eta[t] = tr_eta[t - 1] + transition_function + smoothing*epsilon[t];
    }
  }
  if (t_min > 1){
    for(q in 1:(t_min - 1)) {
      int t = t_min - q;
      transition_function = rate_spline(inv_tr_eta(tr_eta[t + 1]),
        P_tilde,
        a, ext_knots, num_basis, spline_degree);
      tr_eta[t] = tr_eta[t + 1] - transition_function - smoothing*epsilon[ t + 1];
    }
  }
  return(tr_eta);
}

// transformation functions used in process model
real inv_tr_eta(real x) {
  return Phi_approx(x);
}
real tr_eta(real y) {
  return inv_Phi(y);
}

row_vector inv_tr_eta_vector(row_vector x) {
   return Phi_approx(x);
}

matrix inv_tr_eta_matrix(matrix x) {
   return Phi_approx(x);
}

row_vector tr_eta_vector(row_vector y) {
   return inv_Phi(y);
}

vector inv_tr_eta_colvector(vector x) {
// to do: finish with appropriate naming if we keep this
//   return 0.001 + 0.998/(1+exp(-x));
//  return 0.001 + 0.998*Phi_approx(x);
 //  return 0.001 + 0.998*Phi(x);
   return Phi_approx(x);
}
vector tr_eta_colvector(vector y) {
// to do: finish with appropriate naming if we keep this
//   return 0.001 + 0.998/(1+exp(-x));
   // no _approx version
//   return inv_Phi((y - 0.001)/0.998);
   // (y - ymin)/(ymax - ymin)
   return inv_Phi(y);
}

real rate_spline(real P, real P_tilde, row_vector a, vector ext_knots, int num_basis, int spline_degree) {
      return deboor(P / P_tilde, ext_knots, a, spline_degree);
    }

//   // extra functions for simplified fitting, non-efficient implementation to follow same for as rate_spline
//   // not yet fully finished/tested
//   real rate_constant(real P, real P_tilde, row_vector a, vector ext_knots, int num_basis, int spline_degree) {
//     return a[1]; // * P / P_tilde;
//     }
//
//
//  real rate_spline_noasymptote(real P, real P_tilde, row_vector a, vector ext_knots, int num_basis, int spline_degree) {
// //      row_vector[num_basis] spline_coeff = rep_row_vector(a[1], num_basis);
//       row_vector[num_basis] spline_coeff = rep_row_vector(0, num_basis);
//     //   vector[num_basis] weights = rep_vector(1, num_basis);
//     // return sum(weights*spline_coeff);
//     return deboor(P, ext_knots, spline_coeff, spline_degree);
//   }
//
//
//   real rate_constant_asymptote(real P, real P_tilde, row_vector a, vector ext_knots, int num_basis, int spline_degree) {
//     row_vector[num_basis] spline_coeff = rep_row_vector(a[1], num_basis);
//     return deboor(P / P_tilde, ext_knots, spline_coeff, spline_degree);
//   }





///////////

// // rw1 for 1 pop, not currently used/tested
// row_vector get_rw1_1pop(
//      real Omega,
//      int smoothing,
//     row_vector epsilon,
//     int t_star, int T){
//   row_vector[T] logit_eta;
//   logit_eta[t_star] =  Omega;
//   for(t in (t_star + 1):T) {
//     logit_eta[t] = logit_eta[t - 1] + smoothing*epsilon[t];
//   }
//   for(q in 1:(t_star - 1)) {
//     int t = t_star - q;
//     logit_eta[t] = logit_eta[t + 1] - smoothing*epsilon[ t + 1];
//   }
//   return(inv_logit_wbounds_vector(logit_eta));
// }

    // matrix[n_geounit, T] eta = rw1_model(n_geounit, T, t_star,
    //           smoothing,
    //           fix_smoothing,
    //           rho_fixed, rho_estimate,
    //           tau_fixed, tau_estimate,
    //           epsilon_innovation,
    //           fix_subnat_corr, rho_correlationeps_fixed, rho_correlationeps_estimate,
    //           correlated_smoothing, max_cor_smoothing_block_size, n_cor_smoothing_blocks,
    //           cor_smoothing_block_sizes,
    //           // omega
    //           Omega_sigma_fixed, Omega_sigma_estimate, 1,//Omega_prior_sglobal
    //           Omega_n_sigma, Omega_n_sigma_fixed, Omega_n_sigma_estimate,
    //           Omega_raw_fixed, Omega_raw_estimate,
    //           0, 1, //Omega_low = , Omega_up
    //           Omega_re_start, Omega_re_end,
    //           Omega_n_terms, Omega_n_terms_fixed, Omega_n_terms_estimate,
    //           Omega_model_matrix_w,  Omega_model_matrix_v,  Omega_model_matrix_u
    //          );

// // RW1 model, not currently used or tested
// matrix rw1_model(
//     int n_geounit, int T, array[] int t_star,
//     // smoothing
//     int smoothing,
//     int fix_smoothing, array[] real rho_fixed, array[] real rho_estimate,
//     array[] real tau_fixed , array[] real tau_estimate,
//     matrix epsilon_innovation,
//        int fix_subnat_corr,
//    array[] real rho_correlationeps_fixed, array[] real rho_correlationeps_estimate,
//     int correlated_smoothing, int max_cor_smoothing_block_size, int n_cor_smoothing_blocks,
//     array[] int cor_smoothing_block_sizes,
//     // omega
//     vector Omega_sigma_fixed, vector Omega_sigma_estimate, real Omega_prior_sd_global,
//     int Omega_n_sigma, int Omega_n_sigma_fixed, int Omega_n_sigma_estimate,
//     vector Omega_raw_fixed, vector Omega_raw_estimate,
//     real Omega_low, real Omega_up,
//     array[] int Omega_re_start, array[] int Omega_re_end,
//    int Omega_n_terms, int Omega_n_terms_fixed, int Omega_n_terms_estimate,
//     vector  Omega_model_matrix_w, array[] int Omega_model_matrix_v, array[] int Omega_model_matrix_u
//    ){
//
//   // smoothing
//   matrix[n_geounit, T] epsilon;
//   if (smoothing == 1) {
//     real rho_correlationeps = fix_subnat_corr ? rho_correlationeps_fixed[1] : rho_correlationeps_estimate[1];
//     real rho = fix_smoothing ? rho_fixed[1] : rho_estimate[1];
//     real tau = fix_smoothing ? tau_fixed[1] : tau_estimate[1];
//     epsilon = get_epsilon(rho, tau, epsilon_innovation, rho_correlationeps,
//                           n_geounit, T, t_star,
//                           correlated_smoothing, max_cor_smoothing_block_size, n_cor_smoothing_blocks,
//                           cor_smoothing_block_sizes);
//   } else {
//     epsilon = rep_matrix(0, n_geounit, T);
//   }
//
//   // Omega
//   vector[Omega_n_sigma] Omega_sigma = get_omega_sigma(Omega_sigma_fixed, Omega_sigma_estimate, Omega_prior_sd_global,
//       Omega_n_sigma, Omega_n_sigma_fixed,
//       Omega_n_sigma_estimate);
//   vector[n_geounit] Omega = get_omega(Omega_sigma, Omega_raw_fixed, Omega_raw_estimate, //Omega_low, Omega_up,
//       Omega_re_start, Omega_re_end, n_geounit, Omega_n_terms, Omega_n_terms_fixed, Omega_n_terms_estimate,
//       Omega_model_matrix_w, Omega_model_matrix_v, Omega_model_matrix_u);
//
//
//   // mcpr
//   matrix[n_geounit, T] eta;
//   for(c in 1:n_geounit) {
//     eta[c, ] = get_rw1_1pop(Omega[c],
//             smoothing,  epsilon[c,],
//             t_star[c], T);
//   }
//   return(eta);
// }


