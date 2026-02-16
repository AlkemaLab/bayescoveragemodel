functions {

#include ./function_deboor.stan
#include ./functions_dm.stan
#include ./function_eps.stan
#include ./functions_hiernew.stan
#include ./functions_transformations.stan
#include ./functions_processmodel.stan
#include ./functions_truncatednormal.stan

}
/////////////////////////////////////////////////////
data {
  real<lower = 0> verysmallnumber; // lower bound for sigmas
  int add_dataoutliers;
  int generate_quantities;
  int validation_run;
  int T; // Number of time points
  int n_geounit; // Number of lowest-level geographic units, e.g. countries or states
  array[n_geounit] int<lower=1, upper=T> t_star;
  array[n_geounit] int<lower=1, upper=T> t_min;
  array[n_geounit] int<lower=1, upper=T> t_max;
  int<lower=1, upper=T> t_last;
  int N; // Number of observations
  int S; // Number of sources
  array[N] int<lower=1, upper=T> time;     // Time of each observation
  array[N] int<lower=0, upper=n_geounit> geo_unit; // Geographic unit of each observation, 0 if observation is for an aggregated unit
  // updated to lower of 0 for subnational runs
  array[N] int<lower=0, upper=S> source;   // Source of each observation
  array[N] int<lower=0, upper=1> held_out; // Whether to hold out each observation
  array[N] int<lower=0> isDHS;            // DHS?
  array[N] int<lower=0> any_bias;

  // related to splines (some for previous use in generated quantities)
  int num_knots;
  vector[num_knots] knots;
  int spline_degree;
  // int num_constrained_zero;
  int num_basis;
  int k; // number of non-zero splines coefficients
  int num_grid;
  vector[num_grid] grid;
  matrix[num_knots + spline_degree - 1, num_grid] B;


 // smoothing
  // 0: no smoothing component. 1: AR(1) smoothing component.
  int<lower=0, upper=1> smoothing;
  // 0: don't fix smoothing component. 1: fix AR(1) smoothing component
  // if fixed, we fix rho and tau.
  // only relevant if smoothing = 1, should be 0 if smoothing is 0
  int<lower=0, upper=1> fix_smoothing;

  // for parameters of process model
  real Betas_lower_bound;
  real Betas_upper_bound;
  real Ptilde_low;

  int<lower=0> Ptilde_raw_n_terms;
  int<lower=0> Ptilde_raw_n_terms_fixed;
  int<lower=0> Ptilde_raw_n_terms_estimate;
  int<lower=0> Ptilde_n_sigma;
  int<lower=0> Ptilde_n_sigma_fixed;
  int<lower=0> Ptilde_n_sigma_estimate;
  array[Ptilde_n_sigma + 1] int<lower=1, upper=Ptilde_raw_n_terms> Ptilde_re_start;
  array[Ptilde_n_sigma + 1] int<lower=1, upper=Ptilde_raw_n_terms> Ptilde_re_end;
  matrix[n_geounit, Ptilde_raw_n_terms] Ptilde_model_matrix;
  real<lower = verysmallnumber> Ptilde_scalarprior_sd;
  real Ptilde_scalarprior_mean;
  real<lower = verysmallnumber> Ptilde_prior_sd_sigma_estimate;
  vector[Ptilde_raw_n_terms_fixed] Ptilde_raw_fixed;
  vector<lower=0>[Ptilde_n_sigma_fixed] Ptilde_sigma_fixed;

  int<lower=0> Omega_raw_n_terms;
  int<lower=0> Omega_n_sigma;
  int<lower=0> Omega_raw_n_terms_fixed;
  int<lower=0> Omega_raw_n_terms_estimate;
  int<lower=0> Omega_n_sigma_fixed;
  int<lower=0> Omega_n_sigma_estimate;
  array[Omega_n_sigma + 1] int<lower=1, upper=Omega_raw_n_terms> Omega_re_start;
  array[Omega_n_sigma + 1] int<lower=1, upper=Omega_raw_n_terms> Omega_re_end;
  matrix[n_geounit, Omega_raw_n_terms] Omega_model_matrix;
  real<lower = verysmallnumber> Omega_scalarprior_sd;
  real Omega_scalarprior_mean;
  real<lower = verysmallnumber> Omega_prior_sd_sigma_estimate;
  vector[Omega_raw_n_terms_fixed] Omega_raw_fixed;
  vector<lower=0>[Omega_n_sigma_fixed] Omega_sigma_fixed;

  int Betas_k_terms; // same as k; to do clean to have just 1
  int<lower=0> Betas_raw_n_terms;
  int<lower=0> Betas_raw_n_terms_fixed;
  int<lower=0> Betas_raw_n_terms_estimate;
  int<lower=0> Betas_n_sigma;
  int<lower=0> Betas_n_sigma_fixed;
  int<lower=0> Betas_n_sigma_estimate;
  array[Betas_n_sigma + 1] int<lower=1, upper = Betas_raw_n_terms> Betas_re_start;
  array[Betas_n_sigma + 1] int<lower=1, upper = Betas_raw_n_terms> Betas_re_end;
  real<lower = verysmallnumber> Betas_scalarprior_sd;
  real Betas_scalarprior_mean;
  real<lower = verysmallnumber> Betas_prior_sd_sigma_estimate;
  matrix[n_geounit, Betas_raw_n_terms] Betas_model_matrix;
  matrix[Betas_raw_n_terms_fixed, k] Betas_raw_fixed;
  matrix<lower=0>[Betas_n_sigma_fixed, k] Betas_sigma_fixed;

  // for data model
  vector[N] y;                // Observations
  vector[N] s;                // Standard deviation
  array[N] int<lower=0> obs_isNA;
  array[N] int<lower=0> nooutlier;                // should outlier be included?
  // 0: estimate nonse. 1: fix nonse at estimates from previous fit.
  int<lower=0, upper=1> fix_nonse;
  // if fix_nonse == 1, one nonse standard deviation estimate per source type
  array[fix_nonse ? S : 0] real<lower=0> nonse_fixed;

  // and outlier hyper fixed
  array[add_dataoutliers*fix_nonse ? 1 : 0] real<lower=0> global_shrinkage_dm_fixed;
  array[add_dataoutliers*fix_nonse ? 1 : 0] real<lower=0> caux_dm_fixed;
  array[fix_nonse ? 1 : 0] real<lower=0> sdbias_fixed;

  // // these can be used as max's when adding a level
  // // not yet used
  // real Ptilde_sigma_max;
  // real Omega_sigma_max;
  // real a_sigma_max_1;
  // real a_sigma_max_2;
  // real a_sigma_max_3;
  // real a_sigma_max_4;

  array[fix_smoothing] real<lower=0, upper=1> rho_fixed;
  array[fix_smoothing] real<lower=0> tau_fixed;

  // subnational
  int n_agg_units;                            // Number of geo units observed at aggregated spatial scale (e.g. number of nations)
  array[N] int<lower=0, upper=1> is_agg_obs; // Whether each observation is for a lowest-level geo unit or aggregated geo units
  array[N] int<lower=0, upper=n_agg_units> agg_unit; // For observations at aggregated spatial scales, index of aggregated unit, 0 if observation is not for an aggregated unit
  array[n_agg_units, n_geounit, T] real geo_unit_pop_wt; // Weights used to aggregate geo units. Each column should sum to 1
  // subnational indicator determines whether aggregates are calculated or not
  int<lower=0, upper=1> subnational;
  // 0: smoothing terms independent across lowest-level geo units
  // 1: smoothing terms correlated among lowest-level geo units in the same group
  int<lower=0, upper=1> correlated_smoothing;
  int n_cor_smoothing_blocks; // basically, number of national level units
  // number of low-level geo units in each block
  array[n_cor_smoothing_blocks] int<lower=1, upper=n_geounit> cor_smoothing_block_sizes;
  int max_cor_smoothing_block_size;

  int<lower=0, upper=1> fix_subnat_corr;
  array[fix_subnat_corr ? 1 : 0]  real<lower=0, upper = 1> rho_correlationeps_fixed;

  {{AGGREGATES_DATA}}

  {{routine_DATA}}

}

/////////////////////////////////////////////////////
transformed data {
  // transformations for hier parameters
  vector[rows(csr_extract_w(Ptilde_model_matrix))] Ptilde_model_matrix_w    = csr_extract_w(Ptilde_model_matrix);
  array[size(csr_extract_v(Ptilde_model_matrix))] int Ptilde_model_matrix_v = csr_extract_v(Ptilde_model_matrix);
  array[size(csr_extract_u(Ptilde_model_matrix))] int Ptilde_model_matrix_u = csr_extract_u(Ptilde_model_matrix);

  vector[rows(csr_extract_w(Omega_model_matrix))] Omega_model_matrix_w     = csr_extract_w(Omega_model_matrix);
  array[size(csr_extract_v(Omega_model_matrix))] int Omega_model_matrix_v  = csr_extract_v(Omega_model_matrix);
  array[size(csr_extract_u(Omega_model_matrix))] int Omega_model_matrix_u  = csr_extract_u(Omega_model_matrix);

  vector[rows(csr_extract_w(Betas_model_matrix))] Betas_model_matrix_w    = csr_extract_w(Betas_model_matrix);
  array[size(csr_extract_v(Betas_model_matrix))] int Betas_model_matrix_v = csr_extract_v(Betas_model_matrix);
  array[size(csr_extract_u(Betas_model_matrix))] int Betas_model_matrix_u = csr_extract_u(Betas_model_matrix);

  // splines set up: ext_knots (could pass from R too)
  //int num_basis = num_knots + spline_degree - 1;
  vector[2 * spline_degree + num_knots] ext_knots;
  //int num_constrained_zero = spline_degree + 1;
  ext_knots[1:spline_degree] = rep_vector(knots[1], spline_degree);
  ext_knots[(num_knots + spline_degree + 1):(num_knots + 2 * spline_degree)] = rep_vector(knots[num_knots], spline_degree);
  ext_knots[(spline_degree + 1):(num_knots + spline_degree)] = knots;

}

/////////////////////////////////////////////////////
parameters {

  // process model

  // splines coefficients
  matrix[Betas_raw_n_terms_estimate, k] Betas_raw_estimate;
  // matrix<lower = 0>[a_n_sigma_estimate, k>3 ? 3 : k] a_sigma_estimate;
  // with ordered variances
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_1;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_2;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_3;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_4;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_5;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_6;
  positive_ordered [Betas_n_sigma_estimate] Betas_sigma_estimate_reverse_7;

  vector[Ptilde_raw_n_terms_estimate] Ptilde_raw_estimate;
  //vector<lower=verysmallnumber>[Ptilde_n_sigma_estimate] Ptilde_sigma_estimate;
  positive_ordered [Ptilde_n_sigma_estimate] Ptilde_sigma_estimate_reverse;

  vector[Omega_raw_n_terms_estimate] Omega_raw_estimate;
  vector<lower=0>[Omega_n_sigma_estimate] Omega_sigma_estimate;

  matrix[n_geounit * smoothing, T * smoothing] epsilon_innovation;
  array[smoothing * (1 - fix_smoothing)] real<lower=0, upper=1> rho_estimate;
  array[smoothing * (1 - fix_smoothing)] real<lower=0> tau_estimate;

  // Data model
  array[fix_nonse ? 0 : S] real<lower=0> nonse_estimate;
  array[fix_nonse ? 0 : 1] real<lower=0> sdbias_estimate;
  array[add_dataoutliers * (1-fix_nonse)] real<lower=0> global_shrinkage_dm_estimate;
  array[add_dataoutliers * (1-fix_nonse)] real<lower=0> sqrt_caux_dm_estimate;
  vector<lower=0>[add_dataoutliers*N] local_shrinkage_dm;

  // for subnational
  array[fix_subnat_corr ? 0 : 1] real<lower=0, upper = 1> rho_correlationeps_estimate;   // for correlated eps

  {{routine_PARAMETERS}}

}

/////////////////////////////////////////////////////
transformed parameters {
  // asymptote
  vector[Ptilde_n_sigma_estimate] Ptilde_sigma_estimate = reverse(Ptilde_sigma_estimate_reverse);
  vector[Ptilde_raw_n_terms] Ptilde_star = get_mu_star(
      Ptilde_n_sigma, Ptilde_n_sigma_fixed, Ptilde_n_sigma_estimate,
      Ptilde_sigma_fixed, Ptilde_sigma_estimate,
      Ptilde_scalarprior_mean,
      Ptilde_scalarprior_sd,
      Ptilde_raw_n_terms, Ptilde_raw_n_terms_fixed, Ptilde_raw_n_terms_estimate,
      Ptilde_raw_fixed, Ptilde_raw_estimate,
      Ptilde_re_start, Ptilde_re_end);
//   vector[n_geounit] tr_Ptilde = Ptilde_low + (1 - Ptilde_low)*inv_logit(get_mu(
// use probit
   vector[n_geounit] tr_Ptilde = //rep_vector(0.9999, n_geounit);
   Ptilde_low + (1 - Ptilde_low)*Phi_approx(get_mu(
      Ptilde_star,
      Ptilde_raw_n_terms,
      n_geounit,
      Ptilde_model_matrix_w, Ptilde_model_matrix_v, Ptilde_model_matrix_u));
  // omega
  vector[Omega_raw_n_terms] Omega_star = get_mu_star(
    Omega_n_sigma, Omega_n_sigma_fixed, Omega_n_sigma_estimate,
    Omega_sigma_fixed, Omega_sigma_estimate,
    Omega_scalarprior_mean, Omega_scalarprior_sd,
    Omega_raw_n_terms, Omega_raw_n_terms_fixed, Omega_raw_n_terms_estimate,
    Omega_raw_fixed, Omega_raw_estimate,
    Omega_re_start, Omega_re_end);
  vector[n_geounit] Omega = get_mu(
    Omega_star,
    Omega_raw_n_terms,
    n_geounit,
    Omega_model_matrix_w, Omega_model_matrix_v, Omega_model_matrix_u);

  // splines coeff
  matrix<lower=0>[Betas_n_sigma_estimate, k] Betas_sigma_estimate;
  if (Betas_n_sigma_estimate > 0) {
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 1] = reverse(Betas_sigma_estimate_reverse_1);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 2] = reverse(Betas_sigma_estimate_reverse_2);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 3] = reverse(Betas_sigma_estimate_reverse_3);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 4] = reverse(Betas_sigma_estimate_reverse_4);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 5] = reverse(Betas_sigma_estimate_reverse_5);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 6] = reverse(Betas_sigma_estimate_reverse_6);
    Betas_sigma_estimate[1:Betas_n_sigma_estimate, 7] = reverse(Betas_sigma_estimate_reverse_7);
  }
  matrix[Betas_raw_n_terms,Betas_k_terms] Betas_star = get_mudimhk_star(k ,
       Betas_n_sigma, Betas_n_sigma_fixed, Betas_n_sigma_estimate,
       Betas_sigma_fixed, Betas_sigma_estimate,
       Betas_scalarprior_mean, Betas_scalarprior_sd,
       Betas_raw_n_terms, Betas_raw_n_terms_fixed, Betas_raw_n_terms_estimate,
       Betas_raw_fixed, Betas_raw_estimate,
       Betas_re_start, Betas_re_end);
   matrix[n_geounit,Betas_k_terms] Betas = get_mudimhk(k,
       Betas_star,
       Betas_raw_n_terms,
       n_geounit,
       Betas_model_matrix_w, Betas_model_matrix_v, Betas_model_matrix_u);
  matrix[n_geounit,Betas_k_terms] tr_Betas_nonzero = Betas_lower_bound + (Betas_upper_bound - Betas_lower_bound) * inv_logit(Betas);

  // smoothing
  matrix[n_geounit, T] epsilon;
  epsilon = compute_epsilon(
    smoothing, fix_subnat_corr, fix_smoothing,
    rho_correlationeps_fixed, rho_correlationeps_estimate,
    rho_fixed, rho_estimate,
    tau_fixed, tau_estimate,
    epsilon_innovation, n_geounit, T, t_star,
    correlated_smoothing, max_cor_smoothing_block_size, n_cor_smoothing_blocks,
    cor_smoothing_block_sizes
  );
  // eta
   matrix[n_geounit, T] tr_eta_obs = process_model_returns_etatr(
                        n_geounit, T, t_star, t_min, t_max,
                        smoothing, epsilon,
                        Omega, tr_Ptilde, tr_Betas_nonzero,
                        k, num_basis,   ext_knots,  spline_degree);

  // subnational estimation:
  // aggregation, to finish
   matrix[n_agg_units, T] tr_eta_agg;
  // if (!subnational){
  //   // not a subnat run, so not used. fix at value between 0 and 1
    for(aggunit in 1:n_agg_units) {
      for(t in 1:T) {
        tr_eta_agg[aggunit,t] = 0; // use eta_obs
      }
    }
  // } else {
  //   // TO DO: finish
  //   for(i in 1:n_agg_units) {
  //     for(t in 1:T) {
  //       eta_agg[i, t] = sum(eta[, t] .* to_vector(geo_unit_pop_wt[i, , t]));
  //     }
  //   }
  // }

  // Data model: scales
  // minor note that this includes obs that are NA, could subset instead
   array[add_dataoutliers * (1-fix_nonse)] real<lower=0> caux_dm_estimate = sqrt_caux_dm_estimate^2;
   vector[N] scale;
   if (add_dataoutliers){
    scale = get_scale(
        local_shrinkage_dm, nonse_fixed, global_shrinkage_dm_fixed, caux_dm_fixed,
        nonse_estimate, global_shrinkage_dm_estimate, caux_dm_estimate,
        N, s, isDHS, source, nooutlier, S, fix_nonse,
        any_bias, sdbias_fixed, sdbias_estimate);
   } else {
    scale = get_scale_nooutliers(
        nonse_fixed,
        nonse_estimate,
        N, s, isDHS, source, nooutlier, S, fix_nonse);

   }

}

/////////////////////////////////////////////////////
model {
  // hierarchical parameters
  Ptilde_raw_estimate ~ std_normal();
  Omega_raw_estimate ~ std_normal();
  to_vector(Betas_raw_estimate) ~ std_normal();

  // variances
  //Ptilde_sigma_estimate ~ normal(0, Ptilde_prior_sd_sigma_estimate)T[0, positive_infinity()];
  Ptilde_sigma_estimate_reverse ~ normal(0, Ptilde_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  Omega_sigma_estimate ~ normal(0, Omega_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  //to_vector(Betas_sigma_estimate) ~ normal(0, Betas_prior_sd_sigma_estimate)T[0, positive_infinity()];
   // one vector per spline coefficient
  to_vector(Betas_sigma_estimate_reverse_1) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_2) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_3) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_4) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_5) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_6) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  to_vector(Betas_sigma_estimate_reverse_7) ~ normal(0, Betas_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];


  // smoothing terms
  if(smoothing) {
    to_vector(epsilon_innovation) ~ std_normal();
    if(fix_smoothing == 0) {
      rho_estimate[1] ~ normal(0, 0.5)T[0,1];
      tau_estimate[1] ~ normal(0, 0.2)T[verysmallnumber, positive_infinity()];
    }
  }

  // subnational
  if (!fix_subnat_corr){
    rho_correlationeps_estimate[1] ~ uniform(0, 1);
  }

  // data model parameters
  sdbias_estimate ~ normal(0, 0.5)T[verysmallnumber, positive_infinity()];//2;
  nonse_estimate ~ normal(0, 0.5)T[verysmallnumber, positive_infinity()];
  if (add_dataoutliers){
    local_shrinkage_dm ~ student_t(1, 0, 1)T[0, positive_infinity()];
    global_shrinkage_dm_estimate ~ student_t(1, 0, 0.04)T[0, positive_infinity()];
    sqrt_caux_dm_estimate ~ normal(0,1)T[0, positive_infinity()];
  }

    {{datamodel_MODEL}}
    {{routine_MODEL}}

}


generated quantities {

   matrix[n_geounit, T] eta = inv_tr_eta_matrix(
            process_model_outsideobs(tr_eta_obs,
                      n_geounit, T,  t_min, t_max,
                      smoothing, epsilon,
                      tr_Ptilde, tr_Betas_nonzero,
                      k, num_basis,   ext_knots,  spline_degree));

  {{CHOOSE_AGGREGATESYESNO_GENQUANTITIES}}

  vector[N] y_sim;
  if (generate_quantities) {
     if (validation_run) {
       for (i in 1:N) {
         if (held_out[i] == 1) {
            y_sim[i] = normal_rng(tr_eta(eta_i[i]), scale[i]);
         }
       }
     } else {
       for (i in 1:N) {
          y_sim[i] = normal_rng(tr_eta(eta_i[i]), scale[i]);
       }
     }
  }


}





