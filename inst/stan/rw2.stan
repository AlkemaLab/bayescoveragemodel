functions {

#include ./functions_dm.stan
#include ./function_eps.stan
#include ./functions_hiernew.stan
#include ./functions_rw2processmodel.stan
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

  int num_grid;
  vector[num_grid] grid;

 // smoothing
  // 0: no smoothing component. 1: AR(1) smoothing component.
  int<lower=0, upper=1> smoothing;
  // 0: don't fix smoothing component. 1: fix AR(1) smoothing component
  // if fixed, we fix rho and tau.
  // only relevant if smoothing = 1, should be 0 if smoothing is 0
  int<lower=0, upper=1> fix_smoothing;

  // for parameters of process model
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

  // roc parameter for rw2
    int<lower=0> gamma_raw_n_terms;
  int<lower=0> gamma_n_sigma;
  int<lower=0> gamma_raw_n_terms_fixed;
  int<lower=0> gamma_raw_n_terms_estimate;
  int<lower=0> gamma_n_sigma_fixed;
  int<lower=0> gamma_n_sigma_estimate;
  array[gamma_n_sigma + 1] int<lower=1, upper=gamma_raw_n_terms> gamma_re_start;
  array[gamma_n_sigma + 1] int<lower=1, upper=gamma_raw_n_terms> gamma_re_end;
  matrix[n_geounit, gamma_raw_n_terms] gamma_model_matrix;
  real<lower = verysmallnumber> gamma_scalarprior_sd;
  real gamma_scalarprior_mean;
  real<lower = verysmallnumber> gamma_prior_sd_sigma_estimate;
  vector[gamma_raw_n_terms_fixed] gamma_raw_fixed;
  vector<lower=0>[gamma_n_sigma_fixed] gamma_sigma_fixed;


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

  vector[rows(csr_extract_w(gamma_model_matrix))] gamma_model_matrix_w     = csr_extract_w(gamma_model_matrix);
  array[size(csr_extract_v(gamma_model_matrix))] int gamma_model_matrix_v  = csr_extract_v(gamma_model_matrix);
  array[size(csr_extract_u(gamma_model_matrix))] int gamma_model_matrix_u  = csr_extract_u(gamma_model_matrix);


}

/////////////////////////////////////////////////////
parameters {

  // process model


  vector[Ptilde_raw_n_terms_estimate] Ptilde_raw_estimate;
  //vector<lower=verysmallnumber>[Ptilde_n_sigma_estimate] Ptilde_sigma_estimate;
  positive_ordered [Ptilde_n_sigma_estimate] Ptilde_sigma_estimate_reverse;

  vector[Omega_raw_n_terms_estimate] Omega_raw_estimate;
  vector<lower=0>[Omega_n_sigma_estimate] Omega_sigma_estimate;

  vector[gamma_raw_n_terms_estimate] gamma_raw_estimate;
  vector<lower=0>[gamma_n_sigma_estimate] gamma_sigma_estimate;

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

  // gamma
  vector[gamma_raw_n_terms] gamma_star = get_mu_star(
    gamma_n_sigma, gamma_n_sigma_fixed, gamma_n_sigma_estimate,
    gamma_sigma_fixed, gamma_sigma_estimate,
    gamma_scalarprior_mean, gamma_scalarprior_sd,
    gamma_raw_n_terms, gamma_raw_n_terms_fixed, gamma_raw_n_terms_estimate,
    gamma_raw_fixed, gamma_raw_estimate,
    gamma_re_start, gamma_re_end);
  vector[n_geounit] gamma = get_mu(
    gamma_star,
    gamma_raw_n_terms,
    n_geounit,
    gamma_model_matrix_w, gamma_model_matrix_v, gamma_model_matrix_u);

 // to do: update
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
  // to do: update
  // eta
   matrix[n_geounit, T] tr_eta_obs = rw2process_model_returns_etatr(
                        n_geounit, T, t_star, t_min, t_max,
                        smoothing, epsilon,
                        Omega, gamma, tr_Ptilde);

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
  gamma_raw_estimate ~ std_normal();

  // variances
  //Ptilde_sigma_estimate ~ normal(0, Ptilde_prior_sd_sigma_estimate)T[0, positive_infinity()];
  Ptilde_sigma_estimate_reverse ~ normal(0, Ptilde_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  Omega_sigma_estimate ~ normal(0, Omega_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];
  gamma_sigma_estimate ~ normal(0, gamma_prior_sd_sigma_estimate)T[verysmallnumber, positive_infinity()];

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

    // fit to the data
  vector[N] tr_eta_i; // introduced to have same mean as when incl aggregates
  for(i in 1:N) {
    tr_eta_i[i] = tr_eta_obs[geo_unit[i], time[i]];
    if(held_out[i] == 0) {
       if(obs_isNA[i] == 0) {
          y[i] ~ normal(tr_eta_i[i], scale[i]);
        }
      }
  }




}


generated quantities {

// to do: update
   matrix[n_geounit, T] eta = inv_tr_eta_matrix(
            rw2process_model_outsideobs(tr_eta_obs,
                      n_geounit, T,  t_min, t_max,
                      smoothing, epsilon,
                      tr_Ptilde));

  //nonaggregates_genquantities

// eta_i needs to be outside {} else not saved
vector[N] eta_i;
for(i in 1:N) {
  eta_i[i] = inv_tr_eta(tr_eta_obs[geo_unit[i], time[i]]);
}


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






