
// transition model for all populations considered
matrix rw2process_model_returns_etatr(
    int n_geounit, int T, array[] int t_star,
    array[] int t_min, array[] int t_max,
    int smoothing, matrix epsilon,
    vector Omega,
    vector gamma,
    vector Ptilde
   ){

  matrix[n_geounit, T] tr_eta;
  for(c in 1:n_geounit) {
    tr_eta[c, 1:T] = get_rw2_1pop_returns_etatr(
      Omega[c],
      gamma[c],
      Ptilde[c],
            smoothing,  epsilon[c,],
            //  int t_star, int t_min, int t_max, int T,
            t_star[c], t_min[c], t_max[c], T);
  }
  return(tr_eta);
}


// transition model for 1 population
row_vector get_rw2_1pop_returns_etatr(
   real Omega,
   real gamma,
   real P_tilde,
     int smoothing,
    row_vector epsilon,
    // matrix epsilon,
    int t_star, int t_min, int t_max, int T){
  row_vector[T] tr_eta;
  tr_eta[t_star] =  Omega;
  tr_eta[t_star + 1] = Omega + gamma;
  // to do: add requirement somewhere that tmax >= tstar+1
  for(t in (t_star + 2):t_max) {
    tr_eta[t] = 2*tr_eta[t - 1] - tr_eta[t - 2] + smoothing*epsilon[t];
  }
  // to do: add requirement somewhere that tmin <= tstar-1
  for(q in 1:(t_star - t_min)) {
    int t = t_star - q;
    tr_eta[t] = 2*tr_eta[t +1] - tr_eta[t + 2]  - smoothing*epsilon[ t + 2];
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
matrix rw2process_model_outsideobs(
  matrix tr_eta, // transformed eta of dime geo_unit x T with estimates given inside obs period
    int n_geounit, int T,
    array[] int t_min, array[] int t_max,
    int smoothing, matrix epsilon,
    vector Ptilde
   ){
  matrix[n_geounit, T] shock = rep_matrix(0, n_geounit, T);
  matrix[n_geounit, T] tr_eta_again = tr_eta;
  for(c in 1:n_geounit) {
    tr_eta_again[c, 1:T] = get_rw2_1pop_outsideobs(
            tr_eta[c, 1:T],
         Ptilde[c],
            smoothing,  epsilon[c,],
            //  int t_star, int t_min, int t_max, int T,
            t_min[c], t_max[c], T);
  }
  return(tr_eta_again);
}

// transition model for 1 population outside obs period
row_vector get_rw2_1pop_outsideobs(
  row_vector tr_eta_obs, // transformed eta of length T with estimates given inside obs period
    real P_tilde,
     int smoothing,
    row_vector epsilon,
    // matrix epsilon,
    int t_min, int t_max, int T){
  row_vector[T] tr_eta;
  tr_eta[t_min:t_max] = tr_eta_obs[t_min:t_max];
  if (t_max < T){
    for(t in (t_max + 1):T) {
      tr_eta[t] = 2*tr_eta[t - 1] - tr_eta[t - 2]  + smoothing*epsilon[t];
    }
  }
  if (t_min > 1){
    for(q in 1:(t_min - 1)) {
      int t = t_min - q;
      tr_eta[t] = 2*tr_eta[t + 1] - tr_eta[t +2]  - smoothing*epsilon[ t + 2];
    }
  }
  return(tr_eta);
}



