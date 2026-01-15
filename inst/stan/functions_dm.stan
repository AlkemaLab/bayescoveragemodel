// simplified data model, no outliers used, get scale (total sd)
vector  get_scale_nooutliers(
    array[] real nonse_fixed,
    array[] real nonse_estimate,
    int N,  vector s, //sd
    array[] int isDHS, array[] int source, array[] int nooutlier,
    int S, int fix_nonse){

  array[S] real nonse;
  if (fix_nonse) {
    nonse = nonse_fixed;
  } else {
    nonse = nonse_estimate;
  }
  vector[N] scale;
  for(i in 1:N) {
      scale[i] = sqrt(
                 square(s[i])
                + square((1-isDHS[i])*nonse[source[i]] )
                 );
  }
  return(scale);
}

// NOS data model: get scale (total sd)
vector  get_scale(    vector local_shrinkage_dm,
    array[] real nonse_fixed, array[] real global_shrinkage_dm_fixed, array[] real caux_dm_fixed,
    array[] real nonse_estimate, array[] real global_shrinkage_dm_estimate, array[] real caux_dm_estimate,
    int N,  vector s, //sd
    array[] int isDHS, array[] int source, array[] int nooutlier, int S, int fix_nonse, array[] int any_bias, array[] real sdbias_fixed, array[] real sdbias_estimate){

  array[S] real nonse;
  real global_shrinkage_dm;
  real caux_dm;
  real sdbias;
  if (fix_nonse) {
    nonse = nonse_fixed;
    global_shrinkage_dm = global_shrinkage_dm_fixed[1];
    caux_dm = caux_dm_fixed[1];
    sdbias = sdbias_fixed[1];
  } else {
    nonse = nonse_estimate;
    global_shrinkage_dm = global_shrinkage_dm_estimate[1];
    caux_dm = caux_dm_estimate[1];
    sdbias = sdbias_estimate[1];
  }
  vector[N] truncated_local_shrinkage_dm;
  vector[N] nonse_outlier;
  truncated_local_shrinkage_dm = sqrt(caux_dm * square(local_shrinkage_dm) ./
              (caux_dm + global_shrinkage_dm^2 * square(local_shrinkage_dm)));
  nonse_outlier = truncated_local_shrinkage_dm * global_shrinkage_dm;

  vector[N] scale;
  for(i in 1:N) {
      scale[i] = sqrt(
                 square(s[i])
                + square((1-isDHS[i])*nonse[source[i]] )
                + square((1-nooutlier[i])*nonse_outlier[i])
                + square(any_bias[i]*sdbias)
                 );
  }
  return(scale);
}
