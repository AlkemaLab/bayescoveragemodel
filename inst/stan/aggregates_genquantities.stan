//aggregates_generatedquantities
vector[N] eta_i;
for(i in 1:N) {
  if (geo_unit[i] == 0){ // aggregate obs, calculate national aggregates
    vector[n_geounit] eta_r = inv_tr_eta_colvector(tr_eta_obs[, time[i]]);
    eta_i[i] = sum(eta_r .* to_vector(prop_tr[time[i], ]));
  } else {
    eta_i[i] = inv_tr_eta(tr_eta_obs[geo_unit[i], time[i]]);
  }
}
vector[T] aggr;
for(t in 1:T) {
   aggr[t] = sum(eta[,t] .* to_vector(prop_tr[t, ]));
}
