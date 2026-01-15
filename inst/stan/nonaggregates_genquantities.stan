//nonaggregates_genquantities

// eta_i needs to be outside {} else not saved
vector[N] eta_i;
for(i in 1:N) {
  eta_i[i] = inv_tr_eta(tr_eta_obs[geo_unit[i], time[i]]);
}
