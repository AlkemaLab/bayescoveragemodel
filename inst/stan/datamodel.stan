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

