// fit to the data
  // get the right eta, incl for aggregates obs
  vector[N] tr_eta_i;
  for(i in 1:N) {
    if(held_out[i] == 0) {
       if(obs_isNA[i] == 0) {
          if (geo_unit[i] == 0){ // aggregate obs, calculate national aggregates
            vector[n_geounit] eta_r = inv_tr_eta_colvector(tr_eta_obs[, time[i]]);
            tr_eta_i[i] = tr_eta(sum(eta_r .* to_vector(prop_tr[time[i], ])));
          } else {
            tr_eta_i[i] = tr_eta_obs[geo_unit[i], time[i]];
          }
          y[i] ~ normal(tr_eta_i[i], scale[i]);
        }
      }
  }
