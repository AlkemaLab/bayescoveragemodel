  // routines
  int N_routine;
  real mean_log_sigma; // mean for roc
  real<lower = 0>  hierarchical_sigma; // sd for roc
  vector[N_routine] routine_roc; // observed delta routine
  //vector<lower = 0>[N_routine]  sd2_routine_roc; //sd^2 of observed delta routine
  vector[N_routine] log_sigma_mean_routine_roc; //expected log sigma
  array[N_routine] int c_routine_j; // geo unit index
  array[N_routine] int t_routine_j; // year index
