/* Fill out a partially filled AR process vector of length T
 *
 * @param epsilon_star A matrix which can be partially filled
 *        with an AR process.  The un-filled part of the
 *        matrix must be filled with N(0,1) draws.  Those
 *        draws are used as a source of randomness.  This function
 *        does not generate any random numbers itself.
 * @param rho the autocorrelation of the AR process.  No constraints
 *        are applied to this parameter although an exact value of 1
 *        will generate a 'divide by zero' error.
 * @param tau the standard deviation of the AR process.  No constraints
 *        are applied to this parameter although negative values will
 *        cause the AR process to flip at each step.
 * @param t_star an integer determining where the AR simulation will start.
 */
row_vector fill_AR(row_vector epsilon_star, real rho, real tau, int t_star) {
  int T = cols(epsilon_star);
  row_vector[T] epsilon = epsilon_star;

  epsilon[t_star] = epsilon[t_star] * tau / sqrt(1 - rho^2);

  for (q in 1:(t_star - 1)) {
    int t = t_star - q;
    epsilon[t] = rho * epsilon[t + 1] + epsilon[t] * tau;
  }
  for (t in (t_star + 1):T) {
    epsilon[t] = rho * epsilon[t - 1] + epsilon[t] * tau;
  }
  return epsilon;
}




// get smoothing term, with option for correlated terms across regions for subnat modeling
matrix get_epsilon(real rho, real tau, matrix epsilon_innovation, real rho_correlationeps,
                              int C, int T, array[] int t_star,
                        int correlated_smoothing, int max_cor_smoothing_block_size, int n_cor_smoothing_blocks,
                        array[] int cor_smoothing_block_sizes
                        ){
  matrix[C, T] epsilon_innovation_correlated;
  if (correlated_smoothing) {
    // Using here the result that if A and B are mxm and nxn matrices with n>m,
    // all off-diagonal entries equal, diagonal entries 1, then chol(A) is the
    // upper left mxm submatrix of chol(B)
    // so we just need to compute the correlation matrix and its Cholesky decomposition
    // once, for the max block size
    matrix[max_cor_smoothing_block_size, max_cor_smoothing_block_size] correlation_matrix;
    for (i in 1:max_cor_smoothing_block_size) {
      correlation_matrix[i,] = rep_row_vector(rho_correlationeps, max_cor_smoothing_block_size);
      correlation_matrix[i, i] = 1;
    }
    matrix[max_cor_smoothing_block_size, max_cor_smoothing_block_size] chol_correlation_matrix;
    // if we could figure out a formula, it might be faster to just build the
    // chol matrix directly rather than having to call cholesky_decompose...?
    chol_correlation_matrix = cholesky_decompose(correlation_matrix);

    int b_start = 1;
    for (b in 1:n_cor_smoothing_blocks) {
      int b_size = cor_smoothing_block_sizes[b];
      int b_end = b_start + b_size - 1;

      // matrix multiplication works to compute results for all time points
      epsilon_innovation_correlated[b_start:b_end, ] = chol_correlation_matrix[1:b_size, 1:b_size]*epsilon_innovation[b_start:b_end, ];

      // update start index for next block
      b_start = b_start + b_size;
    }
  } else {
    epsilon_innovation_correlated = epsilon_innovation;
  }

  matrix[C, T] epsilon;
  for(c in 1:C) {
    epsilon[c,] = fill_AR(epsilon_innovation_correlated[c, ], rho, tau, t_star[c]);
  }
  return(epsilon);
}

matrix compute_epsilon(
    int smoothing,
    int fix_subnat_corr,
    int fix_smoothing,
    array[] real rho_correlationeps_fixed,
    array[] real rho_correlationeps_estimate,
    array[] real rho_fixed,
    array[] real rho_estimate,
    array[] real tau_fixed,
    array[] real tau_estimate,
    matrix epsilon_innovation,
    int C, int T, array[] int t_star,
    int correlated_smoothing,
    int max_cor_smoothing_block_size,
    int n_cor_smoothing_blocks,
    array[] int cor_smoothing_block_sizes
  ) {
    matrix[C, T] epsilon;

    if (smoothing == 1) {
      real rho_correlationeps = fix_subnat_corr ? rho_correlationeps_fixed[1] : rho_correlationeps_estimate[1];
      real rho = fix_smoothing ? rho_fixed[1] : rho_estimate[1];
      real tau = fix_smoothing ? tau_fixed[1] : tau_estimate[1];
      epsilon = get_epsilon(rho, tau, epsilon_innovation, rho_correlationeps,
                            C, T, t_star,
                            correlated_smoothing, max_cor_smoothing_block_size, n_cor_smoothing_blocks,
                            cor_smoothing_block_sizes);
    } else {
      epsilon = rep_matrix(0, C, T);
    }

    return epsilon;
  }
