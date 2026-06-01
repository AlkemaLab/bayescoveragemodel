# Backend wrapper functions for cmdstanr and rstan compatibility

These internal functions provide a unified interface for extracting
draws and summaries from both cmdstanr and rstan fit objects.

## Details

The main compatibility issues addressed:

- **Output extraction**: Different APIs for extracting draws and
  summaries

- **Formula support**: Uses posterior::summarise_draws for consistent
  formula handling

- **Initialization**: rstan requires explicit array dimensions (see
  fix_init_dims_for_rstan in inits.R)

- **Data preparation**: rstan requires explicit matrix/vector structures
  (see fit_model.R lines 913-995)
