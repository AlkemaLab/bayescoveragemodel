# Extract draws from either cmdstanr or rstan fit object

Converts both backends to a unified draws_array format for compatibility
with tidybayes, bayesplot, and other posterior analysis packages.

## Usage

``` r
extract_draws(fit, variables = NULL)
```

## Arguments

- fit:

  A fitted model object containing a `samples` component and `backend`
  indicator

- variables:

  Character vector of variable names to extract (ignored, all params
  extracted)

## Value

A draws_array object from the posterior package
