# Get summary statistics from either cmdstanr or rstan fit object

Get summary statistics from either cmdstanr or rstan fit object

## Usage

``` r
extract_summary(fit, variables, funs = NULL, .cores = 1)
```

## Arguments

- fit:

  A fitted model object containing a `samples` component and `backend`
  indicator

- variables:

  Character vector of variable names to summarize

- funs:

  Optional function, formula, or list of functions/formulas to apply for
  summary statistics. Examples:

  - Function: `mean`, `sd`

  - Formula: `~ quantile(.x, probs = c(0.025, 0.5, 0.975))`

  - List: `list(mean = mean, sd = sd)`

- .cores:

  Number of cores to use for parallel processing (cmdstanr only)

## Value

A tibble with summary statistics. Column names depend on the functions
used.
