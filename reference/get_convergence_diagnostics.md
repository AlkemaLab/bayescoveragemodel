# Get convergence diagnostics for a fitted model

Computes and saves convergence diagnostics (Rhat, ESS) for
hyperparameters and creates density overlay plots.

## Usage

``` r
get_convergence_diagnostics(fit, model_name = "spline")
```

## Arguments

- fit:

  A fitted model object from
  [`fit_model`](https://alkemalab.github.io/bayescoveragemodel/reference/fit_model.md)

- model_name:

  Character string specifying the model type. Either "spline" (checks
  Omega, Ptilde, Betas) or "rw2" (checks Omega, gamma). Default is
  "spline".

## Value

NULL (invisibly). Saves diagnostics.csv and diagnostics.pdf to the
output directory.
