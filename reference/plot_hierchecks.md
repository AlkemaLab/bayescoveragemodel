# Plot hierarchical model checks

Creates diagnostic plots for hierarchical model parameters including
prior-posterior comparisons and mu_raw summaries.

## Usage

``` r
plot_hierchecks(fit, model_name = "spline")
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

NULL (invisibly). Saves hierchecks.pdf to the output directory.
