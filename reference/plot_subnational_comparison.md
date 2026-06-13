# Plot subnational comparison

Plot subnational comparison

## Usage

``` r
plot_subnational_comparison(
  results,
  results2 = NULL,
  model_names = c("Bayesian Model 1", "Bayesian Model 2"),
  year_select,
  ymin_select = 0,
  ymax_select = NA,
  arrange_point = TRUE
)
```

## Arguments

- results:

  Model fit.

- results2:

  Option model fit 2. If NULL, only one model will be plotted.

- model_names:

  Optional names for the models to be plotted. Should be of length 2 if
  results2 is not NULL, otherwise length 1.

- year_select:

  Year to plot. Should be a single value.

- ymin_select:

  Minimum y-axis value. Default is 0.

- ymax_select:

  Maximum y-axis value. Default is NA (no limit).

- arrange_point:

  Boolean to arrange points by median value. Default is TRUE.

## Value

ggplot object with comparison plot
