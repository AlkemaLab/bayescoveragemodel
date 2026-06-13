# Plot model fits, with options to add routine data or additional fits for comparison.

Plot model fits, with options to add routine data or additional fits for
comparison.

## Usage

``` r
plot_estimates_local_all(
  results,
  dat_routine = NULL,
  indicator_name = NULL,
  results2 = NULL,
  results3 = NULL,
  results4 = NULL,
  modelnames = c("model1", "model2", "model3", "model4"),
  region_codes = NULL,
  save_plots = FALSE,
  output_folder = NULL,
  add_caption = FALSE,
  add_estimates = TRUE,
  use_for_facetting = FALSE,
  plot_name = "fit"
)
```

## Arguments

- results:

  Model fit.

- dat_routine:

  Optional routine data to add to model estimates for comparison.

- indicator_name:

  Custom title for the y label of the plot. If `NULL`, name will be
  pulled from model fit.

- results2:

  Optional second model fit.

- results3:

  Optional third model fit.

- results4:

  Optional fourth model fit.

- modelnames:

  Optional names of the models to display in the plot legend.

- region_codes:

  Optional character vector of region codes to plot (ISO or admin1). If
  NULL, plots all regions.

- save_plots:

  Boolean indicator, if set to TRUE plots will be saved in output
  directory of results fit.

- output_folder:

  Folder to save plots in, if save_plots is TRUE, to overwrite where
  plots are saved.

- plot_name:

  Plot name if saved as pdf. Defaults to "fit".
