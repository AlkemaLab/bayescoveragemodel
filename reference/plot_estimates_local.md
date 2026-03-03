# Plot local model fits.

Plot local model fits.

## Usage

``` r
plot_estimates_local(
  estimates,
  filtered_data,
  estimates2 = NULL,
  estimates3 = NULL,
  estimates4 = NULL,
  modelnames,
  indicator_name,
  add_estimates = TRUE,
  all_survey_types = NULL,
  cols_sourcetypes = c(DHS = "red", DHS0 = "red", MICS = "blue", PMA = "darkgreen", Other
    = "orange", `National survey` = "purple", NSS = "deepskyblue2", `Routine data` =
    "black")
)
```

## Arguments

- estimates:

  A data frame estimates from the model fit.

- filtered_data:

  A data frame containing data from the model fit.

- estimates2:

  Optional estimates from a second model.

- estimates3:

  Optional estimates from a third model.

- estimates4:

  Optional estimates from a fourth model.

- modelnames:

  A vector of model names used for plotting. Default is
  `c("model1", "model2", "model3", "model4")`.

- indicator_name:

  Name of indicator used for plotting.

- add_estimates:

  Boolean to show model estimates/ribbons.

- all_survey_types:

  Optional character vector of all survey types to include in legend
  (for consistent legends across multiple plots).

- cols_sourcetypes:

  A named vector of colors for the different data series types (e.g.,
  DHS, MICS, etc.).
