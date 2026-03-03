# Add Probit Scale Transformation Columns

Adds standardized columns for probit/proportion scale transformations.
Used by get_residuals_samples and get_inno_samples to ensure consistent
column naming and transformations.

## Usage

``` r
add_probit_scale_columns(
  df,
  eta_col,
  sd_col = NULL,
  sd_value = 1,
  y_col = NULL
)
```

## Arguments

- df:

  Data frame to transform

- eta_col:

  Name of column containing eta values on probit scale

- sd_col:

  Name of column containing standard deviation on probit scale, or NULL
  to use sd_value

- sd_value:

  Numeric value for sd_y if sd_col is NULL (default: 1)

- y_col:

  Name of column containing y values on proportion scale, or NULL if y
  should be computed from residual + yhat

## Value

Data frame with added columns:

- `level_prop`: Eta on probit scale

- `level`: Coverage on proportion scale

- `yhat`: Fitted value on proportion scale (same as level)

- `sd_y`: Standard deviation on probit scale

- `sd_y_prop`: Standard deviation on proportion scale

- `y_prop`: y on probit scale

If y_col is NULL and residual column exists, also adds:

- `y`: Reconstructed observation (residual + yhat)
