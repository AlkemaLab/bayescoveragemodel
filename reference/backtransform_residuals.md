# Back-transform Residuals to Proportion Scale

Transforms residuals from probit scale back to the original proportion
scale. Computes the response on probit scale, then calculates residuals
and standard errors on the proportion scale.

## Usage

``` r
backtransform_residuals(df)
```

## Arguments

- df:

  Data frame containing columns:

  - `level`: Predicted value on probit scale

  - `residual`: Residual on probit scale

  - `scale`: Standard error on probit scale

## Value

Data frame with added columns:

- `response`: Observed value on probit scale (level + residual)

- `residual_ori`: Residual on proportion scale

- `scale_ori`: Standard error on proportion scale
