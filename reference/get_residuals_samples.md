# Extract Residuals from Posterior Samples

Extracts residuals from a fitted model object by comparing observed data
to posterior draws of eta_i (predicted coverage).

## Usage

``` r
get_residuals_samples(fit)
```

## Arguments

- fit:

  Fitted model object containing: samplescmdstan samples object with
  eta_i and scale parameters stan_dataList with y (observations) and
  held_out flag

## Value

Nested tibble with one row per observation containing:

- obs_index:

  Observation index

- y:

  Observed value on modeled scale

- y_prop:

  Observed value on proportion scale

- held_out:

  Logical indicating if observation was held out

- iso:

  Country ISO code

- year:

  Year of observation

- cluster:

  WHO region cluster (if available in fit\$data)

- subcluster:

  Regional subcluster (if available in fit\$data)

- name_region:

  Region name

- draws:

  Nested tibble with draw-specific columns:

  - draw: Posterior draw number

  - yhat: Predicted mean on modeled scale (= level)

  - sd_y: Standard deviation for observation on modeled scale

  - level: Level on modeled (inv-probit) scale

  - level_prop: Level on proportion scale

  - sd_y_prop: Standard deviation on proportion scale

  - y_sim: Simulated observation (validation runs only)
