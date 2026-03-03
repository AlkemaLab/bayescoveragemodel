# Extract Innovation Samples from Fitted Model

Extracts epsilon_innovation (AR1 process innovations) and eta from
posterior samples. These represent the temporal deviations from the
expected coverage trajectory along with the underlying fitted values.

## Usage

``` r
get_inno_samples(fit)
```

## Arguments

- fit:

  Fitted model object containing: samplescmdstan samples with
  epsilon_innovation CT and eta CT dataData frame with iso, country,
  name_region, year time_indexTime index mapping with columns t and year
  geo_unit_indexGeographic unit index with cluster/subcluster info

## Value

Nested tibble with one row per (iso, year) containing:

- iso:

  ISO country code

- year:

  Year

- cluster:

  WHO region cluster (if available)

- subcluster:

  Regional subcluster (if available)

- name_region:

  Region name (if available)

- draws:

  Nested tibble with draw-specific columns:

  - draw: Posterior draw number

  - eta: Eta parameter on probit scale

  - sd_y: Scale parameter (always 1 for standardized innovations)

  - residual: Innovation value (epsilon_innovation)

  - level: Fitted coverage on inv-probit scale

  - level_prop: Eta on proportion scale

  - yhat: Fitted value on inv-probit scale (same as level)

  - y: Reconstructed observation (residual + yhat)

  - sd_y_prop: Standard deviation on proportion scale

  - y_prop: Reconstructed observation on proportion scale

## Details

Only returns innovations for years within the observation range for each
country (min_obs_yr to max_obs_yr).
