# Extract posterior samples of eta (latent coverage) for a specific year

Extracts posterior draws of the latent coverage parameter eta from a
fitted model object for a specified year. For validation runs, only
includes countries that have both training and test data.

## Usage

``` r
get_eta_samples(fit, year_select = 2023, countryyear_select = NULL)
```

## Arguments

- fit:

  A fitted model object containing `samples` (cmdstanr or rstan fit
  object), `geo_unit_index`, `time_index`, `stan_data`, and `data`.

- year_select:

  The year for which to extract eta samples. Default is 2023. Ignored if
  `countryyear_select` is not NULL.

- countryyear_select:

  A tibble with columns `iso` and `year` specifying country-year
  combinations to extract. If not NULL, takes precedence over
  `year_select`. Default is NULL.

## Value

A tibble with columns:

- iso:

  Country ISO code

- year:

  Selected year

- eta:

  Posterior draw of eta (probit-scale coverage)

- draw:

  Draw number

- cluster, subcluster, name_region:

  Additional geographic identifiers if present

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- load_fit("my_model_run")
eta_2023 <- get_eta_samples(fit, year_select = 2023)

# Extract specific country-year combinations
cy_select <- tibble::tibble(iso = c("USA", "CAN"), year = c(2023, 2022))
eta_subset <- get_eta_samples(fit, countryyear_select = cy_select)
} # }
```
