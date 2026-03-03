# Global Model Fitting

This article describes the complete workflow for fitting global Bayesian
transition models to health coverage indicators. Global models estimate
hierarchical parameters across all countries, which are then used for
country-specific (local) estimation.

## Setup

``` r
library(bayescoveragemodel)
library(dplyr)
library(ggplot2)
library(haven)
library(cmdstanr)
library(localhierarchy)
```

## Load and Process Data

``` r
# Read national survey data and region metadata
data_folder <- "data_raw"
dat0 <- read_dta(here::here(data_folder, "ICEH_national.dta"))
regions_dat <- readr::read_csv(
  here::here(data_folder, "regions_updated.csv"))

# Choose an indicator: "anc4", "ideliv", or "vdpt"
indicator_select <- "anc4"

# Process data
dat <- process_data(
  dat = dat0,
  regions_dat = regions_dat,
  indicator = indicator_select
)
```

## Global Model Fitting (Step 1ab)

The primary workflow uses step 1ab to fit the global model:

``` r
fit1ab <- fit_model(
  runstep = "step1ab",
  survey_df = dat,
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  get_posteriors = TRUE,
  chains = 4
)
```

## Step 1a for Residual Extraction (Optional)

Step 1a can be run separately to extract residuals for model checking.

``` r
fit1a <- fit_model(
  runstep = "step1a",
  survey_df = dat,
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  get_posteriors = TRUE,
  chains = 4
)

# Extract residuals
residuals <- get_residuals_samples(fit = fit1a)
```

## Local Fitting for All Countries

After global fitting, run local models for all countries to obtain final
estimates:

``` r
fit_local_global <- fit_model(
  runstep = "local_national",
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  survey_df = dat,
  save_post_summ = TRUE,
  get_posteriors = TRUE,
  chains = 4
)
```

## Visualize Results

``` r
# Plot estimates for all countries
p <- plot_estimates_local_all(
  results = fit_local_global,
  save_plots = TRUE,
  indicator_name = indicator_select
)
```

## Output Storage

Model fits are stored in a `bayestransitionoutput` folder one level up
from the project root. Use
[`get_fit()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_fit.md)
to load saved fits:

``` r
# Load a previously saved fit
fit <- get_fit(
  indicator = "anc4",
  runstep = "step1b",
  folder_suffix = "run_workflow"
)
```

## Next Steps

- [Out-of-Sample
  Validation](https://alkemalab.github.io/bayescoveragemodel/articles/validation.md):
  Assess predictive performance
- [Model
  Diagnostics](https://alkemalab.github.io/bayescoveragemodel/articles/diagnostics.md):
  Check convergence and model fit
