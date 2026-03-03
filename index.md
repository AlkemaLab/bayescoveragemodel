# BayesCoverageModel

R package to fit Bayesian hierarchical transition models to health
coverage indicators (e.g., ANC4, institutional delivery) using survey
and routine data. Implements models using Stan for Bayesian inference,
with support for global and local (country-specific) model fitting,
out-of-sample validation, and subnational estimation.

This work was supported, in whole or in part, by the Bill & Melinda
Gates Foundation (INV-001299).

# Installation

Dependencies

- `cmdstanr`: Instructions for installing `cmdstanr` are available in
  their [Getting
  started](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) guide.

- R package `localhierarchy`, available at
  [github.com/AlkemaLab/localhierarchy](https://github.com/AlkemaLab/localhierarchy).
  You can install it using

`remotes::install_github("AlkemaLab/localhierarchy")`

Install `bayescoveragemodel` from Github:

`remotes::install_github("AlkemaLab/bayescoveragemodel")`

# Getting started

Below we demonstrate a quick example workflow for fitting Bayesian
transition models to health coverage indicators. We use minimal sampling
iterations for demonstration purposes.

For full model fitting with proper sampling settings, see the [Global
Model
Fitting](https://alkemalab.github.io/bayescoveragemodel/articles/global_model_fitting.html)
article.

## Setup

``` r
library(bayescoveragemodel)
library(dplyr)
library(ggplot2)
```

## Load Data

The package includes example survey data for health coverage indicators
(ANC4, institutional delivery, vaccination).

``` r
# Load survey data
data_folder <- system.file("extdata", package = "bayescoveragemodel")

# If using development version with data_raw folder
if (data_folder == "") {
  data_folder <- "data_raw"
}

# Load regions metadata
regions_dat <- readr::read_csv(
  file.path(data_folder, "regions_updated.csv"),
  show_col_types = FALSE
)
```

## Process Data

Use
[`process_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/process_data.md)
to prepare the survey data for model fitting.

``` r
# Load raw survey data (example using haven for .dta files)
dat0 <- haven::read_dta(file.path(data_folder, "ICEH_national.dta"))

# Process data for a specific indicator
dat <- process_data(
  dat = dat0,
  regions_dat = regions_dat,
  indicator = "anc4"
)

head(dat)
```

## Explore Data

Before fitting models, explore the survey data using
[`explore_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/explore_data.md).

``` r
# Create exploration plots
plots <- explore_data(

data = dat,
  indicator_col = "indicator",
  group_col = "country",
  data_types_col = "data_series_type",
  indicator_name = "ANC4",
  region_col = "cluster"
)

# View one of the plots
plots[[1]]
```

## Fit Model (Test Settings)

Fit a model using test settings with minimal iterations. This runs
quickly but should not be used for inference.

``` r
# Write Stan models (run once)
write_model()

# Fit model with test settings (fast, not for inference)
fit <- fit_model(
  runstep = "step1ab",
  survey_df = dat,
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  runnumber = "test_run",
  # Model defaults to "spline" (a Bayesian transition model), "rw2" argument can be used for fitting an ARIMA(1,2,0) model. 
  model_name = "spline", 
  chains = 2,
  # Test settings for quick demonstration
  iter_sampling = 10,
  iter_warmup = 5,
  get_posteriors = TRUE
)
```

## Plot Results

Visualize the fitted estimates.

``` r
# Plot estimates for all countries
p <- plot_estimates_local_all(
  results = fit,
  save_plots = FALSE,
  indicator_name = "anc4"
)

# Display a plot
p[[1]]
```

# Next Steps

For production model fitting with proper sampling:

1.  **Global Model Fitting**: See the [Global Model
    Fitting](https://alkemalab.github.io/bayescoveragemodel/articles/global_model_fitting.html)
    article for the complete workflow with recommended sampling
    settings.

2.  **Validation**: See the [Out-of-Sample
    Validation](https://alkemalab.github.io/bayescoveragemodel/articles/validation.html)
    article for assessing predictive performance.

3.  **Diagnostics**: See the [Model
    Diagnostics](https://alkemalab.github.io/bayescoveragemodel/articles/diagnostics.html)
    article for convergence checks and hierarchical model assessment.
