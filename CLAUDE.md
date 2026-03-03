# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

**bayescoveragemodel** is an R package that fits Bayesian hierarchical
transition models to health coverage indicators (ANC4, institutional
delivery, vaccination) using survey and routine data. It uses Stan via
cmdstanr for Bayesian estimation.

## Ignored Directories

Do not read, modify, or reference any files in the `/private` folder.

## Development Commands

``` r
# Load package for development
devtools::load_all()

# Generate/update roxygen documentation
devtools::document()

# Build the package
devtools::build()

# Check the package
devtools::check()

# Generate Stan model from templates
write_model(add_aggregates = FALSE, add_routine = FALSE)

# Compile Stan model
compile_model(model_name = "spline", threads_per_chain = 1)
```

## Directory Structure

- `R/` - Core package functions (fit_model.R is the main entry point)
- `inst/stan/` - Stan model files and templates (uses `{{}}`
  placeholders)
- `data_raw/` - Raw public input data (surveys, regional metadata)
- `data/` - Processed data and saved model summaries
- `vignettes/` - Package documentation and tutorials
- `man/` - Auto-generated roxygen documentation

## Key Patterns

### Stan Model Generation

Stan models are generated from templates via
[`write_model()`](https://alkemalab.github.io/bayescoveragemodel/reference/write_model.md): -
Templates use `{{placeholder}}` syntax - Combines building blocks from
`inst/stan/` - Produces 4 variants: fpem, fpem_routine, fpem_aggregates,
fpem_routine_aggregates

### Data Transformations

The package uses multiple scales with delta method transformations: -
[`probit()`](https://alkemalab.github.io/bayescoveragemodel/reference/probit.md),
[`inv_probit()`](https://alkemalab.github.io/bayescoveragemodel/reference/inv_probit.md),
`logit()`, `inv_logit()` - SE helpers:
[`get_se_logitprop()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_se_logitprop.md),
[`get_se_invprobitprop()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_se_invprobitprop.md)

### Model Fitting Workflow

1.  [`process_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/process_data.md) -
    Prepare data (filter, transform, assign outliers)
2.  [`explore_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/explore_data.md) -
    Generate EDA plots
3.  [`fit_model()`](https://alkemalab.github.io/bayescoveragemodel/reference/fit_model.md) -
    Fit with runstep parameter (“step1a”, “step1ab”, “step1b”,
    “local_national”)
4.  [`plot_estimates_local_all()`](https://alkemalab.github.io/bayescoveragemodel/reference/plot_estimates_local_all.md) -
    Visualize results

### Output Directory

Model outputs are stored in `../bayestransition_output/` (one level up
from repo). Use
[`get_output_dir()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_output_dir.md)
to construct paths.

## Coding Conventions

- All exported functions have roxygen documentation with `#'` comments
- Uses tidyverse conventions (dplyr, tidyr, ggplot2)
- Function parameters use snake_case
- Hierarchical structures defined via character vectors (e.g.,
  `c("intercept", "name_region", "name_country")`)

## Important Notes

- Stan compilation may fail if folder paths contain spaces
- Model objects have class `"fpemplus"`
- The `nooutlier` column uses inverted logic: 1=not outlier, 0=possible
  outlier
