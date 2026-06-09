# Calculate Rate of Change and DQ Indicators

This function calculates rate of change metrics and data quality (DQ)
indicators for routine coverage data. It computes year-over-year changes
and creates "start" and "worst" versions of DQ covariates for
consecutive year pairs.

## Usage

``` r
routinedata_add_roc_info(
  data,
  add_est_roc = FALSE,
  dq_covariates_min = c("countdownmean"),
  dq_covariates_max = c(),
  dq_covariates_max_abs = c()
)
```

## Arguments

- data:

  A data frame containing routine data with columns: iso (country code),
  year, indicator_name, routine_value, and optionally median_estimate.
  Must also contain the DQ covariate columns specified in the
  dq_covariates\_\* parameters.

- add_est_roc:

  Logical. If TRUE, calculates rate of change for median_estimate in
  addition to routine_value. Default is FALSE.

- dq_covariates_min:

  Character vector of DQ covariate names where "worst" means minimum
  value between current and start. Defaults to c("countdownmean").

- dq_covariates_max:

  Character vector of DQ covariate names where "worst" means maximum
  value between current and start. Default is empty vector.

- dq_covariates_max_abs:

  Character vector of DQ covariate names where "worst" means maximum
  absolute value between current and start. Default is c().

## Value

A data frame with the original columns plus:

- routine_roc: Year-over-year change in routine_value (always added)

- est_roc: Year-over-year change in median_estimate (if add_est_roc =
  TRUE)

- [posterior::var](https://mc-stan.org/posterior/reference/rvar-summaries-over-draws.html)*start:
  Lagged value of each DQ covariate for consecutive years
  worst*[posterior::var](https://mc-stan.org/posterior/reference/rvar-summaries-over-draws.html):
  Worst (min or max) value between current and start for each DQ
  covariate

- worst_abs\_[posterior::var](https://mc-stan.org/posterior/reference/rvar-summaries-over-draws.html):
  Worst (max absolute) value for covariates in dq_covariates_max_abs

Note: Rate of change and DQ values are only calculated for consecutive
years (i.e., when year - lag(year) == 1), otherwise NA.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Create toy data
toy_data <- data.frame(
  iso = rep(c("CD1", "CD2"), each = 3),
  year = rep(2020:2022, 2),
  indicator_name = "dtp3",
  routine_value = c(0.85, 0.87, 0.90, 0.80, 0.82, 0.85),
  median_estimate = c(88, 89, 91, 83, 84, 87),
  notoutliers = c(100, 95, 90, 100, 100, 95),
  notmissing = c(100, 100, 95, 100, 95, 90),
  rr = c(95, 90, 85, 90, 88, 85),
  diff_level = c(3, 2, 1, 3, 2, 2)
)

# Apply function
result <- routinedata_add_roc_info(
  toy_data,
  add_est_roc = TRUE,
  dq_covariates_min = c("notoutliers", "notmissing", "rr"),
  dq_covariates_max_abs = c("diff_level")
)

# View results for CD1
result %>% filter(iso == "CD1") %>% select(year, routine_value, routine_roc, rr, worst_rr)
#> # A tibble: 3 × 5
#>    year routine_value routine_roc    rr worst_rr
#>   <int>         <dbl>       <dbl> <dbl>    <dbl>
#> 1  2020          0.85     NA         95       NA
#> 2  2021          0.87      0.0200    90       90
#> 3  2022          0.9       0.0300    85       85
```
