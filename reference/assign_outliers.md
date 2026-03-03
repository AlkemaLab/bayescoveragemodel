# Assign which observations are modeled with an outlier error term

Determines which observations should be assigned an outlier error in the
model fit. Uses either pre-specified outlier indicators from the data,
or an algorithm based on observation year, data series type, and
country.

## Usage

``` r
assign_outliers(dat, outlier_record_ids = NULL)
```

## Arguments

- dat:

  A data frame with survey observations. May contain columns:

  possible_outlier

  :   Binary indicator if observation is a possible outlier

  possible_outlier_userinput

  :   User override for possible_outlier

  record_id_fixed

  :   Unique observation identifier (required if outlier_record_ids
      provided)

  data_series_type

  :   Type of data series (e.g., "DHS", "National survey", "Other")

  any_bias

  :   Binary indicator for known biases

  iso

  :   Country ISO code

  year

  :   Observation year

- outlier_record_ids:

  Optional vector of record_id_fixed values to flag as possible
  outliers. Typically obtained from step 1a fit using
  [`identify_outliers_in_global_fit`](https://alkemalab.github.io/bayescoveragemodel/reference/identify_outliers_in_global_fit.md).

## Value

The input data frame with additional column `nooutlier` (1 = not an
outlier, 0 = possible outlier that gets an outlier error term).

## Details

The function uses the following priority for determining outliers:

1.  If `possible_outlier_userinput` is provided, use that

2.  If `possible_outlier` column exists, use that

3.  Otherwise, apply algorithm: DHS surveys before 1990, or observations
    with known biases, are flagged as possible outliers
