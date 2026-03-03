# Explore Coverage Indicators Over Time

This function performs exploratory data analysis on a dataset containing
coverage indicators over time.

## Usage

``` r
explore_data(
  data,
  indicator_col,
  indicator_se_col = NULL,
  data_types_col = NULL,
  group_col,
  region_col = NULL,
  indicator_name = NULL,
  routine_data = NULL
)
```

## Arguments

- data:

  A data frame containing the coverage indicators and year variable.

- indicator_col:

  A string specifying the column name for the coverage indicator.

- indicator_se_col:

  An optional string specifying the column name for the standard error
  of the indicator.

- data_types_col:

  An optional string specifying the column name for data types in the
  data frame.

- group_col:

  A string specifying the column name for first-level grouping (e.g.,
  country).

- region_col:

  An optional string specifying the column name for second-level
  grouping (e.g., region).

- indicator_name:

  An optional string to use as the title for plots instead of the
  default "Indicator".

- routine_data:

  An optional data frame containing routine data.

## Value

A series of plots for exploratory data analysis.
