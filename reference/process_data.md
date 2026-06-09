# Process survey data for a selected indicator

Process survey data for a selected indicator

## Usage

``` r
process_data(dat, regions_dat, indicator = "ideliv", verbose = TRUE)
```

## Arguments

- dat:

  data frame with iso, year, indic, r (indicator), se, source,
  final_year columns

- regions_dat:

  data frame with iso and cluster columns

- indicator:

  character, default "ideliv"

- verbose:

  logical, whether to print messages about data processing

## Value

processed data frame
