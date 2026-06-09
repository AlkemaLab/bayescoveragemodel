# get_standata_routine

get_standata_routine

## Usage

``` r
get_standata_routine(routine_data, fit_routine_obj, time_index, geo_unit_index)
```

## Arguments

- routine_data:

  tibble with iso, year, indicator_name, routine_value, countdownmean
  routine_value is the value of the routine data (eg coverage) and
  countdownmean is the data quality indicator for that
  iso-year-indicator combi needs to be filtered to iso of interest

- fit_routine_obj:

  brm-fit object

- time_index:

  from the model fit

- geo_unit_index:

  from the model fit

## Value

list with dat_routine (for plotting) and routine_list (to pass to stan)
