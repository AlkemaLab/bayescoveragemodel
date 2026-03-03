# get_standata_routine

get_standata_routine

## Usage

``` r
get_standata_routine(
  service_statistic_df,
  hyper_param,
  time_index,
  geo_unit_index
)
```

## Arguments

- service_statistic_df:

  tibble with iso; admin1 for subnat; routine; sd_routine; routine_roc;
  sd_routine_roc; year filtered to pop (eg country)

- hyper_param:

  mean and sd hyperparameters for roc data model

- time_index:

  from the model fit

- geo_unit_index:

  from the model fit

## Value

list with dat_routine (for plotting) and routine_list (to pass to stan)
