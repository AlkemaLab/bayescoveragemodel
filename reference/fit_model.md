# Fit the transition model to data.

Fit the transition model to data.

## Usage

``` r
fit_model(
  survey_df,
  national_dat_df = NULL,
  routine_df = NULL,
  mean_log_sigma = NULL,
  popweights = NULL,
  y = "invprobit_indicator",
  se = "se_invprobit_indicator",
  year = "year",
  source = "data_series_type",
  area = "iso",
  iso_select = NULL,
  routine_data = NULL,
  start_year = 2000,
  end_year = 2030,
  runstep,
  global_fit = NULL,
  t_star = 2010,
  spline_degree = 2,
  num_knots = 8,
  Betas_upper_bound = 0.5,
  Betas_lower_bound = 0.01,
  Ptilde_low = 0,
  add_dataoutliers = TRUE,
  extra_stan_data = list(),
  hierarchical_level = c("intercept", "cluster", "subcluster", "iso"),
  hierarchical_splines = c("intercept", "cluster", "subcluster", "iso"),
  hierarchical_asymptote = c("intercept", "iso"),
  add_subnational_hierarchy = "admin1",
  use_globalsubnat_fromnat = FALSE,
  model_name = "spline",
  held_out = FALSE,
  validation_cutoff_year = NULL,
  validation_run = FALSE,
  save_post_summ = FALSE,
  generate_quantities = TRUE,
  get_posteriors = TRUE,
  create_runname_and_outputdir = TRUE,
  runnumber = 1,
  rungroup = NULL,
  runname = NULL,
  chains = 4,
  iter_sampling = 200,
  iter_warmup = 150,
  add_sample = TRUE,
  compile_model = TRUE,
  force_recompile = FALSE,
  seed = 1234,
  refresh = 10,
  adapt_delta = 0.9,
  max_treedepth = 14,
  variational = FALSE,
  nthreads_variational = 8,
  add_inits = TRUE
)
```

## Arguments

- survey_df:

  tibble with survey data

- routine_df:

  tibble with routine data

- y:

  column name of outcome.

- se:

  column name of outcome standard error.

- year:

  column name of outcome year.

- source:

  column name of data source.

- area:

  column name of the area of each observation

  for subnational:

- iso_select:

  ISO code to use for local national run

- start_year:

  start year of estimates.

- end_year:

  end year of estimates.

  Settings for global model fit 1a

- runstep:

  type of run, currently one of "step1a", "step1ab", "step1b",
  "local_national" (see Details).

- global_fit:

  optional object of class `"fpemplus"`, used to obtain fixed values to
  use for some parameters in the current fit (see Details).

- t_star:

  reference year used in model.

- spline_degree:

  spline degree. Degree 2 or 3 is supported.

- num_knots:

  number of spline knots.

- Betas_upper_bound:

  upper bound for the splines parameters

- Betas_lower_bound:

  lower bound for the splines parameters

- Ptilde_low:

  lower bound for the asymptote Ptilde

- add_dataoutliers:

  boolean indicator of whether to include data outliers in 1b

- extra_stan_data:

  list of additional data to pass to Stan model

- hierarchical_level:

  vector specifying hierarchical structure for the level in reference
  year (see Details).

- hierarchical_splines:

  vector specifying hierarchical structure for spline coefficients (see
  Details).

- hierarchical_asymptote:

  vector specifying hierarchical structure for asymptote (see Details).

- model_name:

  character string specifying the model to fit. Options are:

  - `"spline"` (default): Transition model, ARIMA(1,1,0) with
    spline-based drift.

  - `"rw2"`: Local rate of change model, ARIMA(1,2,0)

- held_out:

  binary vector indicating which observations are held out. Set to FALSE
  to hold out no observations.

- validation_cutoff_year:

  year to use for out-of-sample validation, overwrites held_out (to
  confirm it does)

- validation_run:

  boolean indicator of whether it's a validation model run or not

- generate_quantities:

  binary vector indicating whether to simulate data from the fitted
  model

  Setting for where to save things

- get_posteriors:

  boolean indicator of whether to return posterior samples

- create_runname_and_outputdir:

  boolean indicator of whether to create a runname and output directory

- runnumber:

  number to add to runname

- rungroup:

  group to add to runname

- runname:

  name to use for run

  Settings for sampling

- chains:

  number of chains to run

- iter_sampling:

  number of posterior samples to draw

- iter_warmup:

  number of warmup iterations

- add_sample:

  boolean indicator of whether to return samples

- compile_model:

  boolean indicator of whether to compile the Stan model

- force_recompile:

  boolean indicator of whether to force recompilation of the Stan model

- seed:

  random seed

- refresh:

  number of iterations between progress updates

- adapt_delta:

  target acceptance rate for the No-U-Turn Sampler

- max_treedepth:

  maximum tree depth for the No-U-Turn Sampler

- variational:

  boolean indicator of whether to use variational inference (not yet
  tested)

- nthreads_variational:

  number of threads to use for variational inference

- add_inits:

  boolean indicator of whether to add initial values to the Stan model

- population_data:

  a data frame with yearly population counts for subnational regions. It
  should have columns matching the names specified for `year` and
  `area`. This data frame is only required if the primary `data` set
  contains a mix of observations at national and subnational levels.

## Value

fpemplus object.

## Details

The `fit_model` function fits the transition model to data. The model is
fitted using Stan, and the function returns an object of class
`"fpemplus"`. The argument `runstep` determines the type of run to
perform. The following run steps are supported:

- "step1a": Fit the model without the smoothing term, to estimate longer
  term trends.

- "step1ab": Fit the model with smoothing terms, estimating all
  parameters (none fixed). Unlike step1b, this does not require a prior
  fit and estimates everything in one pass.

- "step1b": Fit the model with smoothing terms, using a fit from step1a,
  to estimate all smoothing and data model parameters.

- "local_national": Fit the model to data from a single country, using a
  1b fit. This is also explained in the documentation folder.

Details on hierarchical set ups used Several area-specific parameters of
the fpemplus model have hierarchical priors assigned to them so that
information can be shared between areas. The package allows the
structure of the hierarchical prior to be configured by the user through
the `hierarchical_asymptote`, `hierarchical_level`, and
`hierarchical_splines` arguments. These arguments expect a character
vector that specifies a nesting hierarchical structure. Each element of
the vector must be either "intercept" or a column name in the dataset,
where "intercept" will add a global intercept for the parameter. The
vector must be in descending order in terms of the hierarchy: that is,
it starts with "intercept" and proceeds down the hierarchy.

For example, suppose we are fitting country-level data, where the
dataset has columns "name_country", "name_sub_region", and "name_region"
containing the name of the country, sub-region, and region that each
observation belongs to. To specify that the spline coefficients should
be fitted with a hierarchical model in which countries are nested within
sub-regions within regions within world, we would use the argument
`hierarchical_splines = c("intercept", "name_region", "name_sub_region", "name_country")`.

Optionally, model parameters can be fixed to values from a previous
model fit provided via the `global_fit` argument. In a typical use case,
the `global_fit` will have been fit to data from many geographic units
(e.g., all countries), while the current fit uses data from a smaller
number of locations. Note: remainder of details is currently determined
by the runstep To use a global fit to fix parameter values, a number of
settings must be the same in the global fit and the current call to
`fpemplus`:

- For any of the hierarchical settings, e.g. `hierarchical_asymptote`,
  all of the hierarchical levels used for that parameter in the global
  fit must also be used in the current fit. For instance, if the
  `global_fit` used
  `hierarchical_asymptote = c("intercept", "name_region", "name_country")`,
  then in the current fit it is valid to use
  `hierarchical_asymptote = c("intercept", "name_region", "name_country")`
  again or
  `hierarchical_asymptote = c("intercept", "name_region", "name_country", "name_subnational")`,
  but it is not valid to use
  `hierarchical_asymptote = c("intercept", "name_region", "name_subnational")`.

- All hierarchical levels for terms and sigmas to fix in the current fit
  are contained in the levels in the hierarchy used for the
  corresponding parameters in the global fit. For example, if
  `hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region", "name_country")`
  then the global fit must have included at least "intercept",
  "name_region", and "name_country" for `hierarchical_asymptote`.

- Any hierarchical levels to fix in the current fit are at the highest
  levels of the hierarchical structure. For example, if
  `hierarchical_asymptote = c("intercept", "name_region", "name_country")`,
  then it is valid to use
  `hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region")`,
  but it is not valid to use
  `hierarchical_asymptote_sigmas_fixed = c("intercept", "name_country")`.

- It is not valid to fix terms at a given hierarchy level without also
  fixing the sigma estimate at that hierarchy level. For example, we
  cannot specify `hierarchical_asymptote_sigmas_fixed = c("intercept")`
  and
  `hierarchical_asymptote_terms_fixed = c("intercept", "name_region")`

- It is only valid to set `fix_smoothing = TRUE` if also
  `smoothing = TRUE`.

- All settings for the arguments `model`, `t_star`, `smoothing`,
  `tau_prior`, and `rho_prior` must be the same in the `global_fit` and
  the current fit.

- If `hierarchical_splines_sigmas_fixed` or
  `hierarchical_splines_terms_fixed` include any hierarchical levels
  (i.e., if either is different from the empty vector
  [`c()`](https://rdrr.io/r/base/c.html)), all settings for `num_knots`
  and `spline_degree` must be the same in the `global_fit` and the
  current fit.

- All geographic units that appear in the `data` for the current fit at
  hierarchical levels for which any parameter is fixed must have also
  been included in the `data` used for the `global_fit`.

- If `fix_nonse = TRUE`, all data `source`s that appear in the `data`
  for the current fit must also have been included in the `data` used
  for the `global_fit`.
