# Package index

## Model Fitting

Core functions for fitting Bayesian transition models

- [`fit_model()`](https://alkemalab.github.io/bayescoveragemodel/reference/fit_model.md)
  : Fit the transition model to data.
- [`write_model()`](https://alkemalab.github.io/bayescoveragemodel/reference/write_model.md)
  : Write Stan models for different model settings.
- [`get_fit()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_fit.md)
  : Load a saved model fit

## Data Processing

Functions for preparing data

- [`process_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/process_data.md)
  : Process survey data for a selected indicator
- [`explore_data()`](https://alkemalab.github.io/bayescoveragemodel/reference/explore_data.md)
  : Explore Coverage Indicators Over Time

## Visualization & Output

Functions for plotting and model comparison

- [`plot_estimates_local_all()`](https://alkemalab.github.io/bayescoveragemodel/reference/plot_estimates_local_all.md)
  : Plot model fits, with options to add routine data or additional fits
  for comparison.
- [`plot_hierchecks()`](https://alkemalab.github.io/bayescoveragemodel/reference/plot_hierchecks.md)
  : Plot hierarchical model checks
- [`compare_fits()`](https://alkemalab.github.io/bayescoveragemodel/reference/compare_fits.md)
  : Compare multiple model fits in a single plot

## Diagnostics

Functions for convergence and model checking

- [`get_convergence_diagnostics()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_convergence_diagnostics.md)
  : Get convergence diagnostics for a fitted model

## Extraction for model checks

Functions for extracting posterior samples of residuals, innovation
terms, and etas.

- [`get_eta_samples()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_eta_samples.md)
  : Extract posterior samples of eta (latent coverage) for a specific
  year
- [`get_inno_samples()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_inno_samples.md)
  : Extract Innovation Samples from Fitted Model
- [`get_residuals_samples()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_residuals_samples.md)
  : Extract Residuals from Posterior Samples

## Utilities

Helper and transformation functions

- [`probit()`](https://alkemalab.github.io/bayescoveragemodel/reference/probit.md)
  : Probit Transform (Probit to Proportion)
- [`inv_probit()`](https://alkemalab.github.io/bayescoveragemodel/reference/inv_probit.md)
  : Inverse Probit Transform (Proportion to InvProbit)
- [`backtransform_residuals()`](https://alkemalab.github.io/bayescoveragemodel/reference/backtransform_residuals.md)
  : Back-transform Residuals to Proportion Scale
- [`get_relative_output_dir()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_relative_output_dir.md)
  : Get relative output directory path
- [`rename_output_folder()`](https://alkemalab.github.io/bayescoveragemodel/reference/rename_output_folder.md)
  : Rename output folder and directory in fit object
- [`get_se_invprobitprop()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_se_invprobitprop.md)
  : Get Standard Error on invProbit Scale from Proportion
- [`get_se_logitprop()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_se_logitprop.md)
  : Get Standard Error on Logit Scale
- [`get_se_probitofinvprobitprop()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_se_probitofinvprobitprop.md)
  : Get Standard Error on Proportion Scale from invProbit proportion
- [`identify_outliers_in_global_fit()`](https://alkemalab.github.io/bayescoveragemodel/reference/identify_outliers_in_global_fit.md)
  : identify_outliers_in_global_fit
- [`get_standata_routine()`](https://alkemalab.github.io/bayescoveragemodel/reference/get_standata_routine.md)
  : get_standata_routine
