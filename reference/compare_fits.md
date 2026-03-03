# Compare multiple model fits in a single plot

This function takes run names, loads the corresponding fits, and plots
them together using plot_estimates_local_all.

## Usage

``` r
compare_fits(
  runnames,
  modelnames = NULL,
  iso_codes = NULL,
  dat_routine = NULL,
  indicator_name = NULL,
  save_plots = FALSE,
  output_folder = NULL,
  add_caption = FALSE,
  add_estimates = TRUE,
  use_for_facetting = FALSE
)
```

## Arguments

- runnames:

  Character vector of run names (2-4 names). Run names follow the
  pattern: indicator*runstep*folder_suffix, e.g.,
  "anc4_step1b_run_alldata_jan16"

- modelnames:

  Character vector of display names for the legend (same length as
  runnames). If NULL, uses the runnames.

- iso_codes:

  Optional character vector of ISO codes to filter plots. If NULL,
  returns plots for all countries.

- dat_routine:

  Optional routine data to overlay on plots

- indicator_name:

  Custom title for y-axis (if NULL, pulled from fit\$data)

- save_plots:

  Boolean to save plots to PDF

- output_folder:

  Override directory for saving

- add_caption:

  Boolean to add explanatory caption

- add_estimates:

  Boolean to show model estimates/ribbons

- use_for_facetting:

  Boolean to adjust plot titles for facetted output

## Value

List of ggplot objects, one per geographic unit (filtered by iso_codes
if provided)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compare step1a and step1b for anc4
plots <- compare_fits(
  runnames = c("anc4_step1b_run_alldata_jan16", "anc4_step1a_run_alldata_jan16"),
  modelnames = c("Step 1b", "Step 1a")
)

# Compare validation and non-validation fits for specific countries
plots <- compare_fits(
  runnames = c("vdpt_step1b_run_alldata_jan16", "vdpt_step1b_val_alldata_jan16"),
  modelnames = c("Full model", "Validation"),
  iso_codes = c("ETH", "GHA", "KEN", "NGA", "UGA")
)
} # }
```
