
# template_createoutputsmodelchecks.R
# update: no model check, just convergence and prior-post and hier checks

# To create the fitted models, see templates template_fit_globalmodels.R

### Set up
# libraries
library(here)
library(tidyverse)
library(ggplot2)
devtools::load_all(here::here())
# remotes::install_github("AlkemaLab/localhierarchy")
# after installation, load the package
library(localhierarchy)

### Indicator
# Choose your indicator
indicator <- "ideliv"
#indicator <- "vdpt"
#indicator <- "anc4"

#for (indicator in c("ideliv", "vdpt", "anc4")) {

### Analysis of global fits using all data

# Load the fitted model from output folder
fit1a <- get_fit(indicator, runstep = "step1a", folder_suffix = "run_workflow")
fit1b <- get_fit(indicator, runstep = "step1b", folder_suffix = "run_workflow")

# create outputs
plots_fit1a <- plot_estimates_local_all(results = fit1a, save_plots = TRUE, indicator_name = indicator)
plots_fit1b <- plot_estimates_local_all(results = fit1b, save_plots = TRUE, indicator_name = indicator)


# ### additional outputs for model diagnostics
get_convergence_diagnostics(fit1b) # some to dos left in this function
plot_hierchecks(fit1b)
get_convergence_diagnostics(fit1a)
plot_hierchecks(fit1a)

