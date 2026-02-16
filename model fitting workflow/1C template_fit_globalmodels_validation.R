
# template_global_validationruns.R

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
# remotes::install_github("AlkemaLab/localhierarchy")
# after installation, load the package
library(localhierarchy)
# load code from this package
devtools::load_all(here::here())

# setting
# Choose an indicator
indicator_select <- "anc4"#"ideliv" #"anc4"# "vdpt"#ideliv"
# we'll set validation_cutoff_year below (to check how much data left out)

#### read data ####
data_folder <- "data_raw"
#read national data and region info
dat0 <- read_dta(here::here(data_folder, "ICEH_national.dta"))
regions_dat <- read_csv(here::here(data_folder, "regions.csv"))
# process data, do this once
dat <- process_data(dat = dat0, regions_dat = regions_dat,
                         indicator = indicator_select)

# decide validation cutoff year
validation_cutoff_year <- 2018 # data are left out for start_date >= validation_cutoff_year
# percentage of data left out
mean(dat$start_date >= validation_cutoff_year)

#### fit the 2 global models ####
# model fits are stored in bayestransition_output

# model fit 1a
devtools::load_all(here::here())
fit1a <- fit_model(runstep = "step1ab",
                  survey_df = dat,
                  get_posteriors = TRUE,
                  chains = 4,
                  model_name = "rw2",
               #   runnumber = "rw2_val",
                  validation_cutoff_year = validation_cutoff_year
                  # sampling settings for a test run
                  # ,iter_sampling = 10,
                  # iter_warmup = 5
)
fit <- fit1a

# plotting of results from 1a (these are fits w/o smoother)
plots1a <- plot_estimates_local_all(results = fit, save_plots = TRUE)
#results <- get_validation_results(fit)
results

# step1b

# you need a fit1a object
# option 1 is to continue with fit from above
# option 2: you can also read from elsewhere,
# # # start option 2, ie if needing to read in older 1a
#runstep <- "step1a"
#folder_suffix <- run_workflowpaper
#fit <- get_fit(indicator_select, runstep, folder_suffix)
# # # end option 2

# processing of 1a, to use in 1b
# find possible outliers in step1a, and label as such for run 1b
fit$outliers <- identify_outliers_in_global_fit(fit)
dat3 <- assign_outliers(dat, outlier_record_ids = fit$outliers) # only applies to 1a, what obs are outlying and flag those, could be added to 1a output
# use fit1a and updated data
fit$samples <- NULL
fit1b <- fit_model(runstep = "step1b",
                  global_fit = fit,
                  survey_df = dat3,
                  get_posteriors = TRUE,
                  chains = 4,
                  validation_cutoff_year = validation_cutoff_year
              # # sampling settings for a test run
              # ,iter_sampling = 10,
              # iter_warmup = 5
)

# processing to use fit 1b in local fits
fit <- fit1b
plots1b <- plot_estimates_local_all(results = fit, save_plots = TRUE)
results <- get_validation_results(fit, save_results = TRUE)
results
###

### example of a local validation runs for one country
# iso_select <- "KEN" #AFG"
# fit_local <- fit_model(runstep = "local_national",
#                        iso_select  = iso_select,
#                        global_fit = fit,
#                       survey_df = dat3,
#                       chains = 8,
#                       iter_sampling = 300,
#                       iter_warmup = 150,
#                       get_posteriors = TRUE,
#                       validation_cutoff_year = validation_cutoff_year)
# # plot local estimates
# p <- plot_estimates_local_all(results = fit_local, save_plots = TRUE)
