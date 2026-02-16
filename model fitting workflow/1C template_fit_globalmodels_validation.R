
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

#### fit the global model ####
# model fits are stored in bayestransition_output

# model fit 1a
devtools::load_all(here::here())
fit1ab <- fit_model(runstep = "step1ab",
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
fit <- fit1ab

plots1ab <- plot_estimates_local_all(results = fit, save_plots = TRUE)
