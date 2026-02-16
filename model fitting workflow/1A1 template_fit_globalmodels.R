
# fit_globalmodels.R

# this script can be used to fit global models
# using step 1ab


# in this script, we will fit the global models (currently referred to as steps 1a and 1b)
# we use low-ish numbers of iterations and chains to make this possible on laptops
# for final results/at a later step, we will increase those settings and fit on a server

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)

# you need the localhierarchy package
# this package is now public
# info is here: https://alkemalab.github.io/localhierarchy/
# to install (do this once):
# remotes::install_github("AlkemaLab/localhierarchy")
# after installation, load the package
library(localhierarchy)

# load code from this package
devtools::load_all(here::here())

# check documentation in
# ?fit_model
# documentation/modelspec.qmd and knitted files

# setting

#### read data ####
data_folder <- "data_raw"
#read national data and region info
dat0 <- read_dta(here::here(data_folder, "ICEH_national.dta"))
regions_dat <- read_csv(here::here(data_folder, "regions.csv"))


# Choose an indicator
indicator_select <- "vdpt"#"anc4"#"vdpt"#"ideliv"#  #"anc4""vdpt"#


# process data, do this once
dat <- process_data(dat = dat0, regions_dat = regions_dat,
                           indicator = indicator_select)

#### fit the global model ####
# model fits are stored in bayestransition_output

# model fit 1ab
fit1ab <- fit_model(runstep = "step1ab",
                  survey_df = dat,
                  y = "invprobit_indicator",
                  se = "se_invprobit_indicator",
                  get_posteriors = TRUE,
                 # generate_quantities = FALSE,
                 model_name = "rw2",
                 runnumber = "rw2",
                  chains = 4
                  #test
                  # ,
                  # iter_sampling = 10,
                  # iter_warmup = 5
        )




p <- plot_estimates_local_all(results = fit1ab,
                              save_plots = TRUE,
                              indicator_name = indicator_select)




