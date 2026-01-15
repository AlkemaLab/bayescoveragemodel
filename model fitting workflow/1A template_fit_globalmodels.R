
# fit_globalmodels.R

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
indicator_select <- "anc4"#"vdpt"#"ideliv"#  #"anc4""vdpt"#


# process data, do this once
dat <- process_data(dat = dat0, regions_dat = regions_dat,
                           indicator = indicator_select)

#### fit the 2 global models ####
# model fits are stored in bayestransition_output

# model fit 1a
fit1a <- fit_model(runstep = "step1a",
                  survey_df = dat,
                  y = "invprobit_indicator",
                  se = "se_invprobit_indicator",
                  get_posteriors = TRUE,
                 # generate_quantities = FALSE,
                  chains = 4
                  #test
                  ,
                  iter_sampling = 10,
                  iter_warmup = 5
        )

### start step 1b
# processing of 1a, to use in 1b
# you need a fit1a object
# option 1 is to continue with fit from above

#### option 2: you can also read from elsewhere
#runstep <- "step1a"
#runnumber <- "run_2"
#fit <- get_fit(indicator = indicator_select, runstep = runstep,runnumber)
#### end option 2

# find possible outliers in step1a, and label as such for run 1b
fit <- fit1a
fit$outliers <- identify_outliers_in_global_fit(fit)
fit$samples <- NULL
dat3 <- assign_outliers(dat, outlier_record_ids = fit$outliers) # only applies to 1a, what obs are outlying and flag those, could be added to 1a output

# use fit1a and updated data
fit1b <- fit_model(runstep = "step1b",
                  global_fit = fit,
                  y = "invprobit_indicator",
                  se = "se_invprobit_indicator",
                  survey_df = dat3,
                  get_posteriors = TRUE,
                  chains = 4
                  #test
                  ,
                  iter_sampling = 10,
                  iter_warmup = 5
)


# a local run for all countries, to get final estimates to use for subnat global
# also for countries where in 1b, we may have outliers assigned
# ethiopia is an example
fit_local_global <- fit_model(runstep = "local_national",
                              # use internal data if saved already, else 1b
                   y = "invprobit_indicator",
                   se = "se_invprobit_indicator",
                   survey_df = dat, # use original dat, not dat3 with additional outliers assigned
                   save_post_summ = TRUE,
                   get_posteriors = TRUE,
                   chains = 4
                   #test
                   ,
                   iter_sampling = 10,
                   iter_warmup = 5
)

p <- plot_estimates_local_all(results = fit_local_global,
                              save_plots = TRUE,
                              indicator_name = indicator_select)


# ### example of a local runs

## testing local - read in fit1b if needed

#fit <- readRDS(file.path(get_output_dir(runname), paste0(indicator, "_fit_wpostsumm.rds")))
# iso_select <- "KEN" #AFG"
# fit_local <- fit_model(runstep = "local_national",
#                       global_fit = fit1b,
#                        y = "invprobit_indicator",
#                       se = "se_invprobit_indicator",
#                       survey_df = dat3,
#                       chains = 8,
#                       iter_sampling = 300,
#                       iter_warmup = 150,
#                       iso_select  = iso_select,
#                       # validation_cutoff_year = 2010,
#                       get_posteriors = TRUE)
#
# # plot local estimates
# p <- plot_estimates_local_all(results = fit_local)

###############################################################################
######## CONTINUE ONLY IF YOU WANT TO OVERWRITE THE INTERNAL DATA FILES ########

## if you want to load an existing fit
##fit_local_global <- get_fit(indicator = indicator_select, runstep = "step1b", "run_workflowpaper")

# fit_local_global$samples <- NULL
# object_name <- paste0(indicator_select, "_summary1b")
# assign(object_name, fit_local_global, envir = .GlobalEnv)

# save data
# do.call(usethis::use_data, args = list(as.name(object_name), overwrite = TRUE))



###############################################################################





