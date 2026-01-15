# This script estimates hyper parameters for routine data use

# Load Libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(readxl)
devtools::load_all(here::here())
devtools::load_all("~/Documents/GitHub/cdroutine")

# where clean routine data are saved
routinedata_processed <- "private/routine data processed/"
# choose indicator
indicator <- "ideliv" #"anc4"#"vdpt" # #

# Load routine data
routine_dat <- read_csv(here::here(routinedata_processed,
                                   paste0("routine_data_nat_", indicator, ".csv")))
# Load the fitted model from bayestransition_output folder
runstep <- "step1b"
global_fit1b <- get_fit(indicator = indicator, runstep = runstep, folder_suffix = "run_workflowpaper")

# plot routine data on survey-only model fits and save in fit folder
fits_with_routine_data <- plot_estimates_local_all(global_fit1b,
                                                   dat_routine = routine_dat,
                                                   save_plots = TRUE)
# routine data exploratory analysis function
output <- compare_routine_estimates(dat_routine = routine_dat,
                                    survey_only_fit = global_fit1b,
                                    save_plots = TRUE)

# show some examples of the output from compare_routine_estimates
output$data
output$plots$country_data_plot
output$plots$survey_informed_data_plot

# perform variance analysis on routine data
# defaults to using within_survey_period = TRUE
variance_output <- compute_routine_variance(output$data)
variance_output$routine_hyperparameters

# just checking the results for within_survey_period = FALSE (not used)
# we find that mu is similar, slight lower mean when using all data
# and smaller variance when using all data
variance_output_all <- compute_routine_variance(output$data, within_survey_period = FALSE)
variance_output_all$routine_hyperparameters

###############################################################################
######## CONTINUE ONLY IF YOU WANT TO OVERWRITE THE INTERNAL DATA FILES ########

# to do: update to save as internal data, data/.rda
# we save the hyper params form the within survey period fit
#saveRDS(variance_output$routine_hyperparameters[, c("mean_log_sigma", "hierarchical_sigma")],
#        file = here::here("data_raw/internal/", paste0("routine_hyperparameters_", indicator, ".rds")))

###############################################################################
