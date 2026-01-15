
# get local national fits with and without routine data
# for all cd countries
# using processed routine data

# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
path_localhierclone <- here::here("../local-hierarchy")
devtools::load_all(path_localhierclone)
devtools::load_all(here::here())

#### read data ####
data_folder <- "data_raw"
routinedata_processed <- "private/routine data processed/"

#read national data and region info
dat0 <- read_dta(here::here(data_folder, "ICEH_national.dta"))
regions_dat <- read_csv(here::here(data_folder, "regions.csv"))

# Choose an indicator
indicator_select <- "vdpt"#"anc4"##"anc4"#"vdpt"#"ideliv"#  #"anc4""vdpt"#
iso_select <- "BRL"
names(regions_dat)
#regions_dat %>%
#  filter(name_country == "Cabo Verde")
isos_cd <- regions_dat$iso[regions_dat$cluster %in% c("Sub-Saharan Africa", "North Africa and Middle East")]

for (indicator_select in "ideliv"){ #c("vdpt","ideliv")){ #"anc4",

  # process data, do this once
  dat <- process_data(dat = dat0, regions_dat = regions_dat,
                      indicator = indicator_select)

  # read in routine data
  routine_dat <- read_csv(here::here(routinedata_processed,
                                     paste0("routine_data_nat_", indicator_select, ".csv")))


  isos <- unique(dat$iso)
  isos <- sort(isos[is.element(isos, isos_cd)])
  #iso_select <- "ETH" # # #KEN"#ETH"#KEN" #AFG"MLI"
  #which(isos == iso_select)
  for (iso_select in isos){
    dat_use <- dat %>% filter(year >= 2000, iso == iso_select)
    if (dim(dat_use)[1] > 0){
      fit_local <- fit_model(runstep = "local_national",
                           y = "invprobit_indicator",
                          se = "se_invprobit_indicator",
                          runname = paste0("national_fits/", indicator_select,
                                           "/natlocalfit_", indicator_select, "_", iso_select),
                          survey_df = dat,
                          chains = 4,
                          iter_sampling = 300,
                          iter_warmup = 150,
                          iso_select  = iso_select,
                          # validation_cutoff_year = 2010,
                          get_posteriors = TRUE)

      # plot local estimates
      # p <- plot_estimates_local_all(results = fit_local)

      routine_dat_use <- routine_dat %>% filter(iso == iso_select)
      if (dim(routine_dat_use)[1] > 0){
        fit_local_w_routine <- fit_model(runstep = "local_national",
                                         runname = paste0("national_fits/", indicator_select,
                                                          "/natlocalfit_wroutine_", indicator_select, "_", iso_select),
                                         survey_df = dat  %>% filter(iso == iso_select),
                                         routine_data = routine_dat_use,
                                         chains = 4,
                                         iter_sampling = 300,
                                         iter_warmup = 150,
                                         iso_select  = iso_select,
                                         get_posteriors = TRUE)
      }
    }
    } # end iso loop with data
} # end indicator_select loop
#do 27

# plots
p <- plot_estimates_local_all(results = fit_local)
p
p <- plot_estimates_local_all(results = fit_local_w_routine)
p
if (dim(routine_dat_use)[1] > 0){
  compare_p <- plot_estimates_local_all(results = fit_local_w_routine,
                                        results2 = fit_local,
                                        modelnames = c("survey w/ routine", "survey-only"))
  compare_p[[1]] +
    theme_grey(base_size = 100)
}
