
# Leontine, May 11 2026
# save global fit objects as internal data for local runs
# and regions file

library(tidyverse)
#### indicators
indicator_alldata <- c("anc4",  "bfexcl0_5" ) # those where data prior to 2010 can be used
indicator_2010onwards <- c("anc1trimester" ,"vmsl" , "ideliv", "vdpt" , "ancq8" , "cci" , "sba")
indicator_all <- c(indicator_alldata, indicator_2010onwards)

# read in rds's with global fits
# add indicator: data_from2010only (used in fit_model for filtering)
# save rda, see https://r-pkgs.org/data.html

dir_with_global_fits <- here::here("../bayescoverageanalysis/data_raw/internal/")
for (indicator_select in indicator_all){
  if (indicator_select %in% indicator_alldata){
   global_fit1b <- readRDS(file = file.path(dir_with_global_fits,
                                             paste0("globalfit_", indicator_select, ".rds")))
   global_fit1b$data_from2010only <- FALSE
  } else {
   global_fit1b <- readRDS(file = file.path(dir_with_global_fits,
                                             paste0("globalfit_", indicator_select, "_2010onwards.rds")))
   global_fit1b$data_from2010only <- TRUE
  }
  obj_name <- paste0("globalfit1b_", indicator_select)
  assign(obj_name, global_fit1b, envir = .GlobalEnv)
  do.call(usethis::use_data, list(as.name(obj_name), overwrite = TRUE))
}
# update documentation in R/global_fit_objects.R
# rebuild package and reinstall the package
names(global_fit1b)

# read all files

# dir to use for global fits
datadir <- here::here("../data-raw/internal/")

files <- list.files(datadir, pattern = "\\.[rR][dD][sS]$")
files
for (f in files){
  obj_name <- tools::file_path_sans_ext(f)
  obj <- readRDS(paste0(datadir, f))
  assign(obj_name, obj, envir = .GlobalEnv)
  do.call(usethis::use_data, list(as.name(obj_name), overwrite = TRUE))
}
# update documentation in R/global_fit_objects.R
# rebuild package and reinstall the package

# csv file
regions_all <- read_csv(here::here("data_raw/regions_updated.csv"))
usethis::use_data(regions_all, overwrite = TRUE)
