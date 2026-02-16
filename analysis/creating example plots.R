
# note: manually change plot_estimates_local to get the formatting we need for toy pltos


# load libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
library(cmdstanr)
path_bayescoveragemodel <- here::here("../bayescoveragemodel")
devtools::load_all(path_bayescoveragemodel)



# check documentation in
# ?fit_model
# documentation/modelspec.qmd and knitted files

# setting

#### read data ####
data_folder <- "data_raw"

#read subnational data and region info
regions_dat <- read_csv(here::here(data_folder, "regions.csv"))
# subnational data
dat0 <- read_csv(here::here(data_folder, "ICEH_adm1_dataforcurrentregions.csv"))
# let's rename into admin1
dat0 <- dat0 %>% rename(admin1 = unique_name)

# Choose an indicator
indicator_select <- "vdpt"#"ideliv"#  #"anc4""vdpt"#


# process data, do this once
dat <- bayescoveragemodel::process_data(dat = dat0, regions_dat = regions_dat,
                    indicator = indicator_select)

# nigeria
iso_select <- "NGA" #ETH" #MLI" # #KEN"#ETH"#KEN" #AFG"

popweights <- read_csv(here::here(paste0("data_raw/est_denominatorweights_", indicator_select,  ".csv")))

# read national data
dat0_nat <- read_dta(here::here(data_folder, "ICEH_national.dta"))
national_dat_df <- bayescoveragemodel::process_data(dat = dat0_nat, regions_dat = regions_dat,
                    indicator = indicator_select)
#dat %>% filter(iso == iso_select) %>% pull(admin1) %>% unique()

fit_local <- bayescoveragemodel::fit_model(runstep = "local_subnational",
                      survey_df = dat  %>% filter(iso == iso_select),
                      national_dat_df = national_dat_df %>% filter(iso == iso_select),
                      popweights  = popweights ,
                      chains = 4,
                      iter_sampling = 300,
                      iter_warmup = 150,
                      iso_select  = iso_select,
                      get_posteriors = TRUE)

# plot local estimates
p <- bayescoveragemodel::plot_estimates_local_all(results = fit_local)
p

# some specific plots for slides
# using nigeria

p1 <- p[[4]] +
  theme_minimal(base_size=18) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  # just updating the name in legend
 # scale_colour_manual(values = c("DHS" = "red", "MICS" = "blue"),
 #                     name = "Survey data") +
  # no legend
  theme(legend.position = "none")
p2 <- p[[length(p) - 5]] +
  theme_minimal(base_size=18) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  # just updating the name in legend
  scale_colour_manual(values = c("DHS" = "red", "MICS" = "blue"),
                      name = "Survey data") +
  # no legend
  theme(legend.position = "none")
ggarrange(plotlist = list(p1, p2),
          ncol = 2, nrow = 1)


# also comparison plot
names(fit_local)
fit_local$estimates
bayescoveragemodel::plot_subnational_comparison(results = fit_local, year_select = 2024) +
  # no x axis labels
  theme(base_size=18) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
    labs(x = "Region", y = "Coverage") +
  # larger fonts
    theme(legend.position = "none")


# add mali too
iso_select <- "MLI" # #KEN"#ETH"#KEN" #AFG"
# for ideli
indicator_select <- "ideliv"#  #"anc4""vdpt"#"vdpt"#
# process data, do this once
dat <- bayescoveragemodel::process_data(dat = dat0, regions_dat = regions_dat,
                    indicator = indicator_select)
popweights <- read_csv(here::here(paste0("data_raw/est_denominatorweights_", indicator_select,  ".csv")))

# read national data
dat0_nat <- read_dta(here::here(data_folder, "ICEH_national.dta"))
national_dat_df <- bayescoveragemodel::process_data(dat = dat0_nat, regions_dat = regions_dat,
                                indicator = indicator_select)
fit_local_mali <- bayescoveragemodel::fit_model(runstep = "local_subnational",
                       survey_df = dat  %>% filter(iso == iso_select),
                       national_dat_df = national_dat_df %>% filter(iso == iso_select),
                       popweights  = popweights ,
                       chains = 4,
                       iter_sampling = 300,
                       iter_warmup = 150,
                       iso_select  = iso_select,
                       get_posteriors = TRUE)


# plot local estimates
p_mali <- bayescoveragemodel::plot_estimates_local_all(results = fit_local_mali)

# w routine data
routine_dat <- read_csv(here::here(routinedata_processed,
                                   paste0("routine_data_subnat_", indicator_select, ".csv")))
routine_admins <- routine_dat %>% filter(iso == iso_select) %>% pull(admin1) %>% unique()
is.element(routine_admins, fit_local_mali$geo_unit_index$admin1)
# remove those that don't match
routine_dat_use <- routine_dat %>%
  filter(iso == iso_select) %>%
  filter(admin1 %in% fit_local_mali$geo_unit_index$admin1)
routine_dat_use$routine_roc

#devtools::load_all(here::here())
fit_local_w_routine <- bayescoveragemodel::fit_model(runstep = "local_subnational",
                                 survey_df = dat  %>% filter(iso == iso_select),
                                 national_dat_df = national_dat_df %>% filter(iso == iso_select),
                                 popweights  = popweights ,
                                 routine_data = routine_dat_use,
                                 chains = 4,
                                 iter_sampling = 300,
                                 iter_warmup = 150,
                                 iso_select  = iso_select,
                                 get_posteriors = TRUE)


# plot local estimates
p_maliwroutine <- bayescoveragemodel::plot_estimates_local_all(results = fit_local_w_routine)



# compare survey-only and incl. routine results
compare_p <- bayescoveragemodel::plot_estimates_local_all(results = fit_local_w_routine,results2 = fit_local_mali,
                                      modelnames = c("survey w/ routine", "survey-only"))
compare_p[[3]]


# create one combined plot for handout
# note that adding h and vlines does odd things in rerun
# leaving them here for now
# at end, adding plot wiht just data
# without oddities

p1 <- p[[length(p) - 5]] +
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black")+
  ggtitle("Region A")#+

p2 <- p[[4]] +
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  ggtitle("Region B")#+
#  theme(legend.position = "none")
p_combined <- ggarrange(plotlist = list(p1, p2),
                        ncol = 1, nrow = 2,
                        # one legend in bottom
                        common.legend = TRUE, legend = "none")
p_combined



p3 <- compare_p[[2]] +
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black")+
  # add title
  ggtitle("Region C")
  ##+
#  theme(legend.position = "none")
p4 <- compare_p[[3]]+
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  ggtitle("Region D")#+
# theme(legend.position = "none")
p_combined <- ggarrange(plotlist = list(p1, p2, p3, p4),
                        ncol = 2, nrow = 2,
                        # one legend in bottom
                        common.legend = TRUE, legend = "none")
p_combined

# just compare mali
p_combined <- ggarrange(plotlist = list(p3, p4),
                        ncol = 1, nrow = 2,
                        # one legend in bottom
                        common.legend = TRUE, legend = "none")
p_combined

# for a legend to copy
p_combined <- ggarrange(plotlist = list(p3, p4),
                        ncol = 2, nrow = 1,
                        # one legend in bottom
                        common.legend = TRUE, legend = "bottom")
p_combined

# with just the routine fit for mali
p3 <- p_maliwroutine[[2]] +
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black")+
  # add title
  ggtitle("Region C")
##+
#  theme(legend.position = "none")
p4 <- p_maliwroutine[[3]]+
  theme_minimal(base_size=11) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  ggtitle("Region D")#+
# theme(legend.position = "none")
p_combined <- ggarrange(plotlist = list(p1, p2, p3, p4),
                        ncol = 2, nrow = 2,
                        # one legend in bottom
                        common.legend = TRUE, legend = "none")
p_combined

p

devtools::load_all(path_bayescoveragemodel)
#  just data
p_justdata <- bayescoveragemodel::plot_estimates_local_all(results = fit_local, add_estimates = FALSE)
compare_p_justdata <- bayescoveragemodel::plot_estimates_local_all(results = fit_local_w_routine,
                                               results2 = fit_local_mali,
                                      modelnames = c("survey w/ routine", "survey-only"),
                                      add_estimates = FALSE)


p1 <- p_justdata[[length(p) - 5]] +
  theme_minimal(base_size=14) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black")+
  ggtitle("Region A")#+
p1
p2 <- p_justdata[[4]] +
  theme_minimal(base_size=14) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  ggtitle("Region B")#+
p3 <- compare_p_justdata[[2]] +
  theme_minimal(base_size=14) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black")+
  # add title
  ggtitle("Region C")
##+
#  theme(legend.position = "none")
p4 <- compare_p_justdata[[3]]+
  theme_minimal(base_size=14) +
  labs(x = "Year", y = "Coverage") +
  geom_hline(yintercept = 0,  color = "black") +
  geom_vline(xintercept = 2000,  color = "black") +
  ggtitle("Region D")#+
# theme(legend.position = "none")
p_combined <- ggarrange(plotlist = list(p1, p2, p3, p4),
                        ncol = 2, nrow = 2,
                        # one legend in bottom
                        common.legend = TRUE, legend = "none")
p_combined
