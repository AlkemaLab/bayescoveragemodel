# This script shows how to process and plot national routine data
# and survey data using the explore_data function

# Load Libraries
library(here)
library(tidyverse)
library(ggplot2)
library(haven)
library(stringr)
devtools::load_all(here::here())

# Load data
data_folder <- "data_raw"
regions_dat <- read_csv(here::here(data_folder, "regions.csv"))

# old
dat <- read_dta(here::here(data_folder, "ICEH_national.dta"))

# or new
dat <- read_dta(here::here(data_folder, "ICEH_all.long.dta")) %>%
  # renaming to have same as in dat_old
  rename(r = r_raw, se = se_raw) %>%
  mutate(final_year = year)


# remove data before 2000, as currently not used in fitting
dat <- dat %>%
  filter(final_year >= 2000)

names(dat)
unique(dat$indic)
dat %>%
  filter(indic == "anc4")

# Process data
dat_anc <- process_data(dat = dat,
                        regions_dat = regions_dat,
                        indicator = "anc4")
dat_anc %>%
  group_by(subcluster) %>%
  summarize(meany = mean(year))


# Use the explore_data function to plot routine and survey data
plots_anc <- explore_data(
  data = dat_anc%>%filter(data_series_type == "DHS"), # survey data to explore
  indicator_col = "indicator", # Coverage indicator column
  group_col = "country", # Grouping column
  data_types_col = "data_series_type",
  indicator_name = "ANC4", # Custom plot title
  region_col = "cluster"
)
plots_anc[[3]]

# same for other indicators
dat_ideliv <- process_data(dat = dat,
                        regions_dat = regions_dat,
                        indicator = "ideliv")
dat_ideliv %>%
  group_by(subcluster) %>%
  summarize(meany = mean(year), n = n())


# Use the explore_data function to plot routine and survey data
plots_ideliv <- explore_data(
  data = dat_ideliv %>%filter(data_series_type == "DHS"), # survey data to explore
  indicator_col = "indicator", # Coverage indicator column
  group_col = "country", # Grouping column
  data_types_col = "data_series_type",
  indicator_name = "Facility-based deliveries", # Custom plot title
  region_col = "cluster"
)



dat_vdpt <- process_data(dat = dat,
                        regions_dat = regions_dat,
                        indicator = "vdpt")



# Use the explore_data function to plot routine and survey data
plots_vdpt <- explore_data(
  data = dat_vdpt%>%filter(data_series_type == "DHS"), # survey data to explore
  indicator_col = "indicator", # Coverage indicator column
  group_col = "country", # Grouping column
  data_types_col = "data_series_type",
  indicator_name = "Vaccination coverage", # Custom plot title
  region_col = "cluster"
)


# to make plots with data ~ time for selected countries
dat_all <- process_data(dat = dat,
                         regions_dat = regions_dat,
                         indicator = "all")
names(dat_all)
datplot <- dat_all %>%
  mutate(name_indicator = case_when(
    indic == "anc4" ~ "ANC4",
    indic == "ideliv" ~ "Institutional deliveries",
    indic == "vdpt" ~ "Vaccination coverage",
    TRUE ~ indic
  )) %>%
  mutate(ci_low = probit(invprobit_indicator - 2*se_invprobit_indicator),
         ci_high = probit(invprobit_indicator + 2*se_invprobit_indicator))

datplot %>%
  ggplot(aes(x = year, y = indicator, group = country)) +
  geom_point() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  facet_wrap(~ name_indicator, ncol = 4)

# plot (ncountries times indicators)
datplot %>%
  filter(name_country  %in% c("Sao Tome and Principe", "Cambodia", "Ethiopia",  "Ghana",  "Uganda",  "Nigeria")) %>%#"Mali",
  ggplot(aes(x = year, y = indicator, color = data_series_type)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high)) +
  facet_grid(name_country~ name_indicator) +
  geom_vline(xintercept = 2025, linetype = "dashed", color = "darkgreen") +
  theme_bw() +
  labs(color = "Data source type", x = "Year", y = "Indicator") +
  theme(legend.position = "bottom")
#plotdir <- "/Users/lalkema/Dropbox/Apps/Overleaf/workflow_countdown/fig"
# here::here("../bayestransition_output/summary_global_results")
#ggsave(file.path(plotdir, paste0("countries.png")), width = 12, height = 16)

datplot %>%
  filter( indic == "ideliv") %>%
  group_by(name_country) %>%
  summarize(tmax = max(year)) %>%
  pull(tmax) %>% mean()


