# update_regions.R

# Update regions.csv to:
# (1) Relabel cluster and subcluster to add "_bearak" suffix
# (2) Add new cluster and subcluster columns with modifications:
#     - Vietnam -> cluster "South Asia, Southeast Asia, and Oceania", subcluster "Southeast Asia"
#     - Mongolia -> subcluster "Central Asia"

library(here)
library(tidyverse)

# Load regions data
data_folder <- "data_raw"
regions_dat <- read_csv(here::here(data_folder, "regions.csv"),
                        show_col_types = FALSE)

# Check current values for Vietnam and Mongolia
cat("Current values for Vietnam:\n")
regions_dat %>%
  filter(iso == "VNM") %>%
  select(iso, name_country, cluster, subcluster) %>%
  print()

cat("\nCurrent values for Mongolia:\n")
regions_dat %>%
  filter(iso == "MNG") %>%
  select(iso, name_country, cluster, subcluster) %>%
  print()

# Update regions data
regions_updated <- regions_dat %>%
  # (1) Rename existing cluster and subcluster columns to add "_bearak" suffix
  rename(
    cluster_bearak = cluster,
    subcluster_bearak = subcluster
  ) %>%
  # (2) Create new cluster and subcluster columns with modifications

  mutate(
    cluster = case_when(
      iso == "VNM" ~ "South Asia, Southeast Asia, and Oceania",
      TRUE ~ cluster_bearak
    ),
    subcluster = case_when(
      iso == "VNM" ~ "Southeast Asia",
      iso == "MNG" ~ "Central Asia",
      # add countries in South America in that subccluster
      name_sub_region %in% c("South America") ~ "South America",
      TRUE ~ subcluster_bearak
    )
  )

# Verify changes
cat("\nUpdated values for Vietnam:\n")
regions_updated %>%
  filter(iso == "VNM") %>%
  select(iso, name_country, cluster_bearak, cluster, subcluster_bearak, subcluster) %>%
  print()

cat("\nUpdated values for Mongolia:\n")
regions_updated %>%
  filter(iso == "MNG") %>%
  select(iso, name_country, cluster_bearak, cluster, subcluster_bearak, subcluster) %>%
  print()

# subclusters for countries in LAC:

cat("\nSubclusters for countries in LAC:\n")
regions_updated %>%
  filter(cluster %in% "LAC") %>%
  group_by(subcluster) %>%
  summarize(countries = paste(name_country, collapse = ", "))

# Save updated regions data
write_csv(regions_updated, here::here(data_folder, "regions_updated.csv"))
cat("\nUpdated regions.csv saved to", here::here(data_folder, "regions_updated.csv"), "\n")
