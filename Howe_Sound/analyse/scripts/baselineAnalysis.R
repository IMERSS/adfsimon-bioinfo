# Baseline analysis of Átl’ka7tsem vascular plant records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# TO DO: Do we need a choropleth for the total set of records? This revised script only exports
# choropleths for historical, confirmed, and new records...

# Load libraries

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Source dependencies

source("utils.R")

# Analysis of historical collection activities

summary <- read.csv("../tabular_data/vascular_plant_summary_resynthesized_2024-11-14.csv")

plants <- read.csv("../outputs/gridded_plants_2025.csv")

# Summarize taxa by reporting status

new <- summary %>% filter(str_detect(reportingStatus, "new"))
confirmed <- summary %>% filter(reportingStatus == "confirmed")
reported <- summary %>% filter(reportingStatus == "reported")

new.taxa <- unique(new$scientificName)
confirmed.taxa <- unique(confirmed$scientificName)
historical.taxa <- unique(reported$scientificName)

# Filter gridded catalog based on reporting status of species

new_records <- plants %>% filter(scientificName %in% new.taxa)
new_records$status <- 'new'

confirmed_records <- plants %>% filter(scientificName %in% confirmed.taxa)
confirmed_records$status <- 'confirmed'

historical_records <- plants %>% filter(scientificName %in% historical.taxa)
historical_records$status <- 'historical'

# Summarize records to form gridded basis for choropleths

# Summarize the number of records by cell for each catalog
new_gridded_summary <- new_records %>%
  group_by(status, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

confirmed_gridded_summary <- confirmed_records %>%
  group_by(status, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

reported_gridded_summary <- historical_records %>%
  group_by(status, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# Consolidate gridded plant data # What did we do with this before?

reporting.status.grid <- rbind(new_gridded_summary, confirmed_gridded_summary, reported_gridded_summary)

# Output gridded summary data

write.csv(reporting.status.grid, "../outputs/gridded_reporting_status.csv", row.names = FALSE)
