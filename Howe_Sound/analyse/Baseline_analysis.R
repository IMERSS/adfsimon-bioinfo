# Baseline analysis of Átl’ka7tsem vascular plant records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# TO DO: Do we need a choropleth for the total set of records? This revised script only exports
# choropleths for historical, confirmed, and new records...

# Load libraries

library(dplyr)
library(ecodist)
library(here)
library(gapminder)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(sf)
library(stringr)
library(tidyr)

# Source dependencies

source("scripts/utils.R")

# Analysis of historical collection activities

summary <- read.csv("tabular_data/vascular_plant_summary_resynthesized_2024-11-14.csv")

plants <- read.csv("tabular_data/Howe_Sound_vascular_plant_records_consolidated_2024-11-14.csv")

# Subset historic, confirmed and new records

new <- summary %>% filter(str_detect(reportingStatus, "new"))
confirmed <- summary %>% filter(reportingStatus == "confirmed")
reported <- summary %>% filter(reportingStatus == "reported")

# Create vectors of historical, confirmed and new taxa to query catalog of occurrence records

new.taxa <- unique(new$scientificName)
confirmed.taxa <- unique(confirmed$scientificName)
reported.taxa <- unique(reported$scientificName)

confirmed.taxa.records <- summary %>% filter(scientificName %in% confirmed.taxa)
# Note that this approach works whether the list is empty or not
confirmed.taxa.records <- confirmed.taxa.records %>% mutate(status = "confirmed")

new.taxa.records <- summary %>% filter(scientificName %in% new.taxa)
new.taxa.records <- new.taxa.records %>% mutate(status = "new")

reported.taxa.records <- summary %>% filter(scientificName %in% reported.taxa)
reported.taxa.records <- reported.taxa.records %>% mutate(status = "historical")

# Prepare gridded choropleths of historic, confirmed, new records

# First load 1km2 grid (already CRS = WGS84)

grid <- st_read("spatial_data/vectors/gridded_plants_2025")

# New records

new.plants.grid <- grid %>% 
  filter(scntfcN %in% new.taxa)

new.plants.grid$status <- 'new'

# Historical records

reported.plants.grid <- grid %>% 
  filter(scntfcN %in% reported.taxa)

reported.plants.grid$status <- 'historical'

# Confirmed records

confirmed.plants.grid <- grid %>% 
  filter(scntfcN %in% confirmed.taxa)

confirmed.plants.grid$status <- 'confirmed'

# Add richness values to gridded data to render as choropleth

# Historic records

reported.matrix <- reported.plants.grid

reported.matrix$n <- 1

reported.matrix <- ecodist::crosstab(reported.matrix$id, reported.matrix$scntfcN, reported.matrix$n)

reported.matrix <- cbind(reported.matrix, richness = rowSums(reported.matrix))

reported.matrix$id <- row.names(reported.matrix)

reported.plants.grid$richness <- reported.matrix$richness[match(unlist(reported.plants.grid$id), reported.matrix$id)]

# New records

new.matrix <- new.plants.grid

new.matrix$n <- 1

new.matrix <- ecodist::crosstab(new.matrix$id, new.matrix$scntfcN, new.matrix$n)

new.matrix <- cbind(new.matrix, richness = rowSums(new.matrix))

new.matrix$id <- row.names(new.matrix)

new.plants.grid$richness <- new.matrix$richness[match(unlist(new.plants.grid$id), new.matrix$id)]

# Confirmed records

confirmed.matrix <- confirmed.plants.grid

confirmed.matrix$n <- 1

confirmed.matrix <- ecodist::crosstab(confirmed.matrix$id, confirmed.matrix$scntfcN, confirmed.matrix$n)

confirmed.matrix <- cbind(confirmed.matrix, richness = rowSums(confirmed.matrix))

confirmed.matrix$id <- row.names(confirmed.matrix)

confirmed.plants.grid$richness <- confirmed.matrix$richness[match(unlist(confirmed.plants.grid$id), confirmed.matrix$id)]

# Consolidate gridded plant data # What did we do with this before?

reporting.status.grid <- rbind(reported.plants.grid, confirmed.plants.grid, new.plants.grid)

# Output gridded data 

st_write(reported.plants.grid, "outputs/gridded_historical_records_2025.shp", layer_options = "ENCODING=UTF-8", delete_dsn = TRUE)

st_write(new.plants.grid, "outputs/gridded_new_records_2025.shp", layer_options = "ENCODING=UTF-8", delete_dsn = TRUE)

st_write(confirmed.plants.grid, "outputs/gridded_confirmed_records_2025.shp", layer_options = "ENCODING=UTF-8", delete_dsn = TRUE)