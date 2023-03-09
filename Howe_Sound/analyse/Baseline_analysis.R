# Baseline analysis of Átl’ka7tsem vascular plant records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(here)
library(gapminder)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(hrbrthemes)
library(sf)
library(stringr)
library(tidyr)

# Source dependencies

source("scripts/utils.R")

# Analysis of historical collection activities

summary <- read.csv("tabular_data/vascular_plant_summary_resynthesized_2023-03-05.csv")

plants <- read.csv("tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")

# Subset historic, confirmed and new records

new <- summary %>% filter(str_detect(reportingStatus, "new"))
confirmed <- summary %>% filter(reportingStatus == "confirmed")
reported <- summary %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

new.taxa <- unique(new$scientificName)
new.taxa <- new.taxa %>% paste(collapse = "|")

confirmed.taxa <- unique(confirmed$scientificName)
confirmed.taxa <- confirmed.taxa %>% paste(collapse = "|")

reported.taxa <- unique(reported$scientificName)
reported.taxa <- reported.taxa %>% paste(collapse = "|")


new.taxa.records <- plants %>% filter(str_detect(scientificName, new.taxa))

confirmed.taxa.records <- plants %>% filter(str_detect(scientificName,confirmed.taxa))

reported.taxa.records <- plants %>% filter(str_detect(scientificName, reported.taxa))
                                            
new.taxa.records <- new.taxa.records %>% drop_na(decimalLatitude)
confirmed.taxa.records <- confirmed.taxa.records %>% drop_na(decimalLatitude)
reported.taxa.records <- reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# First load 1km2 grid

grid <- st_read("spatial_data/vectors/1km2_grid_x_vascular_plants_2022-12-24")

# Create CRS object

WGS84 <- st_crs("WGS84")

# New taxa records

# Convert plant records to sf points

new.taxa.points <- st_as_sf(new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

new.plants.grid <- st_intersection(new.taxa.points, grid)

