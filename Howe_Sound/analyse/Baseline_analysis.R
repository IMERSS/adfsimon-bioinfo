# Baseline analysis of Átl’ka7tsem vascular plant records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(ecodist)
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

# New records

# Convert plant records to sf points

new.taxa.points <- st_as_sf(new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

new.plants.grid <- st_intersection(new.taxa.points, grid)

new.plants.grid$status <- 'new'

# Historic records

# Convert plant records to sf points

reported.taxa.points <- st_as_sf(reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

reported.plants.grid <- st_intersection(reported.taxa.points, grid)

reported.plants.grid$status <- 'historic'

# Confirmed records

# Convert plant records to sf points

confirmed.taxa.points <- st_as_sf(confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

confirmed.plants.grid <- st_intersection(confirmed.taxa.points, grid)

confirmed.plants.grid$status <- 'confirmed'

# Add richness values to gridded data to render as choropleth

# Historic records

reported.matrix <- reported.plants.grid

reported.matrix$n <- 1

reported.matrix <- ecodist::crosstab(reported.matrix$id, reported.matrix$scientificName, reported.matrix$n)

reported.matrix <- cbind(reported.matrix, richness = rowSums(reported.matrix))

reported.matrix$id <- row.names(reported.matrix)

reported.plants.grid$richness <- reported.matrix$richness[match(unlist(reported.plants.grid$id), reported.matrix$id)]

# New records

new.matrix <- new.plants.grid

new.matrix$n <- 1

new.matrix <- ecodist::crosstab(new.matrix$id, new.matrix$scientificName, new.matrix$n)

new.matrix <- cbind(new.matrix, richness = rowSums(new.matrix))

new.matrix$id <- row.names(new.matrix)

new.plants.grid$richness <- new.matrix$richness[match(unlist(new.plants.grid$id), new.matrix$id)]

# Confirmed records

confirmed.matrix <- confirmed.plants.grid

confirmed.matrix$n <- 1

confirmed.matrix <- ecodist::crosstab(confirmed.matrix$id, confirmed.matrix$scientificName, confirmed.matrix$n)

confirmed.matrix <- cbind(confirmed.matrix, richness = rowSums(confirmed.matrix))

confirmed.matrix$id <- row.names(confirmed.matrix)

confirmed.plants.grid$richness <- confirmed.matrix$richness[match(unlist(confirmed.plants.grid$id), confirmed.matrix$id)]

