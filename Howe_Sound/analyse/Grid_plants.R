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

# Drop rows with NA lat/long

plants <- plants %>% drop_na(decimalLatitude)

# Intersect plants with 1km2 grid

# First load 1km2 grid

grid <- st_read("spatial_data/vectors/1km2_grid_x_vascular_plants_2022-12-24")

# Create CRS object

WGS84 <- st_crs("WGS84")

# New records

# Convert plant records to sf points

plants.points <- st_as_sf(plants, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

plants.grid <- st_intersection(plants.points, grid)

# Convert to CSV

plants.grid.csv <- dplyr::select(as.data.frame(plants.grid), -geometry)

write.csv(plants.grid.csv, "outputs/gridded_plants_WGS84.csv")

# Export grid

st_write(plants.grid, "outputs/gridded_plants.shp")
