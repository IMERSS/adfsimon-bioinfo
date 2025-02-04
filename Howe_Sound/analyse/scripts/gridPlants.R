# Baseline analysis of Átl’ka7tsem vascular plant records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(sf)
library(stringr)
library(tidyr)
library(jsonlite)

# Source dependencies

source("utils.R")
source("geomUtils.R")

# Analysis of historical collection activities

summary <- read.csv("../tabular_data/vascular_plant_summary_resynthesized_2024-11-14.csv")

plants <- read.csv("../tabular_data/Howe_Sound_vascular_plant_records_consolidated_2024-11-14.csv")

WGS84 <- st_crs("WGS84")

# Drop rows with NA lat/long and convert to sf

plants_sf <- st_as_sf(plants %>% dplyr::filter(!is.na(decimalLatitude)), coords=c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect plants with 1km2 grid

gridcell <- 1000

howegrid <- make_grid_frame(plants_sf, gridcell)
cat("Constructed grid with ", howegrid$longcount * howegrid$latcount, " cells")

write_grid_frame(howegrid, "../outputs/gridframe.json")

plants.grid <- assign_cell_id(plants_sf, howegrid)

# Convert to CSV

plants.grid.csv <- dplyr::select(as.data.frame(plants.grid), -geometry)

write.csv(plants.grid.csv, "../outputs/gridded_plants_2025.csv", row.names = FALSE, na = "")
