# Howe Sound native vascular plant x protected areas analysis

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
library(tidyr)

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Analysis of native plant diversity as represented in Howe Sound protected areas

plants <- read.csv("tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")

# Load protected areas map

protected.areas <- st_read("spatial_data/vectors/Protected_Areas")

# Create CRS object

WGS84 <- st_crs("WGS84")

# Convert plant records to sf points

plants <- st_as_sf(plants, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect plant occurrences x protected areas

plants.x.protected.areas <- st_intersection(plants, protected.areas)

# Sum species richness by protected area

plants.x.protected.areas$count <- 1

matrix <- ecodist::crosstab(plants.x.protected.areas$id, plants.x.protected.areas$scientificName, plants.x.protected.areas$count)

matrix$richness <- rowSums(matrix)

matrix$id <- row.names(matrix)

# Assign richness values to grid

protected.areas$richness <- matrix$richness[match(unlist(protected.areas$id), matrix.$id)]



