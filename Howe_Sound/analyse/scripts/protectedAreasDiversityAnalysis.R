# Howe Sound native vascular plant x protected areas analysis

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(here)
# library(gapminder)
library(ggplot2)
library(ggthemes)
library(sf)
library(tidyr)

# Source dependencies

source("utils.R")

# Analysis of native plant diversity as represented in Howe Sound protected areas

plants <- read.csv("../tabular_data/Howe_Sound_vascular_plant_records_consolidated_2024-11-14.csv")

# Add temporary unique identifier to reassign coordinates later

plants$temp_id <- 1:nrow(plants)

# Limit analysis to native vascular plants

native.plants <- plants %>% filter(establishmentMeans == 'native')

native.plant.diversity <- length(unique(native.plants$scientificName))

# Remove vascular plants lacking coordinates

native.plants$yearRecorded <- as.numeric(substr(native.plants$eventDate, 1, 4))

native.plants <- native.plants %>% drop_na(yearRecorded)

# Load protected areas map

protected.areas <- st_read("../spatial_data/vectors/Protected_Areas")

protected.areas$native.plant.diversity <- native.plant.diversity

names(protected.areas)[names(protected.areas) == 'NAME_E'] <- 'protectedArea'
names(protected.areas)[names(protected.areas) == 'TYPE_E'] <- 'protectedAreaType'

# Create CRS object

WGS84 <- st_crs("WGS84")

# Convert plant records to sf points

native.plants <- native.plants %>% drop_na(decimalLatitude)

native.plants <- st_as_sf(native.plants, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect plant occurrences x protected areas

plants.x.protected.areas <- st_intersection(native.plants, protected.areas)

# Summarize native plant diversity in protected areas

protected.areas$protected.native.plant.diversity <- length(unique(plants.x.protected.areas$scientificName))

# Sum species richness by protected area

plants.x.protected.areas$count <- 1

protected.area.matrix <- ecodist::crosstab(plants.x.protected.areas$protectedArea, plants.x.protected.areas$scientificName, plants.x.protected.areas$count)

protected.area.matrix[protected.area.matrix > 1] <- 1

protected.area.matrix$richness <- rowSums(protected.area.matrix)

protected.area.matrix$protectedArea <- row.names(protected.area.matrix)

# Sum species richness by protected area type

type.matrix <- ecodist::crosstab(plants.x.protected.areas$protectedAreaType, plants.x.protected.areas$scientificName, plants.x.protected.areas$count)

type.matrix[type.matrix > 1] <- 1

type.matrix$richness <- rowSums(type.matrix)

type.matrix$protectedAreaType <- row.names(type.matrix)

# Assign richness values to polgyons

protected.areas$protected.area.richness <- protected.area.matrix$richness[match(unlist(protected.areas$protectedArea), protected.area.matrix$protectedArea)]

protected.areas$protected.area.type.richness <- type.matrix$richness[match(unlist(protected.areas$protectedAreaType), type.matrix$protectedAreaType)]

# Write protected area analysis geospatial dataset

st_write(protected.areas, "../outputs/AHSBR_vascular_plant_diversity_x_protected_areas_2024.shp")

# Write catalogs of vascular plant diversity by protected area

plants.x.protected.areas$geometry <- NULL
plants.x.protected.areas$count <- NULL
plants.x.protected.areas$native.plant.diversity <- NULL

plants.x.protected.areas$decimalLatitude <- plants$decimalLatitude[match(unlist(plants.x.protected.areas$temp_id), plants$temp_id)]
plants.x.protected.areas$decimalLongitude <- plants$decimalLongitude[match(unlist(plants.x.protected.areas$temp_id), plants$temp_id)]

write.csv(plants.x.protected.areas, "../outputs/plants_x_protected_areas_2024.csv", row.names = FALSE)

# Create dataframes of species recorded for each protected area type

OECM <- plants.x.protected.areas %>% filter(protectedAreaType == 'Other Effective Area-Based Conservation Measure')
A_Park <- plants.x.protected.areas %>% filter(protectedAreaType == 'A - Park')
POCA <- plants.x.protected.areas %>% filter(protectedAreaType == 'Privately Owned Conservation Area')
WHA <- plants.x.protected.areas %>% filter(protectedAreaType == 'Wildlife Habitat Areas')
PA <- plants.x.protected.areas %>% filter(protectedAreaType == 'Protected Area')
S2SWZ <- plants.x.protected.areas %>% filter(protectedAreaType == 'Sea to Sky Wildland Zones')
MBS <- plants.x.protected.areas %>% filter(protectedAreaType == 'Migratory Bird Sanctuary')
Conservancy <- plants.x.protected.areas %>% filter(protectedAreaType == 'Conservancy')
ER <- plants.x.protected.areas %>% filter(protectedAreaType == 'Ecological Reserve')
OGMA <- plants.x.protected.areas %>% filter(protectedAreaType == 'Old Growth Management Area (Mapped Legal)')
WMA <- plants.x.protected.areas %>% filter(protectedAreaType == 'Wildlife Management Area')

# Create summaries of native species recorded per protected area

plants.x.protected.area.summary <- plants.x.protected.areas %>%
  group_by(protectedAreaType) %>%
  summarize(record_count = n(), .groups = "drop")

write.csv(plants.x.protected.area.summary, "../outputs/plants_x_protected_areas_summary.csv")
