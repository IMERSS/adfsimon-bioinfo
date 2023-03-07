# Howe Sound native vascular plant x protected areas analysis

# Load libraries

library(dplyr)
library(here)
# library(gapminder)
library(ggplot2)
library(ggthemes)
library(sf)
library(tidyr)

# Source dependencies

source("scripts/utils.R")

# Analysis of native plant diversity as represented in Howe Sound protected areas

plants <- read.csv("tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")

# Limit analysis to native vascular plants

native.plants <- plants %>% filter(establishmentMeans == 'native')

native.plant.diversity <- length(unique(native.plants$scientificName))

# Remove vascular plants lacking coordinates

native.plants$yearRecorded <- as.numeric(substr(native.plants$eventDate, 1, 4))

native.plants <- native.plants %>% drop_na(yearRecorded)

# Load protected areas map

protected.areas <- st_read("spatial_data/vectors/Protected_Areas")

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

# Assign richness values to grid

protected.areas$protected.area.richness <- protected.area.matrix$richness[match(unlist(protected.areas$protectedArea), protected.area.matrix$protectedArea)]

protected.areas$protected.area.type.richness <- type.matrix$richness[match(unlist(protected.areas$protectedAreaType), type.matrix$protectedAreaType)]

