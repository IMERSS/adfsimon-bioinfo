# Map vascular plant diversity in Átl’ka7tsem by BEC unit

# Load libraries

library(here)
library(sf)
library(leaflet)
library(dplyr)
library(raster)
library(viridis)

# Source dependencies

source("scripts/utils.R")

# Intersect vascular plant data and BEC zones for viz

plants <- read.csv("tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")
plants.gridded <- read.csv("tabular_data/1km_gridded_vascular_plant_records_2022-12-24_WGS84.csv")

no.observations <- nrow(plants)
no.species <- length(unique(plants$scientificName))
no.species.rounded <- round_any(length(unique(plants$scientificName)),1000)

# Load additional map layers

# Layer 1: hillshade raster
hillshade <- raster("spatial_data/rasters/Hillshade_80m.tif")

# Layer 2: coastline
coastline <- mx_read("spatial_data/vectors/Islands_and_Mainland")

# Layer 3: watershed boundary
watershed.boundary <- mx_read("spatial_data/vectors/Howe_Sound")

# Layer 4: BEC Zones
BEC <- mx_read("spatial_data/vectors/BEC")

# Create color palette for BEC Zones

# Elevational gradient:  
# CDFmm, CWHxm1, CWHdm, CWHvm1, CWHvm2, CWHds1, CWHms1, MHmm1, MHmm2, ESSFmw2, CMAunp

# Note: I do not think that the palette is mapping the MAP_LABEL feature correctly!

BEC.zones <- BEC$MAP_LABEL
types <- BEC.zones %>% unique
index <- c(9,1,6,5,7,2,8,4,3,11,10)
# index <- c(3,11,6,7,5,10,4,8,9,1,2) # inverse palette
types <- types[order(index)]
t <- length(types)
pal <- leaflet::colorFactor(viridis_pal(option = "D")(t), domain = types)

# Plot map

speciesMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addRasterImage(hillshade, opacity = 0.8) %>%
  addPolygons(data = coastline, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = BEC, fillColor = ~pal(MAP_LABEL), fillOpacity = 0.6, weight = 0) %>% 
  addLegend(position = 'topright',
            colors = viridis_pal(option = "D")(t),
            labels = types) %>%
  addPolygons(data = watershed.boundary, color = "black", weight = 3, fillOpacity = 0)

#Note that this statement is only effective in standalone R
print(speciesMap)
