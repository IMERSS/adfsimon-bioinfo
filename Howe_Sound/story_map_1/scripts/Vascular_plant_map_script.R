# Map vascular plant diversity in Átl’ka7tsem by BEC unit

# Load libraries

library(here)
library(sf)
library(leaflet)
library(dplyr)
library(raster)
library(reshape2)
library(viridis)

# Set paths

library(here)
root <- "Howe_Sound/story_map_1/"

setwd(here(root))

# Source dependencies

source("scripts/utils.R")

# Intersect vascular plant data and BEC zones for viz

# Note: this script calls 'plants.gridded.csv' — 
# bypassing a direct intersection of plant data and BEC units for now! (this may not be desirable)

# First read vascular plant data

plants.gridded <- read.csv("tabular_data/1km_gridded_vascular_plant_records_2022-12-24_WGS84.csv")
metadata <- read.csv("tabular_data/1km_grid_metadata.csv")

# Assign BEC map labels to gridded plant data

plants.gridded$MAP_LABEL <- metadata$MAP_LABEL[match(unlist(plants.gridded$id), metadata$id)]
  
# Summarize plant species by BEC unit

bec.plants <- plants.gridded %>% group_by(MAP_LABEL) %>% 
                    summarize(taxa = paste(sort(unique(scientific)),collapse=", "))

# Load BEC Zones shape

BEC <- mx_read("spatial_data/vectors/BEC")

BEC$TAXA <- bec.plants$taxa[match(unlist(BEC$MAP_LABEL), bec.plants$MAP_LABEL)]

# Simplify BEC shape

# First create comprehensive description field

BEC$DESC <- paste(BEC$VRNTNM, BEC$SBZNNM, BEC$ZONE_NAME, "Zone", sep = " ")

# Remove unecessary variables

BEC$ZONE <- NULL
BEC$VRNTNM <- NULL
BEC$SBZNNM <-  NULL
BEC$PHASE <-  NULL
BEC$OBJECTID <-  NULL
BEC$NTRLDSTRB1 <-  NULL
BEC$NTRLDSTRBN <-  NULL
BEC$FTRLNGTH <-  NULL
BEC$FTRR <-  NULL
BEC$BGC_LABEL <- NULL
BEC$FTRCLSSSK <- NULL
BEC$PHASE_NAME <- NULL
BEC$VARIANT <- NULL
BEC$ZONE_NAME <- NULL
BEC$SUBZONE <- NULL

# Create color palette for BEC Zones

# Following rough elevational gradient:  
# CDFmm, CWHxm1, CWHdm, CWHvm1, CWHvm2, CWHds1, CWHms1, MHmm1, MHmm2, ESSFmw2, CMAunp

# Note: I do not think that the palette is mapping with the MAP_LABEL feature as intended!

BEC.zones <- BEC$MAP_LABEL
types <- BEC.zones %>% unique
index <- c(9,1,6,5,7,2,8,4,3,11,10)
# index <- c(3,11,6,7,5,10,4,8,9,1,2) # inverse palette
types <- types[order(index)]
t <- length(types)
pal <- leaflet::colorFactor(viridis_pal(option = "D")(t), domain = types)

# Load additional map layers

# Layer 1: hillshade raster
hillshade <- raster("spatial_data/rasters/Hillshade_80m.tif")

# Layer 2: coastline
coastline <- mx_read("spatial_data/vectors/Islands_and_Mainland")

# Layer 3: watershed boundary
watershed.boundary <- mx_read("spatial_data/vectors/Howe_Sound")


# Plot map

speciesMap <- leaflet() %>%
  setView(-123.2194, 49.66076, zoom = 8.5) %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addRasterImage(hillshade, opacity = 0.8) %>%
  addPolygons(data = coastline, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = BEC, fillColor = ~pal(MAP_LABEL), fillOpacity = 0.6, weight = 0) %>% 
  addLegend(position = 'topright',
            colors = viridis_pal(option = "D")(t),
            labels = types) %>%
  addPolygons(data = watershed.boundary, color = "black", weight = 4, fillOpacity = 0)

#Note that this statement is only effective in standalone R
print(speciesMap)
