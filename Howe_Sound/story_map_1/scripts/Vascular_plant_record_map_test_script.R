# This is a test script to see if I can generate a basic Leaflet map 
# outside of the R Markdown template (copied from Maxwell)
# I have used the set relative paths function for my purposes here;
# The 'R-Markdown-AHSBR_Map_1.RMD' file is coded to run something similar to this 
# script using the R Markdown 'scrolly-telling' framework we've developed, but
# it fails to knit...

# Load libraries

library(here)
library(sf)
library(leaflet)
library(dplyr)
library(raster)

# Set path

root <- "Howe_Sound/story_map_1/"
setwd(here(root))

# Source dependencies

source("scripts/utils.R")

# Load map layers

#Layer 1: hillshade raster
hillshade <- raster("spatial_data/rasters/Hillshade_80m.tif")

#Layer 2: coastline
coastline <- mx_read("spatial_data/vectors/Islands_and_Mainland")

#Layer 3: watershed boundary
watershed.boundary <- mx_read("spatial_data/vectors/Howe_Sound")

#Layer 4: BEC Zones
BEC.zones <- mx_read("spatial_data/vectors/BEC")

speciesMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addRasterImage(hillshade, opacity = 0.8) %>%
  addPolygons(data = coastline, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = watershed.boundary, color = "black", weight = 2, fillOpacity = 0) %>%
  addPolygons(data = BEC.zones, color = "white", weight = 2, fillOpacity = 0)

#Note that this statement is only effective in standalone R
print(speciesMap)
