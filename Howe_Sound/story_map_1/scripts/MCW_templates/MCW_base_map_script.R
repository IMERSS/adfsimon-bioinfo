##Island Base map:
library(sf)
library(leaflet)
library(dplyr)

source("scripts/utils.R")

#Layer 1: island coastline
islcoast <- mx_read("spatial_data/vectors/Shp_files/Island")

#Layer 2: MCW
MCW <- mx_read("spatial_data/vectors/Shp_files/MCW")

baseMap <- leaflet() %>%
  addPolygons(data = islcoast, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addPolygons(data = MCW, color = "blue", weight = 2, fillOpacity = 0) %>%
  fitBounds(-123.564, 48.802, -123.516, 48.855)

print(baseMap)
