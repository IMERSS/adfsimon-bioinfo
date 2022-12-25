library(sf)
library(leaflet)
library(dplyr)

source("scripts/utils.R")

#Layer 1: island coastline
islcoast <- mx_read("spatial_data/vectors/Shp_files/Island")

#Layer 2: CAD_MCW
CAD_MCW <- mx_read("spatial_data/vectors/Shp_files/MXCK_CAD_clipped_MXCW")

#Layer 3: Protected Areas (clipped to MCW)
PA_MCW <- mx_read("spatial_data/vectors/Shp_files/IT_protected_areas_clipped")

#Layer 4: Logging roads and trails
Lroads <- mx_read("spatial_data/vectors/Shp_files/Logging_roads_trails")

Trails <- Lroads %>%
  filter(Lroads$Type == "trail")
nrow(Trails)

Roads <- Lroads %>%
  filter(Lroads$Type == "road")
nrow(Roads)

landUseMap <- leaflet() %>%
    addTiles(options = providerTileOptions(opacity = 0.5)) %>%
    addPolygons(data = islcoast, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
    addPolygons(data = CAD_MCW, color = "black", weight = 2, fillOpacity = 0) %>%
    addPolylines(data = Trails, color = "green", weight = 4) %>%
    addPolylines(data = Roads, color = "orange", weight = 4) %>%
    fitBounds(-123.564, 48.802, -123.516, 48.855) %>%
    addLegend(position="topright", labels = c("Trails", "Logging Roads"), colors = c("green", "orange"))

#Note that this statement is only effective in standalone R
print(landUseMap)

