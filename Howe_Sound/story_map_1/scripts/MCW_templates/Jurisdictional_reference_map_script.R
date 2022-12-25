library(sf)
library(leaflet)
library(dplyr)

source("scripts/utils.R")

#Layer 1: island coastline
islcoast <- mx_read("spatial_data/vectors/Shp_files/Island")

#Layer 2: watersheds
watersheds <- mx_read("spatial_data/vectors/Shp_files/Watershed_CRD")

#Layer 3: CAD
CAD <- mx_read("spatial_data/vectors/Shp_files/MXCK_CAD")

#Layer 4: MCW
MCW <- mx_read("spatial_data/vectors/Shp_files/MCW")

#Layer 5: Protected areas clipped to MCW
PA_MCW <- mx_read("spatial_data/vectors/Shp_files/IT_protected_areas_clipped")

#Layer 6: Protected Areas (SSI)
PA <- mx_read("spatial_data/vectors/Shp_files/IT_protected_areas_2022-05")

#Layer 7: Roads
roads <- mx_read("spatial_data/vectors/Shp_files/DataBC_roads")

#Layer 8: waterbodies 
waterbodies <- mx_read("spatial_data/vectors/Shp_files/RAR_water_bodies")

#Layer 9: watercourses
watercourses <- mx_read("spatial_data/vectors/Shp_files/RAR_water_courses")
#CAD values = xmin: 458858.3 ymin: 5406257 xmax: 463151.3 ymax: 5411194

#Layer 10: CAD_MCW
CAD_MCW <- mx_read("spatial_data/vectors/Shp_files/MXCK_CAD_clipped_MXCW")
CAD_MCW$LABEL[CAD_MCW$LABEL == "NSSWD"] <- "North Salt Spring Waterworks District"

#Layer 11: Protected areas (PA) clipped to MCW area
PA_MCW <- mx_read("spatial_data/vectors/Shp_files/IT_protected_areas_clipped")

juris <- c("Islands Trust Conservancy", "North Salt Spring Waterworks District", "Participating neighbours", "Private", "Salt Spring Island Local Trust Committee")
jurisColors = c("coral", "mediumpurple", "palegreen", "paleturquoise", "orange")

jurisPalette <- colorFactor(jurisColors, juris)

jurisMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addPolygons(data = islcoast, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = CAD_MCW, color = "black", weight = 1.5, fillOpacity = 0.8, fillColor = ~jurisPalette(LABEL)) %>%
  addPolygons(data = PA_MCW, color = "black", weight = 1.5, fillOpacity = 0.8, fillColor = ~jurisPalette(COV_HLD1)) %>%
  addPolylines(data = roads, color = "grey", weight = 4) %>%
#  addPolygons(data = lat_lon(MCW), color = "black", weight = 1.5, fillColor = NA) %>%
#  addPolylines(data = lat_lon(watersheds), color = "#0000EE", weight = 2) %>%
  addPolygons(data = waterbodies, weight = 1.5, fillOpacity = 0.8, fillColor = "dodgerblue") %>%
  addPolygons(data = watercourses, weight = 1.5, fillOpacity = 0.8, fillColor = "royalblue") %>%
  fitBounds(-123.564, 48.802, -123.516, 48.855) %>%
  addLegend(position="topright", labels = juris, colors = jurisColors)

print(jurisMap)
