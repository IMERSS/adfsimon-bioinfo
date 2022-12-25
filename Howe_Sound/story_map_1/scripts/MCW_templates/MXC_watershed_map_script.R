library(sf)
library(leaflet)
library(raster)

source("scripts/utils.R")

#Layer 1: island coast
islcoast <- mx_read("spatial_data/vectors/Shp_files/Island")

#Layer 2: MCW
MCW <- mx_read("spatial_data/vectors/Shp_files/MCW")

#Layer 3: water bodies 
waterbodies <- mx_read("spatial_data/vectors/Shp_files/RAR_water_bodies")

#Layer 4: watercourses
watercourses <- mx_read("spatial_data/vectors/Shp_files/RAR_water_courses")

#Layer 5: watersheds
watersheds <- mx_read("spatial_data/vectors/Shp_files/Watershed_CRD")

#Layer 6: bare earth raster 
hillshade <- raster("spatial_data/rasters/BareEarth_hillshade_resampled_5m.tif")

#Render leaflet map

#Create palette
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(hillshade),
                    na.color = "transparent")

WatershedMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addRasterImage(hillshade, colors = pal, opacity = 0.8) %>%
  addPolygons(data = islcoast, color = "black", weight = 1.5, fillOpacity = 0) %>%
  addPolygons(data = MCW, color = "black", weight = 1.5, fillColor = NA) %>%
  addPolylines(data = watersheds, color = "#0000EE", weight = 2) %>%
  addPolylines(data = watercourses, weight = 1.5, fillOpacity = 0.8, fillColor = "royalblue") %>%
  addPolygons(data = waterbodies, weight = 1.5, fillOpacity = 0.8, fillColor = "dodgerblue")
 
print(WatershedMap)