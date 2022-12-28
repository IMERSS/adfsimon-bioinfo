library(sf)
library(leaflet)
library(dplyr)

source("scripts/utils.R")

#Layer 1: hillshade raster
hillshade <- raster("spatial_data/rasters/Hillshade_20m_clip_study_area_zoomed_out.tif")

#Layer 2: coastline
coastline <- mx_read("spatial_data/vectors/Salish_Sea_Bioregion_Islands_and_Mainland_clip_study_area_zoomed_out")

#Layer 3: watershed boundary
watershed.boundary <- mx_read("spatial_data/vectors/Howe_Sound_boundary.shp")

#Layer 4: BEC Zones
BEC.zones <- mx_read("spatial_data/vectors/BEC_clipped_study_area_zoomed_out_difference_Strait_of_Georgia")

speciesMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addRasterImage(hillshade, colors = pal, opacity = 0.8) %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addPolygons(data = coastline, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = watershed.boundary, color = "black", weight = 2, fillOpacity = 0) %>%

#Note that this statement is only effective in standalone R
print(speciesMap)
