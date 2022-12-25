##Historic fires ggplot
library(sf)
library(leaflet)

source("scripts/utils.R")

#Layer 1: island coastline
islcoast <- mx_read("spatial_data/vectors/Shp_files/Island")

#Layer 2: MCW
MCW <- mx_read("spatial_data/vectors/Shp_files/MCW")

#Layer #3: historic fires
h_fires <- mx_read("spatial_data/vectors/Shp_files/Historic_fires")

##Mapping based on fire year: 
#changing FIRE_YEAR numeric attribute to character:
h_fires$FIRE_YEAR <- as.character(h_fires$FIRE_YEAR)

sf::st_coordinates(st_centroid(h_fires$geometry))

##Leaflet map: 

FirehistMap <- leaflet() %>%
  addTiles(options = providerTileOptions(opacity = 0.5)) %>%
  addPolygons(data = islcoast, color = "black", weight = 1.5, fillOpacity = 0, fillColor = NA) %>%
  addPolygons(data = h_fires, color = "#050505", weight = 2, fillOpacity = 0.7, fillColor = "red") %>%
  addPolygons(data = MCW, color = "blue", weight = 2, fillOpacity = 0) %>%
  addLabelOnlyMarkers(data = h_fires, lng = -123.5670, lat = 48.84805, label = "1934",
        labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, className = "mxcw-mapLabel mxcw-fireLabel")) %>%
  addLabelOnlyMarkers(data = h_fires, lng = -123.5519, lat = 48.85045, label = "1935",
        labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, className = "mxcw-mapLabel mxcw-fireLabel")) %>%
  addLabelOnlyMarkers(data = h_fires, lng = -123.5230, lat = 48.82397 , label = "1935",
        labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, className = "mxcw-mapLabel mxcw-fireLabel")) %>%
  addLabelOnlyMarkers(data = h_fires, lng = -123.5207, lat = 48.81398, label = "1942",
        labelOptions = labelOptions(noHide = TRUE, direction = 'middle', textOnly = TRUE, className = "mxcw-mapLabel mxcw-fireLabel")) %>%
  fitBounds(-123.564, 48.802, -123.516, 48.855) %>%
  addLegend(position = "topright", labels = "Fire Years", colors = "red")

print(FirehistMap)
