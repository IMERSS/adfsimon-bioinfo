##Hydrology and geology map:

library(sf)
#Layer 1: island coastline
islcoast <- st_read("spatial_data/vectors/Shp_files/Island")

#Layer 1: CAD_MCW
CAD_MCW <- st_read("spatial_data/vectors/Shp_files/MXCK_CAD_clipped_MXCW")

#Layer 2: watercourses
watercourses <- st_read("spatial_data/vectors/Shp_files/RAR_water_courses")

#Layer 3: watersheds
watersheds <- st_read("spatial_data/vectors/Shp_files/Watershed_CRD")

#layer 4: geology
geology <- st_read("spatial_data/vectors/Shp_files/Geology")

#Layer 5: wetlands
wetlands <- st_read("spatial_data/vectors/Shp_files/Wetlands")

#layer 6: streams
streams <- st_read("spatial_data/vectors/Shp_files/Streams")

#Layer 7: waterbodies
waterbodies <- st_read("spatial_data/vectors/Shp_files/RAR_water_bodies")

#Layer 8: MCW
MCW <- st_read("spatial_data/vectors/Shp_files/MCW")

#Making ggplot:

p <- ggplot() + 
  geom_sf(data = islcoast, color = "black") +
  geom_sf(data = MCW, color = "lightblue", fill = NA) +
  geom_sf(data = watersheds, color = "royalblue3", fill = NA) +
  geom_sf(data = geology, color ="darkorange") +
  geom_sf(data = streams, color = "aquamarine3") +
  geom_sf(data = waterbodies, fill = "cyan") +
  geom_sf(data = watercourses, fill = "dodgerblue4") +
  coord_sf(xlim = c(458938.9, 462136.9), ylim = c(5406261, 5409918)) +
  theme(legend.title = element_text("geology$geometry")) +
  scale_color_manual(values = c("lightblue","royalblue3", "darkorange", "aquamarine3", "cyan", "dodgerblue4"), labels = c("Maxwell Creek Watershed", "watersheds", "geology", "steams", "waterbodies", "watercourses")) +
  ggtitle("Maxwell Creek Wateshed Geology and Hydrology Map")
 
print(p)
