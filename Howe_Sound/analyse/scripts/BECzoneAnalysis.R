# Map vascular plant diversity in Átl’ka7tsem by BEC unit

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(htmlwidgets)
library(jsonlite)
library(leaflet)
library(raster)
library(reshape2)
library(scales)
library(sf)
library(tidyr)
library(viridis)

# Source dependencies

source("utils.R")

# Intersect vascular plant data and BEC zones for viz

# First read vascular plant data

plants <- read.csv("../tabular_data/Howe_Sound_vascular_plant_records_consolidated_2024-11-14.csv")

# Remove vascular plants lacking coordinates

plants <- plants %>% drop_na(eventDate)

# Create CRS object

WGS84 <- st_crs("WGS84")

# Convert plant records to sf points

plants <- plants %>% drop_na(decimalLatitude)

plants <- st_as_sf(plants, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Load BEC Zones and assign CRS

BEC <- mx_read("../spatial_data/vectors/BEC")

BEC <- st_set_crs(BEC, WGS84)

# Create comprehensive description field

BEC$DESC <- paste(BEC$VRNTNM, BEC$SBZNNM, BEC$ZONE_NAME, "Zone", sep = " ")

# Remove any instance of 'NA ' from character strings:

BEC$DESC <- gsub("NA ", "", BEC$DESC)

# Remove unnecessary variables

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

# Generalize to multipart polygons
BEC <- BEC %>%
  group_by(MAP_LABEL, DESC) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Intersect plant occurrences x BEC Zones
# Note: intersecting BEC occurrences x BEC zones using the reverse order to retain
# polygon geometries is extremely slow. We shall bypass the need to extract these geometries
# at this stage and assign taxa to BEC Zones later on for visualization

plants.x.BEC <- st_intersection(plants, BEC)

# Export catalog of records with BEC Zone attribute (discarding meaningless point geometries)

plants.x.BEC <- st_as_sf(plants.x.BEC) %>% st_drop_geometry()

write.csv(plants.x.BEC, "../outputs/AHSBR_vascular_plant_records_x_BEC_Zones_2024.csv", row.names = FALSE)

# Export CSVs: vascular plant records by BEC unit

CMAunp.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CMAunp')

CWHdm.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHdm')

CWHds1.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHds1')

CWHms1.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHms1')

CWHvm1.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHvm1')

CWHvm2.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHvm2')

CWHxm1.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'CWHxm1')

MHmm1.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'MHmm1')

MHmm2.plants <- plants.x.BEC %>% filter(MAP_LABEL == 'MHmm2')

write.csv(CMAunp.plants, "../outputs/AHSBR_CMAunp_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHdm.plants, "../outputs/AHSBR_CWHdm_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHds1.plants, "../outputs/AHSBR_CWHds1_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHms1.plants, "../outputs/AHSBR_CWHms1_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHvm1.plants, "../outputs/AHSBR_CWHvm1_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHvm2.plants, "../outputs/AHSBR_CWHvm2_vascular_plants_2024.csv", row.names = FALSE)
write.csv(CWHxm1.plants, "../outputs/AHSBR_CWHxm1_vascular_plants_2024.csv", row.names = FALSE)
write.csv(MHmm1.plants, "../outputs/AHSBR_MHmm1_vascular_plants_2024.csv", row.names = FALSE)
write.csv(MHmm2.plants, "../outputs/AHSBR_MHmm2_vascular_plants_2024.csv", row.names = FALSE)

# Summarize by species and BEC Zone
plants.x.BEC.summary <- plants.x.BEC %>%
  distinct(scientificName, MAP_LABEL, .keep_all = TRUE)

# Output plants.x.BEC as a dataframe for visualization

write.csv(plants.x.BEC.summary, "../outputs/BEC_x_vascular_plants_summary_2024.csv", row.names = FALSE)
