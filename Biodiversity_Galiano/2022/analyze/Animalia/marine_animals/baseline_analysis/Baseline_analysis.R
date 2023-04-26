# Baseline analysis of Galiano Island marine animals

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# Load libraries

library(dplyr)
library(ecodist)
library(here)
library(gapminder)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(hrbrthemes)
library(sf)
library(stringr)
library(tidyr)


# Source dependencies

source("scripts/utils.R")


# Load summary and catalog data

summary <- read.csv("../../../../review/Animalia/marine_animals/summaries/Marine_animals_review_summary_2023-04-22.csv")

animals <- read.csv("../../../../consolidate_records/Animalia/marine_animals/synthesized/Galiano_marine_animal_records_consolidated_2023-04-22.csv")


# Create grid

# Create CRS object

EPSG.4326 <- st_crs(4326)

# Drop rows lacking coordinates

animals.points <- animals %>% drop_na(decimalLatitude)

# Convert records to sf points

animals.points <- st_as_sf(animals.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)

# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

animals.points <- st_transform(animals.points, crs = st_crs(EPSG.32610))

# Set cell size

cs <- c(1000, 1000)

# Create grid

area.grid = st_make_grid(animals.points, what = "polygons", square = TRUE, cellsize = cs)

sf::st_length(area.grid)
st_crs(area.grid)$proj4string

# Add grid ID

grid = st_sf(area.grid) %>%
  mutate(cell_id = 1:length(lengths(area.grid)))


# Molluscs

molluscs <- summary %>% filter(phylum == 'Mollusca')
molluscs.records <- animals %>% filter(phylum == 'Mollusca')

# Subset historic, confirmed and new records

molluscs.new <- molluscs %>% filter(str_detect(reportingStatus, "new"))
molluscs.confirmed <- molluscs %>% filter(reportingStatus == "confirmed")
molluscs.reported <- molluscs %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

molluscs.new.taxa <- unique(molluscs.new$scientificName)
molluscs.new.taxa <- molluscs.new.taxa %>% paste(collapse = "|")

molluscs.confirmed.taxa <- unique(molluscs.confirmed$scientificName)
molluscs.confirmed.taxa <- molluscs.confirmed.taxa %>% paste(collapse = "|")

molluscs.reported.taxa <- unique(molluscs.reported$scientificName)
molluscs.reported.taxa <- molluscs.reported.taxa %>% paste(collapse = "|")

molluscs.new.taxa.records <- molluscs.records %>% filter(str_detect(scientificName, molluscs.new.taxa))

molluscs.confirmed.taxa.records <- molluscs.records %>% filter(str_detect(scientificName, molluscs.confirmed.taxa))

molluscs.reported.taxa.records <- molluscs.records %>% filter(str_detect(scientificName, molluscs.reported.taxa))
                                            
molluscs.new.taxa.records <- molluscs.new.taxa.records %>% drop_na(decimalLatitude)
molluscs.confirmed.taxa.records <- molluscs.confirmed.taxa.records %>% drop_na(decimalLatitude)
molluscs.reported.taxa.records <- molluscs.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

molluscs.new.taxa.points <- st_as_sf(molluscs.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

molluscs.new.taxa.points <- st_transform(molluscs.new.taxa.points, crs = st_crs(EPSG.32610))

molluscs.new.records.gridded <- molluscs.new.taxa.points %>% 
  st_join(grid)

molluscs.new.records.gridded <- as.data.frame(molluscs.new.records.gridded)

molluscs.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

molluscs.new.taxa.points.sum <- st_transform(molluscs.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

molluscs.new.grid <- grid %>%
  st_join(molluscs.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

molluscs.new.grid = filter(molluscs.new.grid, richness > 0)

molluscs.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

molluscs.reported.taxa.points <- st_as_sf(molluscs.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

molluscs.reported.taxa.points <- st_transform(molluscs.reported.taxa.points, crs = st_crs(EPSG.32610))

molluscs.reported.records.gridded <- molluscs.reported.taxa.points %>% 
  st_join(grid)

molluscs.reported.records.gridded <- as.data.frame(molluscs.reported.records.gridded)

molluscs.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

molluscs.reported.taxa.points.sum <- st_transform(molluscs.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

molluscs.reported.grid <- grid %>%
  st_join(molluscs.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

molluscs.reported.grid = filter(molluscs.reported.grid, richness > 0)

molluscs.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

molluscs.confirmed.taxa.points <- st_as_sf(molluscs.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

molluscs.confirmed.taxa.points <- st_transform(molluscs.confirmed.taxa.points, crs = st_crs(EPSG.32610))

molluscs.confirmed.records.gridded <- molluscs.confirmed.taxa.points %>% 
  st_join(grid)

molluscs.confirmed.records.gridded <- as.data.frame(molluscs.confirmed.records.gridded)

molluscs.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

molluscs.confirmed.taxa.points.sum <- st_transform(molluscs.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

molluscs.confirmed.grid <- grid %>%
  st_join(molluscs.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

molluscs.confirmed.grid = filter(molluscs.confirmed.grid, richness > 0)

molluscs.confirmed.grid$status <- 'confirmed'

plot(molluscs.reported.grid)

# Write choropleths

st_write(molluscs.confirmed.grid, "outputs/vectors/molluscs_confirmed_grid.shp")
st_write(molluscs.new.grid, "outputs/vectors/molluscs_new_grid.shp")
st_write(molluscs.reported.grid, "outputs/vectors/molluscs_reported_grid.shp")

# Write gridded dataframes

write.csv(molluscs.confirmed.records.gridded, "outputs/tabular_data/molluscs_confirmed_records_gridded.csv", row.names = FALSE)
write.csv(molluscs.new.records.gridded, "outputs/tabular_data/molluscs_new_records_gridded.csv", row.names = FALSE)
write.csv(molluscs.reported.records.gridded, "outputs/tabular_data/molluscs_reported_records_gridded.csv", row.names = FALSE)