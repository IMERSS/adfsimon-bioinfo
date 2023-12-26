# Script to prepare gridded maps of Galiano Island's macroalgae and phytoplankton diversity

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# Load libraries

library(dplyr)
library(here)
library(gapminder)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(hrbrthemes)
library(sf)
library(shinyjs)
library(tidyr)
library(tmap)
library(tmaptools)


# Load data (note: for marine algae, including marine animals as well so that grid encompasses all marine data)

algae <- read.csv("../../../../consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2023-04-22.csv")
animals <- read.csv("../../../../consolidate_records/Animalia/marine_animals/synthesized/Galiano_marine_animal_records_consolidated_2023-04-22.csv")


# Filter data by group

green <- algae %>% filter(phylum == 'Chlorophyta')

red <- algae %>% filter(phylum == 'Rhodophyta')

brown <- algae %>% filter(phylum == 'Ochrophyta')


# Filter out data missing coordinates

algae <- algae %>% drop_na(decimalLongitude)

green <- green %>% drop_na(decimalLongitude)

red <- red %>% drop_na(decimalLongitude)

brown <- brown %>% drop_na(decimalLongitude)

animals <- animals %>% drop_na(decimalLongitude)


# Create CRS object

EPSG.4326 <- st_crs(4326)


# Convert records to sf points

algae.points <- st_as_sf(algae, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

green.points <- st_as_sf(green, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

red.points <- st_as_sf(red, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

brown.points <- st_as_sf(brown, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

animal.points <- st_as_sf(animals, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)


# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)


# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

algae.points <- st_transform(algae.points, crs = st_crs(EPSG.32610))

green.points <- st_transform(green.points, crs = st_crs(EPSG.32610))

red.points <- st_transform(red.points, crs = st_crs(EPSG.32610))

brown.points <- st_transform(brown.points, crs = st_crs(EPSG.32610))

animal.points <- st_transform(animal.points, crs = st_crs(EPSG.32610))

# Summarized points (for choropleths)

algae.points.sum <- st_transform(algae.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

green.points.sum <- st_transform(green.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

red.points.sum <- st_transform(red.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

brown.points.sum <- st_transform(brown.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

animal.points.sum <- st_transform(animal.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()


# Combine points to create grid

points <- rbind(green.points, red.points, brown.points, animal.points)

# Set cell size

cs <- c(1000, 1000)

# Create grid

area.grid = st_make_grid(points, what = "polygons", square = TRUE, cellsize = cs)

sf::st_length(area.grid)
st_crs(area.grid)$proj4string

# Add grid ID

grid = st_sf(area.grid) %>%
  mutate(cell_id = 1:length(lengths(area.grid)))


# Generate gridded dataframes (each record assigned cell_id)

# All marine algae

algae.records.gridded <- algae.points %>% 
  st_join(grid)

algae.records.gridded <- as.data.frame(algae.records.gridded)

algae.records.gridded$geometry <- NULL

# Green algae

green.algae.records.gridded <- green.points %>% 
  st_join(grid)

green.algae.records.gridded <- as.data.frame(green.algae.records.gridded)

green.algae.records.gridded$geometry <- NULL

# Red algae

red.algae.records.gridded <- red.points %>% 
  st_join(grid)

red.algae.records.gridded <- as.data.frame(red.algae.records.gridded)

red.algae.records.gridded$geometry <- NULL

# Brown algae

brown.algae.records.gridded <- brown.points %>% 
  st_join(grid)

brown.algae.records.gridded <- as.data.frame(brown.algae.records.gridded)

brown.algae.records.gridded$geometry <- NULL


# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

# All marine algae

algae.grid <- grid %>%
  st_join(algae.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

algae.grid.count = filter(algae.grid, richness > 0)

# Green algae

green.algae.grid <- grid %>%
  st_join(green.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

green.algae.grid.count = filter(green.algae.grid, richness > 0)

# Red algae

red.algae.grid <- grid %>%
  st_join(red.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

red.algae.grid.count = filter(red.algae.grid, richness > 0)

# Brown algae

brown.algae.grid <- grid %>%
  st_join(brown.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

brown.algae.grid.count = filter(brown.algae.grid, richness > 0)


# Explore grid maps

# palette_explorer()

tmap_mode("view")

algae.grid.map = tm_shape(brown.algae.grid.count) +
  tm_fill(
    col = "richness",
    palette = "viridis",
    style = "cont",
    title = "Richness",
    showNA = FALSE,
    alpha = 0.5,
    popup.vars = c(
      "Richness: " = "richness"
    ),
    popup.format = list(
      richness = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

algae.grid.map

# Export shapes

st_write(algae.grid.count, "outputs/vectors/algae_grid.shp")
write.csv(algae.records.gridded, "outputs/tabular/algae_records_gridded.csv", row.names = FALSE)

st_write(green.algae.grid.count, "outputs/vectors/green_algae_grid.shp")
write.csv(green.algae.records.gridded, "outputs/tabular/green_algae_records_gridded.csv", row.names = FALSE)

st_write(red.algae.grid.count, "outputs/vectors/red_algae_grid.shp")
write.csv(red.algae.records.gridded, "outputs/tabular/red_algae_records_gridded.csv", row.names = FALSE)

st_write(brown.algae.grid.count, "outputs/vectors/brown_algae_grid.shp")
write.csv(brown.algae.records.gridded, "outputs/tabular/brown_algae_records_gridded.csv", row.names = FALSE)
