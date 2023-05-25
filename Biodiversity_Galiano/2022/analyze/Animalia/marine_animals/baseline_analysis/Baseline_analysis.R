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

algae <- read.csv("../../../../consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2023-04-22.csv")

# Note: algae are called here to define a grid with an extent that includes all marine observations, to ensure a normalised extent
# for all marine diversity choropleths

# Create grid

# Create CRS object

EPSG.4326 <- st_crs(4326)

# Drop rows lacking coordinates

animals.points <- animals %>% drop_na(decimalLatitude)

algae.points <- algae %>% drop_na(decimalLatitude)

# Convert records to sf points

animals.points <- st_as_sf(animals.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

algae.points <- st_as_sf(algae.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)


# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)

# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

animals.points <- st_transform(animals.points, crs = st_crs(EPSG.32610))

algae.points <- st_transform(algae.points, crs = st_crs(EPSG.32610))

# Combine points to create grid

points <- rbind(animals.points, algae.points)

# Set cell size

cs <- c(1000, 1000)

# Create grid

area.grid = st_make_grid(points, what = "polygons", square = TRUE, cellsize = cs)

sf::st_length(area.grid)
st_crs(area.grid)$proj4string

# Add grid ID

grid = st_sf(area.grid) %>%
  mutate(cell_id = 1:length(lengths(area.grid)))

# Sponges

unique(summary$class)

sponges <- summary %>% filter(phylum == 'Porifera')
sponges.records <- animals %>% filter(phylum == 'Porifera')

# Subset historic, confirmed and new records

sponges.new <- sponges %>% filter(str_detect(reportingStatus, "new"))
sponges.confirmed <- sponges %>% filter(reportingStatus == "confirmed")
sponges.reported <- sponges %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

sponges.new.taxa <- unique(sponges.new$scientificName)
sponges.new.taxa <- sponges.new.taxa %>% paste(collapse = "|")

sponges.confirmed.taxa <- unique(sponges.confirmed$scientificName)
sponges.confirmed.taxa <- sponges.confirmed.taxa %>% paste(collapse = "|")

sponges.reported.taxa <- unique(sponges.reported$scientificName)
sponges.reported.taxa <- sponges.reported.taxa %>% paste(collapse = "|")

sponges.new.taxa.records <- sponges.records %>% filter(str_detect(scientificName, sponges.new.taxa))

sponges.confirmed.taxa.records <- sponges.records %>% filter(str_detect(scientificName, sponges.confirmed.taxa))

sponges.reported.taxa.records <- sponges.records %>% filter(str_detect(scientificName, sponges.reported.taxa))

sponges.new.taxa.records <- sponges.new.taxa.records %>% drop_na(decimalLatitude)
sponges.confirmed.taxa.records <- sponges.confirmed.taxa.records %>% drop_na(decimalLatitude)
sponges.reported.taxa.records <- sponges.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

sponges.new.taxa.points <- st_as_sf(sponges.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

sponges.new.taxa.points <- st_transform(sponges.new.taxa.points, crs = st_crs(EPSG.32610))

sponges.new.records.gridded <- sponges.new.taxa.points %>% 
  st_join(grid)

sponges.new.records.gridded <- as.data.frame(sponges.new.records.gridded)

sponges.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

sponges.new.taxa.points.sum <- st_transform(sponges.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

sponges.new.grid <- grid %>%
  st_join(sponges.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

sponges.new.grid = filter(sponges.new.grid, richness > 0)

sponges.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

sponges.reported.taxa.points <- st_as_sf(sponges.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

sponges.reported.taxa.points <- st_transform(sponges.reported.taxa.points, crs = st_crs(EPSG.32610))

sponges.reported.records.gridded <- sponges.reported.taxa.points %>% 
  st_join(grid)

sponges.reported.records.gridded <- as.data.frame(sponges.reported.records.gridded)

sponges.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

sponges.reported.taxa.points.sum <- st_transform(sponges.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

sponges.reported.grid <- grid %>%
  st_join(sponges.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

sponges.reported.grid = filter(sponges.reported.grid, richness > 0)

sponges.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

sponges.confirmed.taxa.points <- st_as_sf(sponges.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

sponges.confirmed.taxa.points <- st_transform(sponges.confirmed.taxa.points, crs = st_crs(EPSG.32610))

sponges.confirmed.records.gridded <- sponges.confirmed.taxa.points %>% 
  st_join(grid)

sponges.confirmed.records.gridded <- as.data.frame(sponges.confirmed.records.gridded)

sponges.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

sponges.confirmed.taxa.points.sum <- st_transform(sponges.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

sponges.confirmed.grid <- grid %>%
  st_join(sponges.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

sponges.confirmed.grid = filter(sponges.confirmed.grid, richness > 0)

sponges.confirmed.grid$status <- 'confirmed'

plot(sponges.reported.grid)

# Write choropleths

# st_write(sponges.confirmed.grid, "outputs/vectors/sponges_confirmed_grid.shp")
# st_write(sponges.new.grid, "outputs/vectors/sponges_new_grid.shp")
# st_write(sponges.reported.grid, "outputs/vectors/sponges_reported_grid.shp")

# Write gridded dataframes

# write.csv(sponges.confirmed.records.gridded, "outputs/tabular_data/sponges_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(sponges.new.records.gridded, "outputs/tabular_data/sponges_new_records_gridded.csv", row.names = FALSE)
# write.csv(sponges.reported.records.gridded, "outputs/tabular_data/sponges_reported_records_gridded.csv", row.names = FALSE)


# Cnidarians

cnidarians <- summary %>% filter(phylum == 'Cnidaria')
cnidarians.records <- animals %>% filter(phylum == 'Cnidaria')

# Subset historic, confirmed and new records

cnidarians.new <- cnidarians %>% filter(str_detect(reportingStatus, "new"))
cnidarians.confirmed <- cnidarians %>% filter(reportingStatus == "confirmed")
cnidarians.reported <- cnidarians %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

cnidarians.new.taxa <- unique(cnidarians.new$scientificName)
cnidarians.new.taxa <- cnidarians.new.taxa %>% paste(collapse = "|")

cnidarians.confirmed.taxa <- unique(cnidarians.confirmed$scientificName)
cnidarians.confirmed.taxa <- cnidarians.confirmed.taxa %>% paste(collapse = "|")

cnidarians.reported.taxa <- unique(cnidarians.reported$scientificName)
cnidarians.reported.taxa <- cnidarians.reported.taxa %>% paste(collapse = "|")

cnidarians.new.taxa.records <- cnidarians.records %>% filter(str_detect(scientificName, cnidarians.new.taxa))

cnidarians.confirmed.taxa.records <- cnidarians.records %>% filter(str_detect(scientificName, cnidarians.confirmed.taxa))

cnidarians.reported.taxa.records <- cnidarians.records %>% filter(str_detect(scientificName, cnidarians.reported.taxa))

cnidarians.new.taxa.records <- cnidarians.new.taxa.records %>% drop_na(decimalLatitude)
cnidarians.confirmed.taxa.records <- cnidarians.confirmed.taxa.records %>% drop_na(decimalLatitude)
cnidarians.reported.taxa.records <- cnidarians.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

cnidarians.new.taxa.points <- st_as_sf(cnidarians.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

cnidarians.new.taxa.points <- st_transform(cnidarians.new.taxa.points, crs = st_crs(EPSG.32610))

cnidarians.new.records.gridded <- cnidarians.new.taxa.points %>% 
  st_join(grid)

cnidarians.new.records.gridded <- as.data.frame(cnidarians.new.records.gridded)

cnidarians.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

cnidarians.new.taxa.points.sum <- st_transform(cnidarians.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

cnidarians.new.grid <- grid %>%
  st_join(cnidarians.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

cnidarians.new.grid = filter(cnidarians.new.grid, richness > 0)

cnidarians.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

cnidarians.reported.taxa.points <- st_as_sf(cnidarians.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

cnidarians.reported.taxa.points <- st_transform(cnidarians.reported.taxa.points, crs = st_crs(EPSG.32610))

cnidarians.reported.records.gridded <- cnidarians.reported.taxa.points %>% 
  st_join(grid)

cnidarians.reported.records.gridded <- as.data.frame(cnidarians.reported.records.gridded)

cnidarians.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

cnidarians.reported.taxa.points.sum <- st_transform(cnidarians.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

cnidarians.reported.grid <- grid %>%
  st_join(cnidarians.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

cnidarians.reported.grid = filter(cnidarians.reported.grid, richness > 0)

cnidarians.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

cnidarians.confirmed.taxa.points <- st_as_sf(cnidarians.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

cnidarians.confirmed.taxa.points <- st_transform(cnidarians.confirmed.taxa.points, crs = st_crs(EPSG.32610))

cnidarians.confirmed.records.gridded <- cnidarians.confirmed.taxa.points %>% 
  st_join(grid)

cnidarians.confirmed.records.gridded <- as.data.frame(cnidarians.confirmed.records.gridded)

cnidarians.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

cnidarians.confirmed.taxa.points.sum <- st_transform(cnidarians.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

cnidarians.confirmed.grid <- grid %>%
  st_join(cnidarians.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

cnidarians.confirmed.grid = filter(cnidarians.confirmed.grid, richness > 0)

cnidarians.confirmed.grid$status <- 'confirmed'

plot(cnidarians.reported.grid)

# Write choropleths

# st_write(cnidarians.confirmed.grid, "outputs/vectors/cnidarians_confirmed_grid.shp")
# st_write(cnidarians.new.grid, "outputs/vectors/cnidarians_new_grid.shp")
# st_write(cnidarians.reported.grid, "outputs/vectors/cnidarians_reported_grid.shp")

# Write gridded dataframes

# write.csv(cnidarians.confirmed.records.gridded, "outputs/tabular_data/cnidarians_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(cnidarians.new.records.gridded, "outputs/tabular_data/cnidarians_new_records_gridded.csv", row.names = FALSE)
# write.csv(cnidarians.reported.records.gridded, "outputs/tabular_data/cnidarians_reported_records_gridded.csv", row.names = FALSE)


# Ctenophores

unique(summary$class)

ctenophores <- summary %>% filter(phylum == 'Ctenophora')
ctenophores.records <- animals %>% filter(phylum == 'Ctenophora')

# Subset historic, confirmed and new records

ctenophores.new <- ctenophores %>% filter(str_detect(reportingStatus, "new"))
ctenophores.confirmed <- ctenophores %>% filter(reportingStatus == "confirmed")
ctenophores.reported <- ctenophores %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

ctenophores.new.taxa <- unique(ctenophores.new$scientificName)
ctenophores.new.taxa <- ctenophores.new.taxa %>% paste(collapse = "|")

ctenophores.confirmed.taxa <- unique(ctenophores.confirmed$scientificName)
ctenophores.confirmed.taxa <- ctenophores.confirmed.taxa %>% paste(collapse = "|")

ctenophores.reported.taxa <- unique(ctenophores.reported$scientificName)
ctenophores.reported.taxa <- ctenophores.reported.taxa %>% paste(collapse = "|")

ctenophores.new.taxa.records <- ctenophores.records %>% filter(str_detect(scientificName, ctenophores.new.taxa))

ctenophores.confirmed.taxa.records <- ctenophores.records %>% filter(str_detect(scientificName, ctenophores.confirmed.taxa))

ctenophores.reported.taxa.records <- ctenophores.records %>% filter(str_detect(scientificName, ctenophores.reported.taxa))

ctenophores.new.taxa.records <- ctenophores.new.taxa.records %>% drop_na(decimalLatitude)
ctenophores.confirmed.taxa.records <- ctenophores.confirmed.taxa.records %>% drop_na(decimalLatitude)
ctenophores.reported.taxa.records <- ctenophores.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

ctenophores.new.taxa.points <- st_as_sf(ctenophores.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

ctenophores.new.taxa.points <- st_transform(ctenophores.new.taxa.points, crs = st_crs(EPSG.32610))

ctenophores.new.records.gridded <- ctenophores.new.taxa.points %>% 
  st_join(grid)

ctenophores.new.records.gridded <- as.data.frame(ctenophores.new.records.gridded)

ctenophores.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

ctenophores.new.taxa.points.sum <- st_transform(ctenophores.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

ctenophores.new.grid <- grid %>%
  st_join(ctenophores.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

ctenophores.new.grid = filter(ctenophores.new.grid, richness > 0)

ctenophores.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

ctenophores.reported.taxa.points <- st_as_sf(ctenophores.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

ctenophores.reported.taxa.points <- st_transform(ctenophores.reported.taxa.points, crs = st_crs(EPSG.32610))

ctenophores.reported.records.gridded <- ctenophores.reported.taxa.points %>% 
  st_join(grid)

ctenophores.reported.records.gridded <- as.data.frame(ctenophores.reported.records.gridded)

ctenophores.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

ctenophores.reported.taxa.points.sum <- st_transform(ctenophores.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

ctenophores.reported.grid <- grid %>%
  st_join(ctenophores.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

ctenophores.reported.grid = filter(ctenophores.reported.grid, richness > 0)

ctenophores.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

ctenophores.confirmed.taxa.points <- st_as_sf(ctenophores.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

ctenophores.confirmed.taxa.points <- st_transform(ctenophores.confirmed.taxa.points, crs = st_crs(EPSG.32610))

ctenophores.confirmed.records.gridded <- ctenophores.confirmed.taxa.points %>% 
  st_join(grid)

ctenophores.confirmed.records.gridded <- as.data.frame(ctenophores.confirmed.records.gridded)

ctenophores.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

ctenophores.confirmed.taxa.points.sum <- st_transform(ctenophores.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

ctenophores.confirmed.grid <- grid %>%
  st_join(ctenophores.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

ctenophores.confirmed.grid = filter(ctenophores.confirmed.grid, richness > 0)

ctenophores.confirmed.grid$status <- 'confirmed'

plot(ctenophores.reported.grid)

# Write choropleths

# st_write(ctenophores.confirmed.grid, "outputs/vectors/ctenophores_confirmed_grid.shp")
# st_write(ctenophores.new.grid, "outputs/vectors/ctenophores_new_grid.shp")
# st_write(ctenophores.reported.grid, "outputs/vectors/ctenophores_reported_grid.shp")

# Write gridded dataframes

# write.csv(ctenophores.confirmed.records.gridded, "outputs/tabular_data/ctenophores_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(ctenophores.new.records.gridded, "outputs/tabular_data/ctenophores_new_records_gridded.csv", row.names = FALSE)
# write.csv(ctenophores.reported.records.gridded, "outputs/tabular_data/ctenophores_reported_records_gridded.csv", row.names = FALSE)


# Nemerteans 

nemerteans <- summary %>% filter(phylum == 'Nemertea')
nemerteans.records <- animals %>% filter(phylum == 'Nemertea')

# Subset historic, confirmed and new records

nemerteans.new <- nemerteans %>% filter(str_detect(reportingStatus, "new"))
nemerteans.confirmed <- nemerteans %>% filter(reportingStatus == "confirmed")
nemerteans.reported <- nemerteans %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

nemerteans.new.taxa <- unique(nemerteans.new$scientificName)
nemerteans.new.taxa <- nemerteans.new.taxa %>% paste(collapse = "|")

nemerteans.confirmed.taxa <- unique(nemerteans.confirmed$scientificName)
nemerteans.confirmed.taxa <- nemerteans.confirmed.taxa %>% paste(collapse = "|")

nemerteans.reported.taxa <- unique(nemerteans.reported$scientificName)
nemerteans.reported.taxa <- nemerteans.reported.taxa %>% paste(collapse = "|")

nemerteans.new.taxa.records <- nemerteans.records %>% filter(str_detect(scientificName, nemerteans.new.taxa))

nemerteans.confirmed.taxa.records <- nemerteans.records %>% filter(str_detect(scientificName, nemerteans.confirmed.taxa))

nemerteans.reported.taxa.records <- nemerteans.records %>% filter(str_detect(scientificName, nemerteans.reported.taxa))

nemerteans.new.taxa.records <- nemerteans.new.taxa.records %>% drop_na(decimalLatitude)
nemerteans.confirmed.taxa.records <- nemerteans.confirmed.taxa.records %>% drop_na(decimalLatitude)
nemerteans.reported.taxa.records <- nemerteans.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

nemerteans.new.taxa.points <- st_as_sf(nemerteans.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nemerteans.new.taxa.points <- st_transform(nemerteans.new.taxa.points, crs = st_crs(EPSG.32610))

nemerteans.new.records.gridded <- nemerteans.new.taxa.points %>% 
  st_join(grid)

nemerteans.new.records.gridded <- as.data.frame(nemerteans.new.records.gridded)

nemerteans.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nemerteans.new.taxa.points.sum <- st_transform(nemerteans.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nemerteans.new.grid <- grid %>%
  st_join(nemerteans.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nemerteans.new.grid = filter(nemerteans.new.grid, richness > 0)

nemerteans.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

nemerteans.reported.taxa.points <- st_as_sf(nemerteans.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nemerteans.reported.taxa.points <- st_transform(nemerteans.reported.taxa.points, crs = st_crs(EPSG.32610))

nemerteans.reported.records.gridded <- nemerteans.reported.taxa.points %>% 
  st_join(grid)

nemerteans.reported.records.gridded <- as.data.frame(nemerteans.reported.records.gridded)

nemerteans.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nemerteans.reported.taxa.points.sum <- st_transform(nemerteans.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nemerteans.reported.grid <- grid %>%
  st_join(nemerteans.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nemerteans.reported.grid = filter(nemerteans.reported.grid, richness > 0)

nemerteans.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

nemerteans.confirmed.taxa.points <- st_as_sf(nemerteans.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nemerteans.confirmed.taxa.points <- st_transform(nemerteans.confirmed.taxa.points, crs = st_crs(EPSG.32610))

nemerteans.confirmed.records.gridded <- nemerteans.confirmed.taxa.points %>% 
  st_join(grid)

nemerteans.confirmed.records.gridded <- as.data.frame(nemerteans.confirmed.records.gridded)

nemerteans.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nemerteans.confirmed.taxa.points.sum <- st_transform(nemerteans.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nemerteans.confirmed.grid <- grid %>%
  st_join(nemerteans.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nemerteans.confirmed.grid = filter(nemerteans.confirmed.grid, richness > 0)

nemerteans.confirmed.grid$status <- 'confirmed'

plot(nemerteans.reported.grid)

# Write choropleths

# st_write(nemerteans.confirmed.grid, "outputs/vectors/nemerteans_confirmed_grid.shp")
# st_write(nemerteans.new.grid, "outputs/vectors/nemerteans_new_grid.shp")
# st_write(nemerteans.reported.grid, "outputs/vectors/nemerteans_reported_grid.shp")

# Write gridded dataframes

# write.csv(nemerteans.confirmed.records.gridded, "outputs/tabular_data/nemerteans_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(nemerteans.new.records.gridded, "outputs/tabular_data/nemerteans_new_records_gridded.csv", row.names = FALSE)
# write.csv(nemerteans.reported.records.gridded, "outputs/tabular_data/nemerteans_reported_records_gridded.csv", row.names = FALSE)


# Platyhelminthes

platyhelminthes <- summary %>% filter(phylum == 'Platyhelminthes')
platyhelminthes.records <- animals %>% filter(phylum == 'Platyhelminthes')

# Subset historic, confirmed and new records

platyhelminthes.new <- platyhelminthes %>% filter(str_detect(reportingStatus, "new"))
platyhelminthes.confirmed <- platyhelminthes %>% filter(reportingStatus == "confirmed")
platyhelminthes.reported <- platyhelminthes %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

platyhelminthes.new.taxa <- unique(platyhelminthes.new$scientificName)
platyhelminthes.new.taxa <- platyhelminthes.new.taxa %>% paste(collapse = "|")

platyhelminthes.confirmed.taxa <- unique(platyhelminthes.confirmed$scientificName)
platyhelminthes.confirmed.taxa <- platyhelminthes.confirmed.taxa %>% paste(collapse = "|")

platyhelminthes.reported.taxa <- unique(platyhelminthes.reported$scientificName)
platyhelminthes.reported.taxa <- platyhelminthes.reported.taxa %>% paste(collapse = "|")

platyhelminthes.new.taxa.records <- platyhelminthes.records %>% filter(str_detect(scientificName, platyhelminthes.new.taxa))

platyhelminthes.confirmed.taxa.records <- platyhelminthes.records %>% filter(str_detect(scientificName, platyhelminthes.confirmed.taxa))

platyhelminthes.reported.taxa.records <- platyhelminthes.records %>% filter(str_detect(scientificName, platyhelminthes.reported.taxa))

platyhelminthes.new.taxa.records <- platyhelminthes.new.taxa.records %>% drop_na(decimalLatitude)
platyhelminthes.confirmed.taxa.records <- platyhelminthes.confirmed.taxa.records %>% drop_na(decimalLatitude)
platyhelminthes.reported.taxa.records <- platyhelminthes.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

platyhelminthes.new.taxa.points <- st_as_sf(platyhelminthes.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

platyhelminthes.new.taxa.points <- st_transform(platyhelminthes.new.taxa.points, crs = st_crs(EPSG.32610))

platyhelminthes.new.records.gridded <- platyhelminthes.new.taxa.points %>% 
  st_join(grid)

platyhelminthes.new.records.gridded <- as.data.frame(platyhelminthes.new.records.gridded)

platyhelminthes.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

platyhelminthes.new.taxa.points.sum <- st_transform(platyhelminthes.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

platyhelminthes.new.grid <- grid %>%
  st_join(platyhelminthes.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

platyhelminthes.new.grid = filter(platyhelminthes.new.grid, richness > 0)

platyhelminthes.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

platyhelminthes.reported.taxa.points <- st_as_sf(platyhelminthes.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

platyhelminthes.reported.taxa.points <- st_transform(platyhelminthes.reported.taxa.points, crs = st_crs(EPSG.32610))

platyhelminthes.reported.records.gridded <- platyhelminthes.reported.taxa.points %>% 
  st_join(grid)

platyhelminthes.reported.records.gridded <- as.data.frame(platyhelminthes.reported.records.gridded)

platyhelminthes.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

platyhelminthes.reported.taxa.points.sum <- st_transform(platyhelminthes.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

platyhelminthes.reported.grid <- grid %>%
  st_join(platyhelminthes.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

platyhelminthes.reported.grid = filter(platyhelminthes.reported.grid, richness > 0)

platyhelminthes.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

platyhelminthes.confirmed.taxa.points <- st_as_sf(platyhelminthes.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

platyhelminthes.confirmed.taxa.points <- st_transform(platyhelminthes.confirmed.taxa.points, crs = st_crs(EPSG.32610))

platyhelminthes.confirmed.records.gridded <- platyhelminthes.confirmed.taxa.points %>% 
  st_join(grid)

platyhelminthes.confirmed.records.gridded <- as.data.frame(platyhelminthes.confirmed.records.gridded)

platyhelminthes.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

platyhelminthes.confirmed.taxa.points.sum <- st_transform(platyhelminthes.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

platyhelminthes.confirmed.grid <- grid %>%
  st_join(platyhelminthes.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

platyhelminthes.confirmed.grid = filter(platyhelminthes.confirmed.grid, richness > 0)

platyhelminthes.confirmed.grid$status <- 'confirmed'

plot(platyhelminthes.reported.grid)

# Write choropleths

# st_write(platyhelminthes.confirmed.grid, "outputs/vectors/platyhelminthes_confirmed_grid.shp")
# st_write(platyhelminthes.new.grid, "outputs/vectors/platyhelminthes_new_grid.shp")
# st_write(platyhelminthes.reported.grid, "outputs/vectors/platyhelminthes_reported_grid.shp")

# Write gridded dataframes

# write.csv(platyhelminthes.confirmed.records.gridded, "outputs/tabular_data/platyhelminthes_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(platyhelminthes.new.records.gridded, "outputs/tabular_data/platyhelminthes_new_records_gridded.csv", row.names = FALSE)
# write.csv(platyhelminthes.reported.records.gridded, "outputs/tabular_data/platyhelminthes_reported_records_gridded.csv", row.names = FALSE)


# Chaetognatha

arrow.worms <- summary %>% filter(phylum == 'Chaetognatha')
arrow.worms.records <- animals %>% filter(phylum == 'Chaetognatha')

# Subset historic, confirmed and new records

arrow.worms.new <- arrow.worms %>% filter(str_detect(reportingStatus, "new"))
arrow.worms.confirmed <- arrow.worms %>% filter(reportingStatus == "confirmed")
arrow.worms.reported <- arrow.worms %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

arrow.worms.new.taxa <- unique(arrow.worms.new$scientificName)
arrow.worms.new.taxa <- arrow.worms.new.taxa %>% paste(collapse = "|")

arrow.worms.confirmed.taxa <- unique(arrow.worms.confirmed$scientificName)
arrow.worms.confirmed.taxa <- arrow.worms.confirmed.taxa %>% paste(collapse = "|")

arrow.worms.reported.taxa <- unique(arrow.worms.reported$scientificName)
arrow.worms.reported.taxa <- arrow.worms.reported.taxa %>% paste(collapse = "|")

arrow.worms.new.taxa.records <- arrow.worms.records %>% filter(str_detect(scientificName, arrow.worms.new.taxa))

arrow.worms.confirmed.taxa.records <- arrow.worms.records %>% filter(str_detect(scientificName, arrow.worms.confirmed.taxa))

arrow.worms.reported.taxa.records <- arrow.worms.records %>% filter(str_detect(scientificName, arrow.worms.reported.taxa))

arrow.worms.new.taxa.records <- arrow.worms.new.taxa.records %>% drop_na(decimalLatitude)
arrow.worms.confirmed.taxa.records <- arrow.worms.confirmed.taxa.records %>% drop_na(decimalLatitude)
arrow.worms.reported.taxa.records <- arrow.worms.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

arrow.worms.new.taxa.points <- st_as_sf(arrow.worms.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

arrow.worms.new.taxa.points <- st_transform(arrow.worms.new.taxa.points, crs = st_crs(EPSG.32610))

arrow.worms.new.records.gridded <- arrow.worms.new.taxa.points %>% 
  st_join(grid)

arrow.worms.new.records.gridded <- as.data.frame(arrow.worms.new.records.gridded)

arrow.worms.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

arrow.worms.new.taxa.points.sum <- st_transform(arrow.worms.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

arrow.worms.new.grid <- grid %>%
  st_join(arrow.worms.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

arrow.worms.new.grid = filter(arrow.worms.new.grid, richness > 0)

arrow.worms.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

arrow.worms.reported.taxa.points <- st_as_sf(arrow.worms.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

arrow.worms.reported.taxa.points <- st_transform(arrow.worms.reported.taxa.points, crs = st_crs(EPSG.32610))

arrow.worms.reported.records.gridded <- arrow.worms.reported.taxa.points %>% 
  st_join(grid)

arrow.worms.reported.records.gridded <- as.data.frame(arrow.worms.reported.records.gridded)

arrow.worms.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

arrow.worms.reported.taxa.points.sum <- st_transform(arrow.worms.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

arrow.worms.reported.grid <- grid %>%
  st_join(arrow.worms.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

arrow.worms.reported.grid = filter(arrow.worms.reported.grid, richness > 0)

arrow.worms.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

arrow.worms.confirmed.taxa.points <- st_as_sf(arrow.worms.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

arrow.worms.confirmed.taxa.points <- st_transform(arrow.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610))

arrow.worms.confirmed.records.gridded <- arrow.worms.confirmed.taxa.points %>% 
  st_join(grid)

arrow.worms.confirmed.records.gridded <- as.data.frame(arrow.worms.confirmed.records.gridded)

arrow.worms.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

arrow.worms.confirmed.taxa.points.sum <- st_transform(arrow.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

arrow.worms.confirmed.grid <- grid %>%
  st_join(arrow.worms.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

arrow.worms.confirmed.grid = filter(arrow.worms.confirmed.grid, richness > 0)

arrow.worms.confirmed.grid$status <- 'confirmed'

plot(arrow.worms.reported.grid)

# Write choropleths

# st_write(arrow.worms.confirmed.grid, "outputs/vectors/arrow_worms_confirmed_grid.shp")
# st_write(arrow.worms.new.grid, "outputs/vectors/arrow_worms_new_grid.shp")
# st_write(arrow.worms.reported.grid, "outputs/vectors/arrow_worms_reported_grid.shp")

# Write gridded dataframes

# write.csv(arrow.worms.confirmed.records.gridded, "outputs/tabular_data/arrow_worms_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(arrow.worms.new.records.gridded, "outputs/tabular_data/arrow_worms_new_records_gridded.csv", row.names = FALSE)
# write.csv(arrow.worms.reported.records.gridded, "outputs/tabular_data/arrow_worms_reported_records_gridded.csv", row.names = FALSE)


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

# st_write(molluscs.confirmed.grid, "outputs/vectors/molluscs_confirmed_grid.shp")
# st_write(molluscs.new.grid, "outputs/vectors/molluscs_new_grid.shp")
# st_write(molluscs.reported.grid, "outputs/vectors/molluscs_reported_grid.shp")

# Write gridded dataframes

# write.csv(molluscs.confirmed.records.gridded, "outputs/tabular_data/molluscs_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(molluscs.new.records.gridded, "outputs/tabular_data/molluscs_new_records_gridded.csv", row.names = FALSE)
# write.csv(molluscs.reported.records.gridded, "outputs/tabular_data/molluscs_reported_records_gridded.csv", row.names = FALSE)


# Annelids

annelids <- summary %>% filter(phylum == 'Annelida')
annelids.records <- animals %>% filter(phylum == 'Annelida')

# Subset historic, confirmed and new records

annelids.new <- annelids %>% filter(str_detect(reportingStatus, "new"))
annelids.confirmed <- annelids %>% filter(reportingStatus == "confirmed")
annelids.reported <- annelids %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

annelids.new.taxa <- unique(annelids.new$scientificName)
annelids.new.taxa <- annelids.new.taxa %>% paste(collapse = "|")

annelids.confirmed.taxa <- unique(annelids.confirmed$scientificName)
annelids.confirmed.taxa <- annelids.confirmed.taxa %>% paste(collapse = "|")

annelids.reported.taxa <- unique(annelids.reported$scientificName)
annelids.reported.taxa <- annelids.reported.taxa %>% paste(collapse = "|")

annelids.new.taxa.records <- annelids.records %>% filter(str_detect(scientificName, annelids.new.taxa))

annelids.confirmed.taxa.records <- annelids.records %>% filter(str_detect(scientificName, annelids.confirmed.taxa))

annelids.reported.taxa.records <- annelids.records %>% filter(str_detect(scientificName, annelids.reported.taxa))

annelids.new.taxa.records <- annelids.new.taxa.records %>% drop_na(decimalLatitude)
annelids.confirmed.taxa.records <- annelids.confirmed.taxa.records %>% drop_na(decimalLatitude)
annelids.reported.taxa.records <- annelids.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

annelids.new.taxa.points <- st_as_sf(annelids.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

annelids.new.taxa.points <- st_transform(annelids.new.taxa.points, crs = st_crs(EPSG.32610))

annelids.new.records.gridded <- annelids.new.taxa.points %>% 
  st_join(grid)

annelids.new.records.gridded <- as.data.frame(annelids.new.records.gridded)

annelids.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

annelids.new.taxa.points.sum <- st_transform(annelids.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

annelids.new.grid <- grid %>%
  st_join(annelids.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

annelids.new.grid = filter(annelids.new.grid, richness > 0)

annelids.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

annelids.reported.taxa.points <- st_as_sf(annelids.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

annelids.reported.taxa.points <- st_transform(annelids.reported.taxa.points, crs = st_crs(EPSG.32610))

annelids.reported.records.gridded <- annelids.reported.taxa.points %>% 
  st_join(grid)

annelids.reported.records.gridded <- as.data.frame(annelids.reported.records.gridded)

annelids.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

annelids.reported.taxa.points.sum <- st_transform(annelids.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

annelids.reported.grid <- grid %>%
  st_join(annelids.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

annelids.reported.grid = filter(annelids.reported.grid, richness > 0)

annelids.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

annelids.confirmed.taxa.points <- st_as_sf(annelids.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

annelids.confirmed.taxa.points <- st_transform(annelids.confirmed.taxa.points, crs = st_crs(EPSG.32610))

annelids.confirmed.records.gridded <- annelids.confirmed.taxa.points %>% 
  st_join(grid)

annelids.confirmed.records.gridded <- as.data.frame(annelids.confirmed.records.gridded)

annelids.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

annelids.confirmed.taxa.points.sum <- st_transform(annelids.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

annelids.confirmed.grid <- grid %>%
  st_join(annelids.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

annelids.confirmed.grid = filter(annelids.confirmed.grid, richness > 0)

annelids.confirmed.grid$status <- 'confirmed'

plot(annelids.reported.grid)

# Write choropleths

# st_write(annelids.confirmed.grid, "outputs/vectors/annelids_confirmed_grid.shp")
# st_write(annelids.new.grid, "outputs/vectors/annelids_new_grid.shp")
# st_write(annelids.reported.grid, "outputs/vectors/annelids_reported_grid.shp")

# Write gridded dataframes

# write.csv(annelids.confirmed.records.gridded, "outputs/tabular_data/annelids_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(annelids.new.records.gridded, "outputs/tabular_data/annelids_new_records_gridded.csv", row.names = FALSE)
# write.csv(annelids.reported.records.gridded, "outputs/tabular_data/annelids_reported_records_gridded.csv", row.names = FALSE)


# Sipuncula

peanut.worms <- summary %>% filter(phylum == 'Sipuncula')
peanut.worms.records <- animals %>% filter(phylum == 'Sipuncula')

# Subset historic, confirmed and new records

peanut.worms.new <- peanut.worms %>% filter(str_detect(reportingStatus, "new"))
peanut.worms.confirmed <- peanut.worms %>% filter(reportingStatus == "confirmed")
peanut.worms.reported <- peanut.worms %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

peanut.worms.new.taxa <- unique(peanut.worms.new$scientificName)
peanut.worms.new.taxa <- peanut.worms.new.taxa %>% paste(collapse = "|")

peanut.worms.confirmed.taxa <- unique(peanut.worms.confirmed$scientificName)
peanut.worms.confirmed.taxa <- peanut.worms.confirmed.taxa %>% paste(collapse = "|")

peanut.worms.reported.taxa <- unique(peanut.worms.reported$scientificName)
peanut.worms.reported.taxa <- peanut.worms.reported.taxa %>% paste(collapse = "|")

peanut.worms.new.taxa.records <- peanut.worms.records %>% filter(str_detect(scientificName, peanut.worms.new.taxa))

peanut.worms.confirmed.taxa.records <- peanut.worms.records %>% filter(str_detect(scientificName, peanut.worms.confirmed.taxa))

peanut.worms.reported.taxa.records <- peanut.worms.records %>% filter(str_detect(scientificName, peanut.worms.reported.taxa))

peanut.worms.new.taxa.records <- peanut.worms.new.taxa.records %>% drop_na(decimalLatitude)
peanut.worms.confirmed.taxa.records <- peanut.worms.confirmed.taxa.records %>% drop_na(decimalLatitude)
peanut.worms.reported.taxa.records <- peanut.worms.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

peanut.worms.new.taxa.points <- st_as_sf(peanut.worms.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

peanut.worms.new.taxa.points <- st_transform(peanut.worms.new.taxa.points, crs = st_crs(EPSG.32610))

peanut.worms.new.records.gridded <- peanut.worms.new.taxa.points %>% 
  st_join(grid)

peanut.worms.new.records.gridded <- as.data.frame(peanut.worms.new.records.gridded)

peanut.worms.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

peanut.worms.new.taxa.points.sum <- st_transform(peanut.worms.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

peanut.worms.new.grid <- grid %>%
  st_join(peanut.worms.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

peanut.worms.new.grid = filter(peanut.worms.new.grid, richness > 0)

peanut.worms.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

peanut.worms.reported.taxa.points <- st_as_sf(peanut.worms.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

peanut.worms.reported.taxa.points <- st_transform(peanut.worms.reported.taxa.points, crs = st_crs(EPSG.32610))

peanut.worms.reported.records.gridded <- peanut.worms.reported.taxa.points %>% 
  st_join(grid)

peanut.worms.reported.records.gridded <- as.data.frame(peanut.worms.reported.records.gridded)

peanut.worms.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

peanut.worms.reported.taxa.points.sum <- st_transform(peanut.worms.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

peanut.worms.reported.grid <- grid %>%
  st_join(peanut.worms.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

peanut.worms.reported.grid = filter(peanut.worms.reported.grid, richness > 0)

peanut.worms.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

peanut.worms.confirmed.taxa.points <- st_as_sf(peanut.worms.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

peanut.worms.confirmed.taxa.points <- st_transform(peanut.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610))

peanut.worms.confirmed.records.gridded <- peanut.worms.confirmed.taxa.points %>% 
  st_join(grid)

peanut.worms.confirmed.records.gridded <- as.data.frame(peanut.worms.confirmed.records.gridded)

peanut.worms.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

peanut.worms.confirmed.taxa.points.sum <- st_transform(peanut.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

peanut.worms.confirmed.grid <- grid %>%
  st_join(peanut.worms.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

peanut.worms.confirmed.grid = filter(peanut.worms.confirmed.grid, richness > 0)

peanut.worms.confirmed.grid$status <- 'confirmed'

plot(peanut.worms.reported.grid)

# Write choropleths

# st_write(peanut.worms.confirmed.grid, "outputs/vectors/peanut_worms_confirmed_grid.shp")
# st_write(peanut.worms.new.grid, "outputs/vectors/peanut_worms_new_grid.shp")
# st_write(peanut.worms.reported.grid, "outputs/vectors/peanut_worms_reported_grid.shp")

# Write gridded dataframes

# write.csv(peanut.worms.confirmed.records.gridded, "outputs/tabular_data/peanut_worms_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(peanut.worms.new.records.gridded, "outputs/tabular_data/peanut_worms_new_records_gridded.csv", row.names = FALSE)
# write.csv(peanut.worms.reported.records.gridded, "outputs/tabular_data/peanut_worms_reported_records_gridded.csv", row.names = FALSE)


# Crustacea

crustaceans <- summary %>% filter(subphylum == 'Crustacea')
crustaceans.records <- animals %>% filter(subphylum == 'Crustacea')

# Subset historic, confirmed and new records

crustaceans.new <- crustaceans %>% filter(str_detect(reportingStatus, "new"))
crustaceans.confirmed <- crustaceans %>% filter(reportingStatus == "confirmed")
crustaceans.reported <- crustaceans %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

crustaceans.new.taxa <- unique(crustaceans.new$scientificName)
crustaceans.new.taxa <- crustaceans.new.taxa %>% paste(collapse = "|")

crustaceans.confirmed.taxa <- unique(crustaceans.confirmed$scientificName)
crustaceans.confirmed.taxa <- crustaceans.confirmed.taxa %>% paste(collapse = "|")

crustaceans.reported.taxa <- unique(crustaceans.reported$scientificName)
crustaceans.reported.taxa <- crustaceans.reported.taxa %>% paste(collapse = "|")

crustaceans.new.taxa.records <- crustaceans.records %>% filter(str_detect(scientificName, crustaceans.new.taxa))

crustaceans.confirmed.taxa.records <- crustaceans.records %>% filter(str_detect(scientificName, crustaceans.confirmed.taxa))

crustaceans.reported.taxa.records <- crustaceans.records %>% filter(str_detect(scientificName, crustaceans.reported.taxa))

crustaceans.new.taxa.records <- crustaceans.new.taxa.records %>% drop_na(decimalLatitude)
crustaceans.confirmed.taxa.records <- crustaceans.confirmed.taxa.records %>% drop_na(decimalLatitude)
crustaceans.reported.taxa.records <- crustaceans.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

crustaceans.new.taxa.points <- st_as_sf(crustaceans.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

crustaceans.new.taxa.points <- st_transform(crustaceans.new.taxa.points, crs = st_crs(EPSG.32610))

crustaceans.new.records.gridded <- crustaceans.new.taxa.points %>% 
  st_join(grid)

crustaceans.new.records.gridded <- as.data.frame(crustaceans.new.records.gridded)

crustaceans.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

crustaceans.new.taxa.points.sum <- st_transform(crustaceans.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

crustaceans.new.grid <- grid %>%
  st_join(crustaceans.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

crustaceans.new.grid = filter(crustaceans.new.grid, richness > 0)

crustaceans.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

crustaceans.reported.taxa.points <- st_as_sf(crustaceans.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

crustaceans.reported.taxa.points <- st_transform(crustaceans.reported.taxa.points, crs = st_crs(EPSG.32610))

crustaceans.reported.records.gridded <- crustaceans.reported.taxa.points %>% 
  st_join(grid)

crustaceans.reported.records.gridded <- as.data.frame(crustaceans.reported.records.gridded)

crustaceans.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

crustaceans.reported.taxa.points.sum <- st_transform(crustaceans.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

crustaceans.reported.grid <- grid %>%
  st_join(crustaceans.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

crustaceans.reported.grid = filter(crustaceans.reported.grid, richness > 0)

crustaceans.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

crustaceans.confirmed.taxa.points <- st_as_sf(crustaceans.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

crustaceans.confirmed.taxa.points <- st_transform(crustaceans.confirmed.taxa.points, crs = st_crs(EPSG.32610))

crustaceans.confirmed.records.gridded <- crustaceans.confirmed.taxa.points %>% 
  st_join(grid)

crustaceans.confirmed.records.gridded <- as.data.frame(crustaceans.confirmed.records.gridded)

crustaceans.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

crustaceans.confirmed.taxa.points.sum <- st_transform(crustaceans.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

crustaceans.confirmed.grid <- grid %>%
  st_join(crustaceans.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

crustaceans.confirmed.grid = filter(crustaceans.confirmed.grid, richness > 0)

crustaceans.confirmed.grid$status <- 'confirmed'

plot(crustaceans.reported.grid)

# Write choropleths

# st_write(crustaceans.confirmed.grid, "outputs/vectors/crustaceans_confirmed_grid.shp")
# st_write(crustaceans.new.grid, "outputs/vectors/crustaceans_new_grid.shp")
# st_write(crustaceans.reported.grid, "outputs/vectors/crustaceans_reported_grid.shp")

# Write gridded dataframes

# write.csv(crustaceans.confirmed.records.gridded, "outputs/tabular_data/crustaceans_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(crustaceans.new.records.gridded, "outputs/tabular_data/crustaceans_new_records_gridded.csv", row.names = FALSE)
# write.csv(crustaceans.reported.records.gridded, "outputs/tabular_data/crustaceans_reported_records_gridded.csv", row.names = FALSE)


# Entoprocta

nodding.heads <- summary %>% filter(phylum == 'Entoprocta')
nodding.heads.records <- animals %>% filter(phylum == 'Entoprocta')

# Subset historic, confirmed and new records

nodding.heads.new <- nodding.heads %>% filter(str_detect(reportingStatus, "new"))
nodding.heads.confirmed <- nodding.heads %>% filter(reportingStatus == "confirmed")
nodding.heads.reported <- nodding.heads %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

nodding.heads.new.taxa <- unique(nodding.heads.new$scientificName)
nodding.heads.new.taxa <- nodding.heads.new.taxa %>% paste(collapse = "|")

nodding.heads.confirmed.taxa <- unique(nodding.heads.confirmed$scientificName)
nodding.heads.confirmed.taxa <- nodding.heads.confirmed.taxa %>% paste(collapse = "|")

nodding.heads.reported.taxa <- unique(nodding.heads.reported$scientificName)
nodding.heads.reported.taxa <- nodding.heads.reported.taxa %>% paste(collapse = "|")

nodding.heads.new.taxa.records <- nodding.heads.records %>% filter(str_detect(scientificName, nodding.heads.new.taxa))

nodding.heads.confirmed.taxa.records <- nodding.heads.records %>% filter(str_detect(scientificName, nodding.heads.confirmed.taxa))

nodding.heads.reported.taxa.records <- nodding.heads.records %>% filter(str_detect(scientificName, nodding.heads.reported.taxa))

nodding.heads.new.taxa.records <- nodding.heads.new.taxa.records %>% drop_na(decimalLatitude)
nodding.heads.confirmed.taxa.records <- nodding.heads.confirmed.taxa.records %>% drop_na(decimalLatitude)
nodding.heads.reported.taxa.records <- nodding.heads.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

nodding.heads.new.taxa.points <- st_as_sf(nodding.heads.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nodding.heads.new.taxa.points <- st_transform(nodding.heads.new.taxa.points, crs = st_crs(EPSG.32610))

nodding.heads.new.records.gridded <- nodding.heads.new.taxa.points %>% 
  st_join(grid)

nodding.heads.new.records.gridded <- as.data.frame(nodding.heads.new.records.gridded)

nodding.heads.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nodding.heads.new.taxa.points.sum <- st_transform(nodding.heads.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nodding.heads.new.grid <- grid %>%
  st_join(nodding.heads.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nodding.heads.new.grid = filter(nodding.heads.new.grid, richness > 0)

nodding.heads.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

nodding.heads.reported.taxa.points <- st_as_sf(nodding.heads.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nodding.heads.reported.taxa.points <- st_transform(nodding.heads.reported.taxa.points, crs = st_crs(EPSG.32610))

nodding.heads.reported.records.gridded <- nodding.heads.reported.taxa.points %>% 
  st_join(grid)

nodding.heads.reported.records.gridded <- as.data.frame(nodding.heads.reported.records.gridded)

nodding.heads.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nodding.heads.reported.taxa.points.sum <- st_transform(nodding.heads.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nodding.heads.reported.grid <- grid %>%
  st_join(nodding.heads.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nodding.heads.reported.grid = filter(nodding.heads.reported.grid, richness > 0)

nodding.heads.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

nodding.heads.confirmed.taxa.points <- st_as_sf(nodding.heads.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

nodding.heads.confirmed.taxa.points <- st_transform(nodding.heads.confirmed.taxa.points, crs = st_crs(EPSG.32610))

nodding.heads.confirmed.records.gridded <- nodding.heads.confirmed.taxa.points %>% 
  st_join(grid)

nodding.heads.confirmed.records.gridded <- as.data.frame(nodding.heads.confirmed.records.gridded)

nodding.heads.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

nodding.heads.confirmed.taxa.points.sum <- st_transform(nodding.heads.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

nodding.heads.confirmed.grid <- grid %>%
  st_join(nodding.heads.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nodding.heads.confirmed.grid = filter(nodding.heads.confirmed.grid, richness > 0)

nodding.heads.confirmed.grid$status <- 'confirmed'

plot(nodding.heads.reported.grid)

# Write choropleths

# st_write(nodding.heads.confirmed.grid, "outputs/vectors/nodding_heads_confirmed_grid.shp")
# st_write(nodding.heads.new.grid, "outputs/vectors/nodding_heads_new_grid.shp")
# st_write(nodding.heads.reported.grid, "outputs/vectors/nodding_heads_reported_grid.shp")

# Write gridded dataframes

# write.csv(nodding.heads.confirmed.records.gridded, "outputs/tabular_data/nodding_heads_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(nodding.heads.new.records.gridded, "outputs/tabular_data/nodding_heads_new_records_gridded.csv", row.names = FALSE)
# write.csv(nodding.heads.reported.records.gridded, "outputs/tabular_data/nodding_heads_reported_records_gridded.csv", row.names = FALSE)


# Brachiopoda

brachiopods <- summary %>% filter(phylum == 'Brachiopoda')
brachiopods.records <- animals %>% filter(phylum == 'Brachiopoda')

# Subset historic, confirmed and new records

brachiopods.new <- brachiopods %>% filter(str_detect(reportingStatus, "new"))
brachiopods.confirmed <- brachiopods %>% filter(reportingStatus == "confirmed")
brachiopods.reported <- brachiopods %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

brachiopods.new.taxa <- unique(brachiopods.new$scientificName)
brachiopods.new.taxa <- brachiopods.new.taxa %>% paste(collapse = "|")

brachiopods.confirmed.taxa <- unique(brachiopods.confirmed$scientificName)
brachiopods.confirmed.taxa <- brachiopods.confirmed.taxa %>% paste(collapse = "|")

brachiopods.reported.taxa <- unique(brachiopods.reported$scientificName)
brachiopods.reported.taxa <- brachiopods.reported.taxa %>% paste(collapse = "|")

brachiopods.new.taxa.records <- brachiopods.records %>% filter(str_detect(scientificName, brachiopods.new.taxa))

brachiopods.confirmed.taxa.records <- brachiopods.records %>% filter(str_detect(scientificName, brachiopods.confirmed.taxa))

brachiopods.reported.taxa.records <- brachiopods.records %>% filter(str_detect(scientificName, brachiopods.reported.taxa))

brachiopods.new.taxa.records <- brachiopods.new.taxa.records %>% drop_na(decimalLatitude)
brachiopods.confirmed.taxa.records <- brachiopods.confirmed.taxa.records %>% drop_na(decimalLatitude)
brachiopods.reported.taxa.records <- brachiopods.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

brachiopods.new.taxa.points <- st_as_sf(brachiopods.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

brachiopods.new.taxa.points <- st_transform(brachiopods.new.taxa.points, crs = st_crs(EPSG.32610))

brachiopods.new.records.gridded <- brachiopods.new.taxa.points %>% 
  st_join(grid)

brachiopods.new.records.gridded <- as.data.frame(brachiopods.new.records.gridded)

brachiopods.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

brachiopods.new.taxa.points.sum <- st_transform(brachiopods.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

brachiopods.new.grid <- grid %>%
  st_join(brachiopods.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

brachiopods.new.grid = filter(brachiopods.new.grid, richness > 0)

brachiopods.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

brachiopods.reported.taxa.points <- st_as_sf(brachiopods.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

brachiopods.reported.taxa.points <- st_transform(brachiopods.reported.taxa.points, crs = st_crs(EPSG.32610))

brachiopods.reported.records.gridded <- brachiopods.reported.taxa.points %>% 
  st_join(grid)

brachiopods.reported.records.gridded <- as.data.frame(brachiopods.reported.records.gridded)

brachiopods.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

brachiopods.reported.taxa.points.sum <- st_transform(brachiopods.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

brachiopods.reported.grid <- grid %>%
  st_join(brachiopods.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

brachiopods.reported.grid = filter(brachiopods.reported.grid, richness > 0)

brachiopods.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

brachiopods.confirmed.taxa.points <- st_as_sf(brachiopods.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

brachiopods.confirmed.taxa.points <- st_transform(brachiopods.confirmed.taxa.points, crs = st_crs(EPSG.32610))

brachiopods.confirmed.records.gridded <- brachiopods.confirmed.taxa.points %>% 
  st_join(grid)

brachiopods.confirmed.records.gridded <- as.data.frame(brachiopods.confirmed.records.gridded)

brachiopods.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

brachiopods.confirmed.taxa.points.sum <- st_transform(brachiopods.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

brachiopods.confirmed.grid <- grid %>%
  st_join(brachiopods.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

brachiopods.confirmed.grid = filter(brachiopods.confirmed.grid, richness > 0)

brachiopods.confirmed.grid$status <- 'confirmed'

plot(brachiopods.reported.grid)

# Write choropleths

# st_write(brachiopods.confirmed.grid, "outputs/vectors/brachiopods_confirmed_grid.shp")
# st_write(brachiopods.new.grid, "outputs/vectors/brachiopods_new_grid.shp")
# st_write(brachiopods.reported.grid, "outputs/vectors/brachiopods_reported_grid.shp")

# Write gridded dataframes

# write.csv(brachiopods.confirmed.records.gridded, "outputs/tabular_data/brachiopods_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(brachiopods.new.records.gridded, "outputs/tabular_data/brachiopods_new_records_gridded.csv", row.names = FALSE)
# write.csv(brachiopods.reported.records.gridded, "outputs/tabular_data/brachiopods_reported_records_gridded.csv", row.names = FALSE)


# Bryozoa

bryozoa <- summary %>% filter(phylum == 'Bryozoa')
bryozoa.records <- animals %>% filter(phylum == 'Bryozoa')

# Subset historic, confirmed and new records

bryozoa.new <- bryozoa %>% filter(str_detect(reportingStatus, "new"))
bryozoa.confirmed <- bryozoa %>% filter(reportingStatus == "confirmed")
bryozoa.reported <- bryozoa %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

bryozoa.new.taxa <- unique(bryozoa.new$scientificName)
bryozoa.new.taxa <- bryozoa.new.taxa %>% paste(collapse = "|")

bryozoa.confirmed.taxa <- unique(bryozoa.confirmed$scientificName)
bryozoa.confirmed.taxa <- bryozoa.confirmed.taxa %>% paste(collapse = "|")

bryozoa.reported.taxa <- unique(bryozoa.reported$scientificName)
bryozoa.reported.taxa <- bryozoa.reported.taxa %>% paste(collapse = "|")

bryozoa.new.taxa.records <- bryozoa.records %>% filter(str_detect(scientificName, bryozoa.new.taxa))

bryozoa.confirmed.taxa.records <- bryozoa.records %>% filter(str_detect(scientificName, bryozoa.confirmed.taxa))

bryozoa.reported.taxa.records <- bryozoa.records %>% filter(str_detect(scientificName, bryozoa.reported.taxa))

bryozoa.new.taxa.records <- bryozoa.new.taxa.records %>% drop_na(decimalLatitude)
bryozoa.confirmed.taxa.records <- bryozoa.confirmed.taxa.records %>% drop_na(decimalLatitude)
bryozoa.reported.taxa.records <- bryozoa.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

bryozoa.new.taxa.points <- st_as_sf(bryozoa.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

bryozoa.new.taxa.points <- st_transform(bryozoa.new.taxa.points, crs = st_crs(EPSG.32610))

bryozoa.new.records.gridded <- bryozoa.new.taxa.points %>% 
  st_join(grid)

bryozoa.new.records.gridded <- as.data.frame(bryozoa.new.records.gridded)

bryozoa.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

bryozoa.new.taxa.points.sum <- st_transform(bryozoa.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

bryozoa.new.grid <- grid %>%
  st_join(bryozoa.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

bryozoa.new.grid = filter(bryozoa.new.grid, richness > 0)

bryozoa.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

bryozoa.reported.taxa.points <- st_as_sf(bryozoa.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

bryozoa.reported.taxa.points <- st_transform(bryozoa.reported.taxa.points, crs = st_crs(EPSG.32610))

bryozoa.reported.records.gridded <- bryozoa.reported.taxa.points %>% 
  st_join(grid)

bryozoa.reported.records.gridded <- as.data.frame(bryozoa.reported.records.gridded)

bryozoa.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

bryozoa.reported.taxa.points.sum <- st_transform(bryozoa.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

bryozoa.reported.grid <- grid %>%
  st_join(bryozoa.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

bryozoa.reported.grid = filter(bryozoa.reported.grid, richness > 0)

bryozoa.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

bryozoa.confirmed.taxa.points <- st_as_sf(bryozoa.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

bryozoa.confirmed.taxa.points <- st_transform(bryozoa.confirmed.taxa.points, crs = st_crs(EPSG.32610))

bryozoa.confirmed.records.gridded <- bryozoa.confirmed.taxa.points %>% 
  st_join(grid)

bryozoa.confirmed.records.gridded <- as.data.frame(bryozoa.confirmed.records.gridded)

bryozoa.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

bryozoa.confirmed.taxa.points.sum <- st_transform(bryozoa.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

bryozoa.confirmed.grid <- grid %>%
  st_join(bryozoa.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

bryozoa.confirmed.grid = filter(bryozoa.confirmed.grid, richness > 0)

bryozoa.confirmed.grid$status <- 'confirmed'

plot(bryozoa.reported.grid)

# Write choropleths

# st_write(bryozoa.confirmed.grid, "outputs/vectors/bryozoa_confirmed_grid.shp")
# st_write(bryozoa.new.grid, "outputs/vectors/bryozoa_new_grid.shp")
# st_write(bryozoa.reported.grid, "outputs/vectors/bryozoa_reported_grid.shp")

# Write gridded dataframes

# write.csv(bryozoa.confirmed.records.gridded, "outputs/tabular_data/bryozoa_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(bryozoa.new.records.gridded, "outputs/tabular_data/bryozoa_new_records_gridded.csv", row.names = FALSE)
# write.csv(bryozoa.reported.records.gridded, "outputs/tabular_data/bryozoa_reported_records_gridded.csv", row.names = FALSE)


# Phoronida

horseshoe.worms <- summary %>% filter(phylum == 'Phoronida')
horseshoe.worms.records <- animals %>% filter(phylum == 'Phoronida')

# Subset historic, confirmed and new records

horseshoe.worms.new <- horseshoe.worms %>% filter(str_detect(reportingStatus, "new"))
horseshoe.worms.confirmed <- horseshoe.worms %>% filter(reportingStatus == "confirmed")
horseshoe.worms.reported <- horseshoe.worms %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

horseshoe.worms.new.taxa <- unique(horseshoe.worms.new$scientificName)
horseshoe.worms.new.taxa <- horseshoe.worms.new.taxa %>% paste(collapse = "|")

horseshoe.worms.confirmed.taxa <- unique(horseshoe.worms.confirmed$scientificName)
horseshoe.worms.confirmed.taxa <- horseshoe.worms.confirmed.taxa %>% paste(collapse = "|")

horseshoe.worms.reported.taxa <- unique(horseshoe.worms.reported$scientificName)
horseshoe.worms.reported.taxa <- horseshoe.worms.reported.taxa %>% paste(collapse = "|")

horseshoe.worms.new.taxa.records <- horseshoe.worms.records %>% filter(str_detect(scientificName, horseshoe.worms.new.taxa))

horseshoe.worms.confirmed.taxa.records <- horseshoe.worms.records %>% filter(str_detect(scientificName, horseshoe.worms.confirmed.taxa))

horseshoe.worms.reported.taxa.records <- horseshoe.worms.records %>% filter(str_detect(scientificName, horseshoe.worms.reported.taxa))

horseshoe.worms.new.taxa.records <- horseshoe.worms.new.taxa.records %>% drop_na(decimalLatitude)
horseshoe.worms.confirmed.taxa.records <- horseshoe.worms.confirmed.taxa.records %>% drop_na(decimalLatitude)
horseshoe.worms.reported.taxa.records <- horseshoe.worms.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

horseshoe.worms.new.taxa.points <- st_as_sf(horseshoe.worms.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

horseshoe.worms.new.taxa.points <- st_transform(horseshoe.worms.new.taxa.points, crs = st_crs(EPSG.32610))

horseshoe.worms.new.records.gridded <- horseshoe.worms.new.taxa.points %>% 
  st_join(grid)

horseshoe.worms.new.records.gridded <- as.data.frame(horseshoe.worms.new.records.gridded)

horseshoe.worms.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

horseshoe.worms.new.taxa.points.sum <- st_transform(horseshoe.worms.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

horseshoe.worms.new.grid <- grid %>%
  st_join(horseshoe.worms.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

horseshoe.worms.new.grid = filter(horseshoe.worms.new.grid, richness > 0)

horseshoe.worms.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

horseshoe.worms.reported.taxa.points <- st_as_sf(horseshoe.worms.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

horseshoe.worms.reported.taxa.points <- st_transform(horseshoe.worms.reported.taxa.points, crs = st_crs(EPSG.32610))

horseshoe.worms.reported.records.gridded <- horseshoe.worms.reported.taxa.points %>% 
  st_join(grid)

horseshoe.worms.reported.records.gridded <- as.data.frame(horseshoe.worms.reported.records.gridded)

horseshoe.worms.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

horseshoe.worms.reported.taxa.points.sum <- st_transform(horseshoe.worms.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

horseshoe.worms.reported.grid <- grid %>%
  st_join(horseshoe.worms.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

horseshoe.worms.reported.grid = filter(horseshoe.worms.reported.grid, richness > 0)

horseshoe.worms.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

horseshoe.worms.confirmed.taxa.points <- st_as_sf(horseshoe.worms.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

horseshoe.worms.confirmed.taxa.points <- st_transform(horseshoe.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610))

horseshoe.worms.confirmed.records.gridded <- horseshoe.worms.confirmed.taxa.points %>% 
  st_join(grid)

horseshoe.worms.confirmed.records.gridded <- as.data.frame(horseshoe.worms.confirmed.records.gridded)

horseshoe.worms.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

horseshoe.worms.confirmed.taxa.points.sum <- st_transform(horseshoe.worms.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

horseshoe.worms.confirmed.grid <- grid %>%
  st_join(horseshoe.worms.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

horseshoe.worms.confirmed.grid = filter(horseshoe.worms.confirmed.grid, richness > 0)

horseshoe.worms.confirmed.grid$status <- 'confirmed'

plot(horseshoe.worms.reported.grid)

# Write choropleths

# st_write(horseshoe.worms.confirmed.grid, "outputs/vectors/horseshoe_worms_confirmed_grid.shp")
# st_write(horseshoe.worms.new.grid, "outputs/vectors/horseshoe_worms_new_grid.shp")
# st_write(horseshoe.worms.reported.grid, "outputs/vectors/horseshoe_worms_reported_grid.shp")

# Write gridded dataframes

# write.csv(horseshoe.worms.confirmed.records.gridded, "outputs/tabular_data/horseshoe_worms_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(horseshoe.worms.new.records.gridded, "outputs/tabular_data/horseshoe_worms_new_records_gridded.csv", row.names = FALSE)
# write.csv(horseshoe.worms.reported.records.gridded, "outputs/tabular_data/horseshoe_worms_reported_records_gridded.csv", row.names = FALSE)


# Echinodermata

echinoderms <- summary %>% filter(phylum == 'Echinodermata')
echinoderms.records <- animals %>% filter(phylum == 'Echinodermata')

# Subset historic, confirmed and new records

echinoderms.new <- echinoderms %>% filter(str_detect(reportingStatus, "new"))
echinoderms.confirmed <- echinoderms %>% filter(reportingStatus == "confirmed")
echinoderms.reported <- echinoderms %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

echinoderms.new.taxa <- unique(echinoderms.new$scientificName)
echinoderms.new.taxa <- echinoderms.new.taxa %>% paste(collapse = "|")

echinoderms.confirmed.taxa <- unique(echinoderms.confirmed$scientificName)
echinoderms.confirmed.taxa <- echinoderms.confirmed.taxa %>% paste(collapse = "|")

echinoderms.reported.taxa <- unique(echinoderms.reported$scientificName)
echinoderms.reported.taxa <- echinoderms.reported.taxa %>% paste(collapse = "|")

echinoderms.new.taxa.records <- echinoderms.records %>% filter(str_detect(scientificName, echinoderms.new.taxa))

echinoderms.confirmed.taxa.records <- echinoderms.records %>% filter(str_detect(scientificName, echinoderms.confirmed.taxa))

echinoderms.reported.taxa.records <- echinoderms.records %>% filter(str_detect(scientificName, echinoderms.reported.taxa))

echinoderms.new.taxa.records <- echinoderms.new.taxa.records %>% drop_na(decimalLatitude)
echinoderms.confirmed.taxa.records <- echinoderms.confirmed.taxa.records %>% drop_na(decimalLatitude)
echinoderms.reported.taxa.records <- echinoderms.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

echinoderms.new.taxa.points <- st_as_sf(echinoderms.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

echinoderms.new.taxa.points <- st_transform(echinoderms.new.taxa.points, crs = st_crs(EPSG.32610))

echinoderms.new.records.gridded <- echinoderms.new.taxa.points %>% 
  st_join(grid)

echinoderms.new.records.gridded <- as.data.frame(echinoderms.new.records.gridded)

echinoderms.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

echinoderms.new.taxa.points.sum <- st_transform(echinoderms.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

echinoderms.new.grid <- grid %>%
  st_join(echinoderms.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

echinoderms.new.grid = filter(echinoderms.new.grid, richness > 0)

echinoderms.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

echinoderms.reported.taxa.points <- st_as_sf(echinoderms.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

echinoderms.reported.taxa.points <- st_transform(echinoderms.reported.taxa.points, crs = st_crs(EPSG.32610))

echinoderms.reported.records.gridded <- echinoderms.reported.taxa.points %>% 
  st_join(grid)

echinoderms.reported.records.gridded <- as.data.frame(echinoderms.reported.records.gridded)

echinoderms.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

echinoderms.reported.taxa.points.sum <- st_transform(echinoderms.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

echinoderms.reported.grid <- grid %>%
  st_join(echinoderms.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

echinoderms.reported.grid = filter(echinoderms.reported.grid, richness > 0)

echinoderms.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

echinoderms.confirmed.taxa.points <- st_as_sf(echinoderms.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

echinoderms.confirmed.taxa.points <- st_transform(echinoderms.confirmed.taxa.points, crs = st_crs(EPSG.32610))

echinoderms.confirmed.records.gridded <- echinoderms.confirmed.taxa.points %>% 
  st_join(grid)

echinoderms.confirmed.records.gridded <- as.data.frame(echinoderms.confirmed.records.gridded)

echinoderms.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

echinoderms.confirmed.taxa.points.sum <- st_transform(echinoderms.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

echinoderms.confirmed.grid <- grid %>%
  st_join(echinoderms.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

echinoderms.confirmed.grid = filter(echinoderms.confirmed.grid, richness > 0)

echinoderms.confirmed.grid$status <- 'confirmed'

plot(echinoderms.reported.grid)

# Write choropleths

# st_write(echinoderms.confirmed.grid, "outputs/vectors/echinoderms_confirmed_grid.shp")
# st_write(echinoderms.new.grid, "outputs/vectors/echinoderms_new_grid.shp")
# st_write(echinoderms.reported.grid, "outputs/vectors/echinoderms_reported_grid.shp")

# Write gridded dataframes

# write.csv(echinoderms.confirmed.records.gridded, "outputs/tabular_data/echinoderms_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(echinoderms.new.records.gridded, "outputs/tabular_data/echinoderms_new_records_gridded.csv", row.names = FALSE)
# write.csv(echinoderms.reported.records.gridded, "outputs/tabular_data/echinoderms_reported_records_gridded.csv", row.names = FALSE)


# Tunicata

tunicates <- summary %>% filter(subphylum == 'Tunicata')
tunicates.records <- animals %>% filter(subphylum == 'Tunicata')

# Subset historic, confirmed and new records

tunicates.new <- tunicates %>% filter(str_detect(reportingStatus, "new"))
tunicates.confirmed <- tunicates %>% filter(reportingStatus == "confirmed")
tunicates.reported <- tunicates %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

tunicates.new.taxa <- unique(tunicates.new$scientificName)
tunicates.new.taxa <- tunicates.new.taxa %>% paste(collapse = "|")

tunicates.confirmed.taxa <- unique(tunicates.confirmed$scientificName)
tunicates.confirmed.taxa <- tunicates.confirmed.taxa %>% paste(collapse = "|")

tunicates.reported.taxa <- unique(tunicates.reported$scientificName)
tunicates.reported.taxa <- tunicates.reported.taxa %>% paste(collapse = "|")

tunicates.new.taxa.records <- tunicates.records %>% filter(str_detect(scientificName, tunicates.new.taxa))

tunicates.confirmed.taxa.records <- tunicates.records %>% filter(str_detect(scientificName, tunicates.confirmed.taxa))

tunicates.reported.taxa.records <- tunicates.records %>% filter(str_detect(scientificName, tunicates.reported.taxa))

tunicates.new.taxa.records <- tunicates.new.taxa.records %>% drop_na(decimalLatitude)
tunicates.confirmed.taxa.records <- tunicates.confirmed.taxa.records %>% drop_na(decimalLatitude)
tunicates.reported.taxa.records <- tunicates.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

tunicates.new.taxa.points <- st_as_sf(tunicates.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

tunicates.new.taxa.points <- st_transform(tunicates.new.taxa.points, crs = st_crs(EPSG.32610))

tunicates.new.records.gridded <- tunicates.new.taxa.points %>% 
  st_join(grid)

tunicates.new.records.gridded <- as.data.frame(tunicates.new.records.gridded)

tunicates.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

tunicates.new.taxa.points.sum <- st_transform(tunicates.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

tunicates.new.grid <- grid %>%
  st_join(tunicates.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

tunicates.new.grid = filter(tunicates.new.grid, richness > 0)

tunicates.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

tunicates.reported.taxa.points <- st_as_sf(tunicates.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

tunicates.reported.taxa.points <- st_transform(tunicates.reported.taxa.points, crs = st_crs(EPSG.32610))

tunicates.reported.records.gridded <- tunicates.reported.taxa.points %>% 
  st_join(grid)

tunicates.reported.records.gridded <- as.data.frame(tunicates.reported.records.gridded)

tunicates.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

tunicates.reported.taxa.points.sum <- st_transform(tunicates.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

tunicates.reported.grid <- grid %>%
  st_join(tunicates.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

tunicates.reported.grid = filter(tunicates.reported.grid, richness > 0)

tunicates.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

tunicates.confirmed.taxa.points <- st_as_sf(tunicates.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

tunicates.confirmed.taxa.points <- st_transform(tunicates.confirmed.taxa.points, crs = st_crs(EPSG.32610))

tunicates.confirmed.records.gridded <- tunicates.confirmed.taxa.points %>% 
  st_join(grid)

tunicates.confirmed.records.gridded <- as.data.frame(tunicates.confirmed.records.gridded)

tunicates.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

tunicates.confirmed.taxa.points.sum <- st_transform(tunicates.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

tunicates.confirmed.grid <- grid %>%
  st_join(tunicates.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

tunicates.confirmed.grid = filter(tunicates.confirmed.grid, richness > 0)

tunicates.confirmed.grid$status <- 'confirmed'

plot(tunicates.reported.grid)

# Write choropleths

# st_write(tunicates.confirmed.grid, "outputs/vectors/tunicates_confirmed_grid.shp")
# st_write(tunicates.new.grid, "outputs/vectors/tunicates_new_grid.shp")
# st_write(tunicates.reported.grid, "outputs/vectors/tunicates_reported_grid.shp")

# Write gridded dataframes

# write.csv(tunicates.confirmed.records.gridded, "outputs/tabular_data/tunicates_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(tunicates.new.records.gridded, "outputs/tabular_data/tunicates_new_records_gridded.csv", row.names = FALSE)
# write.csv(tunicates.reported.records.gridded, "outputs/tabular_data/tunicates_reported_records_gridded.csv", row.names = FALSE)


# Fishes

unique(summary$class)

fishes <- summary %>% filter(class == 'Actinopterygii' | class == 'Elasmobranchii')
fishes.records <- animals %>% filter(class == 'Actinopterygii' | class == 'Elasmobranchii')

# Subset historic, confirmed and new records

fishes.new <- fishes %>% filter(str_detect(reportingStatus, "new"))
fishes.confirmed <- fishes %>% filter(reportingStatus == "confirmed")
fishes.reported <- fishes %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

fishes.new.taxa <- unique(fishes.new$scientificName)
fishes.new.taxa <- fishes.new.taxa %>% paste(collapse = "|")

fishes.confirmed.taxa <- unique(fishes.confirmed$scientificName)
fishes.confirmed.taxa <- fishes.confirmed.taxa %>% paste(collapse = "|")

fishes.reported.taxa <- unique(fishes.reported$scientificName)
fishes.reported.taxa <- fishes.reported.taxa %>% paste(collapse = "|")

fishes.new.taxa.records <- fishes.records %>% filter(str_detect(scientificName, fishes.new.taxa))

fishes.confirmed.taxa.records <- fishes.records %>% filter(str_detect(scientificName, fishes.confirmed.taxa))

fishes.reported.taxa.records <- fishes.records %>% filter(str_detect(scientificName, fishes.reported.taxa))

fishes.new.taxa.records <- fishes.new.taxa.records %>% drop_na(decimalLatitude)
fishes.confirmed.taxa.records <- fishes.confirmed.taxa.records %>% drop_na(decimalLatitude)
fishes.reported.taxa.records <- fishes.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

fishes.new.taxa.points <- st_as_sf(fishes.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

fishes.new.taxa.points <- st_transform(fishes.new.taxa.points, crs = st_crs(EPSG.32610))

fishes.new.records.gridded <- fishes.new.taxa.points %>% 
  st_join(grid)

fishes.new.records.gridded <- as.data.frame(fishes.new.records.gridded)

fishes.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

fishes.new.taxa.points.sum <- st_transform(fishes.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

fishes.new.grid <- grid %>%
  st_join(fishes.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

fishes.new.grid = filter(fishes.new.grid, richness > 0)

fishes.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

fishes.reported.taxa.points <- st_as_sf(fishes.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

fishes.reported.taxa.points <- st_transform(fishes.reported.taxa.points, crs = st_crs(EPSG.32610))

fishes.reported.records.gridded <- fishes.reported.taxa.points %>% 
  st_join(grid)

fishes.reported.records.gridded <- as.data.frame(fishes.reported.records.gridded)

fishes.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

fishes.reported.taxa.points.sum <- st_transform(fishes.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

fishes.reported.grid <- grid %>%
  st_join(fishes.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

fishes.reported.grid = filter(fishes.reported.grid, richness > 0)

fishes.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

fishes.confirmed.taxa.points <- st_as_sf(fishes.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

fishes.confirmed.taxa.points <- st_transform(fishes.confirmed.taxa.points, crs = st_crs(EPSG.32610))

fishes.confirmed.records.gridded <- fishes.confirmed.taxa.points %>% 
  st_join(grid)

fishes.confirmed.records.gridded <- as.data.frame(fishes.confirmed.records.gridded)

fishes.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

fishes.confirmed.taxa.points.sum <- st_transform(fishes.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

fishes.confirmed.grid <- grid %>%
  st_join(fishes.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

fishes.confirmed.grid = filter(fishes.confirmed.grid, richness > 0)

fishes.confirmed.grid$status <- 'confirmed'

plot(fishes.reported.grid)

# Write choropleths

# st_write(fishes.confirmed.grid, "outputs/vectors/fishes_confirmed_grid.shp")
# st_write(fishes.new.grid, "outputs/vectors/fishes_new_grid.shp")
# st_write(fishes.reported.grid, "outputs/vectors/fishes_reported_grid.shp")

# Write gridded dataframes

# write.csv(fishes.confirmed.records.gridded, "outputs/tabular_data/fishes_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(fishes.new.records.gridded, "outputs/tabular_data/fishes_new_records_gridded.csv", row.names = FALSE)
# write.csv(fishes.reported.records.gridded, "outputs/tabular_data/fishes_reported_records_gridded.csv", row.names = FALSE)

# Mammalia

mammals <- summary %>% filter(class == 'Mammalia')
mammals.records <- animals %>% filter(class == 'Mammalia')

# Subset historic, confirmed and new records

mammals.new <- mammals %>% filter(str_detect(reportingStatus, "new"))
mammals.confirmed <- mammals %>% filter(reportingStatus == "confirmed")
mammals.reported <- mammals %>% filter(reportingStatus == "reported")

# Create vectors of historic, confirmed and new taxa to query catalog of occurrence records

mammals.new.taxa <- unique(mammals.new$scientificName)
mammals.new.taxa <- mammals.new.taxa %>% paste(collapse = "|")

mammals.confirmed.taxa <- unique(mammals.confirmed$scientificName)
mammals.confirmed.taxa <- mammals.confirmed.taxa %>% paste(collapse = "|")

mammals.reported.taxa <- unique(mammals.reported$scientificName)
mammals.reported.taxa <- mammals.reported.taxa %>% paste(collapse = "|")

mammals.new.taxa.records <- mammals.records %>% filter(str_detect(scientificName, mammals.new.taxa))

mammals.confirmed.taxa.records <- mammals.records %>% filter(str_detect(scientificName, mammals.confirmed.taxa))

mammals.reported.taxa.records <- mammals.records %>% filter(str_detect(scientificName, mammals.reported.taxa))

mammals.new.taxa.records <- mammals.new.taxa.records %>% drop_na(decimalLatitude)
mammals.confirmed.taxa.records <- mammals.confirmed.taxa.records %>% drop_na(decimalLatitude)
mammals.reported.taxa.records <- mammals.reported.taxa.records %>% drop_na(decimalLatitude)

# Prepare gridded choropleths of historic, confirmed, new records

# New records

# Convert records to sf points

mammals.new.taxa.points <- st_as_sf(mammals.new.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# # Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

mammals.new.taxa.points <- st_transform(mammals.new.taxa.points, crs = st_crs(EPSG.32610))

mammals.new.records.gridded <- mammals.new.taxa.points %>% 
  st_join(grid)

mammals.new.records.gridded <- as.data.frame(mammals.new.records.gridded)

mammals.new.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

mammals.new.taxa.points.sum <- st_transform(mammals.new.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

mammals.new.grid <- grid %>%
  st_join(mammals.new.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

mammals.new.grid = filter(mammals.new.grid, richness > 0)

mammals.new.grid$status <- 'new'

# Historic records

# Convert records to sf points

mammals.reported.taxa.points <- st_as_sf(mammals.reported.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

mammals.reported.taxa.points <- st_transform(mammals.reported.taxa.points, crs = st_crs(EPSG.32610))

mammals.reported.records.gridded <- mammals.reported.taxa.points %>% 
  st_join(grid)

mammals.reported.records.gridded <- as.data.frame(mammals.reported.records.gridded)

mammals.reported.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

mammals.reported.taxa.points.sum <- st_transform(mammals.reported.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

mammals.reported.grid <- grid %>%
  st_join(mammals.reported.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

mammals.reported.grid = filter(mammals.reported.grid, richness > 0)

mammals.reported.grid$status <- 'historic'

# Confirmed records

# Convert records to sf points

mammals.confirmed.taxa.points <- st_as_sf(mammals.confirmed.taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Reproject to NAD83 UTM Zone 10

# Generate gridded dataframes (each record assigned cell_id)

mammals.confirmed.taxa.points <- st_transform(mammals.confirmed.taxa.points, crs = st_crs(EPSG.32610))

mammals.confirmed.records.gridded <- mammals.confirmed.taxa.points %>% 
  st_join(grid)

mammals.confirmed.records.gridded <- as.data.frame(mammals.confirmed.records.gridded)

mammals.confirmed.records.gridded$geometry <- NULL

# Summarized points (for choropleths)

mammals.confirmed.taxa.points.sum <- st_transform(mammals.confirmed.taxa.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

mammals.confirmed.grid <- grid %>%
  st_join(mammals.confirmed.taxa.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

mammals.confirmed.grid = filter(mammals.confirmed.grid, richness > 0)

mammals.confirmed.grid$status <- 'confirmed'

plot(mammals.reported.grid)

# Write choropleths

# st_write(mammals.confirmed.grid, "outputs/vectors/mammals_confirmed_grid.shp")
# st_write(mammals.new.grid, "outputs/vectors/mammals_new_grid.shp")
# st_write(mammals.reported.grid, "outputs/vectors/mammals_reported_grid.shp")

# Write gridded dataframes

# write.csv(mammals.confirmed.records.gridded, "outputs/tabular_data/mammals_confirmed_records_gridded.csv", row.names = FALSE)
# write.csv(mammals.new.records.gridded, "outputs/tabular_data/mammals_new_records_gridded.csv", row.names = FALSE)
# write.csv(mammals.reported.records.gridded, "outputs/tabular_data/mammals_reported_records_gridded.csv", row.names = FALSE)