# Script to prepare gridded maps of Galiano Island's marine animal diversity

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


# Load data (note: for marine algae, including marine algae so that grid encompasses all marine data)

algae <- read.csv("../../../../consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2023-04-22.csv")
animals <- read.csv("../../../../consolidate_records/Animalia/marine_animals/synthesized/Galiano_marine_animal_records_consolidated_2023-04-22.csv")


# Filter data by group

sponges <- animals %>% filter(phylum == 'Porifera')

cnidarians <- animals %>% filter(phylum == 'Cnidaria')

ctenophores <- animals %>% filter(phylum == 'Ctenophora')

nemerteans <- animals %>% filter(phylum == 'Nemertea')

platyhelminthes <- animals %>% filter(phylum == 'Platyhelminthes')

arrow.worms <- animals %>% filter(phylum == 'Chaetognatha')

molluscs <- animals %>% filter(phylum == 'Mollusca')

annelids <- animals %>% filter(phylum == 'Annelida')

peanut.worms <- animals %>% filter(phylum == 'Sipuncula')

crustaceans <- animals %>% filter(subphylum == 'Crustacea')

nodding.heads <- animals %>% filter(phylum == 'Entoprocta')

brachiopods <- animals %>% filter(phylum == 'Brachiopoda')

bryozoans <- animals %>% filter(phylum == 'Bryozoa')

horseshoe.worms <- animals %>% filter(phylum == 'Phoronida')

echinoderms <- animals %>% filter(phylum == 'Echinodermata')

tunicates <- animals %>% filter(subphylum == 'Tunicata')

fishes <- animals %>% filter(class == 'Actinopterygii'| class == 'Elasmobranchii')

mammals <- animals %>% filter(class == 'Mammalia')


# Filter out data missing coordinates

algae <- algae %>% drop_na(decimalLongitude)

animals <- animals %>% drop_na(decimalLongitude)

sponges <- sponges %>% drop_na(decimalLongitude)

cnidarians <- cnidarians %>% drop_na(decimalLongitude)

ctenophores <- ctenophores %>% drop_na(decimalLongitude)

nemerteans <- nemerteans %>% drop_na(decimalLongitude)

platyhelminthes <- platyhelminthes %>% drop_na(decimalLongitude)

arrow.worms <- arrow.worms %>% drop_na(decimalLongitude)

molluscs <- molluscs %>% drop_na(decimalLongitude)

annelids <- annelids %>% drop_na(decimalLongitude)

peanut.worms <- peanut.worms %>% drop_na(decimalLongitude)

crustaceans <- crustaceans %>% drop_na(decimalLongitude)

nodding.heads <- nodding.heads %>% drop_na(decimalLongitude)

brachiopods <- brachiopods %>% drop_na(decimalLongitude)

bryozoans <- bryozoans %>% drop_na(decimalLongitude)

horseshoe.worms <- horseshoe.worms %>% drop_na(decimalLongitude)

echinoderms <- echinoderms %>% drop_na(decimalLongitude)

tunicates <- tunicates %>% drop_na(decimalLongitude)

fishes <- fishes %>% drop_na(decimalLongitude)

mammals <- mammals %>% drop_na(decimalLongitude)



# Create CRS object

EPSG.4326 <- st_crs(4326)


# Convert records to sf points

algae.points <- st_as_sf(algae, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

animals.points <- st_as_sf(animals, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

sponges.points <- st_as_sf(sponges, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

animals.points <- st_as_sf(sponges, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

cnidarians.points <- st_as_sf(cnidarians, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

ctenophores.points <- st_as_sf(ctenophores, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

nemerteans.points <- st_as_sf(nemerteans, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

platyhelminthes.points <- st_as_sf(platyhelminthes, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

arrow.worms.points <- st_as_sf(arrow.worms, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

molluscs.points <- st_as_sf(molluscs, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

annelids.points <- st_as_sf(annelids, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

peanut.worms.points <- st_as_sf(peanut.worms, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

crustaceans.points <- st_as_sf(crustaceans, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

nodding.heads.points <- st_as_sf(nodding.heads, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

brachiopods.points <- st_as_sf(brachiopods, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

bryozoans.points <- st_as_sf(bryozoans, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

horseshoe.worms.points <- st_as_sf(horseshoe.worms, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

echinoderms.points <- st_as_sf(echinoderms, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

tunicates.points <- st_as_sf(tunicates, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

fishes.points <- st_as_sf(fishes, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

mammals.points <- st_as_sf(mammals, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)


# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)


# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

algae.points <- st_transform(algae.points, crs = st_crs(EPSG.32610))

animals.points <- st_transform(animals.points, crs = st_crs(EPSG.32610))

sponges.points <- st_transform(sponges.points, crs = st_crs(EPSG.32610))

cnidarians.points <- st_transform(cnidarians.points, crs = st_crs(EPSG.32610))

ctenophores.points <- st_transform(ctenophores.points, crs = st_crs(EPSG.32610))

nemerteans.points <- st_transform(nemerteans.points, crs = st_crs(EPSG.32610))

platyhelminthes.points <- st_transform(platyhelminthes.points, crs = st_crs(EPSG.32610))

arrow.worms.points <- st_transform(arrow.worms.points, crs = st_crs(EPSG.32610))

molluscs.points <- st_transform(molluscs.points, crs = st_crs(EPSG.32610))

annelids.points <- st_transform(annelids.points, crs = st_crs(EPSG.32610))

peanut.worms.points <- st_transform(peanut.worms.points, crs = st_crs(EPSG.32610))

crustaceans.points <- st_transform(crustaceans.points, crs = st_crs(EPSG.32610))

nodding.heads.points <- st_transform(nodding.heads.points, crs = st_crs(EPSG.32610))

brachiopods.points <- st_transform(brachiopods.points, crs = st_crs(EPSG.32610))

bryozoans.points <- st_transform(bryozoans.points, crs = st_crs(EPSG.32610))

horseshoe.worms.points <- st_transform(horseshoe.worms.points, crs = st_crs(EPSG.32610))

echinoderms.points <- st_transform(echinoderms.points, crs = st_crs(EPSG.32610))

tunicates.points <- st_transform(tunicates.points, crs = st_crs(EPSG.32610))

fishes.points <- st_transform(fishes.points, crs = st_crs(EPSG.32610))

mammals.points <- st_transform(mammals.points, crs = st_crs(EPSG.32610))


# Summarized points (for choropleths)

algae.points.sum <- st_transform(algae.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

animals.points.sum <- st_transform(animals.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

sponges.points.sum <- st_transform(sponges.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

cnidarians.points.sum <- st_transform(cnidarians.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

ctenophores.points.sum <- st_transform(ctenophores.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

nemerteans.points.sum <- st_transform(nemerteans.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

platyhelminthes.points.sum <- st_transform(platyhelminthes.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

arrow.worms.points.sum <- st_transform(arrow.worms.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

molluscs.points.sum <- st_transform(molluscs.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

annelids.points.sum <- st_transform(annelids.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

peanut.worms.points.sum <- st_transform(peanut.worms.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

crustaceans.points.sum <- st_transform(crustaceans.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

nodding.heads.points.sum <- st_transform(nodding.heads.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

brachiopods.points.sum <- st_transform(brachiopods.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

bryozoans.points.sum <- st_transform(bryozoans.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

horseshoe.worms.points.sum <- st_transform(horseshoe.worms.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

echinoderms.points.sum <- st_transform(echinoderms.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

tunicates.points.sum <- st_transform(tunicates.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

fishes.points.sum <- st_transform(fishes.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

mammals.points.sum <- st_transform(mammals.points, crs = st_crs(EPSG.32610)) %>%
  group_by(scientificName) %>%
  summarize()

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


# Generate gridded dataframes (each record assigned cell_id)

# All marine animals

animal.records.gridded <- animals.points %>% 
  st_join(grid)

animal.records.gridded <- as.data.frame(animal.records.gridded)

animal.records.gridded$geometry <- NULL

# Sponges

sponges.records.gridded <- sponges.points %>% 
  st_join(grid)

sponges.records.gridded <- as.data.frame(sponges.records.gridded)

sponges.records.gridded$geometry <- NULL

# Cnidarians

cnidarians.records.gridded <- cnidarians.points %>% 
  st_join(grid)

cnidarians.records.gridded <- as.data.frame(cnidarians.records.gridded)

cnidarians.records.gridded$geometry <- NULL

# Ctenophores

ctenophores.records.gridded <- ctenophores.points %>% 
  st_join(grid)

ctenophores.records.gridded <- as.data.frame(ctenophores.records.gridded)

ctenophores.records.gridded$geometry <- NULL

# Nemerteans

nemerteans.records.gridded <- nemerteans.points %>% 
  st_join(grid)

nemerteans.records.gridded <- as.data.frame(nemerteans.records.gridded)

nemerteans.records.gridded$geometry <- NULL

# Platyhelminthes

platyhelminthes.records.gridded <- platyhelminthes.points %>% 
  st_join(grid)

platyhelminthes.records.gridded <- as.data.frame(platyhelminthes.records.gridded)

platyhelminthes.records.gridded$geometry <- NULL

# Arrow worms

arrow.worms.records.gridded <- arrow.worms.points %>% 
  st_join(grid)

arrow.worms.records.gridded <- as.data.frame(arrow.worms.records.gridded)

arrow.worms.records.gridded$geometry <- NULL

# Molluscs

molluscs.records.gridded <- molluscs.points %>% 
  st_join(grid)

molluscs.records.gridded <- as.data.frame(molluscs.records.gridded)

molluscs.records.gridded$geometry <- NULL

# Annelids

annelids.records.gridded <- annelids.points %>% 
  st_join(grid)

annelids.records.gridded <- as.data.frame(annelids.records.gridded)

annelids.records.gridded$geometry <- NULL

# Peanut worms

peanut.worms.records.gridded <- peanut.worms.points %>% 
  st_join(grid)

peanut.worms.records.gridded <- as.data.frame(peanut.worms.records.gridded)

peanut.worms.records.gridded$geometry <- NULL

# Crustaceans

crustaceans.records.gridded <- crustaceans.points %>% 
  st_join(grid)

crustaceans.records.gridded <- as.data.frame(crustaceans.records.gridded)

crustaceans.records.gridded$geometry <- NULL

# Nodding heads

nodding.heads.records.gridded <- nodding.heads.points %>% 
  st_join(grid)

nodding.heads.records.gridded <- as.data.frame(nodding.heads.records.gridded)

nodding.heads.records.gridded$geometry <- NULL

# Brachiopods

brachiopods.records.gridded <- brachiopods.points %>% 
  st_join(grid)

brachiopods.records.gridded <- as.data.frame(brachiopods.records.gridded)

brachiopods.records.gridded$geometry <- NULL

# Bryozoans

bryozoans.records.gridded <- bryozoans.points %>% 
  st_join(grid)

bryozoans.records.gridded <- as.data.frame(bryozoans.records.gridded)

bryozoans.records.gridded$geometry <- NULL

# Horseshoe worms

horseshoe.worms.records.gridded <- horseshoe.worms.points %>% 
  st_join(grid)

horseshoe.worms.records.gridded <- as.data.frame(horseshoe.worms.records.gridded)

horseshoe.worms.records.gridded$geometry <- NULL

# Echinoderms

echinoderms.records.gridded <- echinoderms.points %>% 
  st_join(grid)

echinoderms.records.gridded <- as.data.frame(echinoderms.records.gridded)

echinoderms.records.gridded$geometry <- NULL

# Tunicates

tunicates.records.gridded <- tunicates.points %>% 
  st_join(grid)

tunicates.records.gridded <- as.data.frame(tunicates.records.gridded)

tunicates.records.gridded$geometry <- NULL

# Fishes

fishes.records.gridded <- fishes.points %>% 
  st_join(grid)

fishes.records.gridded <- as.data.frame(fishes.records.gridded)

fishes.records.gridded$geometry <- NULL

# Mammals

mammals.records.gridded <- mammals.points %>% 
  st_join(grid)

mammals.records.gridded <- as.data.frame(mammals.records.gridded)

mammals.records.gridded$geometry <- NULL


# Generate species richness choropleths
# Cribbed from https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# and https://luisdva.github.io/rstats/richness/

# All marine animals

animal.grid <- grid %>%
  st_join(animals.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

animal.grid.count = filter(animal.grid, richness > 0)

# Sponges

sponges.grid <- grid %>%
  st_join(sponges.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

sponges.grid.count = filter(sponges.grid, richness > 0)

# Cnidarians

cnidarians.grid <- grid %>%
  st_join(cnidarians.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

cnidarians.grid.count = filter(cnidarians.grid, richness > 0)

# Ctenophores

ctenophores.grid <- grid %>%
  st_join(ctenophores.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

ctenophores.grid.count = filter(ctenophores.grid, richness > 0)

# Nemerteans

nemerteans.grid <- grid %>%
  st_join(nemerteans.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nemerteans.grid.count = filter(nemerteans.grid, richness > 0)

# Platyhelminthes

platyhelminthes.grid <- grid %>%
  st_join(platyhelminthes.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

platyhelminthes.grid.count = filter(platyhelminthes.grid, richness > 0)

# Arrow worms

arrow.worms.grid <- grid %>%
  st_join(arrow.worms.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

arrow.worms.grid.count = filter(arrow.worms.grid, richness > 0)

# Molluscs

molluscs.grid <- grid %>%
  st_join(molluscs.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

molluscs.grid.count = filter(molluscs.grid, richness > 0)

# Annelids

annelids.grid <- grid %>%
  st_join(annelids.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

annelids.grid.count = filter(annelids.grid, richness > 0)

# Peanut worms

peanut.worms.grid <- grid %>%
  st_join(peanut.worms.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

peanut.worms.grid.count = filter(peanut.worms.grid, richness > 0)

# Crustaceans

crustaceans.grid <- grid %>%
  st_join(crustaceans.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

crustaceans.grid.count = filter(crustaceans.grid, richness > 0)

# Nodding heads

nodding.heads.grid <- grid %>%
  st_join(nodding.heads.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

nodding.heads.grid.count = filter(nodding.heads.grid, richness > 0)

# Brachiopods

brachiopods.grid <- grid %>%
  st_join(brachiopods.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

brachiopods.grid.count = filter(brachiopods.grid, richness > 0)

# Bryozoans

bryozoans.grid <- grid %>%
  st_join(bryozoans.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

bryozoans.grid.count = filter(bryozoans.grid, richness > 0)

# Horseshoe worms

horseshoe.worms.grid <- grid %>%
  st_join(horseshoe.worms.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

horseshoe.worms.grid.count = filter(horseshoe.worms.grid, richness > 0)

# Echinoderms

echinoderms.grid <- grid %>%
  st_join(echinoderms.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

echinoderms.grid.count = filter(echinoderms.grid, richness > 0)

# Tunicates

tunicates.grid <- grid %>%
  st_join(tunicates.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

tunicates.grid.count = filter(tunicates.grid, richness > 0)

# Fishes

fishes.grid <- grid %>%
  st_join(fishes.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

fishes.grid.count = filter(fishes.grid, richness > 0)

# Mammals

mammals.grid <- grid %>%
  st_join(mammals.points.sum) %>%
  mutate(overlap = ifelse(!is.na(scientificName), 1, 0)) %>%
  group_by(cell_id) %>%
  summarize(richness = sum(overlap))

# Remove grid cell with zero records

mammals.grid.count = filter(mammals.grid, richness > 0)



# Explore grid maps

# palette_explorer()

tmap_mode("view")

animal.grid.map = tm_shape(mammals.grid.count) +
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

animal.grid.map

# Export shapes

st_write(animal.grid.count, "outputs/vectors/animal_grid.shp")
write.csv(animal.records.gridded, "outputs/tabular/animal_records_gridded.csv", row.names = FALSE)

st_write(sponges.grid.count, "outputs/vectors/sponges_grid.shp")
write.csv(sponges.records.gridded, "outputs/tabular/sponges_records_gridded.csv", row.names = FALSE)

st_write(cnidarians.grid.count, "outputs/vectors/cnidarians_grid.shp")
write.csv(cnidarians.records.gridded, "outputs/tabular/cnidarians_records_gridded.csv", row.names = FALSE)

st_write(ctenophores.grid.count, "outputs/vectors/ctenophores_grid.shp")
write.csv(ctenophores.records.gridded, "outputs/tabular/ctenophores_records_gridded.csv", row.names = FALSE)

st_write(nemerteans.grid.count, "outputs/vectors/nemerteans_grid.shp")
write.csv(nemerteans.records.gridded, "outputs/tabular/nemerteans_records_gridded.csv", row.names = FALSE)

st_write(platyhelminthes.grid.count, "outputs/vectors/platyhelminthes_grid.shp")
write.csv(platyhelminthes.records.gridded, "outputs/tabular/platyhelminthes_records_gridded.csv", row.names = FALSE)

st_write(arrow.worms.grid.count, "outputs/vectors/arrow.worms_grid.shp")
write.csv(arrow.worms.records.gridded, "outputs/tabular/arrow.worms_records_gridded.csv", row.names = FALSE)

st_write(molluscs.grid.count, "outputs/vectors/molluscs_grid.shp")
write.csv(molluscs.records.gridded, "outputs/tabular/molluscs_records_gridded.csv", row.names = FALSE)

st_write(annelids.grid.count, "outputs/vectors/annelids_grid.shp")
write.csv(annelids.records.gridded, "outputs/tabular/annelids_records_gridded.csv", row.names = FALSE)

st_write(peanut.worms.grid.count, "outputs/vectors/peanut.worms_grid.shp")
write.csv(peanut.worms.records.gridded, "outputs/tabular/peanut.worms_records_gridded.csv", row.names = FALSE)

st_write(crustaceans.grid.count, "outputs/vectors/crustaceans_grid.shp")
write.csv(crustaceans.records.gridded, "outputs/tabular/crustaceans_records_gridded.csv", row.names = FALSE)

st_write(nodding.heads.grid.count, "outputs/vectors/nodding_heads_grid.shp")
write.csv(nodding.heads.records.gridded, "outputs/tabular/nodding.heads_records_gridded.csv", row.names = FALSE)

st_write(brachiopods.grid.count, "outputs/vectors/brachiopods_grid.shp")
write.csv(brachiopods.records.gridded, "outputs/tabular/brachiopods_records_gridded.csv", row.names = FALSE)

st_write(bryozoans.grid.count, "outputs/vectors/bryozoans_grid.shp")
write.csv(bryozoans.records.gridded, "outputs/tabular/bryozoans_records_gridded.csv", row.names = FALSE)

st_write(horseshoe.worms.grid.count, "outputs/vectors/horseshoe.worms_grid.shp")
write.csv(horseshoe.worms.records.gridded, "outputs/tabular/horseshoe.worms_records_gridded.csv", row.names = FALSE)

st_write(echinoderms.grid.count, "outputs/vectors/echinoderms_grid.shp")
write.csv(echinoderms.records.gridded, "outputs/tabular/echinoderms_records_gridded.csv", row.names = FALSE)

st_write(tunicates.grid.count, "outputs/vectors/tunicates_grid.shp")
write.csv(tunicates.records.gridded, "outputs/tabular/tunicates_records_gridded.csv", row.names = FALSE)

st_write(fishes.grid.count, "outputs/vectors/fishes_grid.shp")
write.csv(fishes.records.gridded, "outputs/tabular/fishes_records_gridded.csv", row.names = FALSE)

st_write(mammals.grid.count, "outputs/vectors/mammals_grid.shp")
write.csv(mammals.records.gridded, "outputs/tabular/mammals_records_gridded.csv", row.names = FALSE)
