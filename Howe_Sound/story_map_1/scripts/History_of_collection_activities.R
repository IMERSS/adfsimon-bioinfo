
# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(gapminder)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(hrbrthemes)
library(sf)
library(tidyr)
library(viridis)

# Note: when generating gridded choropleths below, 
# I detected a missing Latitude coordinate in a record from the period 2010-2020!

# Tabular analysis of historical collection activities

plants <- read.csv("../tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")

plants$yearRecorded <- as.numeric(substr(plants$eventDate, 1, 4))

plants <- plants %>% drop_na(yearRecorded)

min(plants$yearRecorded) # Earliest record dates to 1897 

plants.1890.1900 <- plants %>% filter(between(yearRecorded,1890,1900))
plants.1890.1910 <- plants %>% filter(between(yearRecorded,1890,1910))
plants.1890.1920 <- plants %>% filter(between(yearRecorded,1890,1920))
plants.1890.1930 <- plants %>% filter(between(yearRecorded,1890,1930))
plants.1890.1940 <- plants %>% filter(between(yearRecorded,1890,1940))
plants.1890.1950 <- plants %>% filter(between(yearRecorded,1890,1950))
plants.1890.1960 <- plants %>% filter(between(yearRecorded,1890,1960))
plants.1890.1970 <- plants %>% filter(between(yearRecorded,1890,1970))
plants.1890.1980 <- plants %>% filter(between(yearRecorded,1890,1980))
plants.1890.1990 <- plants %>% filter(between(yearRecorded,1890,1990))
plants.1890.2000 <- plants %>% filter(between(yearRecorded,1890,2000))
plants.1890.2010 <- plants %>% filter(between(yearRecorded,1890,2010))
plants.1890.2020 <- plants %>% filter(between(yearRecorded,1890,2020))
plants.1890.2022 <- plants %>% filter(between(yearRecorded,1890,2022))


# Compile table of decadal summary statistics

y.1900 <- 1900
y.1900.cum.obs <- nrow(plants.1890.1900) # cumulative no. observations
y.1900.cum.spp <- length(unique(plants.1890.1900$scientificName)) # cumulative no. species
y.1900.cum.ind <- length(unique(plants.1890.1900$recordedBy)) # cumulative observers
y.1900.obs <- nrow(plants.1890.1900) # no. observations
y.1900.spp <- length(unique(plants.1890.1900$scientificName)) # no. species
y.1900.ind <- length(unique(plants.1890.1900$recordedBy)) # no. observers

y.1910 <- 1910
y.1910.cum.obs <- nrow(plants.1890.1910) # cumulative no. observations
y.1910.cum.spp <- length(unique(plants.1890.1910$scientificName)) # cumulative no. species
y.1910.cum.ind <- length(unique(plants.1890.1910$recordedBy)) # cumulative observers
y.1910.obs <- nrow(plants.1890.1910) - nrow(plants.1890.1900) # no. obs
y.1910.spp <- y.1910.cum.spp - y.1900.cum.spp # no. species
y.1910.ind <- y.1910.cum.ind - y.1900.cum.ind # no. observers

y.1920 <- 1920
y.1920.cum.obs <- nrow(plants.1890.1920) # cumulative no. observations
y.1920.cum.spp <- length(unique(plants.1890.1920$scientificName)) # cumulative no. species
y.1920.cum.ind <- length(unique(plants.1890.1920$recordedBy)) # cumulative observers
y.1920.obs <- nrow(plants.1890.1920) - nrow(plants.1890.1910) # no. obs
y.1920.spp <- y.1920.cum.spp - y.1910.cum.spp # no. species
y.1920.ind <- y.1920.cum.ind - y.1910.cum.ind # no. observers

y.1930 <- 1930
y.1930.cum.obs <- nrow(plants.1890.1930) # cumulative no. observations
y.1930.cum.spp <- length(unique(plants.1890.1930$scientificName)) # cumulative no. species
y.1930.cum.ind <- length(unique(plants.1890.1930$recordedBy)) # cumulative observers
y.1930.obs <- nrow(plants.1890.1930) - nrow(plants.1890.1920) # no. obs
y.1930.spp <- y.1930.cum.spp - y.1920.cum.spp # no. species
y.1930.ind <- y.1930.cum.ind - y.1920.cum.ind # no. observers

y.1940 <- 1940
y.1940.cum.obs <- nrow(plants.1890.1940) # cumulative no. observations
y.1940.cum.spp <- length(unique(plants.1890.1940$scientificName)) # cumulative no. species
y.1940.cum.ind <- length(unique(plants.1890.1940$recordedBy)) # cumulative observers
y.1940.obs <- nrow(plants.1890.1940) - nrow(plants.1890.1930) # no. obs
y.1940.spp <- y.1940.cum.spp - y.1930.cum.spp # no. species
y.1940.ind <- y.1940.cum.ind - y.1930.cum.ind # no. observers

y.1950 <- 1950
y.1950.cum.obs <- nrow(plants.1890.1950) # cumulative no. observations
y.1950.cum.spp <- length(unique(plants.1890.1950$scientificName)) # cumulative no. species
y.1950.cum.ind <- length(unique(plants.1890.1950$recordedBy)) # cumulative observers
y.1950.obs <- nrow(plants.1890.1950) - nrow(plants.1890.1940) # no. obs
y.1950.spp <- y.1950.cum.spp - y.1940.cum.spp # no. species
y.1950.ind <- y.1950.cum.ind - y.1940.cum.ind # no. observers

y.1960 <- 1960
y.1960.cum.obs <- nrow(plants.1890.1960) # cumulative no. observations
y.1960.cum.spp <- length(unique(plants.1890.1960$scientificName)) # cumulative no. species
y.1960.cum.ind <- length(unique(plants.1890.1960$recordedBy)) # cumulative observers
y.1960.obs <- nrow(plants.1890.1960) - nrow(plants.1890.1950) # no. obs
y.1960.spp <- y.1960.cum.spp - y.1950.cum.spp # no. species
y.1960.ind <- y.1960.cum.ind - y.1950.cum.ind # no. observers

y.1970 <- 1970
y.1970.cum.obs <- nrow(plants.1890.1970) # cumulative no. observations
y.1970.cum.spp <- length(unique(plants.1890.1970$scientificName)) # cumulative no. species
y.1970.cum.ind <- length(unique(plants.1890.1970$recordedBy)) # cumulative observers
y.1970.obs <- nrow(plants.1890.1970) - nrow(plants.1890.1960) # no. obs
y.1970.spp <- y.1970.cum.spp - y.1960.cum.spp # no. species
y.1970.ind <- y.1970.cum.ind - y.1960.cum.ind # no. observers

y.1980 <- 1980
y.1980.cum.obs <- nrow(plants.1890.1980) # cumulative no. observations
y.1980.cum.spp <- length(unique(plants.1890.1980$scientificName)) # cumulative no. species
y.1980.cum.ind <- length(unique(plants.1890.1980$recordedBy)) # cumulative observers
y.1980.obs <- nrow(plants.1890.1980) - nrow(plants.1890.1970) # no. obs
y.1980.spp <- y.1980.cum.spp - y.1970.cum.spp # no. species
y.1980.ind <- y.1980.cum.ind - y.1970.cum.ind # no. observers

y.1990 <- 1990
y.1990.cum.obs <- nrow(plants.1890.1990) # cumulative no. observations
y.1990.cum.spp <- length(unique(plants.1890.1990$scientificName)) # cumulative no. species
y.1990.cum.ind <- length(unique(plants.1890.1990$recordedBy)) # cumulative observers
y.1990.obs <- nrow(plants.1890.1990) - nrow(plants.1890.1980) # no. obs
y.1990.spp <- y.1990.cum.spp - y.1980.cum.spp # no. species
y.1990.ind <- y.1990.cum.ind - y.1980.cum.ind # no. observers

y.2000 <- 2000
y.2000.cum.obs <- nrow(plants.1890.2000) # cumulative no. observations
y.2000.cum.spp <- length(unique(plants.1890.2000$scientificName)) # cumulative no. species
y.2000.cum.ind <- length(unique(plants.1890.2000$recordedBy)) # cumulative observers
y.2000.obs <- nrow(plants.1890.2000) - nrow(plants.1890.1990) # no. obs
y.2000.spp <- y.2000.cum.spp - y.1990.cum.spp # no. species
y.2000.ind <- y.2000.cum.ind - y.1990.cum.ind # no. observers

y.2010 <- 2010
y.2010.cum.obs <- nrow(plants.1890.2010) # cumulative no. observations
y.2010.cum.spp <- length(unique(plants.1890.2010$scientificName)) # cumulative no. species
y.2010.cum.ind <- length(unique(plants.1890.2010$recordedBy)) # cumulative observers
y.2010.obs <- nrow(plants.1890.2010) - nrow(plants.1890.2000) # no. obs
y.2010.spp <- y.2010.cum.spp - y.2000.cum.spp # no. species
y.2010.ind <- y.2010.cum.ind - y.2000.cum.ind # no. observers

y.2020 <- 2020
y.2020.cum.obs <- nrow(plants.1890.2020) # cumulative no. observations
y.2020.cum.spp <- length(unique(plants.1890.2020$scientificName)) # cumulative no. species
y.2020.cum.ind <- length(unique(plants.1890.2020$recordedBy)) # cumulative observers
y.2020.obs <- nrow(plants.1890.2020) - nrow(plants.1890.2010) # no. obs
y.2020.spp <- y.2020.cum.spp - y.2010.cum.spp # no. species
y.2020.ind <- y.2020.cum.ind - y.2010.cum.ind # no. observers

y.2022 <- 2022
y.2022.cum.obs <- nrow(plants.1890.2022) # cumulative no. observations
y.2022.cum.spp <- length(unique(plants.1890.2022$scientificName)) # cumulative no. species
y.2022.cum.ind <- length(unique(plants.1890.2022$recordedBy)) # cumulative observers
y.2022.obs <- nrow(plants.1890.2022) - nrow(plants.1890.2020) # no. obs
y.2022.spp <- y.2022.cum.spp - y.2020.cum.spp # no. species
y.2022.ind <- y.2022.cum.ind - y.2020.cum.ind # no. observers

year <- c(y.1900,y.1910,y.1920,y.1930,y.1940,y.1950,y.1960,y.1970,y.1980,y.1990,y.2000,y.2010,y.2020,y.2022)
obs <- c(y.1900.obs,y.1910.obs,y.1920.obs,y.1930.obs,y.1940.obs,y.1950.obs,y.1960.obs,y.1970.obs,y.1980.obs,y.1990.obs,y.2000.obs,y.2010.obs,y.2020.obs,y.2022.obs)
spp <- c(y.1900.spp,y.1910.spp,y.1920.spp,y.1930.spp,y.1940.spp,y.1950.spp,y.1960.spp,y.1970.spp,y.1980.spp,y.1990.spp,y.2000.spp,y.2010.spp,y.2020.spp,y.2022.spp)
ind <- c(y.1900.ind,y.1910.ind,y.1920.ind,y.1930.ind,y.1940.ind,y.1950.ind,y.1960.ind,y.1970.ind,y.1980.ind,y.1990.ind,y.2000.ind,y.2010.ind,y.2020.ind,y.2022.ind)
cum.obs <- c(y.1900.cum.obs,y.1910.cum.obs,y.1920.cum.obs,y.1930.cum.obs,y.1940.cum.obs,y.1950.cum.obs,y.1960.cum.obs,y.1970.cum.obs,y.1980.cum.obs,y.1990.cum.obs,y.2000.cum.obs,y.2010.cum.obs,y.2020.cum.obs,y.2022.cum.obs)
cum.spp <- c(y.1900.cum.spp,y.1910.cum.spp,y.1920.cum.spp,y.1930.cum.spp,y.1940.cum.spp,y.1950.cum.spp,y.1960.cum.spp,y.1970.cum.spp,y.1980.cum.spp,y.1990.cum.spp,y.2000.cum.spp,y.2010.cum.spp,y.2020.cum.spp,y.2022.cum.spp)
cum.ind <- c(y.1900.cum.ind,y.1910.cum.ind,y.1920.cum.ind,y.1930.cum.ind,y.1940.cum.ind,y.1950.cum.ind,y.1960.cum.ind,y.1970.cum.ind,y.1980.cum.ind,y.1990.cum.ind,y.2000.cum.ind,y.2010.cum.ind,y.2020.cum.ind,y.2022.cum.ind)

history <- data.frame(year,obs,spp,ind,cum.obs,cum.spp,cum.ind)

# Plot

plot <- history %>% 
  ggplot(aes(x = year, y = cum.spp, color = 'green')) +
  geom_line(alpha=0.8) + 
  geom_point(size=2) +
  labs(title="Vascular plant species recorded in Átl’ka7tsem/Howe Sound 1990-2022",
       caption="Source: Átl’ka7tsem/Howe Sound Biosphere Region Initiative")+
  ylab('Reported Species')+xlab('Year') + 
  theme_solarized_2(light=F) +
  theme(text=element_text(colour="#EEEEEE"),
        title=element_text(colour="#EEEEEE",size=9,face = "bold"),
        plot.title=element_text(hjust=0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x=element_blank(),
        plot.margin = unit(c(0.5,1.3,0.5,0.5), "cm")) + 
  theme(legend.position="none") +
  transition_reveal(year)

plot

animate(
  plot = plot,
  render = gifski_renderer(),
  height = 420,
  width = 700, 
  duration = 10,
  fps = 30,
  res = 100,
  end_pause = 120)

anim_save('../outputs/plants.gif')

# Most historical data collected in the 1920s and between the 1960s and 1980;
# Large increase in observations and recorded species with the emergence of 
# iNaturalist in 2010s; No. species reported for Howe Sound has nearly doubled 
# over the last two decades

# Prepare gridded choropleths of decadal collection activities

# First load 1km2 grid

grid <- st_read("../spatial_data/vectors/1km_grid_WGS84_coordinates_x_vascular_plant_grid_NA_omit.shp")

# Create CRS object

WGS84 <- st_crs("WGS84")

# 1890 - 1900 records

# Convert plant records to sf points

plants.1890.1900.points <- st_as_sf(plants.1890.1900, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1900 <- st_intersection(plants.1890.1900.points, grid)

# Sum species richness by grid cell

grid.1890.1900$count <- 1

matrix.1890.1900 <- ecodist::crosstab(grid.1890.1900$id, grid.1890.1900$scientificName, grid.1890.1900$count)

matrix.1890.1900$richness <- rowSums(matrix.1890.1900)

matrix.1890.1900$id <- row.names(matrix.1890.1900)

# Assign richness values to grid

grid.1890.1900 <- grid

grid.1890.1900$richness <- matrix.1890.1900$richness[match(unlist(grid.1890.1900$id), matrix.1890.1900$id)]

grid.1890.1900$year <- 1900


# 1890 - 1910 records

# Convert plant records to sf points

plants.1890.1910.points <- st_as_sf(plants.1890.1910, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1910 <- st_intersection(plants.1890.1910.points, grid)

# Sum species richness by grid cell

grid.1890.1910$count <- 1

matrix.1890.1910 <- ecodist::crosstab(grid.1890.1910$id, grid.1890.1910$scientificName, grid.1890.1910$count)

matrix.1890.1910$richness <- rowSums(matrix.1890.1910)

matrix.1890.1910$id <- row.names(matrix.1890.1910)

# Assign richness values to grid

grid.1890.1910 <- grid

grid.1890.1910$richness <- matrix.1890.1910$richness[match(unlist(grid.1890.1910$id), matrix.1890.1910$id)]

grid.1890.1910$year <- 1910



# 1890 - 1920 records

# Convert plant records to sf points

plants.1890.1920.points <- st_as_sf(plants.1890.1920, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1920 <- st_intersection(plants.1890.1920.points, grid)

# Sum species richness by grid cell

grid.1890.1920$count <- 1

matrix.1890.1920 <- ecodist::crosstab(grid.1890.1920$id, grid.1890.1920$scientificName, grid.1890.1920$count)

matrix.1890.1920$richness <- rowSums(matrix.1890.1920)

matrix.1890.1920$id <- row.names(matrix.1890.1920)

# Assign richness values to grid

grid.1890.1920 <- grid

grid.1890.1920$richness <- matrix.1890.1920$richness[match(unlist(grid.1890.1920$id), matrix.1890.1920$id)]

grid.1890.1920$year <- 1920



# 1890 - 1930 records

# Convert plant records to sf points

plants.1890.1930.points <- st_as_sf(plants.1890.1930, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1930 <- st_intersection(plants.1890.1930.points, grid)

# Sum species richness by grid cell

grid.1890.1930$count <- 1

matrix.1890.1930 <- ecodist::crosstab(grid.1890.1930$id, grid.1890.1930$scientificName, grid.1890.1930$count)

matrix.1890.1930$richness <- rowSums(matrix.1890.1930)

matrix.1890.1930$id <- row.names(matrix.1890.1930)

# Assign richness values to grid

grid.1890.1930 <- grid

grid.1890.1930$richness <- matrix.1890.1930$richness[match(unlist(grid.1890.1930$id), matrix.1890.1930$id)]

grid.1890.1930$year <- 1930



# 1890 - 1940 records

# Convert plant records to sf points

plants.1890.1940.points <- st_as_sf(plants.1890.1940, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1940 <- st_intersection(plants.1890.1940.points, grid)

# Sum species richness by grid cell

grid.1890.1940$count <- 1

matrix.1890.1940 <- ecodist::crosstab(grid.1890.1940$id, grid.1890.1940$scientificName, grid.1890.1940$count)

matrix.1890.1940$richness <- rowSums(matrix.1890.1940)

matrix.1890.1940$id <- row.names(matrix.1890.1940)

# Assign richness values to grid

grid.1890.1940 <- grid

grid.1890.1940$richness <- matrix.1890.1940$richness[match(unlist(grid.1890.1940$id), matrix.1890.1940$id)]

grid.1890.1940$year <- 1940



# 1890 - 1950 records

# Convert plant records to sf points

plants.1890.1950.points <- st_as_sf(plants.1890.1950, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1950 <- st_intersection(plants.1890.1950.points, grid)

# Sum species richness by grid cell

grid.1890.1950$count <- 1

matrix.1890.1950 <- ecodist::crosstab(grid.1890.1950$id, grid.1890.1950$scientificName, grid.1890.1950$count)

matrix.1890.1950$richness <- rowSums(matrix.1890.1950)

matrix.1890.1950$id <- row.names(matrix.1890.1950)

# Assign richness values to grid

grid.1890.1950 <- grid

grid.1890.1950$richness <- matrix.1890.1950$richness[match(unlist(grid.1890.1950$id), matrix.1890.1950$id)]

grid.1890.1950$year <- 1950



# 1890 - 1960 records

# Convert plant records to sf points

plants.1890.1960.points <- st_as_sf(plants.1890.1960, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1960 <- st_intersection(plants.1890.1960.points, grid)

# Sum species richness by grid cell

grid.1890.1960$count <- 1

matrix.1890.1960 <- ecodist::crosstab(grid.1890.1960$id, grid.1890.1960$scientificName, grid.1890.1960$count)

matrix.1890.1960$richness <- rowSums(matrix.1890.1960)

matrix.1890.1960$id <- row.names(matrix.1890.1960)

# Assign richness values to grid

grid.1890.1960 <- grid

grid.1890.1960$richness <- matrix.1890.1960$richness[match(unlist(grid.1890.1960$id), matrix.1890.1960$id)]

grid.1890.1960$year <- 1960



# 1890 - 1970 records

# Convert plant records to sf points

plants.1890.1970.points <- st_as_sf(plants.1890.1970, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1970 <- st_intersection(plants.1890.1970.points, grid)

# Sum species richness by grid cell

grid.1890.1970$count <- 1

matrix.1890.1970 <- ecodist::crosstab(grid.1890.1970$id, grid.1890.1970$scientificName, grid.1890.1970$count)

matrix.1890.1970$richness <- rowSums(matrix.1890.1970)

matrix.1890.1970$id <- row.names(matrix.1890.1970)

# Assign richness values to grid

grid.1890.1970 <- grid

grid.1890.1970$richness <- matrix.1890.1970$richness[match(unlist(grid.1890.1970$id), matrix.1890.1970$id)]

grid.1890.1970$year <- 1970



# 1890 - 1980 records

# Convert plant records to sf points

plants.1890.1980.points <- st_as_sf(plants.1890.1980, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1980 <- st_intersection(plants.1890.1980.points, grid)

# Sum species richness by grid cell

grid.1890.1980$count <- 1

matrix.1890.1980 <- ecodist::crosstab(grid.1890.1980$id, grid.1890.1980$scientificName, grid.1890.1980$count)

matrix.1890.1980$richness <- rowSums(matrix.1890.1980)

matrix.1890.1980$id <- row.names(matrix.1890.1980)

# Assign richness values to grid

grid.1890.1980 <- grid

grid.1890.1980$richness <- matrix.1890.1980$richness[match(unlist(grid.1890.1980$id), matrix.1890.1980$id)]

grid.1890.1980$year <- 1980



# 1890 - 1990 records

# Convert plant records to sf points

plants.1890.1990.points <- st_as_sf(plants.1890.1990, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.1990 <- st_intersection(plants.1890.1990.points, grid)

# Sum species richness by grid cell

grid.1890.1990$count <- 1

matrix.1890.1990 <- ecodist::crosstab(grid.1890.1990$id, grid.1890.1990$scientificName, grid.1890.1990$count)

matrix.1890.1990$richness <- rowSums(matrix.1890.1990)

matrix.1890.1990$id <- row.names(matrix.1890.1990)

# Assign richness values to grid

grid.1890.1990 <- grid

grid.1890.1990$richness <- matrix.1890.1990$richness[match(unlist(grid.1890.1990$id), matrix.1890.1990$id)]

grid.1890.1990$year <- 1990



# 1890 - 2000 records

# Convert plant records to sf points

plants.1890.2000.points <- st_as_sf(plants.1890.2000, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.2000 <- st_intersection(plants.1890.2000.points, grid)

# Sum species richness by grid cell

grid.1890.2000$count <- 1

matrix.1890.2000 <- ecodist::crosstab(grid.1890.2000$id, grid.1890.2000$scientificName, grid.1890.2000$count)

matrix.1890.2000$richness <- rowSums(matrix.1890.2000)

matrix.1890.2000$id <- row.names(matrix.1890.2000)

# Assign richness values to grid

grid.1890.2000 <- grid

grid.1890.2000$richness <- matrix.1890.2000$richness[match(unlist(grid.1890.2000$id), matrix.1890.2000$id)]

grid.1890.2000$year <- 2000



# 1890 - 2010 records

# Convert plant records to sf points

plants.1890.2010.points <- st_as_sf(plants.1890.2010, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.2010 <- st_intersection(plants.1890.2010.points, grid)

# Sum species richness by grid cell

grid.1890.2010$count <- 1

matrix.1890.2010 <- ecodist::crosstab(grid.1890.2010$id, grid.1890.2010$scientificName, grid.1890.2010$count)

matrix.1890.2010$richness <- rowSums(matrix.1890.2010)

matrix.1890.2010$id <- row.names(matrix.1890.2010)

# Assign richness values to grid

grid.1890.2010 <- grid

grid.1890.2010$richness <- matrix.1890.2010$richness[match(unlist(grid.1890.2010$id), matrix.1890.2010$id)]

grid.1890.2010$year <- 2010



# 1890 - 2020 records

# Convert plant records to sf points

plants.1890.2020 <- plants.1890.2020 %>% drop_na(decimalLatitude) # Strange: some record is missing Latitude for some reason: check!

plants.1890.2020.points <- st_as_sf(plants.1890.2020, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.2020 <- st_intersection(plants.1890.2020.points, grid)

# Sum species richness by grid cell

grid.1890.2020$count <- 1

matrix.1890.2020 <- ecodist::crosstab(grid.1890.2020$id, grid.1890.2020$scientificName, grid.1890.2020$count)

matrix.1890.2020$richness <- rowSums(matrix.1890.2020)

matrix.1890.2020$id <- row.names(matrix.1890.2020)

# Assign richness values to grid

grid.1890.2020 <- grid

grid.1890.2020$richness <- matrix.1890.2020$richness[match(unlist(grid.1890.2020$id), matrix.1890.2020$id)]

grid.1890.2020$year <- 2020



# 1890 - 2022 records

# Convert plant records to sf points

plants.1890.2022 <- plants.1890.2022 %>% drop_na(decimalLatitude) # Strange: some record is missing Latitude for some reason: check!

plants.1890.2022.points <- st_as_sf(plants.1890.2022, coords = c("decimalLongitude", "decimalLatitude"), crs = WGS84)

# Intersect points and grid

grid.1890.2022 <- st_intersection(plants.1890.2022.points, grid)

# Sum species richness by grid cell

grid.1890.2022$count <- 1

matrix.1890.2022 <- ecodist::crosstab(grid.1890.2022$id, grid.1890.2022$scientificName, grid.1890.2022$count)

matrix.1890.2022$richness <- rowSums(matrix.1890.2022)

matrix.1890.2022$id <- row.names(matrix.1890.2022)

# Assign richness values to grid

grid.1890.2022 <- grid

grid.1890.2022$richness <- matrix.1890.2022$richness[match(unlist(grid.1890.2022$id), matrix.1890.2022$id)]

grid.1890.2022$year <- 2022



# Combine data for all decades

gridded.history <- rbind(grid.1890.1900,grid.1890.1910,grid.1890.1920,grid.1890.1930,
                         grid.1890.1940,grid.1890.1950,grid.1890.1960,grid.1890.1970,
                         grid.1890.1980,grid.1890.1990,grid.1890.2000,grid.1890.2010,
                         grid.1890.2020,grid.1890.2022)

class(gridded.history)

# Plot animated choropleth of spatial distribution of species documented over the decades

# Animated plot does not work! 
# But at least I have a shape object that I should be able to embed in Leaflet
# Inc. gridded analysis of richness over time

# Export sf object for historical analysis of species 
# reported in Howe Sound through the decades:

st_write(gridded.history, "../outputs/gridded_history.shp")


# Plot...


`map2` <- ggplot(data = gridded.history) +
  geom_sf(aes(fill = richness, group = id),
               color = "black") +
  scale_fill_viridis(option = "magma", direction = -1, name = "Richness",
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(100, units = "mm"),
                       draw.ulim = FALSE,
                       title.position = 'top',
                       title.hjust = 0.5,
                       title.vjust = 0.5
                     )) + 
  theme_hc() + theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
                     legend.position = "bottom") +
  xlab(" ") + ylab(" ") +
  labs(title = "Species Documented by {frame_time}",
       subtitle = "Source: Átl’ka7tsem/Howe Sound Biosphere Region Initiative") +
  transition_time(year)

#animate and save .gif
map_anim2 <- animate(map2, fps = 2,
                     height = 574, width = 875)
map_anim2

anim_save("howe_sound_plants.gif", map_anim2)