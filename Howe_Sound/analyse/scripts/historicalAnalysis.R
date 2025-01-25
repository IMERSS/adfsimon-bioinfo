# Map history of vascular plant surveys in Átl’ka7tsem

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load libraries

library(dplyr)
library(gifski)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(tidyr)

# Source dependencies

source("utils.R")

# Analysis of historical collection activities

plants <- read.csv("../outputs/gridded_plants_2025.csv")

plants$yearRecorded <- as.numeric(substr(plants$eventDate, 1, 4))

plants <- plants %>% drop_na(yearRecorded)

min(plants$yearRecorded) # Earliest record dates to 1897
max(plants$yearRecorded) # Most recent record dates to 2024

plants.1890.1900 <- plants %>% filter(between(yearRecorded,1890,1900))
plants.1890.1900$period <- '1890-1900'
plants.1890.1910 <- plants %>% filter(between(yearRecorded,1890,1910))
plants.1890.1910$period <- '1890-1910'
plants.1890.1920 <- plants %>% filter(between(yearRecorded,1890,1920))
plants.1890.1920$period <- '1890-1920'
plants.1890.1930 <- plants %>% filter(between(yearRecorded,1890,1930))
plants.1890.1930$period <- '1890-1930'
plants.1890.1940 <- plants %>% filter(between(yearRecorded,1890,1940))
plants.1890.1940$period <- '1890-1940'
plants.1890.1950 <- plants %>% filter(between(yearRecorded,1890,1950))
plants.1890.1950$period <- '1890-1950'
plants.1890.1960 <- plants %>% filter(between(yearRecorded,1890,1960))
plants.1890.1960$period <- '1890-1960'
plants.1890.1970 <- plants %>% filter(between(yearRecorded,1890,1970))
plants.1890.1970$period <- '1890-1970'
plants.1890.1980 <- plants %>% filter(between(yearRecorded,1890,1980))
plants.1890.1980$period <- '1890-1980'
plants.1890.1990 <- plants %>% filter(between(yearRecorded,1890,1990))
plants.1890.1990$period <- '1890-1990'
plants.1890.2000 <- plants %>% filter(between(yearRecorded,1890,2000))
plants.1890.2000$period <- '1890-2000'
plants.1890.2010 <- plants %>% filter(between(yearRecorded,1890,2010))
plants.1890.2010$period <- '1890-2010'
plants.1890.2020 <- plants %>% filter(between(yearRecorded,1890,2020))
plants.1890.2020$period <- '1890-2020'
plants.1890.2024 <- plants %>% filter(between(yearRecorded,1890,2024))
plants.1890.2024$period <- '1890-2024'

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

y.2024 <- 2024
y.2024.cum.obs <- nrow(plants.1890.2024) # cumulative no. observations
y.2024.cum.spp <- length(unique(plants.1890.2024$scientificName)) # cumulative no. species
y.2024.cum.ind <- length(unique(plants.1890.2024$recordedBy)) # cumulative observers
y.2024.obs <- nrow(plants.1890.2024) - nrow(plants.1890.2020) # no. obs
y.2024.spp <- y.2024.cum.spp - y.2020.cum.spp # no. species
y.2024.ind <- y.2024.cum.ind - y.2020.cum.ind # no. observers

year <- c(y.1900,y.1910,y.1920,y.1930,y.1940,y.1950,y.1960,y.1970,y.1980,y.1990,y.2000,y.2010,y.2020,y.2024)
obs <- c(y.1900.obs,y.1910.obs,y.1920.obs,y.1930.obs,y.1940.obs,y.1950.obs,y.1960.obs,y.1970.obs,y.1980.obs,y.1990.obs,y.2000.obs,y.2010.obs,y.2020.obs,y.2024.obs)
spp <- c(y.1900.spp,y.1910.spp,y.1920.spp,y.1930.spp,y.1940.spp,y.1950.spp,y.1960.spp,y.1970.spp,y.1980.spp,y.1990.spp,y.2000.spp,y.2010.spp,y.2020.spp,y.2024.spp)
ind <- c(y.1900.ind,y.1910.ind,y.1920.ind,y.1930.ind,y.1940.ind,y.1950.ind,y.1960.ind,y.1970.ind,y.1980.ind,y.1990.ind,y.2000.ind,y.2010.ind,y.2020.ind,y.2024.ind)
cum.obs <- c(y.1900.cum.obs,y.1910.cum.obs,y.1920.cum.obs,y.1930.cum.obs,y.1940.cum.obs,y.1950.cum.obs,y.1960.cum.obs,y.1970.cum.obs,y.1980.cum.obs,y.1990.cum.obs,y.2000.cum.obs,y.2010.cum.obs,y.2020.cum.obs,y.2024.cum.obs)
cum.spp <- c(y.1900.cum.spp,y.1910.cum.spp,y.1920.cum.spp,y.1930.cum.spp,y.1940.cum.spp,y.1950.cum.spp,y.1960.cum.spp,y.1970.cum.spp,y.1980.cum.spp,y.1990.cum.spp,y.2000.cum.spp,y.2010.cum.spp,y.2020.cum.spp,y.2024.cum.spp)
cum.ind <- c(y.1900.cum.ind,y.1910.cum.ind,y.1920.cum.ind,y.1930.cum.ind,y.1940.cum.ind,y.1950.cum.ind,y.1960.cum.ind,y.1970.cum.ind,y.1980.cum.ind,y.1990.cum.ind,y.2000.cum.ind,y.2010.cum.ind,y.2020.cum.ind,y.2024.cum.ind)

history <- data.frame(year,obs,spp,ind,cum.obs,cum.spp,cum.ind)

# Prepare gridded summaries for each time period as basis for choropleths
# 1890 - 1900 records
grid.1890.1900 <- plants.1890.1900 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1910 records
grid.1890.1910 <- plants.1890.1910 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1920 records
grid.1890.1920 <- plants.1890.1920 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1930 records
grid.1890.1930 <- plants.1890.1930 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1940 records
grid.1890.1940 <- plants.1890.1940 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1950 records
grid.1890.1950 <- plants.1890.1950 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1960 records
grid.1890.1960 <- plants.1890.1960 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1970 records
grid.1890.1970 <- plants.1890.1970 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1980 records
grid.1890.1980 <- plants.1890.1980 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 1990 records
grid.1890.1990 <- plants.1890.1990 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 2000 records
grid.1890.2000 <- plants.1890.2000 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 2010 records
grid.1890.2010 <- plants.1890.2010 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 2020 records
grid.1890.2020 <- plants.1890.2020 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# 1890 - 2024 records
grid.1890.2024 <- plants.1890.2024 %>%
  group_by(period, cell_id) %>%
  summarize(record_count = n(), .groups = "drop")

# Combine data for all decades

gridded.history <- rbind(grid.1890.1900,grid.1890.1910,grid.1890.1920,grid.1890.1930,
                         grid.1890.1940,grid.1890.1950,grid.1890.1960,grid.1890.1970,
                         grid.1890.1980,grid.1890.1990,grid.1890.2000,grid.1890.2010,
                         grid.1890.2020,grid.1890.2024)

write.csv(gridded.history, "../outputs/gridded_history_1897-2024_cumulative.csv")

# Animated plot: historical increase in species reported for the region

plot <- history %>% 
  ggplot(aes(x = year, y = cum.spp, color = 'green')) +
  geom_line(alpha=0.8) + 
  geom_point(size=2) +
  labs(title="Vascular plant species recorded in Átl’ka7tsem/Howe Sound 1890-2024",
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

animate(
  plot = plot,
  render = gifski_renderer(),
  height = 420,
  width = 700, 
  duration = 5,
  fps = 30,
  res = 100,
  end_pause = 120)

# Supress saving gif
# anim_save('../outputs/Howe_Sound_plants_reported_species_1897-2024.gif')

# Most historical data collected in the 1920s and between the 1960s and 1980s;
# Large increase in observations and recorded species with the emergence of 
# iNaturalist in 2010s; no. species reported for Howe Sound has nearly doubled 
# over the last two decades

# Animated plot: historical increase in observations in the region

plot <- history %>% 
  ggplot(aes(x = year, y = cum.obs, color = 'green')) +
  geom_line(alpha=0.8) + 
  geom_point(size=2) +
  labs(title="Vascular plant observations in Átl’ka7tsem/Howe Sound 1890-2024",
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

animate(
  plot = plot,
  render = gifski_renderer(),
  height = 420,
  width = 700, 
  duration = 5,
  fps = 30,
  res = 100,
  end_pause = 120)

# Supress saving gif
# anim_save('../outputs/Howe_Sound_plant_observations_1897-2024.gif')
