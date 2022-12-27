
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

# Analysis of historical collection activities

plants <- read.csv("../tabular_data/Howe_Sound_vascular_plant_records_consolidated.csv")

plants$yearRecorded <- as.numeric(substr(plants$eventDate, 1, 4))

plants <- plants %>% drop_na(yearRecorded)

min(plants$yearRecorded)

hist(plants$yearRecorded) # Limited collection activities during the 1900s;
# Most historical data collected in the 1920s and between 1960s and 1980;
# Large increase in observations with the emergence of tools like iNat in 2010s;
# No. species reported for Howe Sound has nearly doubled over the last two decades
# (See figure below)

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
  ylab('New Species')+xlab('Year') + 
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