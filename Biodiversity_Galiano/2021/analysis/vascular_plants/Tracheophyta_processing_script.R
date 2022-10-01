library(dplyr)

### Import Tracheophyta from Biodiversity Galiano iNaturalist Project

setwd("/Users/Simon/Sync/Simon/Biodiversity_Galiano_Project/Species_Lists/2021_Lists/Vascular_Plants")

galiano.plants <- read.csv("Vascular_plants_2021-09-19.csv")

### Summarize first recorded historical records

galiano.plants$observed_on <- as.Date(galiano.plants$observed_on)

plant.summary <- galiano.plants %>% group_by(scientific_name) %>% filter(observed_on == min(observed_on))

### Remove taxa that remain unresolved to genus

plant.summary <- plant.summary[!(is.na(plant.summary$taxon_genus_name) | plant.summary$taxon_genus_name==""), ]

### Export summary

write.csv(plant.summary, "plant.summary.csv")


