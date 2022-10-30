# Split iNaturalist observations by taxon
# 2022-10-29
# Andrew Simon

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)

# Read iNaturalist observations

chloro.obs <- read.csv("iNaturalist_source_data/iNaturalist_Chlorophyta_observations_2022-10-29.csv")
rhodo.obs <- read.csv("iNaturalist_source_data/iNaturalist_Rhodophyta_observations_2022-10-29.csv")
ochro.obs <- read.csv("iNaturalist_source_data/iNaturalist_Ochrophyta_observations_2022-10-29.csv")
Algae.obs <- rbind(chloro.obs,rhodo.obs,ochro.obs) 

# Summarize first recorded historical records by Taxon

Algae.obs$observed_on <- as.Date(Algae.obs$observed_on)

Algae.summary <- Algae.obs %>% group_by(scientific_name) %>% filter(observed_on == min(observed_on))

# Remove taxa that remain unresolved to genus

Algae.summary <- Algae.summary[!(is.na(Algae.summary$taxon_genus_name) | Algae.summary$taxon_genus_name==""), ]

# Remove duplicate observations from summary

Algae.summary <- Algae.summary %>% distinct('Taxon', .keep_all = TRUE)

# Write iNaturalist summaries by Taxon

write.csv(Algae.summary, "outputs/Algae_iNat_summary.csv")

# Write iNaturalist catalogs by Taxon

write.csv(Algae.obs, "outputs/Algae_iNat_obs.csv")

