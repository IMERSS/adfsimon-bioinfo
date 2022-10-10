# Split iNaturalist observations by taxon
# 2022-09-30
# Andrew Simon

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)

# Read iNaturalist observations

obs <- read.csv("iNaturalist_Tracheophyta_observations_2022-10-09.csv")
head(obs)

# Filter Taxa

Tracheophyta <- obs %>% filter(taxon_phylum_name == 'Tracheophyta')

# Summarize first recorded historical records by Taxon

Tracheophyta$observed_on <- as.Date(Tracheophyta$observed_on)

Tracheophyta.summary <- Tracheophyta %>% group_by(scientific_name) %>% filter(observed_on == min(observed_on))

# Remove taxa that remain unresolved to genus

Tracheophyta.summary <- Tracheophyta.summary[!(is.na(Tracheophyta.summary$taxon_genus_name) | Tracheophyta.summary$taxon_genus_name==""), ]

# Remove duplicate observations from summary

Tracheophyta.summary <- Tracheophyta.summary %>% distinct('Taxon', .keep_all = TRUE)

# Write iNaturalist summaries by Taxon

write.csv(Tracheophyta.summary, "Tracheophyta_iNat_summary.csv")

# Write iNaturalist catalogs by Taxon

write.csv(Tracheophyta, "Tracheophyta_iNat_obs.csv")

