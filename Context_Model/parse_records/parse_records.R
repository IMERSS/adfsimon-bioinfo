# Script to parse GBIF/iNat records taxonomically

## NOTE: Request that future obs inc. common names and also the 'casual' field to filter out casual obs

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

obs <- read.csv("records/GBIF_Context_2023_11_15_Mammalia_Amphibia_Reptilia.csv")

# Normalize date to YYYY-MM-DD

obs$eventDate <- substr(obs$eventDate, start = 1, stop = 10)

# Eliminate records without dates

obs <- obs[!(is.na(obs$eventDate) | obs$eventDate == ""), ]

# Note this script does eliminate captive/cultivated records as it is assumed that GBIF already filters these out

# Parse taxa

## TERRESTRIAL ANIMALS

# Herptiles

Herptiles.obs <- obs %>% filter(class == 'Amphibia' | class == 'Reptilia')

# Terrestrial Mammals

Terrestrial.mammals.obs <- obs %>% filter(class == 'Mammalia')

Terrestrial.mammals.obs <- subset(Terrestrial.mammals.obs, Infraorder != 'Cetacea' & Taxon.name != 'Neogale vison' & Superfamily != 'Phocoidea' & Subfamily != 'Lutrinae')

# Check that all taxa are accounted for

taxa <- rbind(Herptiles.obs,Terrestrial.mammals.obs)

missing.taxa <- anti_join(obs,taxa)

unique(missing.taxa$Taxon.name)

# Export catalogs

write.csv(Aves.obs, "outputs/iNat_obs_birds.csv", row.names = FALSE)

write.csv(Bacteria.obs, "outputs/iNat_obs_bacteria.csv", row.names = FALSE)

write.csv(Bryophyta.Marchantiophyta.Anthocerotophyta.obs, "outputs/iNat_obs_mosses_liverworts_and_hornworts.csv", row.names = FALSE)

write.csv(Desmids.etc.obs, "outputs/iNat_obs_freshwater_and_terrestrial_algae.csv", row.names = FALSE)

write.csv(Freshwater.bryozoans.obs, "outputs/iNat_obs_freshwater_bryozoans.csv", row.names = FALSE)

write.csv(Fungi.obs, "outputs/iNat_obs_fungi.csv", row.names = FALSE)

write.csv(Herptiles.obs, "outputs/iNat_obs_herptiles.csv", row.names = FALSE)

write.csv(Lichens.obs, "outputs/iNat_obs_lichens.csv", row.names = FALSE)

write.csv(Marine.algae.and.protozoa.obs, "outputs/iNat_obs_marine_algae_and_protozoa.csv", row.names = FALSE)

write.csv(Marine.animals.obs, "outputs/iNat_obs_marine_animals.csv", row.names = FALSE)

write.csv(Myxogastria.obs, "outputs/iNat_obs_Myxogastria.csv", row.names = FALSE)

write.csv(Terrestrial.annelids.etc, "outputs/iNat_obs_Terrestrial_annelids_etc.csv", row.names = FALSE)

write.csv(Terrestrial.arthropods.obs, "outputs/iNat_obs_terrestrial_arthropods.csv", row.names = FALSE)

write.csv(Terrestrial.mammals.obs, "outputs/iNat_obs_terrestrial_mammals.csv", row.names = FALSE)

write.csv(Terrestrial.molluscs.obs, "outputs/iNat_obs_terrestrial_molluscs.csv", row.names = FALSE)

write.csv(Tracheophyta.obs, "outputs/iNat_obs_Tracheophyta.csv", row.names = FALSE)
