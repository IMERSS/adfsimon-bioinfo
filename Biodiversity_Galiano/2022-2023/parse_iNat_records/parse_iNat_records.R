# Script to parse iNat records taxonomically

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

iNat.obs <- read.csv("iNat_records/BioGaliano_iNat_data_2023-09-26.csv")

# Eliminate records without dates

iNat.obs <- iNat.obs[!(is.na(iNat.obs$Date.observed) | iNat.obs$Date.observed == ""), ]

nrow(iNat.obs)

# Eliminate extraneous time stamp from date observed string

iNat.obs$Date.observed <- substr(iNat.obs$Date.observed,1,10)

# Parse taxa

# Terrestrial Arthropods

unique(Terrestrial.arthropods.obs$Class)

Terrestrial.arthropods.1.obs <- iNat.obs %>% filter(Class == 'Insecta' | Class == 'Arachnida' | Class == 'Entognatha' | Class == 'Diplopoda' | Class == 'Chilopoda' | Class == 'Ostracoda')
Terrestrial.arthropods.2.obs <- iNat.obs %>% filter(Order == 'Isopoda')
Terrestrial.arthropods.2.obs<- subset(Terrestrial.arthropods.2.obs, Taxon.name != 'Bopyroides hippolytes' & Taxon.name != 'Gnorimosphaeroma oregonense' & Taxon.name != 'Pentidotea resecata' & Taxon.name != 'Pentidotea wosnesenskii')

Terrestrial.arthropods.obs <- rbind(Terrestrial.arthropods.1.obs, Terrestrial.arthropods.2.obs)


# Tracheophyta

Tracheophyta.obs <- iNat.obs %>% filter(Phylum == 'Tracheophyta')

# Export taxonomically parsed catalogs


write.csv(Terrestrial.arthropods.obs, "outputs/iNat_obs_terrestrial_arthropods.csv")
write.csv(Tracheophyta.obs, "outputs/iNat_obs_Tracheophyta.csv")

