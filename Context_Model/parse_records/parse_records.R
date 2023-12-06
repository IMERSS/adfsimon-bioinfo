# Script to parse GBIF/iNat records taxonomically

## NOTE: Request that future records inc. common names and also the 'casual' field to filter out casual records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

records <- read.csv("records/GBIF_Context_2023_11_15_Mammalia_Amphibia_Reptilia.csv")

# Normalize date to YYYY-MM-DD

records$eventDate <- substr(records$eventDate, start = 1, stop = 10)

# Eliminate records without dates

records <- records[!(is.na(records$eventDate) | records$eventDate == ""), ]

# Note this script does eliminate captive/cultivated records as it is assumed that GBIF already filters these out

# Parse taxa

## TERRESTRIAL ANIMALS

# Terrestrial Mammals

Terrestrial.mammals.records <- records %>% filter(class == 'Mammalia')

unique(Terrestrial.mammals.records$scientificName)

Terrestrial.mammals.records <- subset(Terrestrial.mammals.records, Infraorder != 'Cetacea' & Taxon.name != 'Neogale vison' & Superfamily != 'Phocoidea' & Subfamily != 'Lutrinae')

# Herptiles

Herptiles.records <- records %>% filter(class == 'Amphibia' | class == 'Reptilia')


# Check that all taxa are accounted for

taxa <- rbind(Herptiles.records,Terrestrial.mammals.records)

missing.taxa <- anti_join(records,taxa)

unique(missing.taxa$Taxon.name)

# Export catalogs

write.csv(Aves.records, "outputs/records_birds.csv", row.names = FALSE)

write.csv(Bacteria.records, "outputs/records_bacteria.csv", row.names = FALSE)

write.csv(Bryophyta.Marchantiophyta.Anthocerotophyta.records, "outputs/records_mosses_liverworts_and_hornworts.csv", row.names = FALSE)

write.csv(Desmids.etc.records, "outputs/records_freshwater_and_terrestrial_algae.csv", row.names = FALSE)

write.csv(Freshwater.bryozoans.records, "outputs/records_freshwater_bryozoans.csv", row.names = FALSE)

write.csv(Fungi.records, "outputs/records_fungi.csv", row.names = FALSE)

write.csv(Herptiles.records, "outputs/records_herptiles.csv", row.names = FALSE)

write.csv(Lichens.records, "outputs/records_lichens.csv", row.names = FALSE)

write.csv(Marine.algae.and.protozoa.records, "outputs/records_marine_algae_and_protozoa.csv", row.names = FALSE)

write.csv(Marine.animals.records, "outputs/records_marine_animals.csv", row.names = FALSE)

write.csv(Myxogastria.records, "outputs/records_Myxogastria.csv", row.names = FALSE)

write.csv(Terrestrial.annelids.etc, "outputs/records_Terrestrial_annelids_etc.csv", row.names = FALSE)

write.csv(Terrestrial.arthropods.records, "outputs/records_terrestrial_arthropods.csv", row.names = FALSE)

write.csv(Terrestrial.mammals.records, "outputs/records_terrestrial_mammals.csv", row.names = FALSE)

write.csv(Terrestrial.molluscs.records, "outputs/records_terrestrial_molluscs.csv", row.names = FALSE)

write.csv(Tracheophyta.records, "outputs/records_Tracheophyta.csv", row.names = FALSE)
