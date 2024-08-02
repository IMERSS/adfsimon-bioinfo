# Script to parse GBIF/iNat records taxonomically

## NOTE: Request that future records inc. common names and also the 'casual' field to filter out casual records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

records <- read.csv("records/GBIF_Context_2024_07_02_Mammalia_Amphibia_Reptilia-assigned.csv")

# Normalize date to YYYY-MM-DD

records$eventDate <- substr(records$eventDate, start = 1, stop = 10)

# Eliminate records without dates

records <- records[!(is.na(records$eventDate) | records$eventDate == ""), ]

# Note this script does eliminate captive/cultivated records as it is assumed that GBIF already filters these out

# Parse taxa

## TERRESTRIAL ANIMALS

# Terrestrial Mammals

Terrestrial.mammals.records <- records %>% filter(class == 'Mammalia')

unique(Terrestrial.mammals.records$Selected.taxon.name)

Terrestrial.mammals.records <- subset(Terrestrial.mammals.records, infraorder != 'Cetacea' & Selected.taxon.name != 'Mustela vison energumenos' & Selected.taxon.name != 'Neogale vison' & Selected.taxon.name != 'Neogale vison evagor' & Selected.taxon.name != 'Neogale vison energumenos' & Selected.taxon.name != 'Neovison vison evagor' & Selected.taxon.name != 'Mustela vison energumenos' & superfamily != 'Phocoidea' & subfamily != 'Lutrinae')

# Herptiles

unique(Herptiles.records$Selected.taxon.name)

Herptiles.records <- records %>% filter(class == 'Amphibia' | class == 'Reptilia' | class == 'Squamata')

# Check that all taxa are accounted for

taxa <- rbind(Herptiles.records,Terrestrial.mammals.records)

missing.taxa <- anti_join(records,taxa)

unique(missing.taxa$Selected.taxon.name)

# Export catalogs

write.csv(Herptiles.records, "outputs/records_herptiles_2024-08-02.csv", row.names = FALSE)

write.csv(Terrestrial.mammals.records, "outputs/records_terrestrial_mammals_2024-08-02.csv", row.names = FALSE)
