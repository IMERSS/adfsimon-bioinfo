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

# Tracheophyta

Tracheophyta.obs <- iNat.obs %>% filter(Phylum == 'Tracheophyta')

# Export taxonomically parsed catalogs

write.csv(Tracheophyta.obs, "outputs/iNat_obs_Tracheophyta.csv")

