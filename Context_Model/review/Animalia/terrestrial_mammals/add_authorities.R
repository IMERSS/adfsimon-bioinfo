# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Context_model_terrestrial_mammals_review_summary_2023-12-07.csv")

# Read taxonomic authorities

authorities <- read.csv("sources/msw3-all-utf8.csv")

# Merge 'Author' and 'Date' to apply Rules of Nomenclature in Zoology

authorities$authorities <- paste(authorities$Author, authorities$Date, sep = ", ")

# Add common field 'Taxon' to authorities dataframe

authorities$Taxon <- paste(authorities$Genus, authorities$Species, authorities$Subspecies, sep = " ")

# Trim spaces

authorities$Taxon <- trimws(authorities$Taxon)

baseline$scientificNameAuthorship <- authorities$authorities[match(unlist(baseline$scientificName), authorities$Taxon)]

write.csv(baseline, "summaries/Context_model_terrestrial_mammals_review_summary_2023-12-08.csv")
