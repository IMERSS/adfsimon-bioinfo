# Script to parse Howe Sound summary by taxonomic group

# Set relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

# Read summary records
summary <- read.csv("../inputs/AHSBR_species_list_2026-04-11.csv", stringsAsFactors = FALSE)


write.csv(Terrestrial.mammals.summary, "../outputs/Howe_Sound_terrestrial_mammals.csv", row.names = FALSE)
write.csv(Herptiles.summary, "../outputs/Howe_Sound_herptiles.csv", row.names = FALSE)