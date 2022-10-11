# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(betapart)
library(CommEcol)
library(dplyr)
library(ecodist)
library(picante)
library(rgdal)
library(rgeos)
library(stringr)
library(tidyr)
library(vegan)

# Read spatially explicit biodiversity data

data <- read.csv("Galiano_vascular_plants_x_1km_grid_TPI_extent_2022-10-10.csv")

# Subset relevant fields

data <- data %>% select('Taxon'|'id_2')

# Add count field to generate matrix

data$Count <- 1

# Generate matrix 

matrix <- ecodist::crosstab(data$id_2, data$Taxon, data$Count)

# Convert to presence / absence

matrix[matrix > 0] <- 1 

# Turns out it is not straightforward to visualize beta diversity in a spatially explicit framework... SOOO...

# Let's play with the function from this site: https://rfunctions.blogspot.com/2015/08/calculating-beta-diversity-on-grid.html

