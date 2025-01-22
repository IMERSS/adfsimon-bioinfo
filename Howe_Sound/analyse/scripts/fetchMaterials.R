setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/.."))

# When working from GitHub, this script draws bulky biodiversity spatial data from Google Drive

# NOTE: THIS SCRIPT IS NOT CURRENTLY IN USE AS WE HAVE DECIDED TO AVOID USING BULKY SPATIAL DATA

source("scripts/utils.R")

# From location IMERSS Research Projects/Community Research Projects/2022-2025 - Howe Sound Biodiversity/spatial_data/vectors/Gridded_confirmed_records_2025
# https://drive.google.com/drive/folders/1WH8eY1K8ZWLLZux-YsSe6mfdFdyEUs1N
# downloadGdriveFolder("1WH8eY1K8ZWLLZux-YsSe6mfdFdyEUs1N", "spatial_data/vectors/", FALSE)
