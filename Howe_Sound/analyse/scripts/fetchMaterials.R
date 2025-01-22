setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/.."))

# When working from GitHub, this script draws bulky biodiversity data from Google Drive

source("scripts/utils.R")

# From location IMERSS Research Projects/Community Research Projects/2022-2025 - Howe Sound Biodiversity/Spatial_Data/
# https://drive.google.com/drive/folders/1i9-LHxIsIqpXGH8bKAavJXAjZLNOJkOs
downloadGdriveFolder("1i9-LHxIsIqpXGH8bKAavJXAjZLNOJkOs", ".../Spatial_Data", FALSE)

