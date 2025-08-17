# Script to parse GBIF/iNat records taxonomically

## NOTE: Request that future records inc. common names and also the 'casual' field to filter out casual records

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read GBIF records

herptile.records <- read.csv("../parse_records/outputs/records_herptiles_2025-08-09.csv")
mammal.records <- read.csv("../parse_records/outputs/records_terrestrial_mammals_2025-08-09.csv")

# Read GBIF records previously reviewed

records.reviewed.2024 <- read.csv("../parse_records/records/GBIF_Context_2024_07_02_Mammalia_Amphibia_Reptilia-assigned.csv")

# Assign names previously normalized based on matching gbifID field

# herptiles
herptile.records <- herptile.records %>%
  left_join(
    records.reviewed.2024 %>% 
      select(gbifID, Selected.taxon.name),
    by = "gbifID"
  )

# mammals
mammal.records <- mammal.records %>%
  left_join(
    records.reviewed.2024 %>% 
      select(gbifID, Selected.taxon.name),
    by = "gbifID"
  )


# Subset records normalized vs those that aren't already normalized

herptile.records.unmatched <- herptile.records %>%
  filter(is.na(Selected.taxon.name))

herptile.records.matched <- herptile.records %>%
  filter(!is.na(Selected.taxon.name))

mammal.records.unmatched <- mammal.records %>%
  filter(is.na(Selected.taxon.name))

mammal.records.matched <- mammal.records %>%
  filter(!is.na(Selected.taxon.name))

# For those that aren't normalized, add values for names that align with iNat taxonomy (accepted)

herptile.records.unmatched <- herptile.records.unmatched %>%
  mutate(
    Selected.taxon.name = if_else(
      nameStatus == "accepted",
      iNaturalistTaxonName,
      Selected.taxon.name
    )
  )

mammal.records.unmatched <- mammal.records.unmatched %>%
  mutate(
    Selected.taxon.name = if_else(
      nameStatus == "accepted",
      iNaturalistTaxonName,
      Selected.taxon.name
    )
  )

# Subset names aligned vs unaligned

herptile.records.matched.2 <- herptile.records.unmatched %>%
  filter(!is.na(Selected.taxon.name))

herptile.records.unmatched <- herptile.records.unmatched %>%
  filter(is.na(Selected.taxon.name))

mammal.records.matched.2 <- mammal.records.unmatched %>%
  filter(!is.na(Selected.taxon.name))

mammal.records.unmatched <- mammal.records.unmatched %>%
  filter(is.na(Selected.taxon.name))

# Write unmatched records for manual review

write.csv(herptile.records.unmatched, "outputs/records_herptiles_review.csv", row.names = FALSE)

write.csv(mammal.records.unmatched, "outputs/records_terrestrial_mammals_review.csv", row.names = FALSE)

# Read reviewed / normalized data

herptile.records.matched.3 <- read.csv("outputs/records_herptiles_reviewed_2025-08-16.csv")
mammal.records.matched.3 <- read.csv("outputs/records_terrestrial_mammals_reviewed_2025-08-16.csv")

# Combine normalized data

herptile.records.normalized <- rbind(herptile.records.matched, herptile.records.matched.2, herptile.records.matched.3)
mammal.records.normalized <- rbind(mammal.records.matched, mammal.records.matched.2, mammal.records.matched.3)

# Export normalized catalogs

write.csv(herptile.records.normalized, "outputs/herptile_records_normalized_2025-08-16.csv")
write.csv(mammal.records.normalized, "outputs/mammal_records_normalized_2025-08-16.csv")


