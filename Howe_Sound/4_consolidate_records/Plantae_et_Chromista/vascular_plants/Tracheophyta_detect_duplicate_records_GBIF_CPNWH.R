# Script to detect duplicate records in CPNWH and GBIF datasets

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)


# Read GBIF and CPNWH records to detect and remove duplicate records within and between datasets

# CPNWH DATA

# CPNWH TSV locally processed to intersect complete CPNWH dataset with polygon representing AHSBR 
# (Data too large to host on GitHub)
CPNWH.2023 <- read.csv("../../records/digitized/DwC/AHSBR_CPNWH_data_spatial_query_2023-03-03_DwC.csv", header = TRUE)

# No values for phylum :( Must filter by family (note: the following code is out of sorts
#  because R is not reading in the CSV correctly, or has not previously converted TSV
# to CSV correctly; change field name from order to family later)

sort(unique(CPNWH.2023$order))

# Import vector of vascular plant family names from FPNW2 to filter CPNWH.2023 dataset by vascular plant family 

vascular.plant.families <- read.csv("FPNW2_vascular_plant_families.csv", header = TRUE)
vascular.plant.families <- vascular.plant.families$family
vascular.plant.families <- vascular.plant.families %>% paste(collapse = "|")

CPNWH.2023 <- CPNWH.2023 %>% filter(str_detect(order, vascular.plant.families))

sort(unique(CPNWH.2023$scientificName)) # Yup, these all look like vascular plant names
# (Need to double-check this method to make sure this is fool-proof...)


# GBIF DATA

# GBIF TSV converted to CSV from Mac Numbers and Filtered by Taxa (Plantae)
GBIF.2022 <- read.csv("../../records/digitized/DwC/GBIF_2022_Plantae_DwC-assigned_AS_erroneous_localities_removed_reevaluated.csv", header = TRUE)

# Filter vascular plants

GBIF.2022 <- GBIF.2022 %>% filter(phylum == "Tracheophyta")


# Detect duplicate records within GBIF dataset based on occurrenceID

GBIF.2022.duplicates <- data.frame(table(GBIF.2022$occurrenceID))
GBIF.2022.duplicates[GBIF.2022.duplicates$Freq > 1,] # Only potential duplicates based on
# occurrenceID are those records lacking these values

# Manually review GBIF records lacking occurrenceID

GBIF.2022.duplicates <- GBIF.2022 %>% filter(occurrenceID == "")

# While many of these records are from the same collector / date / locality, they are of 
# unique taxa and have unique catalog numbers; I will therefore retain them.

# Detect duplicate records within GBIF dataset based on catalogNumber

GBIF.2022.duplicates <- data.frame(table(GBIF.2022$catalogNumber))
GBIF.2022.duplicates[GBIF.2022.duplicates$Freq > 1,] # Only potential duplicates based on
# catalogNumber are those records lacking these values

# Manually review GBIF records lacking catalogNumber

# Several duplicate records detected based on manual review

duplicate.catalogNumbers.to.remove.from.GBIF.2022 <- c("BABY-11514","V210856","V219243",
          "V219244","V219246","V227194","V232939","V236164","V236236","V236237",
          "V236238","V236243","V236397","V238420","V238421","V238450","V238600",
          "V238601","V238869","V238870","V238911","V239695")

duplicate.catalogNumbers.to.remove.from.GBIF.2022 <- duplicate.catalogNumbers.to.remove.from.GBIF.2022 %>% paste(collapse = "|")

GBIF.2022.duplicates <- GBIF.2022 %>% filter(str_detect(catalogNumber, duplicate.catalogNumbers.to.remove.from.GBIF.2022))

# Reduce duplicates to unique records based on catalogNumber

# Order duplicate dataframe to consistently remove the BOLD duplicates

GBIF.2022.duplicates <- GBIF.2022.duplicates[order(GBIF.2022.duplicates$occurrenceID,decreasing=TRUE),]

GBIF.2022.duplicates <- GBIF.2022.duplicates %>% distinct(catalogNumber, .keep_all = TRUE)

# Create vector of unique identifiers indexing duplicate records

GBIF.2022.duplicates <- GBIF.2022.duplicates$occurrenceID

length(GBIF.2022.duplicates) ## 22 duplicate records in GBIF.2022

# Remove duplicates from GBIF.2022

GBIF.2022 <- GBIF.2022 %>% filter(!occurrenceID %in% GBIF.2022.duplicates)


# Detect duplicate records within CPNWH dataset based on occurrenceID

CPNWH.2023.duplicates <- data.frame(table(CPNWH.2023$occurrenceID))
CPNWH.2023.duplicates[CPNWH.2023.duplicates$Freq > 1,] # No duplicate records based on occurrenceID

# Detect duplicate records within CPNWH dataset based on catalogNumber
# Note: current formatting is messed up; catalogNumbers are in the field dataGeneralizations
# Fix code later once formatting is corrected

CPNWH.2023.duplicates <- data.frame(table(CPNWH.2023$dataGeneralizations))
CPNWH.2023.duplicates[CPNWH.2023.duplicates$Freq > 1,] # No duplicate records based on catalogNumber


# Detect duplicate records between CPNWH and GBIF datasets
# Note: current formatting is messed up for CPNWH; 
# catalogNumbers are in the field dataGeneralizations
# eventDate values are in the field fieldNumber
# recordedBy values are in the field dynamicProperties
# Fix code later once formatting is corrected

# Construct dataframes with occurrenceIDs, catalog numbers, and taxon names to review potential duplicates

GBIF.2022.records.occurrenceID <- GBIF.2022$occurrenceID
CPNWH.2023.records.occurrenceID <- CPNWH.2023$occurrenceID

GBIF.2022.records.catalog.numbers <- GBIF.2022$catalogNumber
CPNWH.2023.records.catalog.numbers <- CPNWH.2023$dataGeneralizations

GBIF.2022.records.taxa <- GBIF.2022$scientificName
CPNWH.2023.records.taxa <- CPNWH.2023$scientificName

GBIF.2022.records.eventDate <- GBIF.2022$eventDate
CPNWH.2023.records.eventDate <- CPNWH.2023$fieldNumber

GBIF.2022.records.recordedBy <- GBIF.2022$recordedBy
CPNWH.2023.records.recordedBy <- CPNWH.2023$dynamicProperties

GBIF.2022.records <- data.frame(GBIF.2022.records.occurrenceID,GBIF.2022.records.catalog.numbers,GBIF.2022.records.taxa,GBIF.2022.records.eventDate,GBIF.2022.records.recordedBy)
CPNWH.2023.records <- data.frame(CPNWH.2023.records.occurrenceID,CPNWH.2023.records.catalog.numbers,CPNWH.2023.records.taxa,CPNWH.2023.records.eventDate,CPNWH.2023.records.recordedBy)

GBIF.2022.records$source <- "GBIF"
CPNWH.2023.records$source <- "CPNWH"

colnames(GBIF.2022.records) <- c("occurrenceID","catalogNumber","scientificName","eventDate","recordedBy","source")
colnames(CPNWH.2023.records) <- c("occurrenceID","catalogNumber","scientificName","eventDate","recordedBy","source")

GBIF.2022.CPNWH.2023.records <- rbind(GBIF.2022.records,CPNWH.2023.records)

# Detect potential duplicate records between CPNWH and GBIF datasets based on occurrenceID

GBIF.2022.CPNWH.2023.duplicates <- data.frame(table(GBIF.2022.CPNWH.2023.records$occurrenceID))
GBIF.2022.CPNWH.2023.duplicates[GBIF.2022.CPNWH.2023.duplicates$Freq > 1,] # Only potential
# duplicates are those lacking occurrenceID; but these are the same as reviewed above 
# (All GBIF records)

# Detect potential duplicate records between CPNWH and GBIF datasets based on catalogNumber

GBIF.2022.CPNWH.2023.duplicates <- data.frame(table(GBIF.2022.CPNWH.2023.records$catalogNumber))

GBIF.CPNWH.duplicates <- GBIF.2022.CPNWH.2023.duplicates[GBIF.2022.CPNWH.2023.duplicates$Freq > 1,]

GBIF.CPNWH.duplicates <- GBIF.CPNWH.duplicates$Var1

length(GBIF.CPNWH.duplicates)

# As many as 1,211 duplicate records between datasets!

# Extract list of potential duplicates by catalogNumber:

GBIF.CPNWH.duplicate.records <- GBIF.2022.CPNWH.2023.records %>% filter(catalogNumber %in% GBIF.CPNWH.duplicates)

# Remove rows with NA values in catalogNumber for separate review process

GBIF.CPNWH.potential.duplicates <- GBIF.CPNWH.duplicate.records %>% filter(catalogNumber == "")
unique(GBIF.CPNWH.potential.duplicates$source) # these records all belong to GBIF;
# All GBIF records of have unique occurrenceIDs so they are unlikely to be duplicates
# as we have already weeded out duplicate GBIF records above

# Reduce list of potential duplicates to those with catalogNumbers

GBIF.CPNWH.duplicate.records <- GBIF.CPNWH.duplicate.records %>% filter(!catalogNumber == "")

GBIF.CPNWH.duplicate.records <- GBIF.CPNWH.duplicate.records[order(GBIF.CPNWH.duplicate.records$catalogNumber),]

(nrow(GBIF.CPNWH.duplicates)/2)/nrow(CPNWH.2023)

# write.csv(GBIF.CPNWH.duplicates, "GBIF_CPNWH_duplicates.csv", row.names = FALSE)

# Identify CPNWH records that appear unique wrt GBIF

CPNWH.unique.records <- CPNWH.2023 %>% filter(!dataGeneralizations %in% GBIF.CPNWH.duplicates)
  
occurrenceID <- CPNWH.unique.records$occurrenceID
scientificName <- CPNWH.unique.records$scientificName
catalogNumber <- CPNWH.unique.records$dataGeneralizations
recordedBy <- CPNWH.unique.records$dynamicProperties
eventDate <- CPNWH.unique.records$fieldNumber
country <- CPNWH.unique.records$countryCode
stateProvince <- CPNWH.unique.records$country
locality <- CPNWH.unique.records$county

CPNWH.unique.records <- data.frame(occurrenceID,scientificName,catalogNumber,recordedBy,eventDate,country,stateProvince,locality)

write.csv(CPNWH.unique.records,"CPNWH_unique_records.csv")


