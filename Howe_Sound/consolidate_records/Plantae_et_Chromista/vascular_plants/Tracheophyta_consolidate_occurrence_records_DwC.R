# Script to consolidate records of Howe Sound's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_review_summary_2024-11-09.csv")

# Create vector of DarwinCore fields for aggregating records

DwCFields <- c('scientificName','scientificNameAuthorship','taxonID','kingdom','phylum','class',
                    'order','suborder','infraorder','superfamily','family','genus','subgenus','specificEpithet',
                    'hybrid','subspecies','variety','form','infraspecificEpithet','taxonRank','institutionCode',
                    'collectionCode','catalogNumber','datasetName','occurrenceID','recordedBy','recordNumber',
                    'fieldNumber','eventDate','year','month','day','basisOfRecord','locality','locationRemarks','island',
                    'stateProvince','country','countryCode','decimalLatitude','decimalLongitude','coordinateUncertaintyInMeters',
                    'georeferencedBy','georeferenceVerificationStatus','georeferenceProtocol','georeferenceRemarks',
                    'habitat','verbatimDepth','verbatimElevation','occurrenceStatus','samplingProtocol','occurrenceRemarks',
                    'individualCount','sex','establishmentMeans','provincialStatus','nationalStatus','identifiedBy',
                    'identificationQualifier','identificationRemarks','previousIdentifications','bibliographicCitation',
                    'associatedReferences')

# Consolidate records

# Sources (4/6 added):

# CPNWH records 2023 - review later wrt GBIF data
# GBIF records 2022 - added (iNat data subtracted for now)
# iNaturalist reocords 2024 
# LGL records 2020-07-01 - added
# Page 2004 - Squamish River Estuary records - added
# Whistler Bioblitz records 2015 -

# Note: GBIF and CPNWH records need to be reviewed for unique records / duplication (CPNWH records excluded for now)

##################
## GBIF Records ##
##################

# GBIF TSV converted to CSV from Mac Numbers and Filtered by Taxa (Plantae)
GBIF.2022 <- read.csv("../../../consolidate_records/records/digitized/DwC/GBIF_2022_Plantae_DwC-assigned_AS_erroneous_localities_removed_reevaluated.csv", header = TRUE)

# Filter vascular plants

GBIF.2022 <- GBIF.2022 %>% filter( == "Tracheophyta")

## Remove iNaturalist records

GBIF.2022 <- GBIF.2022 %>% filter(institutionCode != "iNaturalist")

## Synthesize Records

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(GBIF.2022)))
names(data.frame) <- DwCFields

data.frame[names(GBIF.2022)] <- GBIF.2022

data.frame <- select(data.frame, c(1:length(DwCFields)))

# Add "unscrewed" GBIF names (via Basman 2022 scripts)

data.frame$scientificName <- GBIF.2022$selectedName

GBIF.2022 <- data.frame

# Add metadata

GBIF.2022$country <- "Canada"

# Merge with summary to standardize names and taxon metadata

GBIF.2022$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$taxonID <- summary$ID[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$kingdom <- summary$kingdom[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$phylum <- summary$phylum[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$class <- summary$class[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$order <- summary$order[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$suborder <- summary$suborder[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$superfamily <- summary$Superfamily[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$family <- summary$family[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$genus <- summary$genus[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$specificEpithet <- summary$specificEpithet[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$hybrid <- summary$hybrid[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$subspecies <- summary$subspecies[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$variety <- summary$variety[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$establishmentMeans <- summary$establishmentMeans[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$provincialStatus <- summary$provincialStatus[match(unlist(GBIF.2022$scientificName), summary$scientificName)]
GBIF.2022$nationalStatus <- summary$nationalStatus[match(unlist(GBIF.2022$scientificName), summary$scientificName)]

# Unmatched records

GBIF.2022.names.unmatched <- GBIF.2022[is.na(GBIF.2022$taxonID),]

# Matched records

GBIF.2022.names.matched <- anti_join(GBIF.2022,GBIF.2022.names.unmatched)

# Confirm all records are represented 

nrow(GBIF.2022)
nrow(GBIF.2022.names.matched)
nrow(GBIF.2022.names.unmatched)
nrow(GBIF.2022.names.matched)+nrow(GBIF.2022.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

GBIF.2022.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Swap unmatched names using key

GBIF.2022.names.unmatched.matched <- GBIF.2022.names.unmatched

GBIF.2022.names.unmatched.matched$scientificNameTemp <- GBIF.2022.key$Matched.Taxon[match(unlist(GBIF.2022.names.unmatched.matched$scientificName), GBIF.2022.key$Taxon)]

# Add values based on newly matched name

GBIF.2022.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$taxonID <- summary$ID[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$phylum <- summary$phylum[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$class <- summary$class[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$order <- summary$order[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$suborder <- summary$suborder[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$family <- summary$family[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$genus <- summary$genus[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$variety <- summary$variety[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.2022.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

GBIF.2022.names.unmatched.unmatched <- GBIF.2022.names.unmatched.matched[is.na(GBIF.2022.names.unmatched.matched$taxonID),]

GBIF.2022.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

GBIF.2022.names.unmatched.matched$scientificName <- GBIF.2022.names.unmatched.matched$scientificNameTemp

GBIF.2022.names.unmatched.matched$scientificNameTemp <- NULL

GBIF.2022.names.unmatched.matched <- GBIF.2022.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(GBIF.2022)
nrow(GBIF.2022.names.matched)
nrow(GBIF.2022.names.unmatched)
nrow(GBIF.2022.names.unmatched.matched)
nrow(GBIF.2022.names.unmatched.unmatched)
nrow(GBIF.2022.names.matched)+nrow(GBIF.2022.names.unmatched.matched)+nrow(GBIF.2022.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon', 'Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(GBIF.2022.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- GBIF.2022.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(GBIF.2022.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv", row.names=FALSE)

# Bind records

GBIF.2022.records <- rbind(GBIF.2022.names.matched,GBIF.2022.names.unmatched.matched)

# Set date formatting consistent with other data frames

GBIF.2022.records$eventDate <- as.Date(GBIF.2022.records$eventDate)

# Compare records in and out

nrow(GBIF.2022) - nrow(GBIF.2022.records)

nrow(GBIF.2022)
nrow(GBIF.2022.records) # ~400 records omitted; many taxa are cultivated species;
# However, some are species that cannot be reconciled with the summary due to ambiguity
# at the infrataxonomic level; these taxa should be sorted separately and integrated
# with final catalog with reference to another summary dataframe perhaps?

# Start record of unmatched names

unmatched.vascular.plant.records <- GBIF.2022.names.unmatched.unmatched

unmatched.vascular.plant.records


#################
## iNaturalist ##
#################

# Read iNaturalist records
iNaturalist <- read.csv("../../../consolidate_records/records/digitized/DwC/iNaturalist_Tracheophyta_Catalogue_2024_10_18_DwC.csv", header = TRUE)

# Filter out casual / captive records

iNaturalist <- iNaturalist %>% filter(captive != TRUE)

## Synthesize Records

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(iNaturalist)))
names(data.frame) <- DwCFields

data.frame[names(iNaturalist)] <- iNaturalist

data.frame <- select(data.frame, c(1:length(DwCFields)))

iNaturalist <- data.frame

# Add metadata

iNaturalist$stateProvince <- "British Columbia"
iNaturalist$country <- "Canada"
iNaturalist$countryCode <- "CA"

# Merge with summary to standardize names and taxon metadata

iNaturalist$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$taxonID <- summary$ID[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$kingdom <- summary$kingdom[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$phylum <- summary$phylum[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$class <- summary$class[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$order <- summary$order[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$suborder <- summary$suborder[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$superfamily <- summary$Superfamily[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$family <- summary$family[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$genus <- summary$genus[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$specificEpithet <- summary$specificEpithet[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$hybrid <- summary$hybrid[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$subspecies <- summary$subspecies[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$variety <- summary$variety[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$establishmentMeans <- summary$establishmentMeans[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$provincialStatus <- summary$provincialStatus[match(unlist(iNaturalist$scientificName), summary$scientificName)]
iNaturalist$nationalStatus <- summary$nationalStatus[match(unlist(iNaturalist$scientificName), summary$scientificName)]

# Unmatched records

iNaturalist.names.unmatched <- iNaturalist[is.na(iNaturalist$taxonID),]

# Matched records

iNaturalist.names.matched <- anti_join(iNaturalist,iNaturalist.names.unmatched)

# Confirm all records are represented 

nrow(iNaturalist)
nrow(iNaturalist.names.matched)
nrow(iNaturalist.names.unmatched)
nrow(iNaturalist.names.matched)+nrow(iNaturalist.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

iNaturalist.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Swap unmatched names using key

iNaturalist.names.unmatched.matched <- iNaturalist.names.unmatched

iNaturalist.names.unmatched.matched$scientificNameTemp <- iNaturalist.key$Matched.Taxon[match(unlist(iNaturalist.names.unmatched.matched$scientificName), iNaturalist.key$Taxon)]

# Add values based on newly matched name

iNaturalist.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$taxonID <- summary$ID[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$phylum <- summary$phylum[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$class <- summary$class[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$order <- summary$order[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$suborder <- summary$suborder[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$family <- summary$family[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$genus <- summary$genus[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$variety <- summary$variety[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(iNaturalist.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

iNaturalist.names.unmatched.unmatched <- iNaturalist.names.unmatched.matched[is.na(iNaturalist.names.unmatched.matched$taxonID),]

iNaturalist.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

iNaturalist.names.unmatched.matched$scientificName <- iNaturalist.names.unmatched.matched$scientificNameTemp

iNaturalist.names.unmatched.matched$scientificNameTemp <- NULL

iNaturalist.names.unmatched.matched <- iNaturalist.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(iNaturalist)
nrow(iNaturalist.names.matched)
nrow(iNaturalist.names.unmatched)
nrow(iNaturalist.names.unmatched.matched)
nrow(iNaturalist.names.unmatched.unmatched)
nrow(iNaturalist.names.matched)+nrow(iNaturalist.names.unmatched.matched)+nrow(iNaturalist.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon', 'Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(iNaturalist.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- iNaturalist.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(iNaturalist.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv", row.names=FALSE)

# Bind records

iNaturalist.records <- rbind(iNaturalist.names.matched,iNaturalist.names.unmatched.matched)

# Set date formatting consistent with other data frames

iNaturalist.records$eventDate <- as.Date(iNaturalist.records$eventDate)

# Compare records in and out

nrow(iNaturalist) - nrow(iNaturalist.records)

nrow(iNaturalist)
nrow(iNaturalist.records) 

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,LGL.2020.names.unmatched.unmatched)

unmatched.vascular.plant.records


######################
## LGL 2020 Records ##
######################

# Read data from LGL HSBRI Application Appendices (2020)

LGL.2020 <- read.csv("../../../consolidate_records/records/digitized/DwC/LGL_plant_records_2020-07-01_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(LGL.2020)))
names(data.frame) <- DwCFields

data.frame[names(LGL.2020)] <- LGL.2020

LGL.2020 <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "LGL2020:" 
unique.suffix <- 1:nrow(LGL.2020)

# Add metadata

LGL.2020$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
LGL.2020$stateProvince <- "British Columbia"
LGL.2020$country <- "Canada"
LGL.2020$countryCode <- "CA"
LGL.2020$georeferenceVerificationStatus <- "unable to georeference"

# Merge with summary to standardize names and taxon metadata

LGL.2020$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$taxonID <- summary$ID[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$kingdom <- summary$kingdom[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$phylum <- summary$phylum[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$class <- summary$class[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$order <- summary$order[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$suborder <- summary$suborder[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$superfamily <- summary$Superfamily[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$family <- summary$family[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$genus <- summary$genus[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$specificEpithet <- summary$specificEpithet[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$hybrid <- summary$hybrid[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$subspecies <- summary$subspecies[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$variety <- summary$variety[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$establishmentMeans <- summary$establishmentMeans[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$provincialStatus <- summary$provincialStatus[match(unlist(LGL.2020$scientificName), summary$scientificName)]
LGL.2020$nationalStatus <- summary$nationalStatus[match(unlist(LGL.2020$scientificName), summary$scientificName)]

# Unmatched records

LGL.2020.names.unmatched <- LGL.2020[is.na(LGL.2020$taxonID),]

# Matched records

LGL.2020.names.matched <- anti_join(LGL.2020,LGL.2020.names.unmatched)

# Confirm all records are represented 

nrow(LGL.2020)
nrow(LGL.2020.names.matched)
nrow(LGL.2020.names.unmatched)
nrow(LGL.2020.names.matched)+nrow(LGL.2020.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

LGL.2020.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Swap unmatched names using key

LGL.2020.names.unmatched.matched <- LGL.2020.names.unmatched

LGL.2020.names.unmatched.matched$scientificNameTemp <- LGL.2020.key$Matched.Taxon[match(unlist(LGL.2020.names.unmatched.matched$scientificName), LGL.2020.key$Taxon)]

# Add values based on newly matched name

LGL.2020.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$taxonID <- summary$ID[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$phylum <- summary$phylum[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$class <- summary$class[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$order <- summary$order[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$suborder <- summary$suborder[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$family <- summary$family[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$genus <- summary$genus[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$variety <- summary$variety[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
LGL.2020.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

LGL.2020.names.unmatched.unmatched <- LGL.2020.names.unmatched.matched[is.na(LGL.2020.names.unmatched.matched$taxonID),]

LGL.2020.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

LGL.2020.names.unmatched.matched$scientificName <- LGL.2020.names.unmatched.matched$scientificNameTemp

LGL.2020.names.unmatched.matched$scientificNameTemp <- NULL

LGL.2020.names.unmatched.matched <- LGL.2020.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(LGL.2020)
nrow(LGL.2020.names.matched)
nrow(LGL.2020.names.unmatched)
nrow(LGL.2020.names.unmatched.matched)
nrow(LGL.2020.names.unmatched.unmatched)
nrow(LGL.2020.names.matched)+nrow(LGL.2020.names.unmatched.matched)+nrow(LGL.2020.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon','Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(LGL.2020.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- LGL.2020.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(LGL.2020.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv", row.names=FALSE)

# Bind records

LGL.2020.records <- rbind(LGL.2020.names.matched,LGL.2020.names.unmatched.matched)

# Set date formatting consistent with other data frames

LGL.2020.records$eventDate <- as.Date(LGL.2020.records$eventDate)

# Compare records in and out

nrow(LGL.2020)
nrow(LGL.2020.records) #

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,LGL.2020.names.unmatched.unmatched)

unmatched.vascular.plant.records


###############
## Page 2004 ##
###############

# Read Nick Page, Raincoast Applied Ecology, vegetation assessment of Squamish Estuary

Page.2004 <- read.csv("../../../consolidate_records/records/digitized/DwC/Nick_Page_2004_SRWS_Squamish_Estuary_vegetation_assessment_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Page.2004)))
names(data.frame) <- DwCFields

data.frame[names(Page.2004)] <- Page.2004

Page.2004 <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "NPAGE2004:" 
unique.suffix <- 1:nrow(Page.2004)

# Add metadata

Page.2004$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Page.2004$datasetName <- "Effects of Fish Habitat Restoration Activities on Plant Communities in the Squamish River Estuary: Summary Report"
Page.2004$stateProvince <- "British Columbia"
Page.2004$country <- "Canada"
Page.2004$countryCode <- "CA"
Page.2004$locality <- "Squamish; Squamish Estuary"
Page.2004$decimalLatitude <- 49.70028714560986
Page.2004$decimalLongitude <- -123.16690017175902
Page.2004$coordinateUncertaintyInMeters <- 500
Page.2004$georeferencedBy <- "Andrew Simon"
Page.2004$georeferenceVerificationStatus <- "temporary georeferencing"
Page.2004$georeferenceProtocol <- "coordinates generalized based on locality information"
Page.2004$georeferenceRemarks <- "georeferencing may be improved in consultation with author"
Page.2004$basisOfRecord <- "HUMAN_OBSERVATION"

# Merge with summary to standardize names and taxon metadata

Page.2004$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$taxonID <- summary$ID[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$kingdom <- summary$kingdom[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$phylum <- summary$phylum[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$class <- summary$class[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$order <- summary$order[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$suborder <- summary$suborder[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$superfamily <- summary$Superfamily[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$family <- summary$family[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$genus <- summary$genus[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$specificEpithet <- summary$specificEpithet[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$hybrid <- summary$hybrid[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$subspecies <- summary$subspecies[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$variety <- summary$variety[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$establishmentMeans <- summary$establishmentMeans[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$provincialStatus <- summary$provincialStatus[match(unlist(Page.2004$scientificName), summary$scientificName)]
Page.2004$nationalStatus <- summary$nationalStatus[match(unlist(Page.2004$scientificName), summary$scientificName)]

# Unmatched records

Page.2004.names.unmatched <- Page.2004[is.na(Page.2004$taxonID),]

# Matched records

Page.2004.names.matched <- anti_join(Page.2004,Page.2004.names.unmatched)

# Confirm all records are represented 

nrow(Page.2004)
nrow(Page.2004.names.matched)
nrow(Page.2004.names.unmatched)
nrow(Page.2004.names.matched)+nrow(Page.2004.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Page.2004.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Swap unmatched names using key

Page.2004.names.unmatched.matched <- Page.2004.names.unmatched

Page.2004.names.unmatched.matched$scientificNameTemp <- Page.2004.key$Matched.Taxon[match(unlist(Page.2004.names.unmatched.matched$scientificName), Page.2004.key$Taxon)]

# Add values based on newly matched name

Page.2004.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$phylum <- summary$phylum[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$class <- summary$class[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$order <- summary$order[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$suborder <- summary$suborder[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$family <- summary$family[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$genus <- summary$genus[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$variety <- summary$variety[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Page.2004.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

Page.2004.names.unmatched.unmatched <- Page.2004.names.unmatched.matched[is.na(Page.2004.names.unmatched.matched$taxonID),]

Page.2004.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Page.2004.names.unmatched.matched$scientificName <- Page.2004.names.unmatched.matched$scientificNameTemp

Page.2004.names.unmatched.matched$scientificNameTemp <- NULL

Page.2004.names.unmatched.matched <- Page.2004.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Page.2004)
nrow(Page.2004.names.matched)
nrow(Page.2004.names.unmatched)
nrow(Page.2004.names.unmatched.matched)
nrow(Page.2004.names.unmatched.unmatched)
nrow(Page.2004.names.matched)+nrow(Page.2004.names.unmatched.matched)+nrow(Page.2004.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon','Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Page.2004.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Page.2004.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Page.2004.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review.key.csv", row.names=FALSE)

# Bind records

Page.2004.records <- rbind(Page.2004.names.matched,Page.2004.names.unmatched.matched)

# Set date formatting consistent with other data frames

Page.2004.records$eventDate <- as.Date(Page.2004.records$eventDate)

# Compare records in and out

nrow(Page.2004)
nrow(Page.2004.records) #

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Page.2004.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Assign metadata to reports lacking infrataxonomic resolution based on species epithets

unmatched.vascular.plant.records$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$kingdom <- summary$kingdom[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$phylum <- summary$phylum[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$class <- summary$class[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$order <- summary$order[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$suborder <- summary$suborder[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$superfamily <- summary$Superfamily[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$family <- summary$family[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$genus <- summary$genus[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]
unmatched.vascular.plant.records$specificEpithet <- summary$specificEpithet[match(unlist(unmatched.vascular.plant.records$scientificName), summary$specificEpithet)]

# Unmatched records

unmatched.vascular.plant.records.unmatched <- unmatched.vascular.plant.records[is.na(unmatched.vascular.plant.records$kingdom),]

# Matched records

unmatched.vascular.plant.records.matched <- anti_join(unmatched.vascular.plant.records,unmatched.vascular.plant.records.unmatched)

unique.unmatched.vascular.plant.records.matched <- unique(unmatched.vascular.plant.records.matched$scientificName)

# Create key to attribute TaxonID to taxa that cannot be resolved infra-taxonomically with reference to summary

key.field.names <- c('Taxon',"TaxonID")

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=length(unique.unmatched.vascular.plant.records.matched)))

names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- unique.unmatched.vascular.plant.records.matched

unmatched.taxa[is.na(unmatched.taxa)] <- ""

write.csv(unmatched.taxa,"keys/taxonID_review_key.csv", row.names=FALSE)

# Read key with TaxonIDs ascribed

taxonID.key <- read.csv("keys/taxonID_key.csv")

# Assign TaxonIDs to records of taxa unresolved infra-taxonomically

unmatched.vascular.plant.records.matched$taxonID <- taxonID.key$TaxonID[match(unlist(unmatched.vascular.plant.records.matched$scientificName), taxonID.key$Taxon)]



# Combine all source occurrence records

Vascular.plant.records <- rbind(GBIF.2022.records,LGL.2020.records,Page.2004.records,unmatched.vascular.plant.records.matched)



# Finalize DwC fields (day, month, year, infraspecificEpithet, occurrenceStatus)

# Date

Vascular.plant.records$year <- substr(Vascular.plant.records$eventDate, 1, 4)
Vascular.plant.records$month <- substr(Vascular.plant.records$eventDate, 6, 7)
Vascular.plant.records$day <- substr(Vascular.plant.records$eventDate, 9, 10)

# Infrataxa

Genera <- Vascular.plant.records %>% filter(specificEpithet == "")
Genera <- Genera %>% filter(hybrid == "")
Genera$taxonRank <- "genus" 
Genera <- subset(Genera, select = -c(hybrid, subspecies, variety, form))

Species <- subset(Vascular.plant.records, specificEpithet != "")
Species <- Species %>% filter(hybrid == "")
Species <- Species %>% filter(variety == "")
Species <- Species %>% filter(subspecies == "")
Species$taxonRank <- "species" 
Species <- subset(Species, select = -c(hybrid, subspecies, variety, form))

Hybrids <- subset(Vascular.plant.records, hybrid != "")
Hybrids$taxonRank <- "hybrid"
Hybrids$infraspecificEpithet <- Hybrids$hybrid
Hybrids <- subset(Hybrids, select = -c(hybrid, subspecies, variety, form))

Subspecies <- subset(Vascular.plant.records, subspecies != "")
Subspecies$taxonRank <- "subspecies"
Subspecies$infraspecificEpithet <- Subspecies$subspecies
Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

Varieties <- subset(Vascular.plant.records, variety != "")
Varieties$taxonRank <- "varietas"
Varieties$infraspecificEpithet <- Varieties$variety
Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

Vascular.plant.records <- rbind(Genera, Species, Hybrids, Subspecies, Varieties)

# occurrenceStatus

Vascular.plant.records$occurrenceStatus <- "present"

# Order by taxon

Vascular.plant.records <- Vascular.plant.records[order(Vascular.plant.records$scientificName),] 

head(Vascular.plant.records)

# Tally records

nrow(Vascular.plant.records)

# Summary of records that remain unmatched

nrow(unmatched.vascular.plant.records)

sort(unique(unmatched.vascular.plant.records$scientificName))

# Remove NAs

Vascular.plant.records[is.na(Vascular.plant.records)] <- ""

# Replace 'ca.ubc' with 'UBC'

Vascular.plant.records$institutionCode[Vascular.plant.records$institutionCode == 'ca.ubc'] <- 'UBC'

# Output synthesized catalog of occurrence records

write.csv(Vascular.plant.records,"synthesized/Howe_Sound_vascular_plant_records_consolidated.csv", row.names = FALSE)

# Evaluate georeferencing resolution of vascular plant records

nrow(Vascular.plant.records) # 18K vascular plant occurrence records

Vascular.plant.records$coordinateUncertaintyInMeters <- as.numeric(Vascular.plant.records$coordinateUncertaintyInMeters)

hist(Vascular.plant.records$coordinateUncertaintyInMeters, 
        xlim=c(0,1000), breaks = 100000, main="Vascular Plant Records: Coordinate Uncertainty", xlab = "Coordinate Uncertainty in meters")

sum(is.na(Vascular.plant.records$coordinateUncertaintyInMeters))/nrow(Vascular.plant.records) 
# 32% of records lack coordinate uncertainty
sum(is.na(Vascular.plant.records$coordinateUncertaintyInMeters))/nrow(Vascular.plant.records) * nrow(Vascular.plant.records) 
# Or nearly 6K of 18k records total

georeferenced.records <- nrow(Vascular.plant.records)-sum(is.na(Vascular.plant.records$coordinateUncertaintyInMeters))

sum(Vascular.plant.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records # 80% of georeferenced records mapped to < 100 m coordinate uncertainty

sum(Vascular.plant.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records * georeferenced.records

# That is, about 10K of total 18k records can be analysed with confidence at 100m grid scale

# Some records with NAs are likely mapped at a reasonable degree of accuracy; it should be possible to evaluate the georeferencing by user
# to improve data availability as may be necessary; for a first pass, however, it will suffice to map biodiversity at coarse
# spatial scales (e.g., 500x500m grid)


