# Script to consolidate records of Howe Sound's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../2_review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_review_summary_2022-12-24.csv")

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

# Sources (3/5 added):

# CPNWH records 2023 -
# GBIF records 2022 - added
# LGL records 2020-07-01 - added
# Page Squamish River Estuary records 2004 - added
# Whistler Bioblitz records 2015 -

# First read GBIF and CPNWH records to detect and remove duplicate records between datasets

# GBIF TSV converted to CSV from Mac Numbers and Filtered by Taxa (Plantae)
GBIF.2022 <- read.csv("../../records/digitized/DwC/GBIF_2022_Plantae_DwC-assigned_AS_erroneous_localities_removed_reevaluated.csv", header = TRUE)

# Filter vascular plants

GBIF.2022 <- GBIF.2022 %>% filter(phylum == "Tracheophyta")

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

GBIF.2022$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$taxonID <- summary$ID[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$kingdom <- summary$Kingdom[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$phylum <- summary$Phylum[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$class <- summary$Class[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$order <- summary$Order[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$suborder <- summary$Suborder[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$superfamily <- summary$Superfamily[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$family <- summary$Family[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$genus <- summary$Genus[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$specificEpithet <- summary$Species[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$hybrid <- summary$Hybrid[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$subspecies <- summary$Subspecies[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$variety <- summary$Variety[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$establishmentMeans <- summary$Origin[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$provincialStatus <- summary$Provincial.Status[match(unlist(GBIF.2022$scientificName), summary$Taxon)]
GBIF.2022$nationalStatus <- summary$National.Status[match(unlist(GBIF.2022$scientificName), summary$Taxon)]

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

GBIF.2022.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$taxonID <- summary$ID[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$class <- summary$Class[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$order <- summary$Order[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$family <- summary$Family[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$genus <- summary$Genus[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$variety <- summary$Variety[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
GBIF.2022.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(GBIF.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

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



# Read data from LGL HSBRI Application Appendices (2020)

LGL.2020 <- read.csv("../../records/digitized/DwC/LGL_plant_records_2020-07-01_DwC.csv")

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

LGL.2020$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$taxonID <- summary$ID[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$kingdom <- summary$Kingdom[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$phylum <- summary$Phylum[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$class <- summary$Class[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$order <- summary$Order[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$suborder <- summary$Suborder[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$superfamily <- summary$Superfamily[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$family <- summary$Family[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$genus <- summary$Genus[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$specificEpithet <- summary$Species[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$hybrid <- summary$Hybrid[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$subspecies <- summary$Subspecies[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$variety <- summary$Variety[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$establishmentMeans <- summary$Origin[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$provincialStatus <- summary$Provincial.Status[match(unlist(LGL.2020$scientificName), summary$Taxon)]
LGL.2020$nationalStatus <- summary$National.Status[match(unlist(LGL.2020$scientificName), summary$Taxon)]

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

LGL.2020.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$taxonID <- summary$ID[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$class <- summary$Class[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$order <- summary$Order[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$family <- summary$Family[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$genus <- summary$Genus[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$variety <- summary$Variety[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
LGL.2020.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(LGL.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

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



# Read Nick Page, Raincoast Applied Ecology, vegetation assessment of Squamish Estuary

Page.2004 <- read.csv("../../records/digitized/DwC/Nick_Page_2004_SRWS_Squamish_Estuary_vegetation_assessment_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Page.2004)))
names(data.frame) <- DwCFields

data.frame[names(Page.2004)] <- Page.2004

Page.2004 <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "LGL2020:" 
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

Page.2004$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$taxonID <- summary$ID[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$kingdom <- summary$Kingdom[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$phylum <- summary$Phylum[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$class <- summary$Class[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$order <- summary$Order[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$suborder <- summary$Suborder[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$superfamily <- summary$Superfamily[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$family <- summary$Family[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$genus <- summary$Genus[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$specificEpithet <- summary$Species[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$hybrid <- summary$Hybrid[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$subspecies <- summary$Subspecies[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$variety <- summary$Variety[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$establishmentMeans <- summary$Origin[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$provincialStatus <- summary$Provincial.Status[match(unlist(Page.2004$scientificName), summary$Taxon)]
Page.2004$nationalStatus <- summary$National.Status[match(unlist(Page.2004$scientificName), summary$Taxon)]

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

Page.2004.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$class <- summary$Class[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$order <- summary$Order[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$family <- summary$Family[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$genus <- summary$Genus[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$variety <- summary$Variety[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Page.2004.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Page.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

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

unmatched.vascular.plant.records$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$kingdom <- summary$Kingdom[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$phylum <- summary$Phylum[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$class <- summary$Class[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$order <- summary$Order[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$suborder <- summary$Suborder[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$superfamily <- summary$Superfamily[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$family <- summary$Family[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$genus <- summary$Genus[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]
unmatched.vascular.plant.records$specificEpithet <- summary$Species[match(unlist(unmatched.vascular.plant.records$scientificName), summary$Species)]

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


