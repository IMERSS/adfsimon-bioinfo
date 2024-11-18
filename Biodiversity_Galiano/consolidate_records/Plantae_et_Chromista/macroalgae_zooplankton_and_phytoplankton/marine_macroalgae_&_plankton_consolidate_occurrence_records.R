# Script to consolidate records of Galiano Island's macroalgae and phytoplankton diversity

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Plantae_et_Chromista/marine_algae_and_protozoa/summaries/Galiano_marine_algae_and_protozoa_review_summary_reviewed_2024-11-17.csv")

# Temporarily assign pseudo-DWC fields until system is transitioned to new school reporting methods

names(summary) <- c("scientificName","scientificNameAuthorship","subtaxonAuthorship","commonName","kingdom","phylum","subphylum",
                    "superclass","class","subclass","superorder","order","suborder","superfamily","family","subfamily","tribe",
                    "genus","specificEpithet","hybrid","subspecies","variety","establishmentMeans","provincialStatus","nationalStatus",    
                    "reportingStatus","observation","firstReported","firstReportedBy","Collection.List","firstReportedCollectionNumber",
                    "firstReportedGBIF","firstObservediNat","firstObservedBy","firstObservedID","notes","ID","statsCode")

# Note in Sept. 2023 this summary was revised to incorporate critical feedback from Sandra Lindstrom;
# multiple taxa have been effectively removed from the summary which has resulted in discrepancies w
# taxa reported in the various data sources aggregated by this script. These discrepancies are noted below as 
# mismatched taxa for each data source.

# Create vector of DarwinCore fields for aggregating records

DwCFields <- c('scientificName','scientificNameAuthorship','taxonID','kingdom','phylum','subphylum','class',
               'order','suborder','infraorder','superfamily','family','genus','subgenus','specificEpithet',
               'hybrid','subspecies','variety','form','infraspecificEpithet','taxonRank','institutionCode',
               'collectionCode','catalogNumber','datasetName','occurrenceID','materialSampleID','associatedSequences',
               'identifiedWith','recordedBy','recordNumber',
               'fieldNumber','eventDate','year','month','day','basisOfRecord','locality','locationRemarks','island',
               'stateProvince','country','countryCode','decimalLatitude','decimalLongitude','coordinateUncertaintyInMeters',
               'georeferencedBy','georeferenceVerificationStatus','georeferenceProtocol','georeferenceRemarks',
               'habitat','verbatimDepth','verbatimElevation','occurrenceStatus','samplingProtocol','occurrenceRemarks',
               'individualCount','sex','establishmentMeans','provincialStatus','nationalStatus','identifiedBy',
               'identificationQualifier','identificationRemarks','previousIdentifications','bibliographicCitation',
               'associatedReferences')

# Consolidate records

# Sources (5/10 added):

# BioBlitz 2023 records (Illumina sequencing data) - ! Added (MW to review)
# BOLD records 2021 - ! Added, but needs updating for 2024
# CPNWH records 2022 - ! Added, needs to be updated for 2024 (harmonize with GBIF)
# iNaturalist observations 2024 - ! Added, but needs update (esp. w. focus on red coralline algae)
# PMLS Records 2021 - ! Added (no update required)
# Sandra Lindstrom BioBlitz collections 2023 - ! Added
# Sanger sequence data - Hakai to add (harmonize with BOLD records?)
# Webber et al. 2020 Zostera epiphytes - ! Not yet added!
# Webber et al. 2022 Miner's Bay samples (Illumina sequencing data) - ! Added (but consider excluding given the paucity of data from outside the study area)
# Webber et al. 2022-2023 General Plankton Samples (Illumina sequencing data) - ! Not yet added!
# Webber et al. 2023a Sanger sequencing of clones - ! Not yet added!



########################################
# Bioblitz 2023 Illumina sequence data #
########################################

BioBlitz.2023 <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_rbcL_share_MAL_Galiano_BioBlitz_2023_AS_MW_AS.csv")

Webber.et.al.2024.metadata <- read.csv("../../records/digitized/Webber_et_al_2024_metadata.csv")

# Transform matrix into long-form dataframe and convert recordNumbers to match metadata

BioBlitz.2023 <- BioBlitz.2023 %>%
  pivot_longer(
    cols = starts_with("GBB"),       
    names_to = "materialSampleID",             
  ) %>%
  distinct() %>%
  mutate(materialSampleID = gsub("\\.", "-", materialSampleID))

# Assign scientificName based on resolved names

BioBlitz.2023$scientificName  <- BioBlitz.2023$resolved_taxon

# Assign sequence data to Dwc field associatedSequences

BioBlitz.2023$associatedSequences <- BioBlitz.2023$rbcL_sequence
BioBlitz.2023$identifiedWith <- "rbcL"

# Assign unique occurrenceIDs

unique.prefix <- "GBB2023:"
unique.suffix <- 1:nrow(BioBlitz.2023)

BioBlitz.2023$occurrenceID <- paste(unique.prefix,unique.suffix, sep = "")

# Assign metadata based on sample IDs

BioBlitz.2023$eventDate  <- Webber.et.al.2024.metadata$eventDate[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$recordedBy  <- Webber.et.al.2024.metadata$recordedBy[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$locality <- Webber.et.al.2024.metadata$locality[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$decimalLatitude <- Webber.et.al.2024.metadata$decimalLatitude[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$decimalLongitude <- Webber.et.al.2024.metadata$decimalLongitude[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$coordinateUncertaintyInMeters <- Webber.et.al.2024.metadata$coordinateUncertaintyInMeters[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$georeferencedBy <- Webber.et.al.2024.metadata$georeferencedBy[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$habitat <- Webber.et.al.2024.metadata$habitat[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
BioBlitz.2023$verbatimDepth <- Webber.et.al.2024.metadata$verbatimDepth[match(unlist(BioBlitz.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(BioBlitz.2023)))
names(data.frame) <- DwCFields

data.frame[names(BioBlitz.2023)] <- BioBlitz.2023

BioBlitz.2023 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

BioBlitz.2023$datasetName <- "Galiano BioBlitz 2023"
BioBlitz.2023$georeferenceProtocol <- "Coordinates mapped based on precise locations of BioBlitz sampling sites"
BioBlitz.2023$countryCode <- "CA"
BioBlitz.2023$country <- "Canada"
BioBlitz.2023$stateProvince <- "British Columbia"
BioBlitz.2023$island <- "Galiano Island"
BioBlitz.2023$basisOfRecord <- "MaterialSample"

# Merge with summary to standardize names and taxon metadata

BioBlitz.2023$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$taxonID <- summary$ID[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$kingdom <- summary$kingdom[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$phylum <- summary$phylum[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$subphylum <- summary$subphylum[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$class <- summary$class[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$order <- summary$order[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$suborder <- summary$suborder[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$superfamily <- summary$superfamily[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$family <- summary$family[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$genus <- summary$genus[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$specificEpithet <- summary$specificEpithet[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$hybrid <- summary$hybrid[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$subspecies <- summary$subspecies[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$variety <- summary$variety[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$establishmentMeans <- summary$establishmentMeans[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$provincialStatus <- summary$provincialStatus[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]
BioBlitz.2023$nationalStatus <- summary$nationalStatus[match(unlist(BioBlitz.2023$scientificName), summary$scientificName)]

# Unmatched records

BioBlitz.2023.names.unmatched <- BioBlitz.2023[is.na(BioBlitz.2023$taxonID),]

# Matched records

BioBlitz.2023.names.matched <- anti_join(BioBlitz.2023,BioBlitz.2023.names.unmatched)

# Confirm all records are represented 

nrow(BioBlitz.2023)
nrow(BioBlitz.2023.names.matched)
nrow(BioBlitz.2023.names.unmatched)
nrow(BioBlitz.2023.names.matched)+nrow(BioBlitz.2023.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

BioBlitz.2023.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

BioBlitz.2023.names.unmatched.matched <- BioBlitz.2023.names.unmatched

BioBlitz.2023.names.unmatched.matched$scientificNameTemp <- BioBlitz.2023.key$Matched.Taxon[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificName), BioBlitz.2023.key$Taxon)]

# Add values based on newly matched name

BioBlitz.2023.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$taxonID <- summary$ID[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$phylum <- summary$phylum[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$class <- summary$class[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$order <- summary$order[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$suborder <- summary$suborder[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$family <- summary$family[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$genus <- summary$genus[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$variety <- summary$variety[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BioBlitz.2023.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(BioBlitz.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

BioBlitz.2023.names.unmatched.unmatched <- BioBlitz.2023.names.unmatched.matched[is.na(BioBlitz.2023.names.unmatched.matched$taxonID),]

BioBlitz.2023.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

BioBlitz.2023.names.unmatched.matched$scientificName <- BioBlitz.2023.names.unmatched.matched$scientificNameTemp

BioBlitz.2023.names.unmatched.matched$scientificNameTemp <- NULL

BioBlitz.2023.names.unmatched.matched <- BioBlitz.2023.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(BioBlitz.2023)
nrow(BioBlitz.2023.names.matched)
nrow(BioBlitz.2023.names.unmatched)
nrow(BioBlitz.2023.names.unmatched.matched)
nrow(BioBlitz.2023.names.unmatched.unmatched)
nrow(BioBlitz.2023.names.matched)+nrow(BioBlitz.2023.names.unmatched.matched)+nrow(BioBlitz.2023.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(BioBlitz.2023.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- BioBlitz.2023.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(BioBlitz.2023.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

BioBlitz.2023.records <- rbind(BioBlitz.2023.names.matched,BioBlitz.2023.names.unmatched.matched)

# Set date formatting consistent with other data frames

BioBlitz.2023.records$eventDate <- as.Date(BioBlitz.2023.records$eventDate)

# Compare records in and out

nrow(BioBlitz.2023) - nrow(BioBlitz.2023.records)
nrow(BioBlitz.2023)
nrow(BioBlitz.2023.records) # records removed all indeterminate wrt to curated summary

# Start record of unmatched names

unmatched.algae.records <- BioBlitz.2023.names.unmatched.unmatched

unmatched.algae.records



#####################
# BOLD records 2021 #
#####################

# TO DO: Update based on consolidated BOLD Records

BOLD.2021 <- read.csv("../../records/digitized/DarwinCore/BOLD_marine_algae_2021-11-25_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(BOLD.2021)))
names(data.frame) <- DwCFields

data.frame[names(BOLD.2021)] <- BOLD.2021

BOLD.2021 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

BOLD.2021$datasetName <- "BOLD"
BOLD.2021$stateProvince <- "British Columbia"
BOLD.2021$country <- "Canada"
BOLD.2021$island <- "Galiano Island"

# Merge with summary to standardize names and taxon metadata

BOLD.2021$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$taxonID <- summary$ID[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$kingdom <- summary$kingdom[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$phylum <- summary$phylum[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$subphylum <- summary$subphylum[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$class <- summary$class[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$order <- summary$order[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$suborder <- summary$suborder[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$superfamily <- summary$superfamily[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$family <- summary$family[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$genus <- summary$genus[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$specificEpithet <- summary$specificEpithet[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$hybrid <- summary$hybrid[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$subspecies <- summary$subspecies[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$variety <- summary$variety[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$establishmentMeans <- summary$establishmentMeans[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$provincialStatus <- summary$provincialStatus[match(unlist(BOLD.2021$scientificName), summary$scientificName)]
BOLD.2021$nationalStatus <- summary$nationalStatus[match(unlist(BOLD.2021$scientificName), summary$scientificName)]

# Unmatched records

BOLD.2021.names.unmatched <- BOLD.2021[is.na(BOLD.2021$taxonID),]

# Matched records

BOLD.2021.names.matched <- anti_join(BOLD.2021,BOLD.2021.names.unmatched)

# Confirm all records are represented 

nrow(BOLD.2021)
nrow(BOLD.2021.names.matched)
nrow(BOLD.2021.names.unmatched)
nrow(BOLD.2021.names.matched)+nrow(BOLD.2021.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

BOLD.2021.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

BOLD.2021.names.unmatched.matched <- BOLD.2021.names.unmatched

BOLD.2021.names.unmatched.matched$scientificNameTemp <- BOLD.2021.key$Matched.Taxon[match(unlist(BOLD.2021.names.unmatched.matched$scientificName), BOLD.2021.key$Taxon)]

# Add values based on newly matched name

BOLD.2021.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$taxonID <- summary$ID[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$phylum <- summary$phylum[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$class <- summary$class[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$order <- summary$order[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$suborder <- summary$suborder[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$family <- summary$family[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$genus <- summary$genus[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$variety <- summary$variety[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
BOLD.2021.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(BOLD.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

BOLD.2021.names.unmatched.unmatched <- BOLD.2021.names.unmatched.matched[is.na(BOLD.2021.names.unmatched.matched$taxonID),]

BOLD.2021.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

BOLD.2021.names.unmatched.matched$scientificName <- BOLD.2021.names.unmatched.matched$scientificNameTemp

BOLD.2021.names.unmatched.matched$scientificNameTemp <- NULL

BOLD.2021.names.unmatched.matched <- BOLD.2021.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(BOLD.2021)
nrow(BOLD.2021.names.matched)
nrow(BOLD.2021.names.unmatched)
nrow(BOLD.2021.names.unmatched.matched)
nrow(BOLD.2021.names.unmatched.unmatched)
nrow(BOLD.2021.names.matched)+nrow(BOLD.2021.names.unmatched.matched)+nrow(BOLD.2021.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(BOLD.2021.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- BOLD.2021.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(BOLD.2021.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

BOLD.2021.records <- rbind(BOLD.2021.names.matched,BOLD.2021.names.unmatched.matched)

# Set date formatting consistent with other data frames

BOLD.2021.records$eventDate <- as.Date(BOLD.2021.records$eventDate)

# Compare records in and out

nrow(BOLD.2021) - nrow(BOLD.2021.records)

nrow(BOLD.2021)
nrow(BOLD.2021.records)

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,BOLD.2021.names.unmatched.unmatched)

unmatched.algae.records



##########################################################
# Consortium of Pacific Northwest Herbaria Algae Records #
##########################################################

# TO DO: Update!

CPNWH.2021 <- read.csv("../../records/digitized/DarwinCore/CPNWH_Galiano_Island_macroalgae_2024-01-22_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(CPNWH.2021)))
names(data.frame) <- DwCFields

data.frame[names(CPNWH.2021)] <- CPNWH.2021

CPNWH.2021 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

CPNWH.2021$countryCode <- "CA"
CPNWH.2021$island <- "Galiano Island"

# Merge with summary to standardize names and taxon metadata

CPNWH.2021$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$taxonID <- summary$ID[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$kingdom <- summary$kingdom[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$phylum <- summary$phylum[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$subphylum <- summary$subphylum[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$class <- summary$class[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$order <- summary$order[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$suborder <- summary$suborder[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$superfamily <- summary$superfamily[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$family <- summary$family[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$genus <- summary$genus[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$specificEpithet <- summary$specificEpithet[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$hybrid <- summary$hybrid[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$subspecies <- summary$subspecies[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$variety <- summary$variety[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$establishmentMeans <- summary$establishmentMeans[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$provincialStatus <- summary$provincialStatus[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]
CPNWH.2021$nationalStatus <- summary$nationalStatus[match(unlist(CPNWH.2021$scientificName), summary$scientificName)]

# Unmatched records

CPNWH.2021.names.unmatched <- CPNWH.2021[is.na(CPNWH.2021$taxonID),]

# Matched records

CPNWH.2021.names.matched <- anti_join(CPNWH.2021,CPNWH.2021.names.unmatched)

# Confirm all records are represented 

nrow(CPNWH.2021)
nrow(CPNWH.2021.names.matched)
nrow(CPNWH.2021.names.unmatched)
nrow(CPNWH.2021.names.matched)+nrow(CPNWH.2021.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

CPNWH.2021.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

CPNWH.2021.names.unmatched.matched <- CPNWH.2021.names.unmatched

CPNWH.2021.names.unmatched.matched$scientificNameTemp <- CPNWH.2021.key$Matched.Taxon[match(unlist(CPNWH.2021.names.unmatched.matched$scientificName), CPNWH.2021.key$Taxon)]

# Add values based on newly matched name

CPNWH.2021.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$taxonID <- summary$ID[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$phylum <- summary$phylum[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$class <- summary$class[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$order <- summary$order[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$suborder <- summary$suborder[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$family <- summary$family[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$genus <- summary$genus[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$variety <- summary$variety[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
CPNWH.2021.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(CPNWH.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

CPNWH.2021.names.unmatched.unmatched <- CPNWH.2021.names.unmatched.matched[is.na(CPNWH.2021.names.unmatched.matched$taxonID),]

CPNWH.2021.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

CPNWH.2021.names.unmatched.matched$scientificName <- CPNWH.2021.names.unmatched.matched$scientificNameTemp

CPNWH.2021.names.unmatched.matched$scientificNameTemp <- NULL

CPNWH.2021.names.unmatched.matched <- CPNWH.2021.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(CPNWH.2021)
nrow(CPNWH.2021.names.matched)
nrow(CPNWH.2021.names.unmatched)
nrow(CPNWH.2021.names.unmatched.matched)
nrow(CPNWH.2021.names.unmatched.unmatched)
nrow(CPNWH.2021.names.matched)+nrow(CPNWH.2021.names.unmatched.matched)+nrow(CPNWH.2021.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(CPNWH.2021.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- CPNWH.2021.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(CPNWH.2021.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

CPNWH.2021.records <- rbind(CPNWH.2021.names.matched,CPNWH.2021.names.unmatched.matched)

# Set date formatting consistent with other data frames

CPNWH.2021.records$eventDate <- as.Date(CPNWH.2021.records$eventDate)

# Compare records in and out

nrow(CPNWH.2021) - nrow(CPNWH.2021.records)

nrow(CPNWH.2021)
nrow(CPNWH.2021.records) # 13 records omitted; those determined only to family

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,CPNWH.2021.names.unmatched.unmatched)

unmatched.algae.records



####################
# iNaturalist data #
####################

iNaturalist.observations <- read.csv("../../../parse_records/outputs/iNat_obs_marine_algae_and_protozoa.csv")

# TO DO: Update this script  once data pipelines are standardized.
# Names do not need substitution with the current (last) version of the dataset

# Substitute iNaturalist usernames where actual observer names are missing

# iNaturalist.observations.nameless <- iNaturalist.observations %>% 
#   filter(Recorded.by != "")

# iNaturalist.observations.names <- anti_join(iNaturalist.observations,iNaturalist.observations.nameless)

# iNaturalist.observations.nameless$recorded.By <- iNaturalist.observations.nameless$user_login

# iNaturalist.observations <- rbind(iNaturalist.observations.nameless,iNaturalist.observations.names)

# Drop observations of taxa that are not identified to genus at least

# iNaturalist.observations <- subset(iNaturalist.observations, Genus != "")

iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "phylum")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "class")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "subclass")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "order")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "family")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "subfamily")
iNaturalist.observations <- subset(iNaturalist.observations, taxon_rank != "tribe")

# Add DwC fields to iNaturalist catalog to facilitate joins with DwC dataframe template

iNaturalist.observations <- iNaturalist.observations %>% rename(scientificName = scientific_name)
iNaturalist.observations <- iNaturalist.observations %>% rename(eventDate = observed_on)
iNaturalist.observations <- iNaturalist.observations %>% rename(recordedBy = user_name)
iNaturalist.observations <- iNaturalist.observations %>%
  rename(occurrenceID = id) %>%
  mutate(occurrenceID = paste0("iNat:", occurrenceID))
iNaturalist.observations <- iNaturalist.observations %>% rename(decimalLatitude = latitude)
iNaturalist.observations <- iNaturalist.observations %>% rename(decimalLongitude = longitude)

# Substitute iNaturalist taxon names with names from curated summary based on taxonID

# iNaturalist.observations$swappedNames <- summary$scientificName[match(unlist(iNaturalist.observations$iNaturalist.taxon.ID), summary$ID)]

# iNaturalist.observations.swapped.names <- iNaturalist.observations %>% drop_na(swappedNames)

# iNaturalist.observations.unswapped.names <- anti_join(iNaturalist.observations,iNaturalist.observations.swapped.names)

# iNaturalist.observations.swapped.names$scientificName <- iNaturalist.observations.swapped.names$swappedNames

# iNaturalist.observations <- rbind(iNaturalist.observations.swapped.names,iNaturalist.observations.unswapped.names)

# iNaturalist.observations$swappedNames <- NULL

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(iNaturalist.observations)))
names(data.frame) <- DwCFields

data.frame[names(iNaturalist.observations)] <- iNaturalist.observations

iNaturalist.observations <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

iNaturalist.observations$stateProvince <- "British Columbia"
iNaturalist.observations$island <- "Galiano Island"
iNaturalist.observations$country <- "Canada"
iNaturalist.observations$countryCode <- "CA"
iNaturalist.observations$basisOfRecord <- "HumanObservation"
iNaturalist.observations$datasetName <- "iNaturalist Observations"
iNaturalist.observations$institutionCode <- "iNaturalist"

# Merge with summary to standardize names and taxon metadata

iNaturalist.observations$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$taxonID <- summary$ID[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$kingdom <- summary$kingdom[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$phylum <- summary$phylum[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$subphylum <- summary$subphylum[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$class <- summary$class[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$order <- summary$order[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$suborder <- summary$suborder[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$superfamily <- summary$superfamily[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$family <- summary$family[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$genus <- summary$genus[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$specificEpithet <- summary$specificEpithet[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$hybrid <- summary$hybrid[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$subspecies <- summary$subspecies[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$variety <- summary$variety[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$establishmentMeans <- summary$establishmentMeans[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$provincialStatus <- summary$provincialStatus[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]
iNaturalist.observations$nationalStatus <- summary$nationalStatus[match(unlist(iNaturalist.observations$scientificName), summary$scientificName)]

# Unmatched records

iNaturalist.observations.names.unmatched <- iNaturalist.observations[is.na(iNaturalist.observations$taxonID),]

# Matched records

iNaturalist.observations.names.matched <- anti_join(iNaturalist.observations,iNaturalist.observations.names.unmatched)

# Confirm all records are represented 

nrow(iNaturalist.observations)
nrow(iNaturalist.observations.names.matched)
nrow(iNaturalist.observations.names.unmatched)
nrow(iNaturalist.observations.names.matched)+nrow(iNaturalist.observations.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

iNaturalist.observations.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

iNaturalist.observations.names.unmatched.matched <- iNaturalist.observations.names.unmatched

iNaturalist.observations.names.unmatched.matched$scientificNameTemp <- iNaturalist.observations.key$Matched.Taxon[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificName), iNaturalist.observations.key$Taxon)]

# Add values based on newly matched name

iNaturalist.observations.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$taxonID <- summary$ID[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$phylum <- summary$phylum[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$class <- summary$class[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$order <- summary$order[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$suborder <- summary$suborder[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$family <- summary$family[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$genus <- summary$genus[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$variety <- summary$variety[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
iNaturalist.observations.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

iNaturalist.observations.names.unmatched.unmatched <- iNaturalist.observations.names.unmatched.matched[is.na(iNaturalist.observations.names.unmatched.matched$taxonID),]

iNaturalist.observations.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

iNaturalist.observations.names.unmatched.matched$scientificName <- iNaturalist.observations.names.unmatched.matched$scientificNameTemp

iNaturalist.observations.names.unmatched.matched$scientificNameTemp <- NULL

iNaturalist.observations.names.unmatched.matched <- iNaturalist.observations.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(iNaturalist.observations)
nrow(iNaturalist.observations.names.matched)
nrow(iNaturalist.observations.names.unmatched)
nrow(iNaturalist.observations.names.unmatched.matched)
nrow(iNaturalist.observations.names.unmatched.unmatched)
nrow(iNaturalist.observations.names.matched)+nrow(iNaturalist.observations.names.unmatched.matched)+nrow(iNaturalist.observations.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(iNaturalist.observations.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- iNaturalist.observations.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(iNaturalist.observations.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

names(iNaturalist.observations.names.matched)
names(iNaturalist.observations.names.unmatched.matched)

iNaturalist.records <- rbind(iNaturalist.observations.names.matched,iNaturalist.observations.names.unmatched.matched)

# Set date formatting consistent with other data frames

iNaturalist.records$eventDate <- as.Date(iNaturalist.records$eventDate)

# Compare records in and out

nrow(iNaturalist.observations)
nrow(iNaturalist.records) 
nrow(iNaturalist.observations) - nrow(iNaturalist.records)  # 673 records omitted: all species resolved only to genus, or otherwise suprious records under review in consultation with Sandra

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,iNaturalist.observations.names.unmatched.unmatched)

unmatched.algae.records



############################################
# Sandra Lindstrom's 2023 BioBlitz records #
############################################

#  TO DO: Review. This code is supposedly not complete?

Lindstrom.2023 <- read.csv("../../records/digitized/DarwinCore/Sandra_Lindstrom_Galiano_seaweed_collections_May_2023_Jan_2024_updates_DwC.csv")

Lindstrom.2023.genetic <- read.csv("../../records/digitized/DarwinCore/Sandra_Lindstrom_Galiano_Seaweed_2023_Genetic_Data_Reporting_DwC.csv")

# Create DarwinCore dataframe template (for both genetic and catalog datasets)

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Lindstrom.2023)))
names(data.frame) <- DwCFields

data.frame[names(Lindstrom.2023)] <- Lindstrom.2023

Lindstrom.2023 <- select(data.frame, c(1:length(DwCFields)))

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Lindstrom.2023.genetic)))
names(data.frame) <- DwCFields

data.frame[names(Lindstrom.2023.genetic)] <- Lindstrom.2023.genetic

Lindstrom.2023.genetic <- select(data.frame, c(1:length(DwCFields)))

## Merge catalogs (redundancy already assessed; only unique records represented in genetic datasets)

Lindstrom.2023 <- rbind(Lindstrom.2023, Lindstrom.2023.genetic)

# Add metadata

Lindstrom.2023$datasetName <- "Lindstrom 2023"
Lindstrom.2023$stateProvince <- "British Columbia"
Lindstrom.2023$country <- "Canada"
Lindstrom.2023$island <- "Galiano Island"
Lindstrom.2023$countryCode <- "CA"

# Merge with summary to standardize names and taxon metadata

Lindstrom.2023$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$taxonID <- summary$ID[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$kingdom <- summary$kingdom[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$phylum <- summary$phylum[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$subphylum <- summary$subphylum[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$class <- summary$class[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$order <- summary$order[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$suborder <- summary$suborder[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$superfamily <- summary$superfamily[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$family <- summary$family[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$genus <- summary$genus[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$specificEpithet <- summary$specificEpithet[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$hybrid <- summary$hybrid[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$subspecies <- summary$subspecies[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$variety <- summary$variety[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$establishmentMeans <- summary$establishmentMeans[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$provincialStatus <- summary$provincialStatus[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]
Lindstrom.2023$nationalStatus <- summary$nationalStatus[match(unlist(Lindstrom.2023$scientificName), summary$scientificName)]

# Unmatched records

Lindstrom.2023.names.unmatched <- Lindstrom.2023[is.na(Lindstrom.2023$taxonID),]

# Matched records

Lindstrom.2023.names.matched <- anti_join(Lindstrom.2023,Lindstrom.2023.names.unmatched)

# Confirm all records are represented 

nrow(Lindstrom.2023)
nrow(Lindstrom.2023.names.matched)
nrow(Lindstrom.2023.names.unmatched)
nrow(Lindstrom.2023.names.matched)+nrow(Lindstrom.2023.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Lindstrom.2023.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

Lindstrom.2023.names.unmatched.matched <- Lindstrom.2023.names.unmatched

Lindstrom.2023.names.unmatched.matched$scientificNameTemp <- Lindstrom.2023.key$Matched.Taxon[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificName), Lindstrom.2023.key$Taxon)]

# Add values based on newly matched name

Lindstrom.2023.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$phylum <- summary$phylum[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$class <- summary$class[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$order <- summary$order[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$suborder <- summary$suborder[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$family <- summary$family[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$genus <- summary$genus[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$variety <- summary$variety[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Lindstrom.2023.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(Lindstrom.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

Lindstrom.2023.names.unmatched.unmatched <- Lindstrom.2023.names.unmatched.matched[is.na(Lindstrom.2023.names.unmatched.matched$taxonID),]

Lindstrom.2023.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Lindstrom.2023.names.unmatched.matched$scientificName <- Lindstrom.2023.names.unmatched.matched$scientificNameTemp

Lindstrom.2023.names.unmatched.matched$scientificNameTemp <- NULL

Lindstrom.2023.names.unmatched.matched <- Lindstrom.2023.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Lindstrom.2023)
nrow(Lindstrom.2023.names.matched)
nrow(Lindstrom.2023.names.unmatched)
nrow(Lindstrom.2023.names.unmatched.matched)
nrow(Lindstrom.2023.names.unmatched.unmatched)
nrow(Lindstrom.2023.names.matched)+nrow(Lindstrom.2023.names.unmatched.matched)+nrow(Lindstrom.2023.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Lindstrom.2023.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Lindstrom.2023.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Lindstrom.2023.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

Lindstrom.2023.records <- rbind(Lindstrom.2023.names.matched,Lindstrom.2023.names.unmatched.matched)

# Set date formatting consistent with other data frames

Lindstrom.2023.records$eventDate <- as.Date(Lindstrom.2023.records$eventDate)

# Compare records in and out

nrow(Lindstrom.2023) - nrow(Lindstrom.2023.records)

nrow(Lindstrom.2023)
nrow(Lindstrom.2023.records) # few records omitted; all indeterminate, but note confer with Sandra re: Callophyllis cf. variforma
# Callophyllis variforma is reported based on genetic data by Gary Saunders et al. 

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,Lindstrom.2023.names.unmatched.unmatched)

unmatched.algae.records



#####################
# PMLS 2021 records #
#####################

PMLS.2021 <- read.csv("../../records/digitized/DarwinCore/PMLS_Plantae_Records_Galiano_2021-07-27_DwC.csv")

# Filter macroalgae (this step may be necessary for a future version of this list but not the current version which is already pruned)

PMLS.2021 <- PMLS.2021 %>% filter(phylum == "Chlorophyta" | phylum == 'Rhodophyta' | phylum == 'Ochrophyta')

# Create unique identifiers for observations

unique.prefix <- "PMLS2021:"
unique.suffix <- 1:nrow(PMLS.2021)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(PMLS.2021)))
names(data.frame) <- DwCFields

data.frame[names(PMLS.2021)] <- PMLS.2021

PMLS.2021 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

PMLS.2021$institutionCode <- "PMLS"
PMLS.2021$datasetName <- "Pacific Marine Life Survey Dive Records"
PMLS.2021$occurrenceID <- paste(unique.prefix,unique.suffix, sep = "")
PMLS.2021$coordinateUncertaintyInMeters <- 100
PMLS.2021$island <- "Galiano Island"
PMLS.2021$stateProvince <- "British Columbia"
PMLS.2021$country <- "Canada"
PMLS.2021$countryCode <- "CA"
PMLS.2021$basisOfRecord <- "HumanObservation"

# Merge with summary to standardize names and taxon metadata

PMLS.2021$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$taxonID <- summary$ID[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$kingdom <- summary$kingdom[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$phylum <- summary$phylum[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$subphylum <- summary$subphylum[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$class <- summary$class[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$order <- summary$order[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$suborder <- summary$suborder[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$superfamily <- summary$superfamily[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$family <- summary$family[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$genus <- summary$genus[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$specificEpithet <- summary$specificEpithet[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$hybrid <- summary$hybrid[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$subspecies <- summary$subspecies[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$variety <- summary$variety[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$establishmentMeans <- summary$establishmentMeans[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$provincialStatus <- summary$provincialStatus[match(unlist(PMLS.2021$scientificName), summary$scientificName)]
PMLS.2021$nationalStatus <- summary$nationalStatus[match(unlist(PMLS.2021$scientificName), summary$scientificName)]

# Unmatched records

PMLS.2021.names.unmatched <- PMLS.2021[is.na(PMLS.2021$taxonID),]

# Matched records

PMLS.2021.names.matched <- anti_join(PMLS.2021,PMLS.2021.names.unmatched)

# Confirm all records are represented 

nrow(PMLS.2021)
nrow(PMLS.2021.names.matched)
nrow(PMLS.2021.names.unmatched)
nrow(PMLS.2021.names.matched)+nrow(PMLS.2021.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

PMLS.2021.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

PMLS.2021.names.unmatched.matched <- PMLS.2021.names.unmatched

PMLS.2021.names.unmatched.matched$scientificNameTemp <- PMLS.2021.key$Matched.Taxon[match(unlist(PMLS.2021.names.unmatched.matched$scientificName), PMLS.2021.key$Taxon)]

# Add values based on newly matched name

PMLS.2021.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$taxonID <- summary$ID[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$phylum <- summary$phylum[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$class <- summary$class[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$order <- summary$order[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$suborder <- summary$suborder[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$family <- summary$family[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$genus <- summary$genus[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$variety <- summary$variety[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
PMLS.2021.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(PMLS.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

PMLS.2021.names.unmatched.unmatched <- PMLS.2021.names.unmatched.matched[is.na(PMLS.2021.names.unmatched.matched$taxonID),]

PMLS.2021.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

PMLS.2021.names.unmatched.matched$scientificName <- PMLS.2021.names.unmatched.matched$scientificNameTemp

PMLS.2021.names.unmatched.matched$scientificNameTemp <- NULL

PMLS.2021.names.unmatched.matched <- PMLS.2021.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(PMLS.2021)
nrow(PMLS.2021.names.matched)
nrow(PMLS.2021.names.unmatched)
nrow(PMLS.2021.names.unmatched.matched)
nrow(PMLS.2021.names.unmatched.unmatched)
nrow(PMLS.2021.names.matched)+nrow(PMLS.2021.names.unmatched.matched)+nrow(PMLS.2021.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(PMLS.2021.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- PMLS.2021.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(PMLS.2021.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

PMLS.2021.records <- rbind(PMLS.2021.names.matched,PMLS.2021.names.unmatched.matched)

# Set date formatting consistent with other data frames

PMLS.2021.records$eventDate <- as.Date(PMLS.2021.records$eventDate)

# Compare records in and out

nrow(PMLS.2021) - nrow(PMLS.2021.records)
nrow(PMLS.2021)
nrow(PMLS.2021.records) # 791 records omitted; all indeterminate with reference to summary

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,PMLS.2021.names.unmatched.unmatched)

unmatched.algae.records



##################################################################
# Webber et al. 2020 records (Eelgrass Illumina sequencing data) #
##################################################################

Webber.et.al.2020.rbcL <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_rbcL_share_MAL_Zostera_AS_MW_AS.csv")
Webber.et.al.2020.18S <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_18S_share_MAL_Zostera_AS_MW_AS.csv")

# Normalize rbcL data to combined with 18S data

# Reduce to unique ASVs (dissolving different samples to a singular sample, to remove ecological metadata)

Webber.et.al.2020.rbcL <- Webber.et.al.2020.rbcL %>%
  distinct(row_names, .keep_all = TRUE)

# Remove matrix fields (ecological metadata)

Webber.et.al.2020.rbcL <- Webber.et.al.2020.rbcL[, !grepl("^X", names(Webber.et.al.2020.rbcL))]

# Add new materialSampleID

Webber.et.al.2020.rbcL$materialSampleID <- "M-rbcL"

# Assign scientificName based on resolved names

Webber.et.al.2020.rbcL$scientificName  <- Webber.et.al.2020.rbcL$resolved_taxon

# Assign sequence data to Dwc field associatedSequences

Webber.et.al.2020.rbcL$associatedSequences <- Webber.et.al.2020.rbcL$rbcL_sequence
Webber.et.al.2020.rbcL$identifiedWith <- "rbcL"

# Normalize dataframe to match same dimensions as 18S sequence data

Webber.et.al.2020.rbcL <- Webber.et.al.2020.rbcL[, -(1:7)]
Webber.et.al.2020.rbcL$value <- NULL

# Normalize 18S data to combined with rbcL data

# Rename 18S_sequence field name to conform with R syntactic rules

colnames(Webber.et.al.2020.18S)[colnames(Webber.et.al.2020.18S) == "X18S_sequence"] <- "sequence_18S"

# Reduce to unique ASVs (dissolving different samples to a singular sample, to remove ecological metadata)

Webber.et.al.2020.18S <- Webber.et.al.2020.18S %>%
  distinct(row_names, .keep_all = TRUE)

# Remove matrix fields (ecological metadata)

Webber.et.al.2020.18S <- Webber.et.al.2020.18S[, !grepl("^X", names(Webber.et.al.2020.18S))]

# Add new materialSampleID

Webber.et.al.2020.18S$materialSampleID <- "M-18S"

# Assign scientificName based on resolved names

Webber.et.al.2020.18S$scientificName  <- Webber.et.al.2020.18S$resolved_taxon

# Assign sequence data to Dwc field associatedSequences

Webber.et.al.2020.18S$associatedSequences <- Webber.et.al.2020.18S$sequence_18S
Webber.et.al.2020.18S$identifiedWith <- "18S"

# Normalize dataframe to match same dimensions as 18S sequence data

Webber.et.al.2020.18S <- Webber.et.al.2020.18S[, -(1:7)]
Webber.et.al.2020.18S$value <- NULL

# Merge 18S and rbcL data

Webber.et.al.2020 <- rbind(Webber.et.al.2020.rbcL,Webber.et.al.2020.18S)

# Assign unique occurrenceIDs

unique.prefix <- "Zostera2020:"
unique.suffix <- 1:nrow(Webber.et.al.2020)

Webber.et.al.2020$occurrenceID <- paste(unique.prefix,unique.suffix, sep = "")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Webber.et.al.2020)))
names(data.frame) <- DwCFields

data.frame[names(Webber.et.al.2020)] <- Webber.et.al.2020

Webber.et.al.2020 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Webber.et.al.2020$datasetName <- "Webber et al. 2020"
Webber.et.al.2020$recordedBy<- "Mark Webber, Siobhan Schenck & Arjan van Asselt"
Webber.et.al.2020$eventDate <- '2020-11-15'
Webber.et.al.2020$decimalLatitude <- 48.900714
Webber.et.al.2020$decimalLongitude <- -123.407906
Webber.et.al.2020$georeferenceProtocol <- "Coordinates mapped based on precise location of study area"
Webber.et.al.2020$coordinateUncertaintyInMeters <- 50
Webber.et.al.2020$countryCode <- "CA"
Webber.et.al.2020$country <- "Canada"
Webber.et.al.2020$stateProvince <- "British Columbia"
Webber.et.al.2020$island <- "Galiano Island"
Webber.et.al.2020$locality <- "Montague Harbour Marine Provincial Park (Hulq'umi'num: 'Sumnuw')"
Webber.et.al.2020$basisOfRecord <- "MaterialSample"

# Merge with summary to standardize names and taxon metadata

Webber.et.al.2020$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$taxonID <- summary$ID[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$phylum <- summary$phylum[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$class <- summary$class[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$order <- summary$order[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$suborder <- summary$suborder[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$family <- summary$family[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$genus <- summary$genus[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$variety <- summary$variety[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]
Webber.et.al.2020$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2020$scientificName), summary$scientificName)]

# Unmatched records

Webber.et.al.2020.names.unmatched <- Webber.et.al.2020[is.na(Webber.et.al.2020$taxonID),]

# Matched records

Webber.et.al.2020.names.matched <- anti_join(Webber.et.al.2020,Webber.et.al.2020.names.unmatched)

# Confirm all records are represented 

nrow(Webber.et.al.2020)
nrow(Webber.et.al.2020.names.matched)
nrow(Webber.et.al.2020.names.unmatched)
nrow(Webber.et.al.2020.names.matched)+nrow(Webber.et.al.2020.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Webber.et.al.2020.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

Webber.et.al.2020.names.unmatched.matched <- Webber.et.al.2020.names.unmatched

Webber.et.al.2020.names.unmatched.matched$scientificNameTemp <- Webber.et.al.2020.key$Matched.Taxon[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificName), Webber.et.al.2020.key$Taxon)]

# Add values based on newly matched name

Webber.et.al.2020.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$phylum <- summary$phylum[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$class <- summary$class[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$order <- summary$order[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$suborder <- summary$suborder[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$family <- summary$family[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$genus <- summary$genus[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$variety <- summary$variety[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2020.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2020.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

Webber.et.al.2020.names.unmatched.unmatched <- Webber.et.al.2020.names.unmatched.matched[is.na(Webber.et.al.2020.names.unmatched.matched$taxonID),]

Webber.et.al.2020.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Webber.et.al.2020.names.unmatched.matched$scientificName <- Webber.et.al.2020.names.unmatched.matched$scientificNameTemp

Webber.et.al.2020.names.unmatched.matched$scientificNameTemp <- NULL

Webber.et.al.2020.names.unmatched.matched <- Webber.et.al.2020.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Webber.et.al.2020)
nrow(Webber.et.al.2020.names.matched)
nrow(Webber.et.al.2020.names.unmatched)
nrow(Webber.et.al.2020.names.unmatched.matched)
nrow(Webber.et.al.2020.names.unmatched.unmatched)
nrow(Webber.et.al.2020.names.matched)+nrow(Webber.et.al.2020.names.unmatched.matched)+nrow(Webber.et.al.2020.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Webber.et.al.2020.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Webber.et.al.2020.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Webber.et.al.2020.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

Webber.et.al.2020.records <- rbind(Webber.et.al.2020.names.matched,Webber.et.al.2020.names.unmatched.matched)

# Set date formatting consistent with other data frames

Webber.et.al.2020.records$eventDate <- as.Date(Webber.et.al.2020.records$eventDate)

# Compare records in and out

nrow(Webber.et.al.2020) - nrow(Webber.et.al.2020.records)
nrow(Webber.et.al.2020)
nrow(Webber.et.al.2020.records) 

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records, Webber.et.al.2020.names.unmatched.unmatched)

unmatched.algae.records



#####################################################################
# Webber et al. 2022 Miner's Bay samples (Illumina sequencing data) #
#####################################################################

Webber.et.al.2022.18S <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_18S_share_MAL_Rhodophyta_Miners_Bay_AS_MW_AS.csv")
Webber.et.al.2022.rbcL <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_rbcL_share_MAL_general_plankton_9 samples_AS_MW_AS.csv")

# Read metadata

Webber.et.al.2024.metadata <- read.csv("../../records/digitized/Webber_et_al_2024_metadata.csv")

# Normalise 18S data so format matches with rbcL data

# Reduce to unique ASVs (dissolving different samples to a singular sample, to remove ecological metadata)

Webber.et.al.2022.18S <- Webber.et.al.2022.18S %>%
  distinct(row_names, .keep_all = TRUE)

# Remove extraneous fields

Webber.et.al.2022.18S$rhodophyta.miners.oct.31.2022 <- NULL

# Normalize materialSampleID to corresond with metadata

Webber.et.al.2022.18S$materialSampleID <- "5-miners-oct-31-2022"

# Assign scientificName based on resolved names (and remove rows with NAs for scientificName)

Webber.et.al.2022.18S$scientificName  <- Webber.et.al.2022.18S$resolved_taxon

Webber.et.al.2022.18S <- Webber.et.al.2022.18S[!is.na(Webber.et.al.2022.18S$scientificName), ]

# Rename 18S_sequence field name to conform with R syntactic rules

colnames(Webber.et.al.2022.18S)[colnames(Webber.et.al.2022.18S) == "X18S_sequence"] <- "sequence_18S"

# Assign sequence data to Dwc field associatedSequences

Webber.et.al.2022.18S$associatedSequences <- Webber.et.al.2022.18S$sequence_18S
Webber.et.al.2022.18S$identifiedWith <- "18S"

# Normalize dataframe to match same dimensions as rbcL sequence data

Webber.et.al.2022.18S <- Webber.et.al.2022.18S[, -(1:7)]

# Normalise rbcL data so format matches with 18S data

# Transform matrix into long-form dataframe and convert recordNumbers to match metadata

Webber.et.al.2022.rbcL <- Webber.et.al.2022.rbcL %>%
  pivot_longer(
    cols = starts_with("X"),       
    names_to = "materialSampleID",             
  ) %>%
  distinct() %>%
  mutate(materialSampleID = gsub("\\.", "-", materialSampleID))

# Fetch only data of relevance

Webber.et.al.2022.rbcL <- Webber.et.al.2022.rbcL %>% filter (materialSampleID == "X5-MBW-oct-31-2022")

# Normalise sample ID to conform with metadata

Webber.et.al.2022.rbcL$materialSampleID <- "5-miners-oct-31-2022"

# Assign scientificName based on resolved names (and remove rows with NAs for scientificName)

Webber.et.al.2022.rbcL$scientificName  <- Webber.et.al.2022.rbcL$resolved_taxon

Webber.et.al.2022.rbcL <- Webber.et.al.2022.rbcL[!is.na(Webber.et.al.2022.rbcL$scientificName), ]

# Assign sequence data to Dwc field associatedSequences

Webber.et.al.2022.rbcL$associatedSequences <- Webber.et.al.2022.rbcL$rbcL_sequence
Webber.et.al.2022.rbcL$identifiedWith <- "rbcL"

# Normalize dataframe to match same dimensions as 18S sequence data

Webber.et.al.2022.rbcL <- Webber.et.al.2022.rbcL[, -(1:7)]

Webber.et.al.2022.rbcL$value <- NULL

# Merge dataframes into singular dataframe

Webber.et.al.2022 <- rbind(Webber.et.al.2022.18S,Webber.et.al.2022.rbcL)

# Assign metadata based on sample IDs

Webber.et.al.2022$eventDate  <- Webber.et.al.2024.metadata$eventDate[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$recordedBy  <- Webber.et.al.2024.metadata$recordedBy[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$locality <- Webber.et.al.2024.metadata$locality[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$decimalLatitude <- Webber.et.al.2024.metadata$decimalLatitude[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$decimalLongitude <- Webber.et.al.2024.metadata$decimalLongitude[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$coordinateUncertaintyInMeters <- Webber.et.al.2024.metadata$coordinateUncertaintyInMeters[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$georeferencedBy <- Webber.et.al.2024.metadata$georeferencedBy[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$habitat <- Webber.et.al.2024.metadata$habitat[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022$verbatimDepth <- Webber.et.al.2024.metadata$verbatimDepth[match(unlist(Webber.et.al.2022$materialSampleID), Webber.et.al.2024.metadata$sample)]

# Assign unique occurrenceIDs

unique.prefix <- "MWMiners2022:"
unique.suffix <- 1:nrow(Webber.et.al.2022)

Webber.et.al.2022$occurrenceID <- paste(unique.prefix,unique.suffix, sep = "")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Webber.et.al.2022)))
names(data.frame) <- DwCFields

data.frame[names(Webber.et.al.2022)] <- Webber.et.al.2022

Webber.et.al.2022 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Webber.et.al.2022$datasetName <- "Webber et al. 2022"
Webber.et.al.2022$georeferenceProtocol <- "Coordinates mapped based on precise locations of BioBlitz sampling sites"
Webber.et.al.2022$countryCode <- "CA"
Webber.et.al.2022$country <- "Canada"
Webber.et.al.2022$stateProvince <- "British Columbia"
Webber.et.al.2022$island <- "Mayne Island"
Webber.et.al.2022$basisOfRecord <- "MaterialSample"

# Merge with summary to standardize names and taxon metadata

Webber.et.al.2022$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$taxonID <- summary$ID[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$phylum <- summary$phylum[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$class <- summary$class[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$order <- summary$order[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$suborder <- summary$suborder[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$family <- summary$family[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$genus <- summary$genus[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$variety <- summary$variety[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]
Webber.et.al.2022$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2022$scientificName), summary$scientificName)]

# Unmatched records

Webber.et.al.2022.names.unmatched <- Webber.et.al.2022[is.na(Webber.et.al.2022$taxonID),]

# Matched records

Webber.et.al.2022.names.matched <- anti_join(Webber.et.al.2022,Webber.et.al.2022.names.unmatched)

# Confirm all records are represented 

nrow(Webber.et.al.2022)
nrow(Webber.et.al.2022.names.matched)
nrow(Webber.et.al.2022.names.unmatched)
nrow(Webber.et.al.2022.names.matched)+nrow(Webber.et.al.2022.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Webber.et.al.2022.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

Webber.et.al.2022.names.unmatched.matched <- Webber.et.al.2022.names.unmatched

Webber.et.al.2022.names.unmatched.matched$scientificNameTemp <- Webber.et.al.2022.key$Matched.Taxon[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificName), Webber.et.al.2022.key$Taxon)]

# Add values based on newly matched name

Webber.et.al.2022.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$phylum <- summary$phylum[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$class <- summary$class[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$order <- summary$order[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$suborder <- summary$suborder[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$family <- summary$family[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$genus <- summary$genus[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$variety <- summary$variety[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2022.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

Webber.et.al.2022.names.unmatched.unmatched <- Webber.et.al.2022.names.unmatched.matched[is.na(Webber.et.al.2022.names.unmatched.matched$taxonID),]

Webber.et.al.2022.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Webber.et.al.2022.names.unmatched.matched$scientificName <- Webber.et.al.2022.names.unmatched.matched$scientificNameTemp

Webber.et.al.2022.names.unmatched.matched$scientificNameTemp <- NULL

Webber.et.al.2022.names.unmatched.matched <- Webber.et.al.2022.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Webber.et.al.2022)
nrow(Webber.et.al.2022.names.matched)
nrow(Webber.et.al.2022.names.unmatched)
nrow(Webber.et.al.2022.names.unmatched.matched)
nrow(Webber.et.al.2022.names.unmatched.unmatched)
nrow(Webber.et.al.2022.names.matched)+nrow(Webber.et.al.2022.names.unmatched.matched)+nrow(Webber.et.al.2022.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Webber.et.al.2022.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Webber.et.al.2022.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Webber.et.al.2022.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

Webber.et.al.2022.records <- rbind(Webber.et.al.2022.names.matched,Webber.et.al.2022.names.unmatched.matched)

# Set date formatting consistent with other data frames

Webber.et.al.2022.records$eventDate <- as.Date(Webber.et.al.2022.records$eventDate)

# Compare records in and out

nrow(Webber.et.al.2022) - nrow(Webber.et.al.2022.records)
nrow(Webber.et.al.2022)
nrow(Webber.et.al.2022.records) # 18 records excluded; mostly redundant; one obscure marine oomycete 
# Only two records; may not be worthwhile including these are from outside the study area

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records, Webber.et.al.2022.names.unmatched.unmatched)

unmatched.algae.records



###############################################################################
# Webber et al. 2022-2023 General Plankton Samples (Illumina sequencing data) #
###############################################################################

Webber.et.al.2022.2023 <- read.csv("../../records/digitized/Webber_et_al_2024_sequence_table_rbcL_share_MAL_general_plankton_9 samples_AS_MW_AS.csv")

Webber.et.al.2024.metadata <- read.csv("../../records/digitized/Webber_et_al_2024_metadata.csv")

# Transform matrix into long-form dataframe and convert recordNumbers to match metadata

Webber.et.al.2022.2023 <- Webber.et.al.2022.2023 %>%
  pivot_longer(
    cols = starts_with("X"),       
    names_to = "materialSampleID",             
  ) %>%
  distinct() %>%
  mutate(materialSampleID = gsub("\\.", "-", materialSampleID))

# Normalize sample IDs to conform with metadata

Webber.et.al.2022.2023 <- Webber.et.al.2022.2023 %>%
  mutate(materialSampleID = case_when(
    materialSampleID == "X1--SHW-Apr-11-2022" ~ "1",
    materialSampleID == "X2-site1-dionesio-july-15-2022" ~ "2-site1-dionesio-july-15-2022",
    materialSampleID == "X3-site2-westside-july-15-2022" ~ "3-site2-westside-july-15-2022",
    materialSampleID == "X4-site3-trincomali-july-15-2022" ~ "4-site3-trincomali-july-15-2022",
    materialSampleID == "X6-SHW-Dec-11-2022" ~ "6",
    materialSampleID == "X7-SHW-Feb-22-2023" ~ "7",
    materialSampleID == "X8-MHMPP-July-22-2021" ~ "8-MHMPP-july-22-2021",
    materialSampleID == "X10-site3-trincomali-july-15-1022" ~ "10-site3-trincomali-july-15-1022",
    TRUE ~ materialSampleID 
  ))

# Remove Miner's Bay data, represented in another sample:

Webber.et.al.2022.2023 <- Webber.et.al.2022.2023 %>% filter(materialSampleID != "X5-MBW-oct-31-2022")

# Assign scientificName based on resolved names

Webber.et.al.2022.2023$scientificName  <- Webber.et.al.2022.2023$resolved_taxon

# Assign sequence data to Dwc field associatedSequences

Webber.et.al.2022.2023$associatedSequences <- Webber.et.al.2022.2023$rbcL_sequence
Webber.et.al.2022.2023$identifiedWith <- "rbcL"

# Assign unique occurrenceIDs

unique.prefix <- "MWPlankton2022-2023:"
unique.suffix <- 1:nrow(Webber.et.al.2022.2023)

Webber.et.al.2022.2023$occurrenceID <- paste(unique.prefix,unique.suffix, sep = "")

# Assign metadata based on sample IDs

Webber.et.al.2022.2023$eventDate  <- Webber.et.al.2024.metadata$eventDate[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$recordedBy  <- Webber.et.al.2024.metadata$recordedBy[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$locality <- Webber.et.al.2024.metadata$locality[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$decimalLatitude <- Webber.et.al.2024.metadata$decimalLatitude[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$decimalLongitude <- Webber.et.al.2024.metadata$decimalLongitude[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$coordinateUncertaintyInMeters <- Webber.et.al.2024.metadata$coordinateUncertaintyInMeters[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$georeferencedBy <- Webber.et.al.2024.metadata$georeferencedBy[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$habitat <- Webber.et.al.2024.metadata$habitat[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]
Webber.et.al.2022.2023$verbatimDepth <- Webber.et.al.2024.metadata$verbatimDepth[match(unlist(Webber.et.al.2022.2023$materialSampleID), Webber.et.al.2024.metadata$sample)]

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Webber.et.al.2022.2023)))
names(data.frame) <- DwCFields

data.frame[names(Webber.et.al.2022.2023)] <- Webber.et.al.2022.2023

Webber.et.al.2022.2023 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Webber.et.al.2022.2023$datasetName <- "Webber et al. 2022-2023"
Webber.et.al.2022.2023$georeferenceProtocol <- "Coordinates mapped based on precise locations of BioBlitz sampling sites"
Webber.et.al.2022.2023$countryCode <- "CA"
Webber.et.al.2022.2023$country <- "Canada"
Webber.et.al.2022.2023$stateProvince <- "British Columbia"
Webber.et.al.2022.2023$island <- "Galiano Island"
Webber.et.al.2022.2023$basisOfRecord <- "MaterialSample"

# Merge with summary to standardize names and taxon metadata

Webber.et.al.2022.2023$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$taxonID <- summary$ID[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$phylum <- summary$phylum[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$class <- summary$class[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$order <- summary$order[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$suborder <- summary$suborder[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$family <- summary$family[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$genus <- summary$genus[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$variety <- summary$variety[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]
Webber.et.al.2022.2023$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2022.2023$scientificName), summary$scientificName)]

# Unmatched records

Webber.et.al.2022.2023.names.unmatched <- Webber.et.al.2022.2023[is.na(Webber.et.al.2022.2023$taxonID),]

# Matched records

Webber.et.al.2022.2023.names.matched <- anti_join(Webber.et.al.2022.2023,Webber.et.al.2022.2023.names.unmatched)

# Confirm all records are represented 

nrow(Webber.et.al.2022.2023)
nrow(Webber.et.al.2022.2023.names.matched)
nrow(Webber.et.al.2022.2023.names.unmatched)
nrow(Webber.et.al.2022.2023.names.matched)+nrow(Webber.et.al.2022.2023.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Webber.et.al.2022.2023.key <- read.csv("keys/algae_taxon_key_2024.csv") 

# Swap unmatched names using key

Webber.et.al.2022.2023.names.unmatched.matched <- Webber.et.al.2022.2023.names.unmatched

Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp <- Webber.et.al.2022.2023.key$Matched.Taxon[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificName), Webber.et.al.2022.2023.key$Taxon)]

# Add values based on newly matched name

Webber.et.al.2022.2023.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$phylum <- summary$phylum[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$class <- summary$class[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$order <- summary$order[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$suborder <- summary$suborder[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$family <- summary$family[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$genus <- summary$genus[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$variety <- summary$variety[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
Webber.et.al.2022.2023.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

Webber.et.al.2022.2023.names.unmatched.unmatched <- Webber.et.al.2022.2023.names.unmatched.matched[is.na(Webber.et.al.2022.2023.names.unmatched.matched$taxonID),]

Webber.et.al.2022.2023.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Webber.et.al.2022.2023.names.unmatched.matched$scientificName <- Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp

Webber.et.al.2022.2023.names.unmatched.matched$scientificNameTemp <- NULL

Webber.et.al.2022.2023.names.unmatched.matched <- Webber.et.al.2022.2023.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Webber.et.al.2022.2023)
nrow(Webber.et.al.2022.2023.names.matched)
nrow(Webber.et.al.2022.2023.names.unmatched)
nrow(Webber.et.al.2022.2023.names.unmatched.matched)
nrow(Webber.et.al.2022.2023.names.unmatched.unmatched)
nrow(Webber.et.al.2022.2023.names.matched)+nrow(Webber.et.al.2022.2023.names.unmatched.matched)+nrow(Webber.et.al.2022.2023.names.unmatched.unmatched)

# Generate review key with mismatched names

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Webber.et.al.2022.2023.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Webber.et.al.2022.2023.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Webber.et.al.2022.2023.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

Webber.et.al.2022.2023.records <- rbind(Webber.et.al.2022.2023.names.matched,Webber.et.al.2022.2023.names.unmatched.matched)

# Set date formatting consistent with other data frames

Webber.et.al.2022.2023.records$eventDate <- as.Date(Webber.et.al.2022.2023.records$eventDate)

# Compare records in and out

nrow(Webber.et.al.2022.2023) - nrow(Webber.et.al.2022.2023.records)
nrow(Webber.et.al.2022.2023)
nrow(Webber.et.al.2022.2023.records)

# Start record of unmatched names

unmatched.algae.records <- Webber.et.al.2022.2023.names.unmatched.unmatched

unmatched.algae.records



# Combine all source occurrence records

marine.algae.records <- rbind(BioBlitz.2023.records, BOLD.2021.records, CPNWH.2021.records, iNaturalist.records, Lindstrom.2023.records, PMLS.2021.records, Webber.et.al.2020.records, Webber.et.al.2022.records, Webber.et.al.2022.2023.records)

# Compare to see if summary is complete with reference to source occurrence records

setdiff(summary$scientificName , marine.algae.records$scientificName)
setdiff(marine.algae.records$scientificName, summary$scientificName)

# Finalize DwC fields (day, month, year, infraspecificEpithet, occurrenceStatus)

# First remove NAs

marine.algae.records[is.na(marine.algae.records)] <- ""

# Date

marine.algae.records$year <- substr(marine.algae.records$eventDate, 1, 4)
marine.algae.records$month <- substr(marine.algae.records$eventDate, 6, 7)
marine.algae.records$day <- substr(marine.algae.records$eventDate, 9, 10)

# Infrataxa

Genera <- marine.algae.records %>% filter(specificEpithet == "")
Genera <- Genera %>% filter(hybrid == "")
Genera$taxonRank <- "genus" 
Genera <- subset(Genera, select = -c(hybrid, subspecies, variety, form))

Species <- subset(marine.algae.records, specificEpithet != "")
Species <- Species %>% filter(hybrid == "")
Species <- Species %>% filter(variety == "")
Species <- Species %>% filter(subspecies == "")
Species$taxonRank <- "species" 
Species <- subset(Species, select = -c(hybrid, subspecies, variety, form))

# No hybrids 

# Hybrids <- subset(marine.algae.records, hybrid != "")
# Hybrids$taxonRank <- "hybrid"
# Hybrids$infraspecificEpithet <- Hybrids$hybrid
# Hybrids <- subset(Hybrids, select = -c(hybrid, subspecies, variety, form))

Subspecies <- subset(marine.algae.records, subspecies != "")
Subspecies$taxonRank <- "subspecies"
Subspecies$infraspecificEpithet <- Subspecies$subspecies
Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

Varieties <- subset(marine.algae.records, variety != "")
Varieties$taxonRank <- "varietas"
Varieties$infraspecificEpithet <- Varieties$variety
Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

marine.algae.records <- rbind(Genera, Species, Subspecies, Varieties)

# occurrenceStatus

marine.algae.records$occurrenceStatus <- "present"

# Standardize values in basisOfRecord

marine.algae.records$basisOfRecord <- str_replace(marine.algae.records$basisOfRecord, 'MATERIAL_SAMPLE', 'MaterialSample')
marine.algae.records$basisOfRecord <- str_replace(marine.algae.records$basisOfRecord, 'PRESERVED_SPECIMEN', 'PreservedSpecimen')

# Order by taxon

marine.algae.records <- marine.algae.records[order(marine.algae.records$scientificName), ]

# Tally records

nrow(marine.algae.records)

# Summary of records that remain unmatched

nrow(unmatched.algae.records)

sort(unique(unmatched.algae.records$scientificName))

# Remove NAs

marine.algae.records[is.na(marine.algae.records)] <- ""

# Output synthesized catalog of occurrence records

write.csv(marine.algae.records,"synthesized/Galiano_marine_algae_records_consolidated.csv", row.names = FALSE)
