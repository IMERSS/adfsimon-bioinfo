# Script to consolidate records of Galiano Island's macroalgae and phytoplankton diversity

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/summaries/Galiano_marine_algae_review_summary_reviewed_2023-10-14.csv")

# Note in Sept. 2023 this summary was revised to incorporate critical feedback from Sandra Lindstrom;
# multiple taxa have been effectively removed from the summary which has resulted in discrepancies w
# taxa reported in the various data sources aggregated by this script. These discrepancies are noted below as 
# mismatched taxa for each data source.

# Create vector of DarwinCore fields for aggregating records

DwCFields <- c('scientificName','scientificNameAuthorship','taxonID','kingdom','phylum','subphylum','class',
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

# Sources (5/6 added):

# BOLD records 2021 - ! added, but needs attention
# CPNWH records 2022 - added
# iNaturalist observations 2022 - ! added # Need to update for 2023
# Sandra Lindstrom BioBlitz collections 2023 - ! added
# PMLS Records 2021 - added # ! Need to update dataset
# Webber et al. 2022 Epiphytic diatoms on Zostera 2022 - added ! needs updating!



# Read BOLD records 2021 # Note: requires updating to ensure appropriate metadata are recorded in DwC format

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

BOLD.2021.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(BOLD.2021.records) # no records omitted



# Read records from the Consortium of Pacific Northwest Herbaria

CPNWH.2021 <- read.csv("../../records/digitized/DarwinCore/CPNWH_Galiano_Island_macroalgae_2022-10-30_DwC.csv")

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

CPNWH.2021.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(CPNWH.2021.records) # 3 records omitted; those indeterminate with reference to summary

# Start record of unmatched names

unmatched.algae.records <- CPNWH.2021.names.unmatched.unmatched

unmatched.algae.records



# Read iNaturalist data

iNaturalist.observations <- read.csv("../../records/digitized/DarwinCore/iNaturalist_algae_observations_2022-10-30_DwC.csv")

# Substitute iNaturalist usernames where actual observer names are missing

iNaturalist.observations.nameless <- iNaturalist.observations %>% filter(!str_detect(recordedBy, '')) 

iNaturalist.observations.names <- anti_join(iNaturalist.observations,iNaturalist.observations.nameless)

iNaturalist.observations.nameless$recordedBy <- iNaturalist.observations.nameless$user_login

iNaturalist.observations <- rbind(iNaturalist.observations.nameless,iNaturalist.observations.names)

# Swap coordinates with private coordinates for obscured records

iNaturalist.observations.coordinates.obscured <- iNaturalist.observations %>% drop_na(private_latitude)

iNaturalist.observations.coordinates.unobscured <- anti_join(iNaturalist.observations,iNaturalist.observations.coordinates.obscured)

iNaturalist.observations.coordinates.obscured$decimalLatitude <- iNaturalist.observations.coordinates.obscured$private_latitude
iNaturalist.observations.coordinates.obscured$decimalLongitude <- iNaturalist.observations.coordinates.obscured$private_longitude

iNaturalist.observations <- rbind(iNaturalist.observations.coordinates.obscured,iNaturalist.observations.coordinates.unobscured)

# Drop observations of taxa that are not identified to genus at least

iNaturalist.observations <- subset(iNaturalist.observations, taxon_genus_name != "")

# Substitute iNaturalist taxon names with names from curated summary based on taxonID

iNaturalist.observations$swappedNames <- summary$scientificName[match(unlist(iNaturalist.observations$taxon_id), summary$ID)]

iNaturalist.observations.swapped.names <- iNaturalist.observations %>% drop_na(swappedNames)

iNaturalist.observations.unswapped.names <- anti_join(iNaturalist.observations,iNaturalist.observations.swapped.names)

iNaturalist.observations.swapped.names$scientificName <- iNaturalist.observations.swapped.names$swappedNames

iNaturalist.observations <- rbind(iNaturalist.observations.swapped.names,iNaturalist.observations.unswapped.names)

iNaturalist.observations$swappedNames <- NULL

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

iNaturalist.observations.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'algae_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(iNaturalist.records) # 673 records omitted: all species resolved only to genus (redundant to list) or unrecognized in summary (cultivated plants, etc.)

unmatched.algae.records <- rbind(unmatched.algae.records,iNaturalist.observations.names.unmatched.unmatched)

unmatched.algae.records



# Read Sandra Lindstrom's 2023 BioBlitz records # Note: this code is not complete! 

Lindstrom.2023 <- read.csv("../../records/digitized/DarwinCore/Sandra_Lindstrom_Galiano_seaweed_collections_May_2023_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Lindstrom.2023)))
names(data.frame) <- DwCFields

data.frame[names(Lindstrom.2023)] <- Lindstrom.2023

Lindstrom.2023 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Lindstrom.2023$datasetName <- "Lindstrom 2023"
Lindstrom.2023$stateProvince <- "British Columbia"
Lindstrom.2023$country <- "Canada"
Lindstrom.2023$island <- "Galiano Island"

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

Lindstrom.2023.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(Lindstrom.2023.records) # few records omitted; will update when dna results come in



# Read PMLS 2021 records # TEMPORARY DATASET

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
PMLS.2021$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
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

PMLS.2021.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(PMLS.2021.records) # >1,000 records omitted; all indeterminate with reference to summary

# Add to record of unmatched names

names(unmatched.algae.records)
names(PMLS.2021.names.unmatched.unmatched)

unmatched.algae.records <- rbind(unmatched.algae.records,PMLS.2021.names.unmatched.unmatched)

unmatched.algae.records



# Read Webber et al. 2022 records (Illumina taxon table)

Webber.et.al.2022 <- read.csv("../../records/digitized/DarwinCore/Webber_et_al_2022_Taxonomy_table_epiphytic_diatoms_Zostera_2022-11-01_DwC.csv")

# Create unique identifiers for observations

unique.prefix <- "WEBBERETAL2022:"
unique.suffix <- 1:nrow(Webber.et.al.2022)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Webber.et.al.2022)))
names(data.frame) <- DwCFields

data.frame[names(Webber.et.al.2022)] <- Webber.et.al.2022

Webber.et.al.2022 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Webber.et.al.2022$datasetName <- "Webber et al. 2022"
Webber.et.al.2022$recordedBy<- "Mark Webber, Siobhan Schenck & Arjan van Asselt"
Webber.et.al.2022$eventDate <- '2020-11-15' # temporary date, update
Webber.et.al.2022$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Webber.et.al.2022$decimalLatitude <- 48.90071443043615
Webber.et.al.2022$decimalLongitude <- -123.40790606380006
Webber.et.al.2022$georeferenceProtocol <- "Coordinates mapped based on precise location of study area"
Webber.et.al.2022$coordinateUncertaintyInMeters <- 50
Webber.et.al.2022$countryCode <- "CA"
Webber.et.al.2022$country <- "Canada"
Webber.et.al.2022$stateProvince <- "British Columbia"
Webber.et.al.2022$island <- "Galiano Island"
Webber.et.al.2022$locality <- "Montague Harbour Marine Provincial Park (Hulq'umi'num: 'Sumnuw')"
Webber.et.al.2022$basisOfRecord <- "MaterialSample"

# Remove '_' from 'scientificName'

Webber.et.al.2022$scientificName <-  gsub("_", " ", Webber.et.al.2022$scientificName)

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

Webber.et.al.2022.key <- read.csv("keys/algae_taxon_key_2023.csv") 

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
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

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
nrow(Webber.et.al.2022.records) # 677 records omitted; all indeterminate with reference to summary

# Add to record of unmatched names

unmatched.algae.records <- rbind(unmatched.algae.records,Webber.et.al.2022.names.unmatched.unmatched)

unmatched.algae.records



# Combine all source occurrence records

marine.algae.records <- rbind(BOLD.2021.records, CPNWH.2021.records,iNaturalist.records,PMLS.2021.records,Webber.et.al.2022.records)



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

# No subspecies

# Subspecies <- subset(marine.algae.records, subspecies != "")
# Subspecies$taxonRank <- "subspecies"
# Subspecies$infraspecificEpithet <- Subspecies$subspecies
# Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

Varieties <- subset(marine.algae.records, variety != "")
Varieties$taxonRank <- "varietas"
Varieties$infraspecificEpithet <- Varieties$variety
Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

marine.algae.records <- rbind(Genera, Species, Varieties)

# occurrenceStatus

marine.algae.records$occurrenceStatus <- "present"

# Standardize values in basisOfRecord

marine.algae.records$basisOfRecord <- str_replace(marine.algae.records$basisOfRecord, 'MATERIAL_SAMPLE', 'MaterialSample')
marine.algae.records$basisOfRecord <- str_replace(marine.algae.records$basisOfRecord, 'PRESERVED_SPECIMEN', 'PreservedSpecimen')

# Order by taxon

marine.algae.records <- marine.algae.records[order(marine.algae.records$scientificName),] 

# Tally records

nrow(marine.algae.records)

# Summary of records that remain unmatched

nrow(unmatched.algae.records)

sort(unique(unmatched.algae.records$scientificName))

# Remove NAs

marine.algae.records[is.na(marine.algae.records)] <- ""

# Output synthesized catalog of occurrence records

write.csv(marine.algae.records,"synthesized/Galiano_marine_algae_records_consolidated.csv", row.names = FALSE)
