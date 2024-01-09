# SQUAMISH ENVIRONMENT SOCIETY
# WILDLIFE CONNECTIVITY PROJECT
# CONTEXT MODEL: CONSOLIDATE HERPTILE RECORDS

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Animalia/herptiles/summaries/Context_model_herptiles_review_summary_2023-12-25.csv")

# Create vector of DarwinCore fields for aggregating records

DwCFields <- c('scientificName','scientificNameAuthorship','taxonID','kingdom','phylum','subphylum','class',
                    'order','suborder','infraorder','superfamily','family','genus','subgenus','specificEpithet',
                    'hybrid','subspecies','variety','form','infraspecificEpithet','taxonRank','institutionCode',
                    'collectionCode','catalogNumber','datasetName','occurrenceID','recordedBy','recordNumber',
                    'fieldNumber','gbifID','eventDate','year','month','day','basisOfRecord','locality','locationRemarks','island',
                    'stateProvince','country','countryCode','decimalLatitude','decimalLongitude','coordinateUncertaintyInMeters',
                    'georeferencedBy','georeferenceVerificationStatus','georeferenceProtocol','georeferenceRemarks',
                    'habitat','verbatimDepth','verbatimElevation','occurrenceStatus','samplingProtocol','occurrenceRemarks',
                    'individualCount','sex','establishmentMeans','provincialStatus','nationalStatus','identifiedBy',
                    'identificationQualifier','identificationRemarks','previousIdentifications','bibliographicCitation',
                    'associatedReferences')

# Consolidate records

# Sources (1/1 added):

# GBIF data

# Read GBIF Data

GBIF <- read.csv("../../../parse_records/outputs/records_herptiles.csv")

# Replace 'scientificName' w 'verbatimScientificName'

GBIF$scientificName <- GBIF$verbatimScientificName

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(GBIF)))
names(data.frame) <- DwCFields

data.frame[names(GBIF)] <- GBIF

GBIF <- select(data.frame, c(1:length(DwCFields)))

# Merge with summary to standardize names and taxon metadata

GBIF$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$taxonID <- summary$ID[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$kingdom <- summary$kingdom[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$phylum <- summary$phylum[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$subphylum <- summary$subphylum[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$class <- summary$class[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$order <- summary$order[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$suborder <- summary$suborder[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$superfamily <- summary$superfamily[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$family <- summary$family[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$genus <- summary$genus[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$specificEpithet <- summary$specificEpithet[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$hybrid <- summary$hybrid[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$subspecies <- summary$subspecies[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$variety <- summary$variety[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$establishmentMeans <- summary$establishmentMeans[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$provincialStatus <- summary$provincialStatus[match(unlist(GBIF$scientificName), summary$scientificName)]
GBIF$nationalStatus <- summary$nationalStatus[match(unlist(GBIF$scientificName), summary$scientificName)]

# Unmatched records

GBIF.names.unmatched <- GBIF[is.na(GBIF$taxonID),]

# Matched records

GBIF.names.matched <- anti_join(GBIF,GBIF.names.unmatched)

# Confirm all records are represented 

nrow(GBIF)
nrow(GBIF.names.matched)
nrow(GBIF.names.unmatched)
nrow(GBIF.names.matched)+nrow(GBIF.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

GBIF.key <- read.csv("keys/GBIF_key.csv") 

# Swap unmatched names using key

GBIF.names.unmatched.matched <- GBIF.names.unmatched

GBIF.names.unmatched.matched$scientificNameTemp <- GBIF.key$Matched.Taxon[match(unlist(GBIF.names.unmatched.matched$scientificName), GBIF.key$Taxon)]

# Add values based on newly matched name

GBIF.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$taxonID <- summary$ID[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$phylum <- summary$phylum[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$class <- summary$class[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$order <- summary$order[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$suborder <- summary$suborder[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$family <- summary$family[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$genus <- summary$genus[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$variety <- summary$variety[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GBIF.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(GBIF.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

GBIF.names.unmatched.unmatched <- GBIF.names.unmatched.matched[is.na(GBIF.names.unmatched.matched$taxonID),]

GBIF.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

GBIF.names.unmatched.matched$scientificName <- GBIF.names.unmatched.matched$scientificNameTemp

GBIF.names.unmatched.matched$scientificNameTemp <- NULL

GBIF.names.unmatched.matched <- GBIF.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(GBIF)
nrow(GBIF.names.matched)
nrow(GBIF.names.unmatched)
nrow(GBIF.names.unmatched.matched)
nrow(GBIF.names.unmatched.unmatched)
nrow(GBIF.names.matched)+nrow(GBIF.names.unmatched.matched)+nrow(GBIF.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(GBIF.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- GBIF.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(GBIF.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

GBIF.records <- rbind(GBIF.names.matched,GBIF.names.unmatched.matched)

# Set date formatting consistent with other data frames

GBIF.records$eventDate <- as.Date(GBIF.records$eventDate)

# Compare records in and out

nrow(GBIF) - nrow(GBIF.records)

nrow(GBIF)
nrow(GBIF.records) # 1300 records omitted; many represent species level IDs of taxa recognized at infrataxonomic level in summary

# Start record of unmatched names

unmatched.herptile.records <- GBIF.names.unmatched.unmatched

unmatched.herptile.records


# Combine all source occurrence records

herptile.records <- GBIF.records


# Finalize DwC fields (day, month, year, infraspecificEpithet, occurrenceStatus)

# First remove NAs

herptile.records[is.na(herptile.records)] <- ""

# Date

herptile.records$year <- substr(herptile.records$eventDate, 1, 4)
herptile.records$month <- substr(herptile.records$eventDate, 6, 7)
herptile.records$day <- substr(herptile.records$eventDate, 9, 10)

# Infrataxa

# No taxa identified only to genus

# Genera <- herptile.records %>% filter(specificEpithet == "")
# Genera <- Genera %>% filter(hybrid == "")
# Genera$taxonRank <- "genus" 
# Genera <- subset(Genera, select = -c(hybrid, subspecies, variety, form))

Species <- subset(herptile.records, specificEpithet != "")
Species <- Species %>% filter(hybrid == "")
Species <- Species %>% filter(variety == "")
Species <- Species %>% filter(subspecies == "")
Species$taxonRank <- "species" 
Species <- subset(Species, select = -c(hybrid, subspecies, variety, form))

# No hybrids 

# Hybrids <- subset(herptile.records, hybrid != "")
# Hybrids$taxonRank <- "hybrid"
# Hybrids$infraspecificEpithet <- Hybrids$hybrid
# Hybrids <- subset(Hybrids, select = -c(hybrid, subspecies, variety, form))

Subspecies <- subset(herptile.records, subspecies != "")
Subspecies$taxonRank <- "subspecies"
Subspecies$infraspecificEpithet <- Subspecies$subspecies
Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

# No varieties

# Varieties <- subset(herptile.records, variety != "")
# Varieties$taxonRank <- "varietas"
# Varieties$infraspecificEpithet <- Varieties$variety
# Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

herptile.records <- rbind(Species, Subspecies)

# occurrenceStatus

herptile.records$occurrenceStatus <- "present"

# Order by taxon

herptile.records <- herptile.records[order(herptile.records$scientificName),] 

# Tally records

nrow(herptile.records)

# Summary of records that remain unmatched

nrow(unmatched.herptile.records)

sort(unique(unmatched.herptile.records$scientificName))

# Remove NAs

herptile.records[is.na(herptile.records)] <- ""

# Compare with summary to ensure all recognized taxa are represented in the catalog

taxa.records <- unique(herptile.records$scientificName)
taxa.baseline <- unique(summary$scientificName)

taxa.records <- sort(taxa.records)
taxa.baseline <- sort(taxa.baseline)

length(taxa.records)
length(taxa.baseline)

taxa.records
taxa.baseline

identical(taxa.records, taxa.baseline)   

setdiff(taxa.baseline, taxa.records)
setdiff(taxa.records, taxa.baseline)

# Output synthesized catalog of occurrence records

write.csv(herptile.records,"synthesized/herptile_records_consolidated.csv", row.names = FALSE)

# Evaluate georeferencing resolution of terrestrial mammal

nrow(herptile.records) # 16K herptile records

herptile.records$coordinateUncertaintyInMeters <- as.numeric(herptile.records$coordinateUncertaintyInMeters)

hist(herptile.records$coordinateUncertaintyInMeters, 
     xlim=c(0,1000), breaks = 400000, main="Herptile Records: Coordinate Uncertainty", xlab = "Coordinate Uncertainty in meters")

sum(is.na(herptile.records$coordinateUncertaintyInMeters))/nrow(herptile.records) 
# 19% of records lack coordinate uncertainty

sum(is.na(herptile.records$coordinateUncertaintyInMeters))/nrow(herptile.records) * nrow(herptile.records) 
# or 3059/15923 records total

georeferenced.records <- nrow(herptile.records)-sum(is.na(herptile.records$coordinateUncertaintyInMeters))

sum(herptile.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records # 58% of georeferenced records mapped to < 100 m coordinate uncertainty

sum(herptile.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records * georeferenced.records

# About 7.5K of total 16k records can be analysed with confidence at 100m grid scale

georeferenced.records <- nrow(herptile.records)-sum(is.na(herptile.records$coordinateUncertaintyInMeters))

sum(herptile.records$coordinateUncertaintyInMeters <= 30, na.rm=TRUE)/georeferenced.records # 44% of georeferenced records mapped to < 100 m coordinate uncertainty

sum(herptile.records$coordinateUncertaintyInMeters <= 30, na.rm=TRUE)/georeferenced.records * georeferenced.records

# About 5.7K of total 16k records (44% of all records) can be analysed with confidence at 30m grid scale
