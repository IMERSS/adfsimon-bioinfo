# SQUAMISH ENVIRONMENT SOCIETY
# WILDLIFE CONNECTIVITY PROJECT
# CONTEXT MODEL: MAMMAL DIVERSITY

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Animalia/terrestrial_mammals/summaries/Context_model_terrestrial_mammals_review_summary_2023-12-08.csv")

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

# Sources (1/1 added):

# GBIF data

# Read GBIF Data

GBIF <- read.csv("../../../parse_records/outputs/records_terrestrial_mammals.csv")

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
nrow(GBIF.records) # 237 records omitted; those indeterminate with reference to summary

# Start record of unmatched names

unmatched.terrestrial.mammal.records <- GBIF.names.unmatched.unmatched

unmatched.terrestrial.mammal.records


# Combine all source occurrence records

terrestrial.mammal.records <- GBIF.records


# Finalize DwC fields (day, month, year, infraspecificEpithet, occurrenceStatus)

# First remove NAs

terrestrial.mammal.records[is.na(terrestrial.mammal.records)] <- ""

# Date

terrestrial.mammal.records$year <- substr(terrestrial.mammal.records$eventDate, 1, 4)
terrestrial.mammal.records$month <- substr(terrestrial.mammal.records$eventDate, 6, 7)
terrestrial.mammal.records$day <- substr(terrestrial.mammal.records$eventDate, 9, 10)

# Infrataxa

# No taxa identified only to genus

# Genera <- terrestrial.mammal.records %>% filter(specificEpithet == "")
# Genera <- Genera %>% filter(hybrid == "")
# Genera$taxonRank <- "genus" 
# Genera <- subset(Genera, select = -c(hybrid, subspecies, variety, form))

Species <- subset(terrestrial.mammal.records, specificEpithet != "")
Species <- Species %>% filter(hybrid == "")
Species <- Species %>% filter(variety == "")
Species <- Species %>% filter(subspecies == "")
Species$taxonRank <- "species" 
Species <- subset(Species, select = -c(hybrid, subspecies, variety, form))

# No hybrids 

# Hybrids <- subset(terrestrial.mammal.records, hybrid != "")
# Hybrids$taxonRank <- "hybrid"
# Hybrids$infraspecificEpithet <- Hybrids$hybrid
# Hybrids <- subset(Hybrids, select = -c(hybrid, subspecies, variety, form))

Subspecies <- subset(terrestrial.mammal.records, subspecies != "")
Subspecies$taxonRank <- "subspecies"
Subspecies$infraspecificEpithet <- Subspecies$subspecies
Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

# No varieties

# Varieties <- subset(terrestrial.mammal.records, variety != "")
# Varieties$taxonRank <- "varietas"
# Varieties$infraspecificEpithet <- Varieties$variety
# Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

terrestrial.mammal.records <- rbind(Species, Subspecies)

# occurrenceStatus

terrestrial.mammal.records$occurrenceStatus <- "present"

# Order by taxon

terrestrial.mammal.records <- terrestrial.mammal.records[order(terrestrial.mammal.records$scientificName),] 

# Tally records

nrow(terrestrial.mammal.records)

# Summary of records that remain unmatched

nrow(unmatched.terrestrial.mammal.records)

sort(unique(unmatched.terrestrial.mammal.records$scientificName))

# Remove NAs

terrestrial.mammal.records[is.na(terrestrial.mammal.records)] <- ""

# Compare with summary to ensure all recognized taxa are represented in the catalog

taxa.records <- unique(terrestrial.mammal.records$scientificName)
taxa.baseline <- unique(baseline$scientificName)

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

write.csv(terrestrial.mammal.records,"synthesized/terrestrial_mammal_records_consolidated.csv", row.names = FALSE)

# Evaluate georeferencing resolution of terrestrial mammal

nrow(terrestrial.mammal.records) # 26K terrestrial mammal records

terrestrial.mammal.records$coordinateUncertaintyInMeters <- as.numeric(terrestrial.mammal.records$coordinateUncertaintyInMeters)

hist(terrestrial.mammal.records$coordinateUncertaintyInMeters, 
     xlim=c(0,1000), breaks = 100000, main="Terrestrial Mammal Records: Coordinate Uncertainty", xlab = "Coordinate Uncertainty in meters")

sum(is.na(terrestrial.mammal.records$coordinateUncertaintyInMeters))/nrow(terrestrial.mammal.records) 
# 23% of records lack coordinate uncertainty

sum(is.na(terrestrial.mammal.records$coordinateUncertaintyInMeters))/nrow(terrestrial.mammal.records) * nrow(terrestrial.mammal.records) 
# Or 5993/26604 records total

georeferenced.records <- nrow(terrestrial.mammal.records)-sum(is.na(terrestrial.mammal.records$coordinateUncertaintyInMeters))

sum(terrestrial.mammal.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records # Only 10% of georeferenced records mapped to < 100 m coordinate uncertainty

sum(terrestrial.mammal.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records * georeferenced.records

# Only about 9K of total 26k records can be analysed with confidence at 100m grid scale
