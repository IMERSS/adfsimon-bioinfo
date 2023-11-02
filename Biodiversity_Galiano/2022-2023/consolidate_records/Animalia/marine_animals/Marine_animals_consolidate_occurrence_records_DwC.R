# Script to consolidate records of Galiano Island's marine animal diversity

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Animalia/marine_animals/summaries/Galiano_marine_animals_summary_2023-11-01.csv")

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

# Sources (4/4 added):

# Agassiz (ex McMurrich 1921) - added

# Chu & Leys 2012 - added

# Galiano Island BC Canada - Marine Zoology 1893–2021 (Simon et al., 2022) - added

# iNaturalist data - added 


# Read Galiano Island BC Canada - Marine Zoology 1893–2021 

GI.1893.2021 <- read.csv("../../records/digitized/DarwinCore/Galiano_Island_BC_Canada_Marine_Zoology_1893–2021_updated_2023-04-18_AB_DwC.csv")

# Remove iNaturalist records (to be incorporated separately as a live data source)

GI.1893.2021 <- GI.1893.2021 %>% filter(institutionCode != 'iNaturalist')

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(GI.1893.2021)))
names(data.frame) <- DwCFields

data.frame[names(GI.1893.2021)] <- GI.1893.2021

GI.1893.2021 <- select(data.frame, c(1:length(DwCFields)))

# Merge with summary to standardize names and taxon metadata

GI.1893.2021$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$taxonID <- summary$ID[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$kingdom <- summary$kingdom[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$phylum <- summary$phylum[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$subphylum <- summary$subphylum[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$class <- summary$class[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$order <- summary$order[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$suborder <- summary$suborder[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$superfamily <- summary$superfamily[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$family <- summary$family[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$genus <- summary$genus[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$specificEpithet <- summary$specificEpithet[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$hybrid <- summary$hybrid[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$subspecies <- summary$subspecies[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$variety <- summary$variety[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$establishmentMeans <- summary$establishmentMeans[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$provincialStatus <- summary$provincialStatus[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]
GI.1893.2021$nationalStatus <- summary$nationalStatus[match(unlist(GI.1893.2021$scientificName), summary$scientificName)]

# Unmatched records

GI.1893.2021.names.unmatched <- GI.1893.2021[is.na(GI.1893.2021$taxonID),]

# Matched records

GI.1893.2021.names.matched <- anti_join(GI.1893.2021,GI.1893.2021.names.unmatched)

# Confirm all records are represented 

nrow(GI.1893.2021)
nrow(GI.1893.2021.names.matched)
nrow(GI.1893.2021.names.unmatched)
nrow(GI.1893.2021.names.matched)+nrow(GI.1893.2021.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

GI.1893.2021.key <- read.csv("keys/marine_animal_taxon_key_2023.csv") 

# Swap unmatched names using key

GI.1893.2021.names.unmatched.matched <- GI.1893.2021.names.unmatched

GI.1893.2021.names.unmatched.matched$scientificNameTemp <- GI.1893.2021.key$Matched.Taxon[match(unlist(GI.1893.2021.names.unmatched.matched$scientificName), GI.1893.2021.key$Taxon)]

# Add values based on newly matched name

GI.1893.2021.names.unmatched.matched$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$taxonID <- summary$ID[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$kingdom <- summary$kingdom[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$phylum <- summary$phylum[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$subphylum <- summary$subphylum[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$class <- summary$class[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$order <- summary$order[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$suborder <- summary$suborder[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$superfamily <- summary$superfamily[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$family <- summary$family[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$genus <- summary$genus[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$specificEpithet <- summary$specificEpithet[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$subspecies <- summary$subspecies[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$variety <- summary$variety[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$establishmentMeans <- summary$establishmentMeans[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$provincialStatus <- summary$provincialStatus[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]
GI.1893.2021.names.unmatched.matched$nationalStatus <- summary$nationalStatus[match(unlist(GI.1893.2021.names.unmatched.matched$scientificNameTemp), summary$scientificName)]

# Filter taxa unrecognized in summary 

GI.1893.2021.names.unmatched.unmatched <- GI.1893.2021.names.unmatched.matched[is.na(GI.1893.2021.names.unmatched.matched$taxonID),]

GI.1893.2021.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

GI.1893.2021.names.unmatched.matched$scientificName <- GI.1893.2021.names.unmatched.matched$scientificNameTemp

GI.1893.2021.names.unmatched.matched$scientificNameTemp <- NULL

GI.1893.2021.names.unmatched.matched <- GI.1893.2021.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(GI.1893.2021)
nrow(GI.1893.2021.names.matched)
nrow(GI.1893.2021.names.unmatched)
nrow(GI.1893.2021.names.unmatched.matched)
nrow(GI.1893.2021.names.unmatched.unmatched)
nrow(GI.1893.2021.names.matched)+nrow(GI.1893.2021.names.unmatched.matched)+nrow(GI.1893.2021.names.unmatched.unmatched)

# Generate review key with mismatched names
# (Once key is revised, save as 'vascular_plant_taxon_key_2022.csv' and rerun script to reconcile unmatched taxa)

key.field.names <- c('Taxon', 'Matched.Taxon', 'Critical.Note')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(GI.1893.2021.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- GI.1893.2021.names.unmatched.unmatched$scientificName

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(GI.1893.2021.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

GI.1893.2021.records <- rbind(GI.1893.2021.names.matched,GI.1893.2021.names.unmatched.matched)

# Set date formatting consistent with other data frames

GI.1893.2021.records$eventDate <- as.Date(GI.1893.2021.records$eventDate)

# Compare records in and out

nrow(GI.1893.2021) - nrow(GI.1893.2021.records)

nrow(GI.1893.2021)
nrow(GI.1893.2021.records) # 237 records omitted; those indeterminate with reference to summary

# Start record of unmatched names

unmatched.marine.animal.records <- GI.1893.2021.names.unmatched.unmatched

unmatched.marine.animal.records



# Read records from Agassiz (ex McMurrich 1921)

A.Agassiz <- read.csv("../../records/digitized/DarwinCore/McMurrich_1921_Galiano_cnidarian_records_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(A.Agassiz)))
names(data.frame) <- DwCFields

data.frame[names(A.Agassiz)] <- A.Agassiz

A.Agassiz <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "AGASSIZGI:" 
unique.suffix <- 1:nrow(A.Agassiz)

# Add metadata

A.Agassiz$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
A.Agassiz$stateProvince <- "British Columbia"
A.Agassiz$country <- "Canada"
A.Agassiz$countryCode <- "CA"
A.Agassiz$island <- "Galiano Island"
A.Agassiz$basisOfRecord <- "MaterialCitation"

# Merge with summary to standardize names and taxon metadata

A.Agassiz$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$taxonID <- summary$ID[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$kingdom <- summary$kingdom[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$phylum <- summary$phylum[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$subphylum <- summary$subphylum[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$class <- summary$class[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$order <- summary$order[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$suborder <- summary$suborder[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$superfamily <- summary$superfamily[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$family <- summary$family[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$genus <- summary$genus[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$specificEpithet <- summary$specificEpithet[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$hybrid <- summary$hybrid[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$subspecies <- summary$subspecies[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$variety <- summary$variety[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$establishmentMeans <- summary$establishmentMeans[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$provincialStatus <- summary$provincialStatus[match(unlist(A.Agassiz$scientificName), summary$scientificName)]
A.Agassiz$nationalStatus <- summary$nationalStatus[match(unlist(A.Agassiz$scientificName), summary$scientificName)]

# Unmatched records

A.Agassiz.names.unmatched <- A.Agassiz[is.na(A.Agassiz$taxonID),]

# Matched records

A.Agassiz.names.matched <- anti_join(A.Agassiz,A.Agassiz.names.unmatched)

# Confirm all records are represented 

nrow(A.Agassiz)
nrow(A.Agassiz.names.matched)
nrow(A.Agassiz.names.unmatched)
nrow(A.Agassiz.names.matched)+nrow(A.Agassiz.names.unmatched)

# All matched

A.Agassiz.records <- A.Agassiz.names.matched

A.Agassiz.records$scientificNameTemp <- NULL


# Set date formatting consistent with other data frames

A.Agassiz.records$eventDate <- as.Date(A.Agassiz.records$eventDate)

# Compare records in and out

nrow(A.Agassiz)
nrow(A.Agassiz.records)



# Read records from Chu & Leys 2012

Chu.Leys.2012 <- read.csv("../../records/digitized/DarwinCore/Chu_&_Leys_2012_Galiano_Heterochone_calyx_record_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Chu.Leys.2012)))
names(data.frame) <- DwCFields

data.frame[names(Chu.Leys.2012)] <- Chu.Leys.2012

Chu.Leys.2012 <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "CHU&LEYS2012:" 
unique.suffix <- 1:nrow(Chu.Leys.2012)

# Add metadata

Chu.Leys.2012$eventDate <- "2012-06-01" # Confirm date with Jackson
Chu.Leys.2012$datasetName <- "Chu & Leys (2012)"
Chu.Leys.2012$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Chu.Leys.2012$stateProvince <- "British Columbia"
Chu.Leys.2012$country <- "Canada"
Chu.Leys.2012$countryCode <- "CA"
Chu.Leys.2012$locality <- "Galiano Island"
Chu.Leys.2012$basisOfRecord <- "MaterialCitation"

# Merge with summary to standardize names and taxon metadata

Chu.Leys.2012$scientificNameAuthorship <- summary$scientificNameAuthorship[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$taxonID <- summary$ID[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$kingdom <- summary$kingdom[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$phylum <- summary$phylum[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$subphylum <- summary$subphylum[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$class <- summary$class[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$order <- summary$order[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$suborder <- summary$suborder[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$superfamily <- summary$superfamily[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$family <- summary$family[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$genus <- summary$genus[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$specificEpithet <- summary$specificEpithet[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$hybrid <- summary$hybrid[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$subspecies <- summary$subspecies[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$variety <- summary$variety[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$establishmentMeans <- summary$establishmentMeans[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$provincialStatus <- summary$provincialStatus[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]
Chu.Leys.2012$nationalStatus <- summary$nationalStatus[match(unlist(Chu.Leys.2012$scientificName), summary$scientificName)]

# Unmatched records

Chu.Leys.2012.names.unmatched <- Chu.Leys.2012[is.na(Chu.Leys.2012$taxonID),]

# Matched records

Chu.Leys.2012.names.matched <- anti_join(Chu.Leys.2012,Chu.Leys.2012.names.unmatched)

# Confirm all records are represented 

nrow(Chu.Leys.2012)
nrow(Chu.Leys.2012.names.matched)
nrow(Chu.Leys.2012.names.unmatched)
nrow(Chu.Leys.2012.names.matched)+nrow(Chu.Leys.2012.names.unmatched)

# All matched

Chu.Leys.2012.records <- Chu.Leys.2012.names.matched

Chu.Leys.2012.records$scientificNameTemp <- NULL

# Set date formatting consistent with other data frames

Chu.Leys.2012.records$eventDate <- as.Date(Chu.Leys.2012.records$eventDate)

# Compare records in and out

nrow(Chu.Leys.2012)
nrow(Chu.Leys.2012.records)


# Read iNaturalist data

iNaturalist.observations <- read.csv("../../../parse_iNat_records/outputs/iNat_obs_marine_animals.csv")

# Substitute iNaturalist usernames where actual observer names are missing

iNaturalist.observations.nameless <- iNaturalist.observations %>% filter(!str_detect(Recorded.by, '')) 

iNaturalist.observations.names <- anti_join(iNaturalist.observations,iNaturalist.observations.nameless)

iNaturalist.observations.nameless$recordedBy <- iNaturalist.observations.nameless$user_login

iNaturalist.observations <- rbind(iNaturalist.observations.nameless,iNaturalist.observations.names)

# Swap coordinates with private coordinates for obscured records

# iNaturalist.observations.coordinates.obscured <- iNaturalist.observations %>% drop_na(private_latitude)

# iNaturalist.observations.coordinates.unobscured <- anti_join(iNaturalist.observations,iNaturalist.observations.coordinates.obscured)

# iNaturalist.observations.coordinates.obscured$decimalLatitude <- iNaturalist.observations.coordinates.obscured$private_latitude
# iNaturalist.observations.coordinates.obscured$decimalLongitude <- iNaturalist.observations.coordinates.obscured$private_longitude

# iNaturalist.observations <- rbind(iNaturalist.observations.coordinates.obscured,iNaturalist.observations.coordinates.unobscured)

# Drop observations of taxa that are not identified to genus at least

iNaturalist.observations <- subset(iNaturalist.observations, Genus != "")

# Substitute iNaturalist taxon names with names from curated summary based on taxonID

iNaturalist.observations$swappedNames <- summary$scientificName[match(unlist(iNaturalist.observations$iNaturalist.taxon.ID), summary$ID)]

iNaturalist.observations.swapped.names <- iNaturalist.observations %>% drop_na(swappedNames)

iNaturalist.observations.unswapped.names <- anti_join(iNaturalist.observations,iNaturalist.observations.swapped.names)

iNaturalist.observations.swapped.names$iNaturalist.taxon.name <- iNaturalist.observations.swapped.names$swappedNames

iNaturalist.observations <- rbind(iNaturalist.observations.swapped.names,iNaturalist.observations.unswapped.names)

iNaturalist.observations$swappedNames <- NULL

# Add DwC fields to facilitate joins

iNaturalist.observations <- iNaturalist.observations %>% rename(scientificName = iNaturalist.taxon.name)

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
iNaturalist.observations$datasetName <- "iNaturalist"

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

iNaturalist.observations.key <- read.csv("keys/marine_animal_taxon_key_2023.csv") 

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

review.key <- rbind(GI.1893.2021.key,unmatched.taxa)

review.key[is.na(review.key)] <- ""

write.csv(review.key,"keys/review_key.csv", row.names=FALSE)

# Bind records

iNaturalist.records <- rbind(iNaturalist.observations.names.matched,iNaturalist.observations.names.unmatched.matched)

# Set date formatting consistent with other data frames

iNaturalist.records$eventDate <- as.Date(iNaturalist.records$eventDate)

# Compare records in and out

nrow(iNaturalist.observations)
nrow(iNaturalist.records) # 432 records omitted: all species resolved only to genus (redundant to list) or unrecognized in summary

unmatched.marine.animal.records <- rbind(unmatched.marine.animal.records,iNaturalist.observations.names.unmatched.unmatched)

unmatched.marine.animal.records



# Combine all source occurrence records

marine.animal.records <- rbind(GI.1893.2021.records, A.Agassiz.records, Chu.Leys.2012.records, iNaturalist.records)

# Compare catalog of occurrence records with baseline summary to see whether catalog is complete
# with respect to diversity reported in baseline summary

marine.animal.records.taxa <- unique(marine.animal.records$scientificName)
summary.taxa <-summary$scientificName

length(marine.animal.records.taxa)
length(summary.taxa)

setdiff(summary.taxa, marine.animal.records.taxa)

# Finalize DwC fields (day, month, year, infraspecificEpithet, occurrenceStatus)

# First remove NAs

marine.animal.records[is.na(marine.animal.records)] <- ""

# Date

marine.animal.records$year <- substr(marine.animal.records$eventDate, 1, 4)
marine.animal.records$month <- substr(marine.animal.records$eventDate, 6, 7)
marine.animal.records$day <- substr(marine.animal.records$eventDate, 9, 10)

# Infrataxa

Genera <- marine.animal.records %>% filter(specificEpithet == "")
Genera <- Genera %>% filter(hybrid == "")
Genera$taxonRank <- "genus" 
Genera <- subset(Genera, select = -c(hybrid, subspecies, variety, form))

Species <- subset(marine.animal.records, specificEpithet != "")
Species <- Species %>% filter(hybrid == "")
Species <- Species %>% filter(variety == "")
Species <- Species %>% filter(subspecies == "")
Species$taxonRank <- "species" 
Species <- subset(Species, select = -c(hybrid, subspecies, variety, form))

# No hybrids 

# Hybrids <- subset(marine.animal.records, hybrid != "")
# Hybrids$taxonRank <- "hybrid"
# Hybrids$infraspecificEpithet <- Hybrids$hybrid
# Hybrids <- subset(Hybrids, select = -c(hybrid, subspecies, variety, form))

Subspecies <- subset(marine.animal.records, subspecies != "")
Subspecies$taxonRank <- "subspecies"
Subspecies$infraspecificEpithet <- Subspecies$subspecies
Subspecies <- subset(Subspecies, select = -c(hybrid, subspecies, variety, form))

# Varieties <- subset(marine.animal.records, variety != "")
# Varieties$taxonRank <- "varietas"
# Varieties$infraspecificEpithet <- Varieties$variety
# Varieties <- subset(Varieties, select = -c(hybrid, subspecies, variety, form))

marine.animal.records <- rbind(Genera, Species, Subspecies)

# occurrenceStatus

marine.animal.records$occurrenceStatus <- "present"

# Order by taxon

marine.animal.records <- marine.animal.records[order(marine.animal.records$scientificName),] 

# Tally records

nrow(marine.animal.records)

# Summary of records that remain unmatched

nrow(unmatched.marine.animal.records)

sort(unique(unmatched.marine.animal.records$scientificName))

# Remove NAs

marine.animal.records[is.na(marine.animal.records)] <- ""

# Output synthesized catalog of occurrence records

write.csv(marine.animal.records,"synthesized/Galiano_marine_animal_records_consolidated.csv", row.names = FALSE)

length(unique(marine.animal.records$scientificName))

# Evaluate georeferencing resolution of marine animal records

nrow(marine.animal.records) # 19K marine animal occurrence records

marine.animal.records$coordinateUncertaintyInMeters <- as.numeric(marine.animal.records$coordinateUncertaintyInMeters)

hist(marine.animal.records$coordinateUncertaintyInMeters, 
     xlim=c(0,1000), breaks = 100000, main="Vascular Plant Records: Coordinate Uncertainty", xlab = "Coordinate Uncertainty in meters")

sum(is.na(marine.animal.records$coordinateUncertaintyInMeters))/nrow(marine.animal.records) 
# 6% of records lack coordinate uncertainty
sum(is.na(marine.animal.records$coordinateUncertaintyInMeters))/nrow(marine.animal.records) * nrow(marine.animal.records) 
# Or 1230/19700 records total

georeferenced.records <- nrow(marine.animal.records)-sum(is.na(marine.animal.records$coordinateUncertaintyInMeters))

sum(marine.animal.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records # Only 10% of georeferenced records mapped to < 100 m coordinate uncertainty

sum(marine.animal.records$coordinateUncertaintyInMeters < 100, na.rm=TRUE)/georeferenced.records * georeferenced.records

# Only about 2K of total 19k records can be analysed with confidence at 100m grid scale
