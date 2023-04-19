# Script to consolidate records of Galiano Island's marine animal diversity



# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 



# Load packages

library(dplyr)
library(stringr)
library(tidyr)



# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Animalia/marine_animals/summaries/Marine_animals_review_summary_2023-04-19.csv")



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

# Sources (3/3 added):

# Galiano Island BC Canada - Marine Zoology 1893–2021 (Simon et al., 2022) - added
# Note this version includes updated from iNat as of 2023-04-18

# Agassiz (ex McMurrich 1921) - added

# Chu & Leys 2012 - added



# Read Galiano Island BC Canada - Marine Zoology 1893–2021 

GI.1893.2021 <- read.csv("../../records/digitized/DarwinCore/Galiano_Island_BC_Canada_Marine_Zoology_1893–2021_updated_2023-04-18_AB_DwC.csv")

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

key.field.names <- c('Taxon', 'Matched.Taxon')

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



# Combine all source occurrence records

marine.animal.records <- rbind(GI.1893.2021.records, A.Agassiz.records, Chu.Leys.2012.records)


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

# Evaluate georeferencing resolution of vascular plant records

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
