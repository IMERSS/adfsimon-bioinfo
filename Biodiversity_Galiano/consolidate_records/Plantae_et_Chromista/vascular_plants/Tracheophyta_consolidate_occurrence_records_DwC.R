# Script to consolidate records of Galiano Island's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../review/Plantae_et_Chromista/vascular_plants/summaries/Galiano_Tracheophyta_review_summary_reviewed_2023-11-05.csv")

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

# Sources (11/17 added):

# BC Conservation Data Centre - added
# Brothers 2020 records - added
# ! Consortium of PNW Herbaria: BABY, RBCM, UBC, WS, WTU - some RBCM records already added; UBC records also added; otherwise incomplete
# DL63 Veg List 2001-2002 - added (georeferencing might be improved)
# Ecological Reserve 128 records - added
# ! GCA DL63 VEGETATION INVENTORY - not yet added!
# Hunterston 2010 - added
# iNaturalist 2016-2022 - added
# Janszen 2003 - added
# Laughlin Lake 2002 - added (georeferencing might be improved)
# Lomer 2022 voucher specimens - added
# Lomer 2022 Gossip Island plant list - added
# ! Matt Fairbarns Mt Sutil records - not yet added!
# ! RBCM - added, but incomplete (georeferencing corrected only in part)
# Roemer 2004 - added
# Simon 2018 - added
# ! Terry Taylor Galiano Island list 2012 - not yet added!
# UBC records - added



# Read BC Conservation Data Centre SAR records (BC CDC 2019)
# Note: request fresh data from BC CDC and ask them to include the EO ID for use as unique ID

BC.CDC.2019 <- read.csv("../../records/digitized/DarwinCore/BC_Conservation_Data_Centre_Galiano_Island_SAR_2019-10-24_DwC.csv")

# Filter CDC obs from collections at other institutions

BC.CDC.2019 <- BC.CDC.2019 %>% filter(institutionCode == 'CDC')

# Create unique identifiers for observations
# Note: this unique ID is not going to work in the long term as these data can be refreshed by the BC CDC at any point,
# changing the sequence; ask the BC CDC for EO IDs or SF IDs?

unique.prefix <- "BCCDC2019:" 
unique.suffix <- 1:nrow(BC.CDC.2019)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(BC.CDC.2019)))
names(data.frame) <- DwCFields

data.frame[names(BC.CDC.2019)] <- BC.CDC.2019

BC.CDC.2019 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

BC.CDC.2019$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
BC.CDC.2019$stateProvince <- "British Columbia"
BC.CDC.2019$island <- "Galiano Island"
BC.CDC.2019$country <- "Canada"
BC.CDC.2019$countryCode <- "CA"

# Merge with summary to standardize names and taxon metadata

BC.CDC.2019$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$taxonID <- summary$ID[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$kingdom <- summary$Kingdom[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$phylum <- summary$Phylum[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$class <- summary$Class[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$order <- summary$Order[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$suborder <- summary$Suborder[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$superfamily <- summary$Superfamily[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$family <- summary$Family[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$genus <- summary$Genus[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$specificEpithet <- summary$Species[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$hybrid <- summary$Hybrid[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$subspecies <- summary$Subspecies[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$variety <- summary$Variety[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$establishmentMeans <- summary$Origin[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$provincialStatus <- summary$Provincial.Status[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$nationalStatus <- summary$National.Status[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]

# Unmatched records

BC.CDC.2019.names.unmatched <- BC.CDC.2019[is.na(BC.CDC.2019$taxonID),]

# Matched records

BC.CDC.2019.names.matched <- anti_join(BC.CDC.2019,BC.CDC.2019.names.unmatched)

# Confirm all records are represented 

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.names.matched)
nrow(BC.CDC.2019.names.unmatched)
nrow(BC.CDC.2019.names.matched)+nrow(BC.CDC.2019.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

BC.CDC.2019.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

BC.CDC.2019.names.unmatched.matched <- BC.CDC.2019.names.unmatched

BC.CDC.2019.names.unmatched.matched$scientificName <- BC.CDC.2019.key$Matched.Taxon[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), BC.CDC.2019.key$Taxon)]

# Add values based on newly matched name

BC.CDC.2019.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$taxonID <- summary$ID[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$class <- summary$Class[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$order <- summary$Order[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$family <- summary$Family[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$genus <- summary$Genus[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$variety <- summary$Variety[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]

# Filter taxa unrecognized in summary 

BC.CDC.2019.names.unmatched.unmatched <- BC.CDC.2019.names.unmatched.matched[is.na(BC.CDC.2019.names.unmatched.matched$taxonID),]

# Confirm all records are represented 

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.names.matched)
nrow(BC.CDC.2019.names.unmatched)
nrow(BC.CDC.2019.names.unmatched.matched)
nrow(BC.CDC.2019.names.unmatched.unmatched)
nrow(BC.CDC.2019.names.matched)+nrow(BC.CDC.2019.names.unmatched.matched)+nrow(BC.CDC.2019.names.unmatched.unmatched)

# Bind records

BC.CDC.2019.records <- rbind(BC.CDC.2019.names.matched,BC.CDC.2019.names.unmatched.matched)

# Set date formatting consistent with other data frames

BC.CDC.2019.records$eventDate <- as.Date(BC.CDC.2019.records$eventDate)

# Compare records in and out

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.records) # No records omitted

# Start record of unmatched names

unmatched.vascular.plant.records <- BC.CDC.2019.names.unmatched.unmatched

unmatched.vascular.plant.records



# Read data from Brothers (2020)
# Note: check in with Kent re: datasetName field

Brothers.2020 <- read.csv("../../records/digitized/DarwinCore/Brothers_2020_Galiano_Records_DwC.csv")

# Filter out UBC collections (redundant to include alongside raw data from UBC)

Brothers.2020 <- Brothers.2020 %>% filter(institutionCode != 'UBC')

# Filter vascular plants

Brothers.2020 <- Brothers.2020 %>% filter(Phylum == 'Streptophyta')
Brothers.2020 <- Brothers.2020 %>% filter(Subclass != 'Bryidae')
Brothers.2020 <- Brothers.2020 %>% filter(Subclass != 'Marchantiidae')

# Address questionable reports

Brothers.2020$scientificName[Brothers.2020$scientificName == 'Tiarella trifoliata var. unifoliata'] <- 'Tiarella trifoliata var. trifoliata' 
# No reports of Tiarella trifoliata ssp. unifoliata otherwise reported for Galiano Island or surrounding gulf islands; likely a misreport of T. trifoliata s.str.

# Concatenate date field from raw metadata

year <- substr(Brothers.2020$eventDate, 1, 4)
month <- substr(Brothers.2020$eventDate, 5, 6)
day <- substr(Brothers.2020$eventDate, 7, 8)

Brothers.2020$eventDate <- paste(year,month,day, sep = "-")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Brothers.2020)))
names(data.frame) <- DwCFields

data.frame[names(Brothers.2020)] <- Brothers.2020

Brothers.2020 <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "BROTHERS2020:" 
unique.suffix <- 1:nrow(Brothers.2020)

# Add metadata

Brothers.2020$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Brothers.2020$stateProvince <- "British Columbia"
Brothers.2020$island <- "Galiano Island"
Brothers.2020$country <- "Canada"
Brothers.2020$countryCode <- "CA"
Brothers.2020$basisOfRecord <- "HumanObservation"
Brothers.2020$georeferenceVerificationStatus <- "unable to georeference"

# Merge with summary to standardize names and taxon metadata

Brothers.2020$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$taxonID <- summary$ID[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$kingdom <- summary$Kingdom[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$phylum <- summary$Phylum[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$class <- summary$Class[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$order <- summary$Order[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$suborder <- summary$Suborder[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$superfamily <- summary$Superfamily[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$family <- summary$Family[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$genus <- summary$Genus[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$specificEpithet <- summary$Species[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$hybrid <- summary$Hybrid[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$subspecies <- summary$Subspecies[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$variety <- summary$Variety[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$establishmentMeans <- summary$Origin[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$provincialStatus <- summary$Provincial.Status[match(unlist(Brothers.2020$scientificName), summary$Taxon)]
Brothers.2020$nationalStatus <- summary$National.Status[match(unlist(Brothers.2020$scientificName), summary$Taxon)]

# Unmatched records

Brothers.2020.names.unmatched <- Brothers.2020[is.na(Brothers.2020$taxonID),]

# Matched records

Brothers.2020.names.matched <- anti_join(Brothers.2020,Brothers.2020.names.unmatched)

# Confirm all records are represented 

nrow(Brothers.2020)
nrow(Brothers.2020.names.matched)
nrow(Brothers.2020.names.unmatched)
nrow(Brothers.2020.names.matched)+nrow(Brothers.2020.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Brothers.2020.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Brothers.2020.names.unmatched.matched <- Brothers.2020.names.unmatched

Brothers.2020.names.unmatched.matched$scientificNameTemp <- Brothers.2020.key$Matched.Taxon[match(unlist(Brothers.2020.names.unmatched.matched$scientificName), Brothers.2020.key$Taxon)]

# Add values based on newly matched name

Brothers.2020.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$class <- summary$Class[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$order <- summary$Order[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$family <- summary$Family[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$genus <- summary$Genus[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$variety <- summary$Variety[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Brothers.2020.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Brothers.2020.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Brothers.2020.names.unmatched.unmatched <- Brothers.2020.names.unmatched.matched[is.na(Brothers.2020.names.unmatched.matched$taxonID),]

Brothers.2020.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Brothers.2020.names.unmatched.matched$scientificName <- Brothers.2020.names.unmatched.matched$scientificNameTemp

Brothers.2020.names.unmatched.matched$scientificNameTemp <- NULL

Brothers.2020.names.unmatched.matched <- Brothers.2020.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Brothers.2020)
nrow(Brothers.2020.names.matched)
nrow(Brothers.2020.names.unmatched)
nrow(Brothers.2020.names.unmatched.matched)
nrow(Brothers.2020.names.unmatched.unmatched)
nrow(Brothers.2020.names.matched)+nrow(Brothers.2020.names.unmatched.matched)+nrow(Brothers.2020.names.unmatched.unmatched)

# Bind records

Brothers.2020.records <- rbind(Brothers.2020.names.matched,Brothers.2020.names.unmatched.matched)

# Set date formatting consistent with other data frames

Brothers.2020.records$eventDate <- as.Date(Brothers.2020.records$eventDate)

# Compare records in and out

nrow(Brothers.2020)
nrow(Brothers.2020.records) # 7 records omitted: all taxa identified only to genus or of uncertain identity (cf)

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Brothers.2020.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read DL63 records (GCA 2002)

DL63 <- read.csv("../../records/digitized/DarwinCore/DL63_veg_list_2001-2002_DwC.csv")

# Filter plants

DL63 <- DL63 %>% filter(Group == 'vascular')

# Create unique identifiers for observations

unique.prefix <- "GCA2002:"
unique.suffix <- 1:nrow(DL63)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(DL63)))
names(data.frame) <- DwCFields

data.frame[names(DL63)] <- DL63

DL63 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

DL63$associatedReferences <- "Gaylor N, Scholz O, Erickson K (2002) Restoration Plan for District Lot 63 of the Pebble Beach Nature Reserve, Galiano Island, BC. Unpublished report. Available online:https://www.for.gov.bc.ca/hfd/library/fia/2002/FIA2002MR003.pdf"
DL63$bibliographicCitation <- "Gaylor N, Scholz O, Erickson K (2002) Restoration Plan for District Lot 63 of the Pebble Beach Nature Reserve, Galiano Island, BC. Unpublished report. Available online:https://www.for.gov.bc.ca/hfd/library/fia/2002/FIA2002MR003.pdf"
DL63$datasetName <- "DL63 veg list 2001-2002"
DL63$recordedBy <- "Nathan Gaylor, Odin Scholz & Keith Erickson"
DL63$eventDate <- '2002-03-01'
DL63$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
DL63$decimalLatitude <- 48.946327
DL63$decimalLongitude <- -123.493078
DL63$georeferencedBy <- "Andrew Simon"
DL63$georeferenceProtocol <- "coordinates generalized based on locality information: centroid of District Lot 63; positional accuracy defining a circle that encompasses the property"
DL63$georeferenceVerificationStatus <- "requires verification"
DL63$coordinateUncertaintyInMeters <- 380
DL63$stateProvince <- "British Columbia"
DL63$island <- "Galiano Island"
DL63$locality <- "District Lot 63, Pebble Beach Reserve"
DL63$country <- "Canada"
DL63$countryCode <- "CA"
DL63$basisOfRecord <- "HumanObservation"

# Drop / correct spurious reports

DL63 <- DL63 %>% filter(!str_detect(scientificName, 'Cardamine angulata')) # This might be Cardamine nuttallii but cannot be certain

DL63 <- DL63 %>% filter(!str_detect(scientificName, 'Fraxinus latifolia')) # Not known to southern Gulf Islands; likely misidentified

DL63$scientificName[DL63$scientificName == 'Fragaria virginiana'] <- 'Fragaria vesca' # Reports of F. virginiana invariably apply to F. vesca on Galiano

DL63$scientificName[DL63$scientificName == 'Sorbus sitchensis'] <- 'Sorbus aucuparia' # Sorbus sitchensis not known to occur on Galiano Island; S. aucuparia confirmed for Pebble Beach and commonly conflated

DL63$scientificName[DL63$scientificName == 'Trientalis borealis'] <- 'Lysimachia latifolia' # Trientalis borealis misapplied; Lysimachia latifolia recognized as present

DL63$scientificName[DL63$scientificName == 'Vaccinium ovalifolium'] <- 'Vaccinium ovatum' # Vaccinium ovalifolium likely misapplied, or typo (Keith Erickson, pers. comm. 2022); V. ovatum recognized as present

# Merge with summary to standardize names and taxon metadata

DL63$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$taxonID <- summary$ID[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$kingdom <- summary$Kingdom[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$phylum <- summary$Phylum[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$class <- summary$Class[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$order <- summary$Order[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$suborder <- summary$Suborder[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$superfamily <- summary$Superfamily[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$family <- summary$Family[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$genus <- summary$Genus[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$specificEpithet <- summary$Species[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$hybrid <- summary$Hybrid[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$subspecies <- summary$Subspecies[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$variety <- summary$Variety[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$establishmentMeans <- summary$Origin[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$provincialStatus <- summary$Provincial.Status[match(unlist(DL63$scientificName), summary$Taxon)]
DL63$nationalStatus <- summary$National.Status[match(unlist(DL63$scientificName), summary$Taxon)]

# Unmatched records

DL63.names.unmatched <- DL63[is.na(DL63$taxonID),]

# Matched records

DL63.names.matched <- anti_join(DL63,DL63.names.unmatched)

# Confirm all records are represented 

nrow(DL63)
nrow(DL63.names.matched)
nrow(DL63.names.unmatched)
nrow(DL63.names.matched)+nrow(DL63.names.unmatched)

# Read key to reconcile mismatches with summary

DL63.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

DL63.names.unmatched.matched <- DL63.names.unmatched

DL63.names.unmatched.matched$scientificNameTemp <- DL63.key$Matched.Taxon[match(unlist(DL63.names.unmatched.matched$scientificName), DL63.key$Taxon)]

# Add values based on newly matched name

DL63.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$taxonID <- summary$ID[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$class <- summary$Class[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$order <- summary$Order[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$family <- summary$Family[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$genus <- summary$Genus[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$variety <- summary$Variety[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
DL63.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(DL63.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

DL63.names.unmatched.unmatched <- DL63.names.unmatched.matched[is.na(DL63.names.unmatched.matched$taxonID),]

DL63.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

DL63.names.unmatched.matched <- DL63.names.unmatched.matched %>% drop_na(taxonID)

DL63.names.unmatched.matched$scientificName <- DL63.names.unmatched.matched$scientificNameTemp

DL63.names.unmatched.matched$scientificNameTemp <- NULL

# Confirm all records are represented 

nrow(DL63)
nrow(DL63.names.matched)
nrow(DL63.names.unmatched)
nrow(DL63.names.unmatched.matched)
nrow(DL63.names.unmatched.unmatched)
nrow(DL63.names.matched)+nrow(DL63.names.unmatched.matched)+nrow(DL63.names.unmatched.unmatched)

# Bind records

DL63.records <- rbind(DL63.names.matched,DL63.names.unmatched.matched)

# Set date formatting consistent with other data frames

DL63.records$eventDate <- as.Date(DL63.records$eventDate)

# Compare records in and out

nrow(DL63)
nrow(DL63.records)  # Eight records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog, are all species identified only to genus.
# See also misreported taxa manually excluded from data set above

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,DL63.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Ecological Reserve 128 records (Roemer & Janszen 1980, Roemer 2000)

Ecological.Reserve.128 <- read.csv("../../records/digitized/DarwinCore/Galiano_Bog_Plant_List_Roemer_2000_DwC.csv")

# Filter plants

Ecological.Reserve.128 <- Ecological.Reserve.128 %>% filter(Group == 'vascular plants')

# Create unique identifiers for observations

unique.prefix <- "ROEMER2000:"
unique.suffix <- 1:nrow(Ecological.Reserve.128)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Ecological.Reserve.128)))
names(data.frame) <- DwCFields

data.frame[names(Ecological.Reserve.128)] <- Ecological.Reserve.128

Ecological.Reserve.128 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Ecological.Reserve.128$associatedReferences <- "Roemer H (2000) Galiano Bog Plant List. Unpublished species list."
Ecological.Reserve.128$bibliographicCitation <- "Roemer H (2000) Galiano Bog Plant List. Unpublished species list."
Ecological.Reserve.128$datasetName <- "Galiano Bog Plant List"
Ecological.Reserve.128$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Ecological.Reserve.128$decimalLatitude <- 48.983312863031706
Ecological.Reserve.128$decimalLongitude <- -123.55665029568577
Ecological.Reserve.128$georeferencedBy <- "Andrew Simon"
Ecological.Reserve.128$georeferenceProtocol <- "coordinates generalized based on locality information"
Ecological.Reserve.128$georeferenceVerificationStatus <- "verified by data custodian"
Ecological.Reserve.128$coordinateUncertaintyInMeters <- 80
Ecological.Reserve.128$stateProvince <- "British Columbia"
Ecological.Reserve.128$island <- "Galiano Island"
Ecological.Reserve.128$locality <- "Ecological Reserve 128"
Ecological.Reserve.128$country <- "Canada"
Ecological.Reserve.128$countryCode <- "CA"
Ecological.Reserve.128$basisOfRecord <- "HumanObservation"

# Concatenate 'occurrenceRemarks' field from metadata

Ecological.Reserve.128$occurrenceRemarks <- paste('Abundance:',data.frame$Abundance.Notes,data.frame$Notes, sep = " ")

# Merge with summary to standardize names and taxon metadata

Ecological.Reserve.128$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$taxonID <- summary$ID[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$kingdom <- summary$Kingdom[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$phylum <- summary$Phylum[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$class <- summary$Class[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$order <- summary$Order[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$suborder <- summary$Suborder[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$superfamily <- summary$Superfamily[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$family <- summary$Family[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$genus <- summary$Genus[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$specificEpithet <- summary$Species[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$hybrid <- summary$Hybrid[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$subspecies <- summary$Subspecies[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$variety <- summary$Variety[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$establishmentMeans <- summary$Origin[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$provincialStatus <- summary$Provincial.Status[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]
Ecological.Reserve.128$nationalStatus <- summary$National.Status[match(unlist(Ecological.Reserve.128$scientificName), summary$Taxon)]

# Unmatched records

Ecological.Reserve.128.names.unmatched <- Ecological.Reserve.128[is.na(Ecological.Reserve.128$taxonID),]

# Matched records

Ecological.Reserve.128.names.matched <- anti_join(Ecological.Reserve.128,Ecological.Reserve.128.names.unmatched)

# Confirm all records are represented 

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.names.matched)
nrow(Ecological.Reserve.128.names.unmatched)
nrow(Ecological.Reserve.128.names.matched)+nrow(Ecological.Reserve.128.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Ecological.Reserve.128.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched

Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp <- Ecological.Reserve.128.key$Matched.Taxon[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificName), Ecological.Reserve.128.key$Taxon)]

# Add values based on newly matched name

Ecological.Reserve.128.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$class <- summary$Class[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$order <- summary$Order[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$family <- summary$Family[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$genus <- summary$Genus[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$variety <- summary$Variety[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Ecological.Reserve.128.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Ecological.Reserve.128.names.unmatched.unmatched <- Ecological.Reserve.128.names.unmatched.matched[is.na(Ecological.Reserve.128.names.unmatched.matched$taxonID),]

Ecological.Reserve.128.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Ecological.Reserve.128.names.unmatched.matched$scientificName <- Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp

Ecological.Reserve.128.names.unmatched.matched$scientificNameTemp <- NULL

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.names.matched)
nrow(Ecological.Reserve.128.names.unmatched)
nrow(Ecological.Reserve.128.names.unmatched.matched)
nrow(Ecological.Reserve.128.names.unmatched.unmatched)
nrow(Ecological.Reserve.128.names.matched)+nrow(Ecological.Reserve.128.names.unmatched.matched)+nrow(Ecological.Reserve.128.names.unmatched.unmatched)

# Bind records

Ecological.Reserve.128.records <- rbind(Ecological.Reserve.128.names.matched,Ecological.Reserve.128.names.unmatched.matched)

# Set date formatting consistent with other data frames

Ecological.Reserve.128.records$eventDate <- as.Date(Ecological.Reserve.128.records$eventDate)

# Compare records in and out

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.records) # Good: only five records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog:
# Juncus effusus - infrataxonomic resolution required to meaningfully discriminate this taxon
# Epilobium cf ciliatum - could be E. ciliatum; could also be Epilobium leptophyllum, which has since documented at the bog
# Glyceria sp.

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Ecological.Reserve.128.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Hunterston Farm Bioblitz 2010 records

Hunterston.2010 <- read.csv("../../records/digitized/DarwinCore/Hunterston_Farms_Bioblitz_2010_sorted_2022-10-16_DwC.csv")

# Filter plants

Hunterston.2010 <- Hunterston.2010 %>% filter(Group == 'Plants')

# Create unique identifiers for observations

unique.prefix <- "HUNTERSTON2010:"
unique.suffix <- 1:nrow(Hunterston.2010)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Hunterston.2010)))
names(data.frame) <- DwCFields

data.frame[names(Hunterston.2010)] <- Hunterston.2010

Hunterston.2010 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Hunterston.2010$associatedReferences <- "Hunterston Farms 2010 Hunterston Farm Stewardship Report: A Review of the 2010 BioBlitz. Unpublished species list."
Hunterston.2010$bibliographicCitation <- "Hunterston Farms 2010 Hunterston Farm Stewardship Report: A Review of the 2010 BioBlitz. Unpublished species list."
Hunterston.2010$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Hunterston.2010$georeferencedBy <- "Andrew Simon"
Hunterston.2010$georeferenceProtocol <- "coordinates generalized based on locality information"
Hunterston.2010$georeferenceVerificationStatus <- "verified by data custodian"
Hunterston.2010$coordinateUncertaintyInMeters <- 50
Hunterston.2010$stateProvince <- "British Columbia"
Hunterston.2010$island <- "Galiano Island"
Hunterston.2010$locality <- "Hunterston Farms"
Hunterston.2010$country <- "Canada"
Hunterston.2010$countryCode <- "CA"
Hunterston.2010$basisOfRecord <- "HumanObservation"

# Concatenate 'fieldNumber' field from metadata

Hunterston.2010$fieldNumber <- paste('Zone:', data.frame$Zone, sep = " ")

# Merge with summary to standardize names and taxon metadata

Hunterston.2010$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$taxonID <- summary$ID[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$kingdom <- summary$Kingdom[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$phylum <- summary$Phylum[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$class <- summary$Class[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$order <- summary$Order[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$suborder <- summary$Suborder[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$superfamily <- summary$Superfamily[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$family <- summary$Family[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$genus <- summary$Genus[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$specificEpithet <- summary$Species[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$hybrid <- summary$Hybrid[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$subspecies <- summary$Subspecies[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$variety <- summary$Variety[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$establishmentMeans <- summary$Origin[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$provincialStatus <- summary$Provincial.Status[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]
Hunterston.2010$nationalStatus <- summary$National.Status[match(unlist(Hunterston.2010$scientificName), summary$Taxon)]

# Unmatched records

Hunterston.2010.names.unmatched <- Hunterston.2010[is.na(Hunterston.2010$taxonID),]

# Matched records

Hunterston.2010.names.matched <- anti_join(Hunterston.2010,Hunterston.2010.names.unmatched)

# Confirm all records are represented 

nrow(Hunterston.2010)
nrow(Hunterston.2010.names.matched)
nrow(Hunterston.2010.names.unmatched)
nrow(Hunterston.2010.names.matched)+nrow(Hunterston.2010.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Hunterston.2010.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Hunterston.2010.names.unmatched.matched <- Hunterston.2010.names.unmatched

Hunterston.2010.names.unmatched.matched$scientificNameTemp <- Hunterston.2010.key$Matched.Taxon[match(unlist(Hunterston.2010.names.unmatched.matched$scientificName), Hunterston.2010.key$Taxon)]

# Add values based on newly matched name

Hunterston.2010.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$class <- summary$Class[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$order <- summary$Order[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$family <- summary$Family[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$genus <- summary$Genus[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$variety <- summary$Variety[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Hunterston.2010.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Hunterston.2010.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Hunterston.2010.names.unmatched.unmatched <- Hunterston.2010.names.unmatched.matched[is.na(Hunterston.2010.names.unmatched.matched$taxonID),]

Hunterston.2010.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Hunterston.2010.names.unmatched.matched$scientificName <- Hunterston.2010.names.unmatched.matched$scientificNameTemp

Hunterston.2010.names.unmatched.matched$scientificNameTemp <- NULL

Hunterston.2010.names.unmatched.matched <- Hunterston.2010.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Hunterston.2010)
nrow(Hunterston.2010.names.matched)
nrow(Hunterston.2010.names.unmatched)
nrow(Hunterston.2010.names.unmatched.matched)
nrow(Hunterston.2010.names.unmatched.unmatched)
nrow(Hunterston.2010.names.matched)+nrow(Hunterston.2010.names.unmatched.matched)+nrow(Hunterston.2010.names.unmatched.unmatched)

# Bind records

Hunterston.2010.records <- rbind(Hunterston.2010.names.matched,Hunterston.2010.names.unmatched.matched)

# Set date formatting consistent with other data frames

Hunterston.2010.records$eventDate <- as.Date(Hunterston.2010.records$eventDate)

# Compare records in and out

nrow(Hunterston.2010)
nrow(Hunterston.2010.records) # 32 records discarded
# discarded records identified only to level of genus, i.e., redundant to include in summary
# also discarded: Juncus effusus, which requires infrataxonomic resolution

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Hunterston.2010.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read iNaturalist data

iNaturalist.observations <- read.csv("../../..//parse_records/outputs/iNat_obs_Tracheophyta.csv")

# Drop observations of taxa that are not identified to genus at least

iNaturalist.observations <- subset(iNaturalist.observations, Genus != "")

# Change iNat obs field names to DwC format

iNaturalist.observations

names(iNaturalist.observations)[names(iNaturalist.observations) == 'iNaturalist.taxon.name'] <- 'scientificName'
names(iNaturalist.observations)[names(iNaturalist.observations) == 'observationID'] <- 'occurrenceID'
names(iNaturalist.observations)[names(iNaturalist.observations) == 'Latitude'] <- 'decimalLatitude'
names(iNaturalist.observations)[names(iNaturalist.observations) == 'Longitude'] <- 'decimalLongitude'
names(iNaturalist.observations)[names(iNaturalist.observations) == 'Date.observed'] <- 'eventDate'
names(iNaturalist.observations)[names(iNaturalist.observations) == 'Recorded.by'] <- 'recordedBy'

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

iNaturalist.observations$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$taxonID <- summary$ID[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$kingdom <- summary$Kingdom[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$phylum <- summary$Phylum[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$class <- summary$Class[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$order <- summary$Order[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$suborder <- summary$Suborder[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$superfamily <- summary$Superfamily[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$family <- summary$Family[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$genus <- summary$Genus[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$specificEpithet <- summary$Species[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$hybrid <- summary$Hybrid[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$subspecies <- summary$Subspecies[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$variety <- summary$Variety[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$establishmentMeans <- summary$Origin[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$provincialStatus <- summary$Provincial.Status[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]
iNaturalist.observations$nationalStatus <- summary$National.Status[match(unlist(iNaturalist.observations$scientificName), summary$Taxon)]

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

iNaturalist.observations.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

iNaturalist.observations.names.unmatched.matched <- iNaturalist.observations.names.unmatched

iNaturalist.observations.names.unmatched.matched$scientificNameTemp <- iNaturalist.observations.key$Matched.Taxon[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificName), iNaturalist.observations.key$Taxon)]

# Add values based on newly matched name

iNaturalist.observations.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$taxonID <- summary$ID[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$class <- summary$Class[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$order <- summary$Order[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$family <- summary$Family[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$genus <- summary$Genus[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$variety <- summary$Variety[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
iNaturalist.observations.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(iNaturalist.observations.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

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

# Bind records

iNaturalist.records <- rbind(iNaturalist.observations.names.matched,iNaturalist.observations.names.unmatched.matched)

# Set date formatting consistent with other data frames

iNaturalist.records$eventDate <- as.Date(iNaturalist.records$eventDate)

# Compare records in and out

nrow(iNaturalist.observations)
nrow(iNaturalist.records) # 783 records omitted: all species resolved only to genus (redundant to list) or unrecognized in summary (cultivated plants, etc.)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,iNaturalist.observations.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Janszen 2003 Outer Gulf Islands Vascular Plant Checklist records

Janszen.2003 <- read.csv("../../records/digitized/DarwinCore/Janszen_2003_Outer_Gulf_Islands_Vascular_Plant_Checklist_DwC.csv")

# Create unique identifiers for observations

unique.prefix <- "JANSZEN2003:"
unique.suffix <- 1:nrow(Janszen.2003)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Janszen.2003)))
names(data.frame) <- DwCFields

data.frame[names(Janszen.2003)] <- Janszen.2003

Janszen.2003 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Janszen.2003$associatedReferences <- "Janszen H (2003) Outer Gulf Islands Vascular Plant Checklist. Unpublished species list."
Janszen.2003$bibliographicCitation <- "Janszen H (2003) Outer Gulf Islands Vascular Plant Checklist. Unpublished species list."
Janszen.2003$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Janszen.2003$georeferenceVerificationStatus <- "unable to georeference"
Janszen.2003$stateProvince <- "British Columbia"
Janszen.2003$island <- "Galiano Island"
Janszen.2003$country <- "Canada"
Janszen.2003$countryCode <- "CA"
Janszen.2003$basisOfRecord <- "MaterialCitation"

# Drop / correct spurious reports

Janszen.2003$scientificName[Janszen.2003$scientificName == 'P. oakesianus'] <- 'Potamogeton gramineus' # narrow-leaved form, sometimes called P. gramineus var. myriophyllus, misreported as Potamogeton oakesianus (specimen deposited, though undigitized, at V) (Frank Lomer, pers. comm. 2022)

# Merge with summary to standardize names and taxon metadata

Janszen.2003$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$taxonID <- summary$ID[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$kingdom <- summary$Kingdom[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$phylum <- summary$Phylum[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$class <- summary$Class[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$order <- summary$Order[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$suborder <- summary$Suborder[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$superfamily <- summary$Superfamily[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$family <- summary$Family[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$genus <- summary$Genus[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$specificEpithet <- summary$Species[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$hybrid <- summary$Hybrid[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$subspecies <- summary$Subspecies[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$variety <- summary$Variety[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$establishmentMeans <- summary$Origin[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$provincialStatus <- summary$Provincial.Status[match(unlist(Janszen.2003$scientificName), summary$Taxon)]
Janszen.2003$nationalStatus <- summary$National.Status[match(unlist(Janszen.2003$scientificName), summary$Taxon)]

# Unmatched records

Janszen.2003.names.unmatched <- Janszen.2003[is.na(Janszen.2003$taxonID),]

# Matched records

Janszen.2003.names.matched <- anti_join(Janszen.2003,Janszen.2003.names.unmatched)

# Confirm all records are represented 

nrow(Janszen.2003)
nrow(Janszen.2003.names.matched)
nrow(Janszen.2003.names.unmatched)
nrow(Janszen.2003.names.matched)+nrow(Janszen.2003.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Janszen.2003.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Janszen.2003.names.unmatched.matched <- Janszen.2003.names.unmatched

Janszen.2003.names.unmatched.matched$scientificNameTemp <- Janszen.2003.key$Matched.Taxon[match(unlist(Janszen.2003.names.unmatched.matched$scientificName), Janszen.2003.key$Taxon)]

# Add values based on newly matched name

Janszen.2003.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$class <- summary$Class[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$order <- summary$Order[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$family <- summary$Family[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$genus <- summary$Genus[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$variety <- summary$Variety[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Janszen.2003.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Janszen.2003.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Janszen.2003.names.unmatched.unmatched <- Janszen.2003.names.unmatched.matched[is.na(Janszen.2003.names.unmatched.matched$taxonID),]

Janszen.2003.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Janszen.2003.names.unmatched.matched$scientificName <- Janszen.2003.names.unmatched.matched$scientificNameTemp

Janszen.2003.names.unmatched.matched$scientificNameTemp <- NULL

Janszen.2003.names.unmatched.matched <- Janszen.2003.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Janszen.2003)
nrow(Janszen.2003.names.matched)
nrow(Janszen.2003.names.unmatched)
nrow(Janszen.2003.names.unmatched.matched)
nrow(Janszen.2003.names.unmatched.unmatched)
nrow(Janszen.2003.names.matched)+nrow(Janszen.2003.names.unmatched.matched)+nrow(Janszen.2003.names.unmatched.unmatched)

# Bind records

Janszen.2003.records <- rbind(Janszen.2003.names.matched,Janszen.2003.names.unmatched.matched)

# Set date formatting consistent with other data frames

Janszen.2003.records$eventDate <- as.Date(Janszen.2003.records$eventDate)

# Compare records in and out

nrow(Janszen.2003)
nrow(Janszen.2003.records) # 5 records omitted
# discarded records identified only to level of genus, i.e., redundant to include in summary
# also discarded: Juncus effusus and other taxa which require infrataxonomic resolution
# the following putative hybrids and domesticated taxa are also unrecognized:
# Aster chilensis x subspicatus; Gallium mollugo
# Finally, the report of Potamogeton oakesianus has since been ruled out as a variation of
# P. gramineus (this issue is treated above; not through key)

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Janszen.2003.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Laughlin Lake 2002 records (GCA 2002)

Laughlin.2002 <- read.csv("../../records/digitized/DarwinCore/Laughlin_Vegetation_Data_2002-08-20_DwC.csv")

# Create unique identifiers for observations

unique.prefix <- "GCA2002-2:"
unique.suffix <- 1:nrow(Laughlin.2002)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Laughlin.2002)))
names(data.frame) <- DwCFields

data.frame[names(Laughlin.2002)] <- Laughlin.2002

Laughlin.2002 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Laughlin.2002$associatedReferences <- "Erickson K, Gates S (2002) Laughlin Lake Vegetation Community Mapping. Unpublished species list."
Laughlin.2002$bibliographicCitation <- "Erickson K, Gates S (2002) Laughlin Lake Vegetation Community Mapping. Unpublished species list."
Laughlin.2002$datasetName <- "Laughlin Lake Vegetation Community Mapping"
Laughlin.2002$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Laughlin.2002$recordedBy <- "Keith Erickson & Steven Gates"
Laughlin.2002$eventDate <- "2002-08-20"
Laughlin.2002$decimalLatitude <- 48.949776874128624
Laughlin.2002$decimalLongitude <- -123.50573155652819
Laughlin.2002$coordinateUncertaintyInMeters <- 360
Laughlin.2002$georeferenceProtocol <- "coordinates generalized based on locality information"
Laughlin.2002$georeferenceVerificationStatus <- "verified by data custodian"
Laughlin.2002$stateProvince <- "British Columbia"
Laughlin.2002$locality <- "Laughlin Lake"
Laughlin.2002$island <- "Galiano Island"
Laughlin.2002$country <- "Canada"
Laughlin.2002$countryCode <- "CA"
Laughlin.2002$basisOfRecord <- "HumanObservation"

# Concatenate 'fieldNumber' field from metadata

Laughlin.2002$fieldNumber <- paste('Polygon #:', data.frame$Polygon.., sep = " ")

# Concatenate 'occurrenceRemarks' field from metadata

# First translate values 'P' and 'n'

Laughlin.2002$scientificName[Laughlin.2002$scientificName == 'P'] <- 'Present but less than 1%' # See annotations on species list

Laughlin.2002$scientificName[Laughlin.2002$scientificName == 'n'] <- 'No percent cover estimated' # See annotations on species list

Laughlin.2002$occurrenceRemarks <- paste('Dominance:', data.frame$Dominance, 'Percent Cover:', data.frame$X..cover,  sep = " ")

# Drop / correct spurious reports

Laughlin.2002 <- Laughlin.2002 %>% filter(!str_detect(scientificName, 'Carex rostrata')) # Out of range on Galiano Island; misreported

Laughlin.2002$scientificName[Laughlin.2002$scientificName == 'Typha latifola'] <- 'Typha angustifolia' # Bullrush at Laughlin Lake has since been confirmed as Typha angustifolium

Laughlin.2002$scientificName[Laughlin.2002$scientificName == 'Epilobium watsonii'] <- 'Typha angustifolia' # Epilobium watsonii some recognize as infrascientificName of Epilobium ciliatum; collapsed with Epilobium ciliatum (report noted in curated summary)

# Merge with summary to standardize names and taxon metadata

Laughlin.2002$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$taxonID <- summary$ID[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$kingdom <- summary$Kingdom[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$phylum <- summary$Phylum[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$class <- summary$Class[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$order <- summary$Order[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$suborder <- summary$Suborder[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$superfamily <- summary$Superfamily[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$family <- summary$Family[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$genus <- summary$Genus[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$specificEpithet <- summary$Species[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$hybrid <- summary$Hybrid[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$subspecies <- summary$Subspecies[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$variety <- summary$Variety[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$establishmentMeans <- summary$Origin[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$provincialStatus <- summary$Provincial.Status[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]
Laughlin.2002$nationalStatus <- summary$National.Status[match(unlist(Laughlin.2002$scientificName), summary$Taxon)]

# Unmatched records

Laughlin.2002.names.unmatched <- Laughlin.2002[is.na(Laughlin.2002$taxonID),]

# Matched records

Laughlin.2002.names.matched <- anti_join(Laughlin.2002,Laughlin.2002.names.unmatched)

# Confirm all records are represented 

nrow(Laughlin.2002)
nrow(Laughlin.2002.names.matched)
nrow(Laughlin.2002.names.unmatched)
nrow(Laughlin.2002.names.matched)+nrow(Laughlin.2002.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Laughlin.2002.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Laughlin.2002.names.unmatched.matched <- Laughlin.2002.names.unmatched

Laughlin.2002.names.unmatched.matched$scientificNameTemp <- Laughlin.2002.key$Matched.Taxon[match(unlist(Laughlin.2002.names.unmatched.matched$scientificName), Laughlin.2002.key$Taxon)]

# Add values based on newly matched name

Laughlin.2002.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$class <- summary$Class[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$order <- summary$Order[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$family <- summary$Family[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$genus <- summary$Genus[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$variety <- summary$Variety[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Laughlin.2002.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Laughlin.2002.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Laughlin.2002.names.unmatched.unmatched <- Laughlin.2002.names.unmatched.matched[is.na(Laughlin.2002.names.unmatched.matched$taxonID),]

Laughlin.2002.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Laughlin.2002.names.unmatched.matched$scientificName <- Laughlin.2002.names.unmatched.matched$scientificNameTemp

Laughlin.2002.names.unmatched.matched$scientificNameTemp <- NULL

Laughlin.2002.names.unmatched.matched <- Laughlin.2002.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Laughlin.2002)
nrow(Laughlin.2002.names.matched)
nrow(Laughlin.2002.names.unmatched)
nrow(Laughlin.2002.names.unmatched.matched)
nrow(Laughlin.2002.names.unmatched.unmatched)
nrow(Laughlin.2002.names.matched)+nrow(Laughlin.2002.names.unmatched.matched)+nrow(Laughlin.2002.names.unmatched.unmatched)

# Bind records

Laughlin.2002.records <- rbind(Laughlin.2002.names.matched,Laughlin.2002.names.unmatched.matched)

# Set date formatting consistent with other data frames

Laughlin.2002.records$eventDate <- as.Date(Laughlin.2002.records$eventDate)

# Compare records in and out

nrow(Laughlin.2002)
nrow(Laughlin.2002.records) # 18 records discarded

# Note: taxa unrecognized in summary, and hence excluded from catalog, are either indeterminate taxa or Juncus effusus,
# which cannot be resolved based on the summary due to the lack of infraspecific resolution
# See also misreported taxa manually addressed above

# Add to record of unmatched names

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Laughlin.2002.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Frank Lomer 2022 Records
# Note: specimens will all be deposited at UBC and should be checked for duplicates against UBC records in the future 

Lomer.2022 <- read.csv("../../records/digitized/DarwinCore/Lomer_2022_Galiano_collections_DwC.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Lomer.2022)))
names(data.frame) <- DwCFields

data.frame[names(Lomer.2022)] <- Lomer.2022

Lomer.2022 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Lomer.2022$stateProvince <- "British Columbia"
Lomer.2022$island <- "Galiano Island"
Lomer.2022$country <- "Canada"
Lomer.2022$countryCode <- "CA"
Lomer.2022$coordinateUncertaintyInMeters <- 3
Lomer.2022$georeferenceProtocol <- "coordinates mapped using Garmin GPS"
Lomer.2022$georeferenceVerificationStatus <- "verified by contributor"
Lomer.2022$basisOfRecord <- "PreservedSpecimen"

# Change date to POSIX

Lomer.2022$eventDate <- strptime(Lomer.2022$eventDate, "%Y %b %d")

# Merge with summary to standardize names and taxon metadata

Lomer.2022$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$taxonID <- summary$ID[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$kingdom <- summary$Kingdom[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$phylum <- summary$Phylum[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$class <- summary$Class[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$order <- summary$Order[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$suborder <- summary$Suborder[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$superfamily <- summary$Superfamily[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$family <- summary$Family[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$genus <- summary$Genus[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$specificEpithet <- summary$Species[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$hybrid <- summary$Hybrid[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$subspecies <- summary$Subspecies[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$variety <- summary$Variety[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$establishmentMeans <- summary$Origin[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$provincialStatus <- summary$Provincial.Status[match(unlist(Lomer.2022$scientificName), summary$Taxon)]
Lomer.2022$nationalStatus <- summary$National.Status[match(unlist(Lomer.2022$scientificName), summary$Taxon)]

# Unmatched records

Lomer.2022.names.unmatched <- Lomer.2022[is.na(Lomer.2022$taxonID),]

# Matched records

Lomer.2022.names.matched <- anti_join(Lomer.2022,Lomer.2022.names.unmatched)

# Confirm all records are represented 

nrow(Lomer.2022)
nrow(Lomer.2022.names.matched)
nrow(Lomer.2022.names.unmatched)
nrow(Lomer.2022.names.matched)+nrow(Lomer.2022.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Lomer.2022.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched

Lomer.2022.names.unmatched.matched$scientificNameTemp <- Lomer.2022.key$Matched.Taxon[match(unlist(Lomer.2022.names.unmatched.matched$scientificName), Lomer.2022.key$Taxon)]

# Add values based on newly matched name

Lomer.2022.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$class <- summary$Class[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$order <- summary$Order[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$family <- summary$Family[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$genus <- summary$Genus[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$variety <- summary$Variety[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Lomer.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Lomer.2022.names.unmatched.unmatched <- Lomer.2022.names.unmatched.matched[is.na(Lomer.2022.names.unmatched.matched$taxonID),]

Lomer.2022.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Lomer.2022.names.unmatched.matched$scientificName <- Lomer.2022.names.unmatched.matched$scientificNameTemp

Lomer.2022.names.unmatched.matched$scientificNameTemp <- NULL

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Lomer.2022)
nrow(Lomer.2022.names.matched)
nrow(Lomer.2022.names.unmatched)
nrow(Lomer.2022.names.unmatched.matched)
nrow(Lomer.2022.names.unmatched.unmatched)
nrow(Lomer.2022.names.matched)+nrow(Lomer.2022.names.unmatched.matched)+nrow(Lomer.2022.names.unmatched.unmatched)

# Bind records

Lomer.2022.records <- rbind(Lomer.2022.names.matched,Lomer.2022.names.unmatched.matched)

# Set date formatting consistent with other data frames

Lomer.2022.records$eventDate <- as.Date(Lomer.2022.records$eventDate)

# Compare records in and out

nrow(Lomer.2022)
nrow(Lomer.2022.records) # 1 record discarded, Euphorbia characias, considered domesticated and thus not included in summary

# Add to record of unmatched names

ncol(unmatched.vascular.plant.records)
ncol(Lomer.2022.names.unmatched.unmatched)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Lomer.2022.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Frank Lomer 2022 Records
# Note: specimens will all be deposited at UBC and should be checked for duplicates against UBC records in the future 

Lomer.2022.Gossip.Is <- read.csv("../../records/digitized/DarwinCore/Lomer_Gossip_Island_plant_list_2022-06-13.csv")

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Lomer.2022.Gossip.Is)))
names(data.frame) <- DwCFields

data.frame[names(Lomer.2022.Gossip.Is)] <- Lomer.2022.Gossip.Is

Lomer.2022.Gossip.Is <- select(data.frame, c(1:length(DwCFields)))

# Create unique identifiers for observations

unique.prefix <- "LOMER2022:" 
unique.suffix <- 1:nrow(Lomer.2022.Gossip.Is)

# Add metadata

Lomer.2022.Gossip.Is$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Lomer.2022.Gossip.Is$stateProvince <- "British Columbia"
Lomer.2022.Gossip.Is$country <- "Canada"
Lomer.2022.Gossip.Is$countryCode <- "CA"
Lomer.2022.Gossip.Is$georeferenceProtocol <- "Coordinates generalized based on locality information"
Lomer.2022.Gossip.Is$georeferencedBy <- "Andrew Simon"
Lomer.2022.Gossip.Is$georeferenceVerificationStatus <- "verified by contributor"
Lomer.2022.Gossip.Is$georeferenceRemarks <- "Plants reported for Gossip Island; georeferencing imprecise"
Lomer.2022.Gossip.Is$basisOfRecord <- "HumanObservation"

# Merge with summary to standardize names and taxon metadata

Lomer.2022.Gossip.Is$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$taxonID <- summary$ID[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$kingdom <- summary$Kingdom[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$phylum <- summary$Phylum[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$class <- summary$Class[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$order <- summary$Order[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$suborder <- summary$Suborder[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$superfamily <- summary$Superfamily[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$family <- summary$Family[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$genus <- summary$Genus[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$specificEpithet <- summary$Species[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$hybrid <- summary$Hybrid[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$subspecies <- summary$Subspecies[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$variety <- summary$Variety[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$establishmentMeans <- summary$Origin[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$provincialStatus <- summary$Provincial.Status[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]
Lomer.2022.Gossip.Is$nationalStatus <- summary$National.Status[match(unlist(Lomer.2022.Gossip.Is$scientificName), summary$Taxon)]

# Unmatched records

Lomer.2022.Gossip.Is.names.unmatched <- Lomer.2022.Gossip.Is[is.na(Lomer.2022.Gossip.Is$taxonID),]

# Matched records

Lomer.2022.Gossip.Is.names.matched <- anti_join(Lomer.2022.Gossip.Is,Lomer.2022.Gossip.Is.names.unmatched)

# Confirm all records are represented 

nrow(Lomer.2022.Gossip.Is)
nrow(Lomer.2022.Gossip.Is.names.matched)
nrow(Lomer.2022.Gossip.Is.names.unmatched)
nrow(Lomer.2022.Gossip.Is.names.matched)+nrow(Lomer.2022.Gossip.Is.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Lomer.2022.Gossip.Is.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Lomer.2022.Gossip.Is.names.unmatched.matched <- Lomer.2022.Gossip.Is.names.unmatched

Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp <- Lomer.2022.Gossip.Is.key$Matched.Taxon[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificName), Lomer.2022.Gossip.Is.key$Taxon)]

# Add values based on newly matched name

Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$class <- summary$Class[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$order <- summary$Order[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$family <- summary$Family[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$genus <- summary$Genus[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$variety <- summary$Variety[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Lomer.2022.Gossip.Is.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Lomer.2022.Gossip.Is.names.unmatched.unmatched <- Lomer.2022.Gossip.Is.names.unmatched.matched[is.na(Lomer.2022.Gossip.Is.names.unmatched.matched$taxonID),]

Lomer.2022.Gossip.Is.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Lomer.2022.Gossip.Is.names.unmatched.matched$scientificName <- Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp

Lomer.2022.Gossip.Is.names.unmatched.matched$scientificNameTemp <- NULL

Lomer.2022.Gossip.Is.names.unmatched.matched <- Lomer.2022.Gossip.Is.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Lomer.2022.Gossip.Is)
nrow(Lomer.2022.Gossip.Is.names.matched)
nrow(Lomer.2022.Gossip.Is.names.unmatched)
nrow(Lomer.2022.Gossip.Is.names.unmatched.matched)
nrow(Lomer.2022.Gossip.Is.names.unmatched.unmatched)
nrow(Lomer.2022.Gossip.Is.names.matched)+nrow(Lomer.2022.Gossip.Is.names.unmatched.matched)+nrow(Lomer.2022.Gossip.Is.names.unmatched.unmatched)

# Bind records

Lomer.2022.Gossip.Is.records <- rbind(Lomer.2022.Gossip.Is.names.matched,Lomer.2022.Gossip.Is.names.unmatched.matched)

# Set date formatting consistent with other data frames

Lomer.2022.Gossip.Is.records$eventDate <- as.Date(Lomer.2022.Gossip.Is.records$eventDate)

# Compare records in and out

nrow(Lomer.2022.Gossip.Is)
nrow(Lomer.2022.Gossip.Is.records)

# Add to record of unmatched names

ncol(unmatched.vascular.plant.records)
ncol(Lomer.2022.Gossip.Is.names.unmatched.unmatched)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Lomer.2022.Gossip.Is.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read RBCM Records 
# Note: see email from Bill Crins re: locality information

RBCM.georeferencing.corrected <- read.csv("../../records/digitized/DarwinCore/RBCM_vascular_plant_records_georeferencing_corrected_2021-12-05_DwC.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Generate Taxon field to facilitate join

RBCM.georeferencing.corrected$scientificName <- paste(RBCM.georeferencing.corrected$genus,RBCM.georeferencing.corrected$speciesEpithet)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(RBCM.georeferencing.corrected)))
names(data.frame) <- DwCFields

data.frame[names(RBCM.georeferencing.corrected)] <- RBCM.georeferencing.corrected

RBCM.georeferencing.corrected <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

RBCM.georeferencing.corrected$stateProvince <- "British Columbia"
RBCM.georeferencing.corrected$island <- "Galiano Island"
RBCM.georeferencing.corrected$country <- "Canada"
RBCM.georeferencing.corrected$countryCode <- "CA"
RBCM.georeferencing.corrected$institutionCode <- "V"
RBCM.georeferencing.corrected$coordinateUncertaintyInMeters <- 50
RBCM.georeferencing.corrected$georeferenceProtocol <- "Coordinates generalized based on mapped locality information"
RBCM.georeferencing.corrected$georeferenceVerificationStatus <- "verified by data custodian"
RBCM.georeferencing.corrected$basisOfRecord <- "PreservedSpecimen"

# Merge with summary to standardize names and taxon metadata

RBCM.georeferencing.corrected$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$taxonID <- summary$ID[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$kingdom <- summary$Kingdom[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$phylum <- summary$Phylum[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$class <- summary$Class[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$order <- summary$Order[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$suborder <- summary$Suborder[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$superfamily <- summary$Superfamily[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$family <- summary$Family[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$genus <- summary$Genus[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$specificEpithet <- summary$Species[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$hybrid <- summary$Hybrid[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$subspecies <- summary$Subspecies[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$variety <- summary$Variety[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$establishmentMeans <- summary$Origin[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$provincialStatus <- summary$Provincial.Status[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]
RBCM.georeferencing.corrected$nationalStatus <- summary$National.Status[match(unlist(RBCM.georeferencing.corrected$scientificName), summary$Taxon)]

# Unmatched records

RBCM.georeferencing.corrected.names.unmatched <- RBCM.georeferencing.corrected[is.na(RBCM.georeferencing.corrected$taxonID),]

# Matched records

RBCM.georeferencing.corrected.names.matched <- anti_join(RBCM.georeferencing.corrected,RBCM.georeferencing.corrected.names.unmatched)

# Confirm all records are represented 

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.georeferencing.corrected.names.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched)
nrow(RBCM.georeferencing.corrected.names.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

RBCM.georeferencing.corrected.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

RBCM.georeferencing.corrected.names.unmatched.matched <- RBCM.georeferencing.corrected.names.unmatched

RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp <- RBCM.georeferencing.corrected.key$Matched.Taxon[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificName), RBCM.georeferencing.corrected.key$Taxon)]

# Add values based on newly matched name

RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$taxonID <- summary$ID[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$class <- summary$Class[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$order <- summary$Order[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$family <- summary$Family[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$genus <- summary$Genus[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$variety <- summary$Variety[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
RBCM.georeferencing.corrected.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

RBCM.georeferencing.corrected.names.unmatched.unmatched <- RBCM.georeferencing.corrected.names.unmatched.matched[is.na(RBCM.georeferencing.corrected.names.unmatched.matched$taxonID),]

RBCM.georeferencing.corrected.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

RBCM.georeferencing.corrected.names.unmatched.matched$scientificName <- RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp

RBCM.georeferencing.corrected.names.unmatched.matched$scientificNameTemp <- NULL

RBCM.georeferencing.corrected.names.unmatched.matched <- RBCM.georeferencing.corrected.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.georeferencing.corrected.names.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched)
nrow(RBCM.georeferencing.corrected.names.unmatched.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched.unmatched)
nrow(RBCM.georeferencing.corrected.names.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched.unmatched)

# Bind records

RBCM.records <- rbind(RBCM.georeferencing.corrected.names.matched,RBCM.georeferencing.corrected.names.unmatched.matched)

# Set date formatting consistent with other data frames

RBCM.records$eventDate <- as.Date(RBCM.records$eventDate)

# Compare records in and out

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.records)# 5 records discarded

# discarded records include those that cannot be reconciled with summary due to lack of infrataxonic specificity
# Note: it should be possible to match Phragmites australis with a refresh of RBCM data

# Add to record of unmatched names

ncol(unmatched.vascular.plant.records)
ncol(RBCM.georeferencing.corrected.names.unmatched.unmatched)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,RBCM.georeferencing.corrected.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Hans Roemer - 2004 - Mt. Sutil Records

Roemer.2004.Mt.Sutil.plot.metadata <- read.csv("../../records/digitized/DarwinCore/Roemer_2004_Mt_Sutil_plot_metadata_DwC.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

Roemer.2004.Mt.Sutil.vegetation <- read.csv("../../records/digitized/DarwinCore/Roemer_2004_Mt_Sutil_vegetation_DwC.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Remove non-vasculars from vegetation dataframe

Roemer.2004.Mt.Sutil.vegetation <- subset(Roemer.2004.Mt.Sutil.vegetation, layer != "moss/lichen")

# Compile records with plot metadata

Roemer.2004 <- left_join(Roemer.2004.Mt.Sutil.vegetation, Roemer.2004.Mt.Sutil.plot.metadata)

# Create unique identifiers for observations

unique.prefix <- "ROEMER2004:"
unique.suffix <- 1:nrow(Roemer.2004)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Roemer.2004)))
names(data.frame) <- DwCFields

data.frame[names(Roemer.2004)] <- Roemer.2004

Roemer.2004 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Roemer.2004$associatedReferences <- "Roemer H (2004) Perennial Grass Communities on Mount Sutil. Unpublished species list."
Roemer.2004$bibliographicCitation <- "Roemer H (2004) Perennial Grass Communities on Mount Sutil. Unpublished species list."
Roemer.2004$datasetName <- "Perennial Grass Communities on Mount Sutil"
Roemer.2004$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Roemer.2004$recordedBy <- "Hans Roemer"
Roemer.2004$eventDate <- '2004-07-01'
Roemer.2004$coordinateUncertaintyInMeters <- 3 # Need to ask Hans how good his GPS was...
Roemer.2004$georeferenceProtocol <- "Coordinates mapped by GPS"
Roemer.2004$georeferenceVerificationStatus <- "verified by data custodian"
Roemer.2004$stateProvince <- "British Columbia"
Roemer.2004$locality <- "Mount Sutil"
Roemer.2004$island <- "Galiano Island"
Roemer.2004$country <- "Canada"
Roemer.2004$countryCode <- "CA"
Roemer.2004$basisOfRecord <- "HumanObservation"

# Concatenate 'fieldNumber' field from metadata

Roemer.2004$fieldNumber <- paste('Plot:', data.frame$plot, sep = " ")

# Concatenate 'occurrenceRemarks' field from metadata

Roemer.2004$occurrenceRemarks <- paste('Aspect (degrees):', data.frame$aspect.degrees, 'slope (percent):', data.frame$slope.percent,
                                       'esimated (percent) coarse fragment:', data.frame$est.percent.coarse.fragment,
                                       'estimated (percent) open soil / rock:', data.frame$percent.open.soil.or.rock, 
                                       'percent litter and dead wood:', data.frame$percent.litter.and.dead.wood, sep = " ")

# Drop / correct spurious reports

Roemer.2004 <- Roemer.2004 %>% filter(!str_detect(scientificName, 'Carex rostrata')) # Out of range on Galiano Island; misreported

Roemer.2004$scientificName[Roemer.2004$scientificName == 'Typha latifola'] <- 'Typha angustifolia' # Bullrush at Laughlin Lake has since been confirmed as Typha angustifolium

Roemer.2004$scientificName[Roemer.2004$scientificName == 'Epilobium watsonii'] <- 'Typha angustifolia' # Epilobium watsonii some recognize as infrascientificName of Epilobium ciliatum; collapsed with Epilobium ciliatum (report noted in curated summary)

# Merge with summary to standardize names and taxon metadata

Roemer.2004$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$taxonID <- summary$ID[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$kingdom <- summary$Kingdom[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$phylum <- summary$Phylum[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$class <- summary$Class[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$order <- summary$Order[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$suborder <- summary$Suborder[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$superfamily <- summary$Superfamily[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$family <- summary$Family[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$genus <- summary$Genus[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$specificEpithet <- summary$Species[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$hybrid <- summary$Hybrid[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$subspecies <- summary$Subspecies[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$variety <- summary$Variety[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$establishmentMeans <- summary$Origin[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$provincialStatus <- summary$Provincial.Status[match(unlist(Roemer.2004$scientificName), summary$Taxon)]
Roemer.2004$nationalStatus <- summary$National.Status[match(unlist(Roemer.2004$scientificName), summary$Taxon)]

# Unmatched records

Roemer.2004.names.unmatched <- Roemer.2004[is.na(Roemer.2004$taxonID),]

# Matched records

Roemer.2004.names.matched <- anti_join(Roemer.2004,Roemer.2004.names.unmatched)

# Confirm all records are represented 

nrow(Roemer.2004)
nrow(Roemer.2004.names.matched)
nrow(Roemer.2004.names.unmatched)
nrow(Roemer.2004.names.matched)+nrow(Roemer.2004.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Roemer.2004.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched

Roemer.2004.names.unmatched.matched$scientificNameTemp <- Roemer.2004.key$Matched.Taxon[match(unlist(Roemer.2004.names.unmatched.matched$scientificName), Roemer.2004.key$Taxon)]

# Add values based on newly matched name

Roemer.2004.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$class <- summary$Class[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$order <- summary$Order[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$family <- summary$Family[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$genus <- summary$Genus[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$variety <- summary$Variety[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Roemer.2004.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Roemer.2004.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Roemer.2004.names.unmatched.unmatched <- Roemer.2004.names.unmatched.matched[is.na(Roemer.2004.names.unmatched.matched$taxonID),]

Roemer.2004.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Roemer.2004.names.unmatched.matched$scientificName <- Roemer.2004.names.unmatched.matched$scientificNameTemp

Roemer.2004.names.unmatched.matched$scientificNameTemp <- NULL

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Roemer.2004)
nrow(Roemer.2004.names.matched)
nrow(Roemer.2004.names.unmatched)
nrow(Roemer.2004.names.unmatched.matched)
nrow(Roemer.2004.names.unmatched.unmatched)
nrow(Roemer.2004.names.matched)+nrow(Roemer.2004.names.unmatched.matched)+nrow(Roemer.2004.names.unmatched.unmatched)

# Bind records

Roemer.2004.records <- rbind(Roemer.2004.names.matched,Roemer.2004.names.unmatched.matched)

# Set date formatting consistent with other data frames

Roemer.2004.records$eventDate <- as.Date(Roemer.2004.records$eventDate)

# Compare records in and out

nrow(Roemer.2004)
nrow(Roemer.2004.records)  # 0 records discarded

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Roemer.2004.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read Simon 2018 data

Simon.2018.transects <- read.csv("../../records/digitized/DarwinCore/Simon_2018_transect_coordinates_DwC.csv")

Simon.2018.veg.data <- read.csv("../../records/digitized/DarwinCore/Simon_2018_vegetation_data_DwC.csv")

# Add common name for transect ID to facilitate join

names(Simon.2018.transects) <- c('TID','Latitude','Longitude','Note')

# Merge transect coordinates with veg data

Simon.2018 <- inner_join(Simon.2018.veg.data, Simon.2018.transects, by = 'TID')

# Add 'fieldNumber' column

Simon.2018$fieldNumber <- Simon.2018$SID

# Remove rows with NAs

Simon.2018 <- Simon.2018 %>% drop_na(scientificName)

# Remove '_'s from Taxon field

Simon.2018$scientificName <-  str_replace(Simon.2018$scientificName, "_", " ")

# Correct misreported taxa 

Simon.2018$scientificName[Simon.2018$scientificName == 'Torilis japonica'] <- 'Torilis arvensis' # Most of the Galiano Island material that I have examined more closely is T. arvensis, though T. japonica is also present

# Create unique identifiers for observations

unique.prefix <- "SIMON2021:"
unique.suffix <- 1:nrow(Simon.2018)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(Simon.2018)))
names(data.frame) <- DwCFields

data.frame[names(Simon.2018)] <- Simon.2018

Simon.2018 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

Simon.2018$associatedReferences <- "Simon ADF, Marx HE, Starzomski BM (2021) Phylogenetic restriction of plant invasion in drought-stressed environments: Implications for insect-pollinated plant communities in water-limited ecosystems. Ecology and evolution 11: 10042-10053. https://doi.org/10.1002/ece3.7776"
Simon.2018$bibliographicCitation <- "Simon ADF, Marx HE, Starzomski BM (2021) Phylogenetic restriction of plant invasion in drought-stressed environments: Implications for insect-pollinated plant communities in water-limited ecosystems. Ecology and evolution 11: 10042-10053.https://doi.org/10.1002/ece3.7776"
Simon.2018$datasetName <- "Phylogenetic restriction of plant invasion in drought-stressed environments: Implications for insect-pollinated plant communities in water-limited ecosystems"
Simon.2018$catalogNumber <- paste(unique.prefix,unique.suffix, sep = "")
Simon.2018$recordedBy <- "Andrew Simon"
Simon.2018$coordinateUncertaintyInMeters <- 3 
Simon.2018$georeferenceProtocol <- "Coordinates mapped by GPS"
Simon.2018$georeferenceVerificationStatus <- "verified by data contributor"
Simon.2018$stateProvince <- "British Columbia"
Simon.2018$island <- "Galiano Island"
Simon.2018$country <- "Canada"
Simon.2018$countryCode <- "CA"
Simon.2018$basisOfRecord <- "HumanObservation"
Simon.2018$samplingProtocol <- "Counts of flowering shoots as observed along 15 m belt transects (2 m wide), recorded at one meter intervals and summed by transect"
Simon.2018$occurrenceRemarks <- paste("Count:",data.frame$Count, sep = "")


# Merge with summary to standardize names and taxon metadata

Simon.2018$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$taxonID <- summary$ID[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$kingdom <- summary$Kingdom[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$phylum <- summary$Phylum[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$class <- summary$Class[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$order <- summary$Order[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$suborder <- summary$Suborder[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$superfamily <- summary$Superfamily[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$family <- summary$Family[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$genus <- summary$Genus[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$specificEpithet <- summary$Species[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$hybrid <- summary$Hybrid[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$subspecies <- summary$Subspecies[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$variety <- summary$Variety[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$establishmentMeans <- summary$Origin[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$provincialStatus <- summary$Provincial.Status[match(unlist(Simon.2018$scientificName), summary$Taxon)]
Simon.2018$nationalStatus <- summary$National.Status[match(unlist(Simon.2018$scientificName), summary$Taxon)]

# Unmatched records

Simon.2018.names.unmatched <- Simon.2018[is.na(Simon.2018$taxonID),]

# Matched records

Simon.2018.names.matched <- anti_join(Simon.2018,Simon.2018.names.unmatched)

# Confirm all records are represented 

nrow(Simon.2018)
nrow(Simon.2018.names.matched)
nrow(Simon.2018.names.unmatched)
nrow(Simon.2018.names.matched)+nrow(Simon.2018.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

Simon.2018.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

Simon.2018.names.unmatched.matched <- Simon.2018.names.unmatched

Simon.2018.names.unmatched.matched$scientificNameTemp <- Simon.2018.key$Matched.Taxon[match(unlist(Simon.2018.names.unmatched.matched$scientificName), Simon.2018.key$Taxon)]

# Add values based on newly matched name

Simon.2018.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$taxonID <- summary$ID[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$class <- summary$Class[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$order <- summary$Order[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$family <- summary$Family[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$genus <- summary$Genus[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$variety <- summary$Variety[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
Simon.2018.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(Simon.2018.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

Simon.2018.names.unmatched.unmatched <- Simon.2018.names.unmatched.matched[is.na(Simon.2018.names.unmatched.matched$taxonID),]

Simon.2018.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

Simon.2018.names.unmatched.matched$scientificName <- Simon.2018.names.unmatched.matched$scientificNameTemp

Simon.2018.names.unmatched.matched$scientificNameTemp <- NULL

Simon.2018.names.unmatched.matched <- Simon.2018.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(Simon.2018)
nrow(Simon.2018.names.matched)
nrow(Simon.2018.names.unmatched)
nrow(Simon.2018.names.unmatched.matched)
nrow(Simon.2018.names.unmatched.unmatched)
nrow(Simon.2018.names.matched)+nrow(Simon.2018.names.unmatched.matched)+nrow(Simon.2018.names.unmatched.unmatched)

# Bind records

Simon.2018.records <- rbind(Simon.2018.names.matched,Simon.2018.names.unmatched.matched)

# Set date formatting consistent with other data frames

Simon.2018.records$eventDate <- as.Date(Simon.2018.records$eventDate)

# Compare records in and out

nrow(Simon.2018)
nrow(Simon.2018.records) # 92 records discarded: all domesticated / cultivated plants

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Simon.2018.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Read UBC specimen records (UBC 2022)

UBC.2022 <- read.csv("../../records/digitized/DarwinCore/UBC_Galiano_Island_vascular_plants_2022-10-31_DwC.csv")

# Change date format to POSIX

UBC.2022$eventDate <- strptime(UBC.2022$eventDate, "%Y %b %d")
UBC.2022$eventDate <-  as.Date(UBC.2022$eventDate)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = length(DwCFields), nrow = nrow(UBC.2022)))
names(data.frame) <- DwCFields

data.frame[names(UBC.2022)] <- UBC.2022

UBC.2022 <- select(data.frame, c(1:length(DwCFields)))

# Add metadata

UBC.2022$stateProvince <- "British Columbia"
UBC.2022$island <- "Galiano Island"
UBC.2022$countryCode <- "CA"
UBC.2022$basisOfRecord <- "PreservedSpecimen"
UBC.2022$institutionCode <- "UBC"

# Merge with summary to standardize names and taxon metadata

UBC.2022$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$taxonID <- summary$ID[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$kingdom <- summary$Kingdom[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$phylum <- summary$Phylum[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$class <- summary$Class[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$order <- summary$Order[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$suborder <- summary$Suborder[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$superfamily <- summary$Superfamily[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$family <- summary$Family[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$genus <- summary$Genus[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$specificEpithet <- summary$Species[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$hybrid <- summary$Hybrid[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$subspecies <- summary$Subspecies[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$variety <- summary$Variety[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$establishmentMeans <- summary$Origin[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$provincialStatus <- summary$Provincial.Status[match(unlist(UBC.2022$scientificName), summary$Taxon)]
UBC.2022$nationalStatus <- summary$National.Status[match(unlist(UBC.2022$scientificName), summary$Taxon)]

# Unmatched records

UBC.2022.names.unmatched <- UBC.2022[is.na(UBC.2022$taxonID),]

# Matched records

UBC.2022.names.matched <- anti_join(UBC.2022,UBC.2022.names.unmatched)

# Confirm all records are represented 

nrow(UBC.2022)
nrow(UBC.2022.names.matched)
nrow(UBC.2022.names.unmatched)
nrow(UBC.2022.names.matched)+nrow(UBC.2022.names.unmatched)

# Read key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary

UBC.2022.key <- read.csv("keys/vascular_plant_taxon_key_2024.csv") 

# Swap unmatched names using key

UBC.2022.names.unmatched.matched <- UBC.2022.names.unmatched

UBC.2022.names.unmatched.matched$scientificNameTemp <- UBC.2022.key$Matched.Taxon[match(unlist(UBC.2022.names.unmatched.matched$scientificName), UBC.2022.key$Taxon)]

# Add values based on newly matched name

UBC.2022.names.unmatched.matched$scientificNameAuthorship <- summary$Taxon.Author[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$taxonID <- summary$ID[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$kingdom <- summary$Kingdom[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$phylum <- summary$Phylum[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$class <- summary$Class[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$order <- summary$Order[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$suborder <- summary$Suborder[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$superfamily <- summary$Superfamily[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$family <- summary$Family[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$genus <- summary$Genus[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$specificEpithet <- summary$Species[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$hybrid <- summary$Hybrid[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$variety <- summary$Variety[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$provincialStatus <- summary$Provincial.Status[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]
UBC.2022.names.unmatched.matched$nationalStatus <- summary$National.Status[match(unlist(UBC.2022.names.unmatched.matched$scientificNameTemp), summary$Taxon)]

# Filter taxa unrecognized in summary 

UBC.2022.names.unmatched.unmatched <- UBC.2022.names.unmatched.matched[is.na(UBC.2022.names.unmatched.matched$taxonID),]

UBC.2022.names.unmatched.unmatched$scientificNameTemp <- NULL

# Filter taxa recognized in summary

UBC.2022.names.unmatched.matched$scientificName <- UBC.2022.names.unmatched.matched$scientificNameTemp

UBC.2022.names.unmatched.matched$scientificNameTemp <- NULL

UBC.2022.names.unmatched.matched <- UBC.2022.names.unmatched.matched %>% drop_na(taxonID)

# Confirm all records are represented 

nrow(UBC.2022)
nrow(UBC.2022.names.matched)
nrow(UBC.2022.names.unmatched)
nrow(UBC.2022.names.unmatched.matched)
nrow(UBC.2022.names.unmatched.unmatched)
nrow(UBC.2022.names.matched)+nrow(UBC.2022.names.unmatched.matched)+nrow(UBC.2022.names.unmatched.unmatched)

# Bind records

UBC.2022.records <- rbind(UBC.2022.names.matched,UBC.2022.names.unmatched.matched)

# Set date formatting consistent with other data frames

UBC.2022.records$eventDate <- as.Date(UBC.2022.records$eventDate)

# Compare records in and out

nrow(UBC.2022)
nrow(UBC.2022.records) # All accounted for

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,UBC.2022.names.unmatched.unmatched)

unmatched.vascular.plant.records



# Combine all source occurrence records


Vascular.plant.records <- rbind(BC.CDC.2019.records,DL63.records,Ecological.Reserve.128.records,Hunterston.2010.records,iNaturalist.records,
                                Janszen.2003.records,Laughlin.2002.records,Roemer.2004.records,RBCM.records,Simon.2018.records,Lomer.2022.records,
                                Lomer.2022.Gossip.Is.records,UBC.2022.records)



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

write.csv(Vascular.plant.records,"synthesized/Galiano_Island_vascular_plant_records_consolidated.csv", row.names = FALSE)
