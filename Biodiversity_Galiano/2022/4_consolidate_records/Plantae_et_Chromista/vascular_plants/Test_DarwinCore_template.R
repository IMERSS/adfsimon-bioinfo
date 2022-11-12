# Script to consolidate records of Galiano Island's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../2_review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_review_summary_reviewed_2022-11-09.csv")

# Consolidate records

# Sources:

# BC Conservation Data Centre - added
# ! Brothers 2020 records - not yet added!
# ! Consortium of PNW Herbaria: BABY, RBCM, UBC, WS, WTU - some RBCM records already added; UBC records also added; otherwise incomplete
# DL63 Veg List 2001-2002 - added - ! georeferencing might be improved!
# Ecological Reserve 128 records - added
# ! GCA DL63 VEGETATION INVENTORY - not yet added!
# Hunterston 2010 - added
# iNaturalist 2016-2022 - added
# ! Janszen 2003 - not yet added!
# Laughlin Lake 2002 - added - ! georeferencing might be improved!
# Lomer 2022 - added
# ! Matt Fairbarns Mt Sutil records - ! Need to get a digital copy of this... !
# ! RBCM - added, but incomplete; ! some records with geo-referencing (partly) corrected!
# Roemer 2004 - added
# Simon 2018 - added
# ! Terry Taylor Galiano Island list 2012 - not yet added!
# UBC records - added ! note there is an issue with the date, though!!



# Read BC Conservation Data Centre SAR records (BC CDC 2019)
# Note: request fresh data from BC CDC and ask them to include the EO ID for use as unique ID

BC.CDC.2019 <- read.csv("digitized/DarwinCore/BC_Conservation_Data_Centre_Galiano_Island_SAR_2019-10-24_DC.csv")

# Filter CDC obs from collections at other institutions

BC.CDC.2019 <- BC.CDC.2019 %>% filter(institutionCode == 'CDC')

# Create unique identifiers for observations
# Note: this unique ID is not going to work in the long term as these data can be refreshed by the BC CDC at any point
# Could ask the BC CDC for EO IDs or SF IDs?

unique.prefix <- "BCCDC2019:" 
unique.suffix <- 1:nrow(BC.CDC.2019)

# Create DarwinCore dataframe template 

data.frame <- as.data.frame(matrix(ncol = 61, nrow = nrow(BC.CDC.2019)))
names(data.frame) <- c('scientificName','scientificNameAuthorship','taxonID','kingdom','phylum','class',
        'order','suborder','infraorder','superfamily','family','genus','subgenus','specificEpithet',
        'hybrid','subspecies','variety','form','infraspecificEpithet','taxonRank','institutionCode',
        'collectionCode','catalogNumber','datasetName','occurrenceID','recordedBy','recordNumber',
        'fieldNumber','eventDate','year','month','day','basisOfRecord','locality','island','stateProvince',
        'country','countryCode','decimalLatitude','decimalLongitude','coordinateUncertaintyInMeters',
        'georeferencedBy','georeferenceVerificationStatus','georeferenceProtocol','georeferenceRemarks',
        'verbatimDepth','verbatimElevation','occurrenceStatus','samplingProtocol','occurrenceRemarks',
        'individualCount','sex','establishmentMeans','provincialStatus','nationalStatus','identifiedBy',
        'identificationQualifier','identificationRemarks','previousIdentifications','bibliographicCitation','associatedReferences')

data.frame[names(BC.CDC.2019)] <- BC.CDC.2019

BC.CDC.2019 <- data.frame

# Standardize columns

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
BC.CDC.2019$hybrid <- summary$hybrid[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$subspecies <- summary$Subspecies[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$variety <- summary$Variety[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019$establishmentMeans <- summary$Origin[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019provincialStatus <- summary$Provincial.Status[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]
BC.CDC.2019nationalStatus <- summary$National.Status[match(unlist(BC.CDC.2019$scientificName), summary$Taxon)]

# Unmatched records

BC.CDC.2019.names.unmatched <- BC.CDC.2019[is.na(BC.CDC.2019$taxonID),]

# Matched records

BC.CDC.2019.names.matched <- anti_join(BC.CDC.2019,BC.CDC.2019.names.unmatched)

# Confirm all records are represented 

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.names.matched)
nrow(BC.CDC.2019.names.unmatched)
nrow(BC.CDC.2019.names.matched)+nrow(BC.CDC.2019.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

BC.CDC.2019.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(BC.CDC.2019.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- BC.CDC.2019.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(BC.CDC.2019.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(BC.CDC.2019.names.unmatched.unmatched$Taxon, 2)

# review.key <- rbind(BC.CDC.2019.key,unmatched.taxa)

# write.csv(review.key,"review.key.csv")

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
BC.CDC.2019.names.unmatched.matched$hybrid <- summary$hybrid[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$subspecies <- summary$Subspecies[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$variety <- summary$Variety[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matched$establishmentMeans <- summary$Origin[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matchedprovincialStatus <- summary$Provincial.Status[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]
BC.CDC.2019.names.unmatched.matchednationalStatus <- summary$National.Status[match(unlist(BC.CDC.2019.names.unmatched.matched$scientificName), summary$Taxon)]

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
nrow(BC.CDC.2019.records) # No records omited

# Start record of unmatched names

unmatched.vascular.plant.records <- BC.CDC.2019.names.unmatched.unmatched

unmatched.vascular.plant.records

