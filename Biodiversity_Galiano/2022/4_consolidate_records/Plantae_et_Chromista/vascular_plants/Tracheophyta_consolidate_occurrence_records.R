# Script to consolidate records of Galiano Island's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../2_review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_review_summary_reviewed_2022-10-31.csv")

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
# ! Janszen 2001 - not yet added!
# Laughlin Lake 2002 - added - ! georeferencing might be improved!
# Lomer 2022 - added
# ! Matt Fairbarns - ! Need to get a digital copy of this... !
# ! RBCM - added, but incomplete; ! some records with geo-referencing (partly) corrected!
# Roemer 2004 - added
# Simon 2018 - added
# ! Terry Taylor Galiano Island list 2012 - not yet added!
# UBC records - added ! note there is an issue with the date, though!!



# Read BC Conservation Data Centre SAR records (BC CDC 2019)
# Note: request fresh data from BC CDC and ask them to include the EO ID for use as unique ID

BC.CDC.2019 <- read.csv("digitized/BC_Conservation_Data_Centre_Galiano_Island_SAR_2019-10-24.csv")

# Filter CDC obs from collections at other institutions

BC.CDC.2019 <- BC.CDC.2019 %>% filter(InstitutionCode == 'CDC')

# Create unique identifiers for observations
# Note: this unique ID is not going to work in the long term as these data can be refreshed by the BC CDC at any point
# Could ask the BC CDC for EO IDs or SF IDs?

unique.prefix <- "BCCDC2019:" 
unique.suffix <- 1:nrow(BC.CDC.2019)

# Standardize columns

BC.CDC.2019$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
BC.CDC.2019$Geo_Ref <- NA
BC.CDC.2019$HabitatRemarks <- NA
BC.CDC.2019$PositionalAccuracy <- 80
BC.CDC.2019$GeoPrivacy <- NA
BC.CDC.2019$PrivateLatitude <- NA
BC.CDC.2019$PrivateLongitude <- NA
BC.CDC.2019$PrivateLatitude <- NA
BC.CDC.2019$PrivateLongitude <- NA
BC.CDC.2019$Prov_State <- "British Columbia"
BC.CDC.2019$Region <- "Gulf Islands"
BC.CDC.2019$Location <- "Galiano Island"

# Select key columns

BC.CDC.2019 <- BC.CDC.2019 %>% select(ScientificName,InstitutionCode,CatalogueN,Collector,EventDate,Latitude,Longitude,Geo_Ref,
                                      PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
                                      Locality,OccurrenceRemarks)

# Standardize column names to facilitate join

names(BC.CDC.2019) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
                        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe',
                        'HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

BC.CDC.2019.names.matched <- left_join(BC.CDC.2019,summary, by = c('Taxon'))

# Unmatched records

BC.CDC.2019.names.unmatched <- BC.CDC.2019.names.matched[is.na(BC.CDC.2019.names.matched$Taxon.Author),]

# Matched records

BC.CDC.2019.names.matched <- anti_join(BC.CDC.2019.names.matched,BC.CDC.2019.names.unmatched)

# Standardize matched occcurrence records

BC.CDC.2019.names.matched <- BC.CDC.2019.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,
        Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(BC.CDC.2019.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
        'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks',
        'Origin','Provincial.Status','National.Status')

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

BC.CDC.2019.names.unmatched.matched$Taxon <- BC.CDC.2019.key$Matched.Taxon[match(unlist(BC.CDC.2019.names.unmatched.matched$Taxon), BC.CDC.2019.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

BC.CDC.2019.names.unmatched.matched <- select(BC.CDC.2019.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

BC.CDC.2019.names.unmatched.matched <- left_join(BC.CDC.2019.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

BC.CDC.2019.names.unmatched.matched <- BC.CDC.2019.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

BC.CDC.2019.names.unmatched.matched <- BC.CDC.2019.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,
        Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(BC.CDC.2019.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
        'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

BC.CDC.2019.names.unmatched.unmatched <- anti_join(BC.CDC.2019.names.unmatched,BC.CDC.2019.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.names.matched)
nrow(BC.CDC.2019.names.unmatched)
nrow(BC.CDC.2019.names.unmatched.matched)
nrow(BC.CDC.2019.names.unmatched.unmatched)
nrow(BC.CDC.2019.names.matched)+nrow(BC.CDC.2019.names.unmatched.matched)+nrow(BC.CDC.2019.names.unmatched.unmatched)

# Bind records

BC.CDC.2019.records <- rbind(BC.CDC.2019.names.matched,BC.CDC.2019.names.unmatched.matched)

# Compare records in and out

nrow(BC.CDC.2019)
nrow(BC.CDC.2019.records) # Good: only five records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog:
# Juncus effusus - infrataxonomic resolution required to meaningfully discriminate this taxon
# Epilobium cf ciliatum - could be E. ciliatum; could also be Epilobium leptophyllum, which has since documented at the bog
# Glyceria sp.

# Start record of unmatched names

unmatched.vascular.plant.records <- BC.CDC.2019.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,
                                                                                     Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
                                                                                     Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records



# Read Ecological Reserve 128 records (Roemer & Janszen 1980, Roemer 2000)

Ecological.Reserve.128 <- read.csv("digitized/Galiano_Bog_Plant_List_Roemer_2000.csv")

# Filter plants

Ecological.Reserve.128 <- Ecological.Reserve.128 %>% filter(Group == 'vascular plants')

# Create unique identifiers for observations

unique.prefix <- "ROEMER2000:"
unique.suffix <- 1:nrow(Ecological.Reserve.128)

# Standardize columns

Ecological.Reserve.128$Source <- "Galiano Bog Plant List (Roemer 2000)"
Ecological.Reserve.128$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Ecological.Reserve.128$Latitude <- 48.983312863031706
Ecological.Reserve.128$Longitude <- -123.55665029568577
Ecological.Reserve.128$Geo_Ref <- "Coordinates generalized based on locality information"
Ecological.Reserve.128$HabitatRemarks <- NA
Ecological.Reserve.128$PositionalAccuracy <- 80
Ecological.Reserve.128$GeoPrivacy <- NA
Ecological.Reserve.128$PrivateLatitude <- NA
Ecological.Reserve.128$PrivateLongitude <- NA
Ecological.Reserve.128$PrivateLatitude <- NA
Ecological.Reserve.128$PrivateLongitude <- NA
Ecological.Reserve.128$Prov_State <- "British Columbia"
Ecological.Reserve.128$Region <- "Gulf Islands"
Ecological.Reserve.128$Location <- "Galiano Island"
Ecological.Reserve.128$LocationDe <- "Ecological Reserve 128"

# Select key columns

Ecological.Reserve.128 <- Ecological.Reserve.128 %>% select(Species,Source,CatalogueN,Observer,Date,Latitude,Longitude,Geo_Ref,
        PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks)

# Standardize column names to facilitate join

names(Ecological.Reserve.128) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

Ecological.Reserve.128.names.matched <- left_join(Ecological.Reserve.128,summary, by = c('Taxon'))

# Unmatched records

Ecological.Reserve.128.names.unmatched <- Ecological.Reserve.128.names.matched[is.na(Ecological.Reserve.128.names.matched$Taxon.Author),]

# Matched records

Ecological.Reserve.128.names.matched <- anti_join(Ecological.Reserve.128.names.matched,Ecological.Reserve.128.names.unmatched)

# Standardize matched occcurence records

Ecological.Reserve.128.names.matched <- Ecological.Reserve.128.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,
        Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(Ecological.Reserve.128.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription'
        ,'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.names.matched)
nrow(Ecological.Reserve.128.names.unmatched)
nrow(Ecological.Reserve.128.names.matched)+nrow(Ecological.Reserve.128.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Ecological.Reserve.128.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# Generate key to manually match unmatched names in Roemer 2000
# Note: code no longer required once key was generated

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Ecological.Reserve.128.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- Ecological.Reserve.128.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(Ecological.Reserve.128.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(Ecological.Reserve.128.names.unmatched.unmatched$Taxon, 2)

# Janszen.2000.key <- rbind(Ecological.Reserve.128.key,unmatched.taxa)

# write.csv(Janszen.2000.key,"Janszen.2000.key.csv")

# Swap unmatched names using key

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched

Ecological.Reserve.128.names.unmatched.matched$Taxon <- Ecological.Reserve.128.key$Matched.Taxon[match(unlist(Ecological.Reserve.128.names.unmatched.matched$Taxon), Ecological.Reserve.128.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Ecological.Reserve.128.names.unmatched.matched <- select(Ecological.Reserve.128.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Ecological.Reserve.128.names.unmatched.matched <- left_join(Ecological.Reserve.128.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,
        Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,
        PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,
        Provincial.Status,National.Status)

names(Ecological.Reserve.128.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
        'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
        'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
        'LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

Ecological.Reserve.128.names.unmatched.unmatched <- anti_join(Ecological.Reserve.128.names.unmatched,Ecological.Reserve.128.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.names.matched)
nrow(Ecological.Reserve.128.names.unmatched)
nrow(Ecological.Reserve.128.names.unmatched.matched)
nrow(Ecological.Reserve.128.names.unmatched.unmatched)
nrow(Ecological.Reserve.128.names.matched)+nrow(Ecological.Reserve.128.names.unmatched.matched)+nrow(Ecological.Reserve.128.names.unmatched.unmatched)

# Bind records

Ecological.Reserve.128.records <- rbind(Ecological.Reserve.128.names.matched,Ecological.Reserve.128.names.unmatched.matched)

# Compare records in and out

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.records) # Good: only five records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog:
# Juncus effusus - infrataxonomic resolution required to meaningfully discriminate this taxon
# Epilobium cf ciliatum - could be E. ciliatum; could also be Epilobium leptophyllum, which has since documented at the bog
# Glyceria sp.

# Add to record of unmatched names

Ecological.Reserve.128.names.unmatched.unmatched <- Ecological.Reserve.128.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,
        Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
        Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Ecological.Reserve.128.names.unmatched.unmatched)



# Read DL63 records (GCA 2002)

DL63 <- read.csv("digitized/DL63_veg_list_2001-2002.csv")

# Filter plants

DL63 <- DL63 %>% filter(Group == 'vascular')

# Create unique identifiers for observations

unique.prefix <- "GCA2002:"
unique.suffix <- 1:nrow(DL63)

# Standardize columns

DL63$Source <- "DL63 Veg List (GCA 2002)"
DL63$Observer <- "Nathan Gaylor, Odin Scholz, & Keith Erickson"
DL63$Date <- '2002-03-01'
DL63$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
DL63$Latitude <- 48.946327
DL63$Longitude <- -123.493078
DL63$Geo_Ref <- "Coordinates generalized based on locality information: centroid of District Lot; positional accuracy defining a circle that encompasses the property"
DL63$HabitatRemarks <- NA
DL63$PositionalAccuracy <- 380
DL63$GeoPrivacy <- NA
DL63$PrivateLatitude <- NA
DL63$PrivateLongitude <- NA
DL63$PrivateLatitude <- NA
DL63$PrivateLongitude <- NA
DL63$Prov_State <- "British Columbia"
DL63$Region <- "Gulf Islands"
DL63$Location <- "Galiano Island"
DL63$LocationDe <- "District Lot 63, Pebble Beach Reserve"

# Drop / correct spurious reports

DL63 <- DL63 %>% filter(!str_detect(Scientific.Name, 'Cardamine angulata')) # This might be Cardamine nuttallii but cannot be certain

DL63 <- DL63 %>% filter(!str_detect(Scientific.Name, 'Fraxinus latifolia')) # Not known to southern Gulf Islands; likely misidentified

DL63$Scientific.Name[DL63$Scientific.Name == 'Fragaria virginiana'] <- 'Fragaria vesca' # Reports of F. virginiana invariably apply to F. vesca on Galiano

DL63$Scientific.Name[DL63$Scientific.Name == 'Sorbus sitchensis'] <- 'Sorbus aucuparia' # Sorbus sitchensis not known to occur on Galiano Island; S. aucuparia confirmed for Pebble Beach and commonly conflated

DL63$Scientific.Name[DL63$Scientific.Name == 'Trientalis borealis'] <- 'Lysimachia latifolia' # Trientalis borealis misapplied; Lysimachia latifolia recognized as present

DL63$Scientific.Name[DL63$Scientific.Name == 'Vaccinium ovalifolium'] <- 'Vaccinium ovatum' # Vaccinium ovalifolium likely misapplied, or typo (Keith Erickson, pers. comm. 2022); V. ovatum recognized as present
  
# Select key columns

DL63 <- DL63 %>% select(Scientific.Name,Source,CatalogueN,Observer,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,
        PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks)

# Standardize column names to facilitate join

names(DL63) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref',
                 'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
                 'LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

DL63.names.matched <- left_join(DL63,summary, by = c('Taxon'))

# Unmatched records

DL63.names.unmatched <- DL63.names.matched[is.na(DL63.names.matched$Taxon.Author),]

# Matched records

DL63.names.matched <- anti_join(DL63.names.matched,DL63.names.unmatched)

# Standardize matched occcurence records

DL63.names.matched <- DL63.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,
        Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,
        PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(DL63.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid','Subspecies',
        'Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy',
        'PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin',
        'Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(DL63)
nrow(DL63.names.matched)
nrow(DL63.names.unmatched)
nrow(DL63.names.matched)+nrow(DL63.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

DL63.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(DL63.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- DL63.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(DL63.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(DL63.names.unmatched.unmatched$Taxon, 2)

# review.key <- rbind(DL63.key,unmatched.taxa)

# write.csv(review.key,"review.key.csv")

# Swap unmatched names using key

DL63.names.unmatched.matched <- DL63.names.unmatched

DL63.names.unmatched.matched$Taxon <- DL63.key$Matched.Taxon[match(unlist(DL63.names.unmatched.matched$Taxon), DL63.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

DL63.names.unmatched.matched <- select(DL63.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

DL63.names.unmatched.matched <- left_join(DL63.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

DL63.names.unmatched.matched <- DL63.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

DL63.names.unmatched.matched <- DL63.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,
        Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(DL63.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
        'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
        'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

DL63.names.unmatched.unmatched <- anti_join(DL63.names.unmatched,DL63.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(DL63)
nrow(DL63.names.matched)
nrow(DL63.names.unmatched)
nrow(DL63.names.unmatched.matched)
nrow(DL63.names.unmatched.unmatched)
nrow(DL63.names.matched)+nrow(DL63.names.unmatched.matched)+nrow(DL63.names.unmatched.unmatched)

# Bind records

DL63.records <- rbind(DL63.names.matched,DL63.names.unmatched.matched)

# Compare records in and out

nrow(DL63)
nrow(DL63.records) # Good: only five records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog, are all species identified only to genus.
# See also misreported taxa manually excluded from data set above

# Add to record of unmatched names

DL63.names.unmatched.unmatched <- DL63.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,Latitude,
        Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
        HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,DL63.names.unmatched.unmatched)



# Read Hunterston Farm Bioblitz 2010 records

Hunterston.2010 <- read.csv("digitized/Hunterston_Farms_Bioblitz_2010_sorted_2022-10-16.csv")

# Filter plants

Hunterston.2010 <- Hunterston.2010 %>% filter(Group == 'Plants')

# Create unique identifiers for observations

unique.prefix <- "HUNTERSTON2010:"
unique.suffix <- 1:nrow(Hunterston.2010)

# Standardize columns

Hunterston.2010$Source <- "Hunterston Farms Bioblitz 2010"
Hunterston.2010$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Hunterston.2010$Observer <- NA
Hunterston.2010$Geo_Ref <- "Coordinates generalized based on mapped locality information"
Hunterston.2010$HabitatRemarks <- NA
Hunterston.2010$PositionalAccuracy <- 50
Hunterston.2010$GeoPrivacy <- NA
Hunterston.2010$PrivateLatitude <- NA
Hunterston.2010$PrivateLongitude <- NA
Hunterston.2010$Prov_State <- "British Columbia"
Hunterston.2010$Region <- "Gulf Islands"
Hunterston.2010$Location <- "Galiano Island"
Hunterston.2010$LocationDe <- "Hunterston Farms"

# Select key columns

Hunterston.2010 <- Hunterston.2010 %>% select(Scientific,Source,CatalogueN,Observer,Date,Latitude,Longitude,
        Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks)

# Standardize column names to facilitate join

names(Hunterston.2010) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
        'LocationDe','HabitatRemarks')

# Confirm number of records (303)

nrow(Hunterston.2010) # 303 Galiano Island records

# Merge with summary to standardize names and taxon metadata

Hunterston.2010.names.matched <- left_join(Hunterston.2010,summary, by = c('Taxon'))

# Unmatched records

Hunterston.2010.names.unmatched <- Hunterston.2010.names.matched[is.na(Hunterston.2010.names.matched$Taxon.Author),]

# Matched records

Hunterston.2010.names.matched <- anti_join(Hunterston.2010.names.matched,Hunterston.2010.names.unmatched)

# Standardize matched occurrence records

Hunterston.2010.names.matched <- Hunterston.2010.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,
        Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,
        Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Hunterston.2010.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
        'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
        'Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State',
        'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Hunterston.2010)
nrow(Hunterston.2010.names.matched)
nrow(Hunterston.2010.names.unmatched)
nrow(Hunterston.2010.names.matched)+nrow(Hunterston.2010.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Hunterston.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv")

# Swap unmatched names using key

Hunterston.2010.names.unmatched.matched <- Hunterston.2010.names.unmatched

Hunterston.2010.names.unmatched.matched$Taxon <- Hunterston.key$Matched.Taxon[match(unlist(Hunterston.2010.names.unmatched.matched$Taxon), Hunterston.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Hunterston.2010.names.unmatched.matched <- select(Hunterston.2010.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Hunterston.2010.names.unmatched.matched <- left_join(Hunterston.2010.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Hunterston.2010.names.unmatched.matched <- Hunterston.2010.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Hunterston.2010.names.unmatched.matched <- Hunterston.2010.names.unmatched.matched %>% select(Taxon,ID,Kingdom,
        Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,
        Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,
        Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Hunterston.2010.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
        'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
        'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
        'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
        'National.Status')

# Select names unmatched based on key

Hunterston.2010.names.unmatched.unmatched <- anti_join(Hunterston.2010.names.unmatched,Hunterston.2010.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Hunterston.2010)
nrow(Hunterston.2010.names.matched)
nrow(Hunterston.2010.names.unmatched)
nrow(Hunterston.2010.names.unmatched.matched)
nrow(Hunterston.2010.names.unmatched.unmatched)
nrow(Hunterston.2010.names.matched)+nrow(Hunterston.2010.names.unmatched.matched)+nrow(Hunterston.2010.names.unmatched.unmatched)

# Bind records

Hunterston.2010.records <- rbind(Hunterston.2010.names.matched,Hunterston.2010.names.unmatched.matched)

# Compare records in and out

nrow(Hunterston.2010)
nrow(Hunterston.2010.records) # 32 records discarded
# discarded records identified only to level of genus, i.e., redundant to include in summary
# also discarded: Juncus effusus, which requires infrataxonomic resolution

# Add to record of unmatched names

Hunterston.2010.names.unmatched.unmatched <- Hunterston.2010.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,
        Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
        Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Hunterston.2010.names.unmatched.unmatched)

# Generate key to manually match unmatched names in Hunterston 2010
# Note: code no longer required once key was generated

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Hunterston.2010.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- Hunterston.2010.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(Hunterston.2010.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(Hunterston.2010.names.unmatched.unmatched$Taxon, 2)

# Hunterston.2010.key <- rbind(Ecological.Reserve.128.key,unmatched.taxa)

# write.csv(Hunterston.2010.key,"Hunterston.2010.key.csv")



# Read Laughlin Lake 2002 records (GCA 2002)

Laughlin.2002 <- read.csv("digitized/Laughlin_Vegetation_Data_2002-08-20.csv")

# Create unique identifiers for observations

unique.prefix <- "GCA2002-2:"
unique.suffix <- 1:nrow(Laughlin.2002)

# Standardize columns

Laughlin.2002$Source <- "Laughlin Lake 2002 Vegetation Community List (GCA 2002)"
Laughlin.2002$Observer <- "Keith Erickson & Steven Gates"
Laughlin.2002$Date <- '2002-08-20'
Laughlin.2002$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Laughlin.2002$Latitude <- 48.949776874128624
Laughlin.2002$Longitude <- -123.50573155652819
Laughlin.2002$Geo_Ref <- "Coordinates generalized based on locality information"
Laughlin.2002$PositionalAccuracy <- 360
Laughlin.2002$GeoPrivacy <- NA
Laughlin.2002$PrivateLatitude <- NA
Laughlin.2002$PrivateLongitude <- NA
Laughlin.2002$PrivateLatitude <- NA
Laughlin.2002$PrivateLongitude <- NA
Laughlin.2002$Prov_State <- "British Columbia"
Laughlin.2002$Region <- "Gulf Islands"
Laughlin.2002$Location <- "Galiano Island"
Laughlin.2002$LocationDe <- "Laughlin Lake"

# Drop / correct spurious reports

Laughlin.2002 <- Laughlin.2002 %>% filter(!str_detect(Taxon, 'Carex rostrata')) # Out of range on Galiano Island; misreported

Laughlin.2002$Taxon[Laughlin.2002$Taxon == 'Typha latifola'] <- 'Typha angustifolia' # Bullrush at Laughlin Lake has since been confirmed as Typha angustifolium

Laughlin.2002$Taxon[Laughlin.2002$Taxon == 'Epilobium watsonii'] <- 'Typha angustifolia' # Epilobium watsonii some recognize as infrataxon of Epilobium ciliatum; collapsed with Epilobium ciliatum (report noted in curated summary)

# Select key columns

Laughlin.2002 <- Laughlin.2002 %>% select(Taxon,Source,CatalogueN,Observer,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,Description.Comments)

# Standardize column names to facilitate join

names(Laughlin.2002) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

Laughlin.2002.names.matched <- left_join(Laughlin.2002,summary, by = c('Taxon'))

# Unmatched records

Laughlin.2002.names.unmatched <- Laughlin.2002.names.matched[is.na(Laughlin.2002.names.matched$Taxon.Author),]

# Matched records

Laughlin.2002.names.matched <- anti_join(Laughlin.2002.names.matched,Laughlin.2002.names.unmatched)

# Standardize matched occcurence records

Laughlin.2002.names.matched <- Laughlin.2002.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,
        Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(Laughlin.2002.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
        'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
        'LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Laughlin.2002)
nrow(Laughlin.2002.names.matched)
nrow(Laughlin.2002.names.unmatched)
nrow(Laughlin.2002.names.matched)+nrow(Laughlin.2002.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Laughlin.2002.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Laughlin.2002.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- Laughlin.2002.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(Laughlin.2002.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(Laughlin.2002.names.unmatched.unmatched$Taxon, 2)

# review.key <- rbind(Laughlin.2002.key,unmatched.taxa)

# write.csv(review.key,"review.key.csv")

# Swap unmatched names using key

Laughlin.2002.names.unmatched.matched <- Laughlin.2002.names.unmatched

Laughlin.2002.names.unmatched.matched$Taxon <- Laughlin.2002.key$Matched.Taxon[match(unlist(Laughlin.2002.names.unmatched.matched$Taxon), Laughlin.2002.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Laughlin.2002.names.unmatched.matched <- select(Laughlin.2002.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Laughlin.2002.names.unmatched.matched <- left_join(Laughlin.2002.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Laughlin.2002.names.unmatched.matched <- Laughlin.2002.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Laughlin.2002.names.unmatched.matched <- Laughlin.2002.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,
        Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(Laughlin.2002.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
        'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

Laughlin.2002.names.unmatched.unmatched <- anti_join(Laughlin.2002.names.unmatched,Laughlin.2002.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Laughlin.2002)
nrow(Laughlin.2002.names.matched)
nrow(Laughlin.2002.names.unmatched)
nrow(Laughlin.2002.names.unmatched.matched)
nrow(Laughlin.2002.names.unmatched.unmatched)
nrow(Laughlin.2002.names.matched)+nrow(Laughlin.2002.names.unmatched.matched)+nrow(Laughlin.2002.names.unmatched.unmatched)

# Bind records

Laughlin.2002.records <- rbind(Laughlin.2002.names.matched,Laughlin.2002.names.unmatched.matched)

# Compare records in and out

nrow(Laughlin.2002)
nrow(Laughlin.2002.records) # 18 records discarded

# Note: taxa unrecognized in summary, and hence excluded from catalog, are either indeterminate taxa or Juncus effusus,
# which cannot be resolved based on the summary due to the lack of infraspecific resolution
# See also misreported taxa manually addressed above

# Add to record of unmatched names

Laughlin.2002.names.unmatched.unmatched <- Laughlin.2002.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,Latitude,
                                                                                              Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
                                                                                              HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Laughlin.2002.names.unmatched.unmatched)



# Read Frank Lomer 2022 Records
# Note: specimens will all be deposited at UBC and should be checked for duplicates against UBC records in the future 
# Note: review code below for consistency with others when standardizing fields; might be made more concise?

Lomer.2022 <- read.csv("digitized/Lomer_2022_Galiano_collections.csv")

# Select key columns

Lomer.2022 <- Lomer.2022 %>% select(Taxon,Genus,Species,Subspecies,Variety,Location,Degrees.Lat.,Degrees.Long,Habitat,Date,Collector,Number)

# Change date to POSIX

Lomer.2022$Date <- strptime(Lomer.2022$Date, "%Y %b %d")
Lomer.2022$Date <-  as.Date(Lomer.2022$Date)

# Confirm number of records (67)

nrow(Lomer.2022) # 67 Galiano Island records

# Standardize fields to join records

Lomer.2022$Source <- "UBC"
Lomer.2022$Geo_Ref <- NA
Lomer.2022$Prov_State <- "British Columbia"
Lomer.2022$District <- "Gulf Islands"
Lomer.2022$LocationDe <- NA
Lomer.2022$PositionalAccuracy <- 3
Lomer.2022$GeoPrivacy <- NA
Lomer.2022$PrivateLatitude <- NA
Lomer.2022$PrivateLongitude <- NA

names(Lomer.2022) <- c('Taxon','Genus','Species','Subspecies','Variety','Location','Latitude','Longitude','HabitatRemarks',
        'Date','Collector','CatalogueN','Source','Geo_Ref','Prov_State','Region','LocationDe','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude')

# Merge with summary to standardize names and taxon metadata

Lomer.2022.names.matched <- left_join(Lomer.2022,summary, by = c('Taxon'))

# Unmatched records

Lomer.2022.names.unmatched <- Lomer.2022.names.matched[is.na(Lomer.2022.names.matched$Taxon.Author),]

# Matched records

Lomer.2022.names.matched <- anti_join(Lomer.2022.names.matched,Lomer.2022.names.unmatched)

# Confirm all records are represented 

nrow(Lomer.2022)
nrow(Lomer.2022.names.matched)
nrow(Lomer.2022.names.unmatched)
nrow(Lomer.2022.names.matched)+nrow(Lomer.2022.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Lomer.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv")

# Swap unmatched names using key

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched

Lomer.2022.names.unmatched.matched$Taxon <- Lomer.key$Matched.Taxon[match(unlist(Lomer.2022.names.unmatched.matched$Taxon), Lomer.key$Taxon)]

# Drop NAs (taxa not recognized in summary)

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched.matched %>% drop_na(Taxon)

# Match keyed unmatched records with summary

Lomer.2022.names.unmatched.matched <- left_join(Lomer.2022.names.unmatched.matched,summary)

names(Lomer.2022.names.matched)

Lomer.2022.names.matched <- Lomer.2022.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus.y,
      Species.y,Hybrid,Subspecies.y,Variety.y,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,
      PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
      HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Lomer.2022.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
      'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
      'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region',
      'Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

Lomer.2022.names.unmatched.matched$Source <- "UBC"
Lomer.2022.names.unmatched.matched$Geo_Ref <- NA
Lomer.2022.names.unmatched.matched$Prov_State <- "British Columbia"
Lomer.2022.names.unmatched.matched$District <- "Gulf Islands"
Lomer.2022.names.unmatched.matched$LocationDe <- NA
Lomer.2022.names.unmatched.matched$PositionalAccuracy <- NA
Lomer.2022.names.unmatched.matched$GeoPrivacy <- NA
Lomer.2022.names.unmatched.matched$PrivateLatitude <- NA
Lomer.2022.names.unmatched.matched$PrivateLongitude <- NA

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,
      Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,
      Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
      Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Lomer.2022.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
      'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
      'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
      'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
      'National.Status')

# Select names unmatched based on key

Lomer.2022.names.unmatched.unmatched <- anti_join(Lomer.2022.names.unmatched,Lomer.2022.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Lomer.2022)
nrow(Lomer.2022.names.matched)
nrow(Lomer.2022.names.unmatched)
nrow(Lomer.2022.names.unmatched.matched)
nrow(Lomer.2022.names.unmatched.unmatched)
nrow(Lomer.2022.names.matched)+nrow(Lomer.2022.names.unmatched.matched)+nrow(Lomer.2022.names.unmatched.unmatched)

# Bind records

Lomer.2022.records <- rbind(Lomer.2022.names.matched,Lomer.2022.names.unmatched.matched)

# Compare records in and out

nrow(Lomer.2022)
nrow(Lomer.2022.records) # 1 record discarded, considered domesticated and thus not included in summary

# Add to record of unmatched names

Lomer.2022.names.unmatched.unmatched <- Lomer.2022.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,
        Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
        Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Lomer.2022.names.unmatched.unmatched)


# Generate new key with mismatched names

# Lomer.unmatched.Genus <- Lomer.2022.names.unmatched$Genus
# Lomer.unmatched.Species <- Lomer.2022.names.unmatched$Species
# Lomer.unmatched.Subspecies <- Lomer.2022.names.unmatched$Subspecies
# Lomer.unmatched.Variety <- Lomer.2022.names.unmatched$Variety
# Lomer.unmatched.Hybrid <- Lomer.2022.names.unmatched$Hybrid

# Lomer.2022.mismatched.key <- data.frame(Lomer.unmatched.Genus,Lomer.unmatched.Species,Lomer.unmatched.Subspecies,Lomer.unmatched.Variety,Lomer.unmatched.Hybrid)

# Lomer.2022.mismatched.key$Taxon <- paste(Lomer.unmatched.Genus,Lomer.unmatched.Species)
# Lomer.2022.mismatched.key$Form <- NA
# Lomer.2022.mismatched.key$Matched.Taxon <- NA

# names(Lomer.2022.mismatched.key) <- c('Genus','Species','Subspecies','Variety','Hybrid','Taxon','Form','Matched.Taxon')

# Lomer.2022.mismatched.key <- Lomer.2022.mismatched.key %>% select('Taxon','Genus','Species','Hybrid','Subspecies','Variety','Form','Matched.Taxon')

# Lomer.key.review <- rbind(Lomer.key,Lomer.2022.mismatched.key)

# write.csv(Lomer.key.review,"Lomer_key_review.csv")



# Read RBCM Records 
# Note: it looks like you may be losing 10 records; double check these data outputs!

RBCM.georeferencing.corrected <- read.csv("digitized/RBCM_vascular_plant_records_georeferencing_corrected_2021-12-05.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Generate Taxon field to facilitate join

RBCM.georeferencing.corrected$Taxon <- paste(RBCM.georeferencing.corrected$Genus,RBCM.georeferencing.corrected$Species)

# Standardize columns

RBCM.georeferencing.corrected$Source <- "RBCM"
RBCM.georeferencing.corrected$Geo_Ref <- "Coordinates generalized based on mapped locality information"
RBCM.georeferencing.corrected$PositionalAccuracy <- 100 # reconsider positional accuracy
RBCM.georeferencing.corrected$GeoPrivacy <- NA
RBCM.georeferencing.corrected$PrivateLatitude <- NA
RBCM.georeferencing.corrected$PrivateLongitude <- NA

# Select key columns

RBCM.georeferencing.corrected <- RBCM.georeferencing.corrected %>% select(Taxon,Source,CatalogueN,Collector,
        Collecti_1,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
        Prov_State,District,LocationNa,LocationDe,HabitatRem)

# Standardize column names to facilitate join

names(RBCM.georeferencing.corrected) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude',
        'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region',
        'Location','LocationDe','HabitatRemarks')

# Confirm number of records

nrow(RBCM.georeferencing.corrected) # 139 records

# Merge with summary to standardize names and taxon metadata

RBCM.georeferencing.corrected.names.matched <- left_join(RBCM.georeferencing.corrected, summary, by = c('Taxon'))

# Unmatched records

RBCM.georeferencing.corrected.names.unmatched <- RBCM.georeferencing.corrected.names.matched[is.na(RBCM.georeferencing.corrected.names.matched$Taxon.Author),]

# Matched records

RBCM.georeferencing.corrected.names.matched <- anti_join(RBCM.georeferencing.corrected.names.matched,RBCM.georeferencing.corrected.names.unmatched,by='CatalogueN')

# Standardize matched occurrence records

RBCM.georeferencing.corrected.names.matched <- RBCM.georeferencing.corrected.names.matched %>% select(Taxon,ID,
        Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,
        Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
        Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(RBCM.georeferencing.corrected.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
        'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
        'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
        'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
        'National.Status')

# Confirm all records are represented 

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.georeferencing.corrected.names.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched)
nrow(RBCM.georeferencing.corrected.names.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

RBCM.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv")

# Swap unmatched names using key

RBCM.georeferencing.corrected.names.unmatched.matched <- RBCM.georeferencing.corrected.names.unmatched

RBCM.georeferencing.corrected.names.unmatched.matched$Taxon <- RBCM.key$Matched.Taxon[match(unlist(RBCM.georeferencing.corrected.names.unmatched.matched$Taxon), RBCM.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

RBCM.georeferencing.corrected.names.unmatched.matched <- select(RBCM.georeferencing.corrected.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

RBCM.georeferencing.corrected.names.unmatched.matched <- left_join(RBCM.georeferencing.corrected.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

RBCM.georeferencing.corrected.names.unmatched.matched <- RBCM.georeferencing.corrected.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

RBCM.georeferencing.corrected.names.unmatched.matched <- RBCM.georeferencing.corrected.names.unmatched.matched %>% 
          select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,
          Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
          Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(RBCM.georeferencing.corrected.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order',
        'Family','Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
        'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
        'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

RBCM.georeferencing.corrected.names.unmatched.unmatched <- anti_join(RBCM.georeferencing.corrected.names.unmatched,RBCM.georeferencing.corrected.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.georeferencing.corrected.names.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched)
nrow(RBCM.georeferencing.corrected.names.unmatched.matched)
nrow(RBCM.georeferencing.corrected.names.unmatched.unmatched)
nrow(RBCM.georeferencing.corrected.names.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched.matched)+nrow(RBCM.georeferencing.corrected.names.unmatched.unmatched)

# Bind records

RBCM.records <- rbind(RBCM.georeferencing.corrected.names.matched,RBCM.georeferencing.corrected.names.unmatched.matched)

# Compare records in and out

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.records) # 5 records discarded

# discarded records include those that cannot be reconciled with summary due to lack of infrataxonic specificity
# Note: it should be possible to match Phragmites australis with a refresh of RBCM data

# Add to record of unmatched names

RBCM.georeferencing.corrected.names.unmatched.unmatched <- RBCM.georeferencing.corrected.names.unmatched.unmatched %>% 
        select(Taxon,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,RBCM.georeferencing.corrected.names.unmatched.unmatched)

# Generate new key with mismatched names

# RBCM.unmatched.Genus <- RBCM.georeferencing.corrected.names.unmatched$Genus
# RBCM.unmatched.Species <- RBCM.georeferencing.corrected.names.unmatched$Species
# RBCM.unmatched.Subspecies <- RBCM.georeferencing.corrected.names.unmatched$Subspecies
# RBCM.unmatched.Variety <- RBCM.georeferencing.corrected.names.unmatched$Variety
# RBCM.unmatched.Hybrid <- RBCM.georeferencing.corrected.names.unmatched$Hybrid

# RBCM.mismatched.key <- data.frame(RBCM.unmatched.Genus,RBCM.unmatched.Species,RBCM.unmatched.Subspecies,RBCM.unmatched.Variety,RBCM.unmatched.Hybrid)

# RBCM.mismatched.key$Taxon <- paste(RBCM.unmatched.Genus,RBCM.unmatched.Species)
# RBCM.mismatched.key$Form <- NA
# RBCM.mismatched.key$Matched.Taxon <- NA

# names(RBCM.mismatched.key) <- c('Genus','Species','Subspecies','Variety','Hybrid','Taxon','Form','Matched.Taxon')

# RCM.mismatched.key <- RBCM.mismatched.key %>% select('Taxon','Genus','Species','Hybrid','Subspecies','Variety','Form','Matched.Taxon')

# vascular.plant.taxon.key.review <- rbind(Lomer.key,RBCM.mismatched.key)

# write.csv(vascular.plant.taxon.key.review,"vascular_plant_taxon_key_review.csv")



# Read Hans Roemer - 2004 - Mt. Sutil Records

Roemer.2004.Mt.Sutil.plot.metadata <- read.csv("digitized/Roemer_2004_Mt_Sutil_plot_metadata.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

Roemer.2004.Mt.Sutil.vegetation <- read.csv("digitized/Roemer_2004_Mt_Sutil_vegetation.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Remove non-vasculars from vegetation dataframe

Roemer.2004.Mt.Sutil.vegetation <- subset(Roemer.2004.Mt.Sutil.vegetation, layer != "moss/lichen")

# Compile records with plot metadata

Roemer.2004.Mt.Sutil <- left_join(Roemer.2004.Mt.Sutil.vegetation, Roemer.2004.Mt.Sutil.plot.metadata)

Roemer.2004 <- Roemer.2004.Mt.Sutil %>% select(Taxon, latitude, longitude, description)

# Create unique identifiers for observations

unique.prefix <- "ROEMER2004:"
unique.suffix <- 1:nrow(Roemer.2004)

# Standardize columns

Roemer.2004$Source <- "Mt. Sutil Vegetation Study"
Roemer.2004$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Roemer.2004$Observer <- "Hans Roemer"
Roemer.2004$Date <- '2004-07'
Roemer.2004$Geo_Ref <- "Coordinates mapped by GPS"
Roemer.2004$PositionalAccuracy <- 3 # Need to ask Hans how good his GPS was...
Roemer.2004$GeoPrivacy <- NA
Roemer.2004$PrivateLatitude <- NA
Roemer.2004$PrivateLongitude <- NA
Roemer.2004$Prov_State <- "British Columbia"
Roemer.2004$Region <- "Gulf Islands"
Roemer.2004$Location <- "Galiano Island"
Roemer.2004$LocationDe <- "Mount Sutil"

# Select key columns

Roemer.2004 <- Roemer.2004 %>% select(Taxon,Source,CatalogueN,Observer,Date,latitude,longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,description)

# Standardize column names to facilitate join

names(Roemer.2004) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe','HabitatRemarks')

# Confirm number of records

nrow(Roemer.2004) # 234 records

# Merge with summary to standardize names and taxon metadata

Roemer.2004.names.matched <- left_join(Roemer.2004,summary, by = c('Taxon'))

# Unmatched records

Roemer.2004.names.unmatched <- Roemer.2004.names.matched[is.na(Roemer.2004.names.matched$Taxon.Author),]

# Matched records

Roemer.2004.names.matched <- anti_join(Roemer.2004.names.matched,Roemer.2004.names.unmatched)

# Standardize matched occurrence records

Roemer.2004.names.matched <- Roemer.2004.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,
        Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,
        Provincial.Status,National.Status)

names(Roemer.2004.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
        'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region',
        'Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Roemer.2004)
nrow(Roemer.2004.names.matched)
nrow(Roemer.2004.names.unmatched)
nrow(Roemer.2004.names.matched)+nrow(Roemer.2004.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Roemer.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv")

# Swap unmatched names using key

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched

Roemer.2004.names.unmatched.matched$Taxon <- Roemer.key$Matched.Taxon[match(unlist(Roemer.2004.names.unmatched.matched$Taxon), Roemer.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Roemer.2004.names.unmatched.matched <- select(Roemer.2004.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Roemer.2004.names.unmatched.matched <- left_join(Roemer.2004.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,
        Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(Roemer.2004.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription'
        ,'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

Roemer.2004.names.unmatched.unmatched <- anti_join(Roemer.2004.names.unmatched,Roemer.2004.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Roemer.2004)
nrow(Roemer.2004.names.matched)
nrow(Roemer.2004.names.unmatched)
nrow(Roemer.2004.names.unmatched.matched)
nrow(Roemer.2004.names.unmatched.unmatched)
nrow(Roemer.2004.names.matched)+nrow(Roemer.2004.names.unmatched.matched)+nrow(Roemer.2004.names.unmatched.unmatched)

# Bind records

Roemer.2004.records <- rbind(Roemer.2004.names.matched,Roemer.2004.names.unmatched.matched)

# Compare records in and out

nrow(Roemer.2004)
nrow(Roemer.2004.records) # 0 records discarded

# Add to record of unmatched names

Roemer.2004.names.unmatched.unmatched <- Roemer.2004.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,
        Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Roemer.2004.names.unmatched.unmatched)



# Read Simon 2018 data

Simon.2018.transects <- read.csv("digitized/Simon_2018_transect_coordinates.csv")

Simon.2018.veg.data <- read.csv("digitized/Simon_2018_vegetation_data.csv")

# Add common name for transect ID to facilitate join

names(Simon.2018.transects) <- c('TID','Latitude','Longitude','Note')

# Merge transect coordinates with veg data

Simon.2018 <- inner_join(Simon.2018.veg.data, Simon.2018.transects, by = 'TID')

# Remove rows with NAs

Simon.2018 <- Simon.2018 %>% drop_na(Taxon)

# Remove '_'s from Taxon field

Simon.2018$Taxon <-  str_replace(Simon.2018$Taxon, "_", " ")

# Create unique identifiers for observations

unique.prefix <- "SIMON2021:"
unique.suffix <- 1:nrow(Simon.2018)

# Standardize columns

Simon.2018$Source <- "Phylogenetic restriction of plant invasion in drought stressed environments"
Simon.2018$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Simon.2018$Observer <- "Andrew Simon"
Simon.2018$Geo_Ref <- "Coordinates mapped by GPS"
Simon.2018$PositionalAccuracy <- 3
Simon.2018$GeoPrivacy <- NA
Simon.2018$PrivateLatitude <- NA
Simon.2018$PrivateLongitude <- NA
Simon.2018$Prov_State <- "British Columbia"
Simon.2018$Region <- "Gulf Islands"
Simon.2018$Location <- "Galiano Island"

# Select key columns

Simon.2018 <- Simon.2018 %>% select(Taxon,Source,CatalogueN,Observer,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,Site,Description)

# Standardize column names to facilitate joins

names(Simon.2018) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe','HabitatRemarks')

# Confirm number of records

nrow(Simon.2018) # 3,196 records

# Merge with summary to standardize names and taxon metadata

Simon.2018.names.matched <- left_join(Simon.2018,summary, by = c('Taxon'))

# Unmatched records

Simon.2018.names.unmatched <- Simon.2018.names.matched[is.na(Simon.2018.names.matched$Taxon.Author),]

# Matched records

Simon.2018.names.matched <- anti_join(Simon.2018.names.matched,Simon.2018.names.unmatched)

# Standardize matched occurrence records

Simon.2018.names.matched <- Simon.2018.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,
        Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,
        PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
        HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Simon.2018.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
        'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
        'LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Simon.2018)
nrow(Simon.2018.names.matched)
nrow(Simon.2018.names.unmatched)
nrow(Simon.2018.names.matched)+nrow(Simon.2018.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Simon.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv")

# Swap unmatched names using key

Simon.2018.names.unmatched.matched <- Simon.2018.names.unmatched

Simon.2018.names.unmatched.matched$Taxon <- Simon.key$Matched.Taxon[match(unlist(Simon.2018.names.unmatched.matched$Taxon), Simon.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Simon.2018.names.unmatched.matched <- select(Simon.2018.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Simon.2018.names.unmatched.matched <- left_join(Simon.2018.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Simon.2018.names.unmatched.matched <- Simon.2018.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Simon.2018.names.unmatched.matched <- Simon.2018.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,
        Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,
        Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Simon.2018.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
        'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
        'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
        'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
        'National.Status')

# Select names unmatched based on key

Simon.2018.names.unmatched.unmatched <- anti_join(Simon.2018.names.unmatched,Simon.2018.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Simon.2018)
nrow(Simon.2018.names.matched)
nrow(Simon.2018.names.unmatched)
nrow(Simon.2018.names.unmatched.matched)
nrow(Simon.2018.names.unmatched.unmatched)
nrow(Simon.2018.names.matched)+nrow(Simon.2018.names.unmatched.matched)+nrow(Simon.2018.names.unmatched.unmatched)

# Bind records

Simon.2018.records <- rbind(Simon.2018.names.matched,Simon.2018.names.unmatched.matched)

# Compare records in and out

nrow(Simon.2018)
nrow(Simon.2018.records) # 92 records discarded
# discarded records all domesticated / cultivated plants

# Add to record of unmatched names

Simon.2018.names.unmatched.unmatched <- Simon.2018.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,
                                                                                        Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
                                                                                        Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,Simon.2018.names.unmatched.unmatched)



# Read UBC specimen records (UBC 2022)

UBC.2022 <- read.csv("digitized/UBC_Galiano_Island_vascular_plants_2022-10-31.csv")

# Standardize columns

UBC.2022$GeoPrivacy <- NA
UBC.2022$PrivateLatitude <- NA
UBC.2022$PrivateLongitude <- NA
UBC.2022$PrivateLatitude <- NA
UBC.2022$PrivateLongitude <- NA
UBC.2022$Region <- "Gulf Islands"
UBC.2022$Source <- "UBC"
UBC.2022$LocationDe <- NA

# Select key columns

UBC.2022 <- UBC.2022 %>% select(Taxon,Source,Accession.Number,Primary.Collector,Date.Collected,Geo_LatDecimal,
        Geo_LongDecimal,Geo_Source,Geo_CoordinateUncertainty,GeoPrivacy,PrivateLatitude,PrivateLongitude,ProvinceState,
        Region,Location,LocationDe,Habitat)

# Standardize column names to facilitate join

names(UBC.2022) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe',
        'HabitatRemarks')

# Change date format to POSIX

UBC.2022$Date <- strptime(UBC.2022$Date, "%Y %b %d")
UBC.2022$Date <-  as.Date(UBC.2022$Date)

# Merge with summary to standardize names and taxon metadata

UBC.2022.names.matched <- left_join(UBC.2022,summary, by = c('Taxon'))

# Unmatched records

UBC.2022.names.unmatched <- UBC.2022.names.matched[is.na(UBC.2022.names.matched$Taxon.Author),]

# Matched records

UBC.2022.names.matched <- anti_join(UBC.2022.names.matched,UBC.2022.names.unmatched)

# Standardize matched occcurrence records

UBC.2022.names.matched <- UBC.2022.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,
        Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(UBC.2022.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
        'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks',
        'Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(UBC.2022)
nrow(UBC.2022.names.matched)
nrow(UBC.2022.names.unmatched)
nrow(UBC.2022.names.matched)+nrow(UBC.2022.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

UBC.2022.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(UBC.2022.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- UBC.2022.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(UBC.2022.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(UBC.2022.names.unmatched.unmatched$Taxon, 2)

# review.key <- rbind(UBC.2022.key,unmatched.taxa)

# write.csv(review.key,"review.key.csv")

# Swap unmatched names using key

UBC.2022.names.unmatched.matched <- UBC.2022.names.unmatched

UBC.2022.names.unmatched.matched$Taxon <- UBC.2022.key$Matched.Taxon[match(unlist(UBC.2022.names.unmatched.matched$Taxon), UBC.2022.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

UBC.2022.names.unmatched.matched <- select(UBC.2022.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

UBC.2022.names.unmatched.matched <- left_join(UBC.2022.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

UBC.2022.names.unmatched.matched <- UBC.2022.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

UBC.2022.names.unmatched.matched <- UBC.2022.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,
        Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(UBC.2022.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
        'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

UBC.2022.names.unmatched.unmatched <- anti_join(UBC.2022.names.unmatched,UBC.2022.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(UBC.2022)
nrow(UBC.2022.names.matched)
nrow(UBC.2022.names.unmatched)
nrow(UBC.2022.names.unmatched.matched)
nrow(UBC.2022.names.unmatched.unmatched)
nrow(UBC.2022.names.matched)+nrow(UBC.2022.names.unmatched.matched)+nrow(UBC.2022.names.unmatched.unmatched)

# Bind records

UBC.2022.records <- rbind(UBC.2022.names.matched,UBC.2022.names.unmatched.matched)

# Compare records in and out

nrow(UBC.2022)
nrow(UBC.2022.records) # Good: only four records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog:
# Two taxa identified only two genus
# Two cultivated species

# Add to record of unmatched names

UBC.2022.names.unmatched.unmatched <- UBC.2022.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,
        Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,UBC.2022.names.unmatched.unmatched)



# Combine all source occurrence records

Vascular.plant.records <- rbind(BC.CDC.2019.records,DL63.records,Ecological.Reserve.128.records,Hunterston.2010.records,
                                Laughlin.2002.records,Roemer.2004.records,RBCM.records,Simon.2018.records,Lomer.2022.records,
                                UBC.2022.records)

# Combine with iNaturalist observations

iNaturalist.observations <- read.csv("digitized/iNaturalist_vascular_plant_observations.csv")

names(Vascular.plant.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
      'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
      'Longitude','GeoRef','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','ProvState',
      'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')
names(iNaturalist.observations) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
      'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
      'Longitude','GeoRef','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','ProvState',
      'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

Vascular.plant.records <- rbind(Vascular.plant.records,iNaturalist.observations)

# Output synthesized catalog of occurrence records

# write.csv(Vascular.plant.records,"Galiano_Island_vascular_plant_records_consolidated.csv")

# Tally records

nrow(Vascular.plant.records)

# Summary of records that remain unmatched

nrow(unmatched.vascular.plant.records)

unique(unmatched.vascular.plant.records$Taxon)
