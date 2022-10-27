# Script to consolidate occurrence records of Galiano Island's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../2_review/Plantae_et_Chromista/vascular_plants/summary/Tracheophyta_review_summary_reviewed_2022-10-26.csv")

# Read occurrence records

# Sources:

# BC Conservation Data Centre - not yet added
# Consortium of PNW Herbaria: BABY, RBCM, UBC, WS, WTU - some RBCM records already added; otherwise incomplete
# Hunterston 2010 - added
# iNaturalist 2016-2022 - added
# Janszen 2000 - added
# Lomer 2022 - added
# RBCM - added, but incomplete; some records with geo-referencing (partly) corrected
# Roemer 2004 - added
# Simon 2018 - added



# Read Ecological Reserve 128 records (Roemer & Janszen 1980, Roemer 2000)

Ecological.Reserve.128 <- read.csv("digitized/Galiano_Bog_Plant_List_Janszen_2000.csv")

# Filter plants

Ecological.Reserve.128 <- Ecological.Reserve.128 %>% filter(Group == 'vascular plants')

# Create unique identifiers for observations

unique.prefix <- "JANSZEN2000:"
unique.suffix <- 1:nrow(Ecological.Reserve.128)

# Standardize columns

Ecological.Reserve.128$Source <- "Galiano Bog Plant List (Janszen 2000)"
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

Ecological.Reserve.128 <- Ecological.Reserve.128 %>% select(Species,Source,CatalogueN,Observer,Date,Latitude,
        Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
        Location,LocationDe,HabitatRemarks)

# Standardize column names to facilitate join

names(Ecological.Reserve.128) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
        'LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

Ecological.Reserve.128.names.matched <- left_join(Ecological.Reserve.128,summary, by = c('Taxon'))

# Unmatched records

Ecological.Reserve.128.names.unmatched <- Ecological.Reserve.128.names.matched[is.na(Ecological.Reserve.128.names.matched$Taxon.Author),]

# Matched records

Ecological.Reserve.128.names.matched <- anti_join(Ecological.Reserve.128.names.matched,Ecological.Reserve.128.names.unmatched)

# Standardize matched occcurence records

Ecological.Reserve.128.names.matched <- Ecological.Reserve.128.names.matched %>% select(Taxon,ID,Kingdom,Phylum,
        Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,
        Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
        LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Ecological.Reserve.128.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
        'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
        'Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State',
        'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Ecological.Reserve.128)
nrow(Ecological.Reserve.128.names.matched)
nrow(Ecological.Reserve.128.names.unmatched)
nrow(Ecological.Reserve.128.names.matched)+nrow(Ecological.Reserve.128.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Ecological.Reserve.128.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

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

Ecological.Reserve.128.names.unmatched.matched <- Ecological.Reserve.128.names.unmatched.matched %>% select(Taxon,
        ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,
        Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
        Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Ecological.Reserve.128.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order',
        'Family','Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
        'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
        'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
        'National.Status')

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

# Start record of unmatched names

unmatched.vascular.plant.records <- Ecological.Reserve.128.names.unmatched.unmatched %>% select(Taxon,
        Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
        PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,
        Provincial.Status,National.Status)

unmatched.vascular.plant.records

# Generate key to manually match unmatched names in Janszen 2000
# Note: code no longer required once key was generated

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Ecological.Reserve.128.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- Ecological.Reserve.128.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(Ecological.Reserve.128.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(Ecological.Reserve.128.names.unmatched.unmatched$Taxon, 2)

# Janszen.2000.key <- rbind(Ecological.Reserve.128.key,unmatched.taxa)

# write.csv(Janszen.2000.key,"Janszen.2000.key.csv")



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



# Read Frank Lomer 2022 Records
# Note: specimens will all be deposited at UBC and should be checked for duplicates against UBC records in the future 
# Note: review code below for consistency with others when standardizing fields; might be made more concise?

Lomer.2022 <- read.csv("digitized/Lomer_2022_Galiano_collections.csv")

# Select key columns

Lomer.2022 <- Lomer.2022 %>% select(Taxon,Genus,Species,Subspecies,Variety,Location,Degrees.Lat.,Degrees.Long,Habitat,Date,Collector,Number)

# Normalize date 

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

RBCM.georeferencing.corrected.records <- rbind(RBCM.georeferencing.corrected.names.matched,RBCM.georeferencing.corrected.names.unmatched.matched)

# Compare records in and out

nrow(RBCM.georeferencing.corrected)
nrow(RBCM.georeferencing.corrected.records) # 5 records discarded

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

Roemer.2004.names.unmatched.matched <- Roemer.2004.names.unmatched.matched %>% select(Taxon,ID,Kingdom,
                                                                                      Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,
                                                                                      Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,
                                                                                      Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Roemer.2004.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
                                                'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
                                                'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
                                                'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
                                                'National.Status')

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

Roemer.2004.names.unmatched.unmatched <- Roemer.2004.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,
                                                                                          Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,
                                                                                          Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

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

# Standardize matched occurence records

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



# Combine all source occurrence records

Vascular.plant.records <- rbind(Ecological.Reserve.128.records,Hunterston.2010.records,Roemer.2004.records,RBCM.vascular.plant.records,Simon.2018.records,Lomer.2022.records)

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
