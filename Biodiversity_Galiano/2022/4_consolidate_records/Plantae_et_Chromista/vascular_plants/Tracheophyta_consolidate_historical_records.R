# Script to consolidate occurrence records of Galiano Island's vascular plants

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("summary/Tracheophyta_review_summary_reviewed.csv")

# Read occurrence records


##### NOTE: There may be a mistake in one of the scripts below; make sure to test each data set to make sure they are complete


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

Ecological.Reserve.128.key <- read.csv("keys/Janszen.2000.key.csv") 

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
# Epilobium cf ciliatum - could be E. ciliatum; could also be Epilobium leptophyllum, since documented at the bog
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

# Standardize matched occurence records

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

Hunterston.key <- read.csv("keys/Hunterston.2010.key.csv") # Lomer key is the best key so far

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
nrow(Hunterston.2010.records) # Good: only five records discarded, accounted for above.

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
# Note specimens are all to be deposited at UBC and should be checked for duplicates against UBC records in the future 

Lomer.2022 <- read.csv("digitized/Lomer_2022_Galiano_collections.csv")

# Select key columns

Lomer.2022 <- Lomer.2022 %>% select(Taxon,Genus,Species,Subspecies,Variety,Location,Degrees.Lat.,Degrees.Long,Habitat,Date,Collector,Number)

# Normalize date 

Lomer.2022$Date <- strptime(Lomer.2022$Date, "%Y %b %d")

Lomer.2022$Date <-  as.Date(Lomer.2022$Date)

# Confirm number of records (67)

nrow(Lomer.2022) # 67 Galiano Island records

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

Lomer.key <- read.csv("keys/Lomer_2022_unmatched_taxon_key.csv")

# Swap unmatched names using key

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched

Lomer.2022.names.unmatched.matched$Taxon <- Lomer.key$Matched.Taxon[match(unlist(Lomer.2022.names.unmatched.matched$Taxon), Lomer.key$Taxon)]

# Drop NAs (taxa not recognized in summary)

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched.matched %>% drop_na(Taxon)

# Select only essential fields to facilitate joins

Lomer.2022.names.unmatched.matched <- Lomer.2022.names.unmatched.matched %>% select('Taxon','Location',
        'Degrees.Lat.','Degrees.Long','Habitat','Date','Collector','Number')

# Match keyed unmatched records with summary

Lomer.2022.names.unmatched.matched <- left_join(Lomer.2022.names.unmatched.matched,summary)

# Standardize fields to join records

Lomer.2022.names.matched$Source <- "UBC"
Lomer.2022.names.matched$Geo_Ref <- NA
Lomer.2022.names.matched$Prov_State <- "British Columbia"
Lomer.2022.names.matched$District <- "Gulf Islands"
Lomer.2022.names.matched$LocationDe <- NA
Lomer.2022.names.matched$PositionalAccuracy <- NA
Lomer.2022.names.matched$GeoPrivacy <- NA
Lomer.2022.names.matched$PrivateLatitude <- NA
Lomer.2022.names.matched$PrivateLongitude <- NA

Lomer.2022.names.matched <- Lomer.2022.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus.y,
      Species.y,Hybrid,Subspecies.y,Variety.y,Source,Number,Collector,Date,Degrees.Lat.,Degrees.Long,Geo_Ref,
      PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,District,Location,LocationDe,
      Habitat,Origin,Provincial.Status,National.Status)

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
      Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,Number,Collector,Date,Degrees.Lat.,
      Degrees.Long,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,District,
      Location,LocationDe,Habitat,Origin,Provincial.Status,National.Status)

names(Lomer.2022.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
      'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
      'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
      'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
      'National.Status')

# Combine records

Lomer.2022.records <- rbind(Lomer.2022.names.matched,Lomer.2022.names.unmatched.matched)

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

# Select key columns

RBCM.georeferencing.corrected <-  RBCM.georeferencing.corrected %>% select(Genus, Species, CatalogueN, Collector, 
    Collecti_1, Prov_State, District, LocationNa, LocationDe, Latitude, Longitude, HabitatRem, Geo_Ref)

# Merge with summary to standardize names and taxon metadata

nrow(RBCM.georeferencing.corrected) # Confirm record # (139)

RBCM.georeferencing.corrected.names.matched <- inner_join(summary, RBCM.georeferencing.corrected, by = c('Genus','Species'))

# Standardize fields

RBCM.georeferencing.corrected.names.matched$Source <- 'RBCM'
RBCM.georeferencing.corrected.names.matched$PositionalAccuracy <- NA
RBCM.georeferencing.corrected.names.matched$GeoPrivacy <- NA
RBCM.georeferencing.corrected.names.matched$PrivateLatitude <- NA
RBCM.georeferencing.corrected.names.matched$PrivateLongitude <- NA

RBCM.georeferencing.corrected.names.matched <- RBCM.georeferencing.corrected.names.matched %>% select(Taxon,ID,
    Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,
    Collecti_1,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,
    District,LocationNa,LocationDe,HabitatRem,Origin,Provincial.Status,National.Status)

names(RBCM.georeferencing.corrected.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
    'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate',
    'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
    'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
    'National.Status')

nrow(RBCM.georeferencing.corrected) # Confirm all records are retained after merging with summary columns

RBCM.vascular.plant.records <- RBCM.georeferencing.corrected.names.matched

nrow(RBCM.vascular.plant.records) # ten missing records?


# Read Hans Roemer - 2004 - Mt. Sutil Records

Roemer.2004.Mt.Sutil.plot.metadata <- read.csv("digitized/Roemer_2004_Mt_Sutil_plot_metadata.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

Roemer.2004.Mt.Sutil.vegetation <- read.csv("digitized/Roemer_2004_Mt_Sutil_vegetation.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Remove non-vasculars from vegetation dataframe

Roemer.2004.Mt.Sutil.vegetation <- subset(Roemer.2004.Mt.Sutil.vegetation, layer != "moss/lichen")

# Compile records with plot metadata

Roemer.2004.Mt.Sutil <- left_join(Roemer.2004.Mt.Sutil.vegetation, Roemer.2004.Mt.Sutil.plot.metadata)

Roemer.2004.obs <- Roemer.2004.Mt.Sutil %>% select(Taxon, latitude, longitude, description)

# Confirm record #

nrow(Roemer.2004.obs)

# Merge with summary to standardize names and taxon metadata

Roemer.2004.obs <- left_join(Roemer.2004.obs, summary, by = c('Taxon'))

# Matched records

Roemer.2004.obs.matched.names <- Roemer.2004.obs %>% drop_na(Genus)

nrow(Roemer.2004.obs.matched.names) # Count matched records

# Unmatched records

Roemer.2004.obs.unmatched.names <- Roemer.2004.obs[is.na(Roemer.2004.obs$Genus),]

nrow(Roemer.2004.obs.unmatched.names) # Count unmatched records

# Sum matched and unmatched records: 

nrow(Roemer.2004.obs.unmatched.names) + nrow(Roemer.2004.obs.matched.names)
nrow(Roemer.2004.obs)

# Read taxon key to facilitate join

key <- read.csv("keys/Roemer_2004_unmatched_taxon_key.csv")

Roemer.2004.obs.unmatched.names.matched <- inner_join(Roemer.2004.obs.unmatched.names,key)

# Has the key matched the names? Count the rows and compare with the unmatched dataframe

nrow(Roemer.2004.obs.unmatched.names) 
nrow(Roemer.2004.obs.unmatched.names.matched)

# Isolate unmatched names # Note: The following code is no longer needed after updating the key

# Roemer.2004.obs.unmatched.names.unmatched <- anti_join(Roemer.2004.obs.unmatched.names,Roemer.2004.obs.unmatched.names.matched, by = "Taxon")

# Roemer.2004.obs.unmatched.names.unmatched <- Roemer.2004.obs.unmatched.names.unmatched %>% select(Taxon)

# Roemer.2004.obs.unmatched.names.unmatched <- Roemer.2004.obs.unmatched.names.unmatched %>% distinct(Taxon)

# Roemer.2004.obs.unmatched.names.unmatched$Matched.Taxon <- NA

# Roemer.key <- rbind(key,Roemer.2004.obs.unmatched.names.unmatched)

# write.csv(Roemer.key, "Roemer_unmatched_taxon_key.csv")

# Substitute names with those from curated summary / key

Roemer.2004.obs.unmatched.names.matched$Taxon <- Roemer.2004.obs.unmatched.names.matched$Matched.Taxon

Roemer.2004.obs.unmatched.names.matched$Matched.Taxon

Roemer.2004.obs.unmatched.names.matched <- Roemer.2004.obs.unmatched.names.matched[c(1:4)]

Roemer.2004.obs.unmatched.names.matched <- left_join(Roemer.2004.obs.unmatched.names.matched, summary, by = c('Taxon'))

# Combine formerly unmatched with matched records

Roemer.2004.obs <- rbind(Roemer.2004.obs.unmatched.names.matched, Roemer.2004.obs.matched.names)

nrow(Roemer.2004.obs)

# Standardize columns with fields from all catalogs

Roemer.2004.obs$Source <- 'Mt. Sutil Vegetation Study July 2004'
Roemer.2004.obs$CatalogueN <- NA
Roemer.2004.obs$Collector <- 'Hans Roemer'
Roemer.2004.obs$Collecti_1 <- '2004-07'
Roemer.2004.obs$Geo_Ref <- NA
Roemer.2004.obs$Prov_State <- "British Columbia"
Roemer.2004.obs$District <- "Gulf Islands"
Roemer.2004.obs$LocationNa <- "Galiano Island; Mt. Sutil"
Roemer.2004.obs$LocationDe <- NA
Roemer.2004.obs$HabitatRem <- Roemer.2004.obs$description
Roemer.2004.obs$PositionalAccuracy <- NA
Roemer.2004.obs$GeoPrivacy <- NA
Roemer.2004.obs$PrivateLatitude <- NA
Roemer.2004.obs$PrivateLongitude <- NA

Roemer.2004.obs <- Roemer.2004.obs %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,
      Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Collecti_1,latitude,longitude,Geo_Ref,
      PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,District,LocationNa,
      LocationDe,HabitatRem,Origin,Provincial.Status,National.Status)

names(Roemer.2004.obs) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
      'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
      'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region',
      'Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Read Simon 2018 data

Simon.2018.transects <- read.csv("digitized/Simon_2018_transect_coordinates.csv")

Simon.2018.veg.data <- read.csv("digitized/Simon_2018_vegetation_data.csv")

# Add common name for transect ID to facilitate join

names(Simon.2018.transects) <- c('TID','Latitude','Longitude','Note')

# Merge transect coordinates with veg data

Simon.2018 <- inner_join(Simon.2018.veg.data, Simon.2018.transects, by = 'TID')

# Remove rows with NAs

Simon.2018 <- Simon.2018 %>% drop_na(Taxon)

# Select relevant fields from dataset

Simon.2018 <- Simon.2018 %>% select(Taxon, Date, Site, Description, Latitude, Longitude, Note)

# Add temporary unique IDs for observations (for separating matched from unmatched records)

Simon.2018$id <- c(1:(nrow(Simon.2018)))

# Remove _s from Taxon field

Simon.2018$Taxon <-  str_replace(Simon.2018$Taxon, "_", " ")

# Merge with summary to standardize names and taxon metadata

nrow(Simon.2018) # Confirm record # (3,196)

Simon.2018.matched.records <- inner_join(summary,Simon.2018, by = c('Taxon'))

Simon.2018.matched.records$Source <- "Phylogenetic restriction of plant invasion in drought stressed environments"
Simon.2018.matched.records$CatalogueN <- NA
Simon.2018.matched.records$Collector <- "Andrew Simon"
Simon.2018.matched.records$Geo_Ref <- Simon.2018.matched.records$Note
Simon.2018.matched.records$Prov_State <- "British Columbia"
Simon.2018.matched.records$Region <- "Gulf Islands"
Simon.2018.matched.records$Location <- paste("Galiano Island; ",Simon.2018.matched.records$Site)
Simon.2018.matched.records$LocationDescription <- NA
Simon.2018.matched.records$HabitatRemarks <- Simon.2018.matched.records$Description
Simon.2018.matched.records$PositionalAccuracy <- NA
Simon.2018.matched.records$GeoPrivacy <- NA
Simon.2018.matched.records$PrivateLatitude <- NA
Simon.2018.matched.records$PrivateLongitude <- NA

Simon.2018.matched.records <- Simon.2018.matched.records %>% select('Taxon','ID','Kingdom','Phylum','Class','Order',
      'Family','Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','Date','Latitude',
      'Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State',
      'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status','id')

names(Simon.2018.matched.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
      'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
      'Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region',
      'Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status','id')

Simon.2018.matched.id <- Simon.2018.matched.records$id

Simon.2018.matched.records <- Simon.2018.matched.records %>% select(-c(id))

# Unmatched records

Simon.2018.unmatched.records <- Simon.2018[!Simon.2018$id %in% Simon.2018.matched.id, ]

# Apply key to match names from Simon 2018 with those from curated summary 
# Note: 39 records of domesticated plants are not integrated into the output catalog

key.Simon <- read.csv("keys/Simon_2018_unmatched_taxon_key.csv")

Simon.2018.unmatched.records$Taxon <- key.Simon$Matched.Taxon[match(unlist(Simon.2018.unmatched.records$Taxon), key.Simon$Taxon)]

Simon.2018.unmatched.records <- Simon.2018.unmatched.records %>% drop_na(Taxon)

Simon.2018.unmatched.records <- Simon.2018.unmatched.records %>% select(-c(id))

# Merge with summary to standardize names and taxon metadata

Simon.2018.unmatched.records <- inner_join(summary,Simon.2018.unmatched.records, by = c('Taxon'))

Simon.2018.unmatched.records$Source <- "Phylogenetic restriction of plant invasion in drought stressed environments"
Simon.2018.unmatched.records$CatalogueN <- NA
Simon.2018.unmatched.records$Collector <- "Andrew Simon"
Simon.2018.unmatched.records$Geo_Ref <- Simon.2018.unmatched.records$Note
Simon.2018.unmatched.records$Prov_State <- "British Columbia"
Simon.2018.unmatched.records$Region <- "Gulf Islands"
Simon.2018.unmatched.records$Location <- paste("Galiano Island; ",Simon.2018.unmatched.records$Site)
Simon.2018.unmatched.records$LocationDescription <- NA
Simon.2018.unmatched.records$HabitatRemarks <- Simon.2018.unmatched.records$Description
Simon.2018.unmatched.records$PositionalAccuracy <- NA
Simon.2018.unmatched.records$GeoPrivacy <- NA
Simon.2018.unmatched.records$PrivateLatitude <- NA
Simon.2018.unmatched.records$PrivateLongitude <- NA

Simon.2018.unmatched.records <- Simon.2018.unmatched.records %>% select('Taxon','ID','Kingdom','Phylum','Class',
    'Order','Family','Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','Date',
    'Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude',
    'Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status',
    'National.Status')

names(Simon.2018.unmatched.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
    'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
    'Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State',
    'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Combine Simon 2018 records

Simon.2018.records <- rbind(Simon.2018.matched.records,Simon.2018.unmatched.records)


# Combine all source occurrence records

Vascular.plant.records <- rbind(Ecological.Reserve.128.records,Roemer.2004.obs,RBCM.vascular.plant.records,Simon.2018.records,Lomer.2022.records)

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

# Summary of taxa that remain unmatched

unmatched.vascular.plant.records
