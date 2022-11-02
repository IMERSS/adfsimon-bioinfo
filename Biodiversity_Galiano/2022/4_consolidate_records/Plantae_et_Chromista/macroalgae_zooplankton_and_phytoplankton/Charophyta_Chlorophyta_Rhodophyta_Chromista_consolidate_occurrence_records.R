# Script to consolidate records of Galiano Island's vascular plants

# Note the best scripts developed so far are under the Webber et al. 2022 section

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("../../../2_review/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/summaries/Charophyta_Chlorophyta_Rhodophyta_Chromista_review_summary_reviewed_2022-11-02.csv")

# Consolidate records

# Sources:

# ! BOLD records 2021 - ! not yet added! # Need to figure out how to index BOLD records
# CPNWH records 2022 - added.
# ! iNaturalist observations 2022 - ! not yet added!
# ! PMLS Records 2021 - ! not yet added!
# ! Webber et al. 2022 Epiphytic diatoms on Zostera 2022 - ! not yet added!

# Read BOLD records 2021 #
# Return to this once you know how to best index BOLD records

# BOLD.2021 <- read.csv("digitized/BOLD_marine_algae_2021-11-25.csv")

# Filter CDC obs from collections at other institutions

# BOLD.2021 <- BOLD.2021 %>% filter(InstitutionCode == 'CDC')

# Create unique identifiers for observations
# Note: this unique ID is not going to work in the long term as these data can be refreshed by the BC CDC at any point
# Could ask the BC CDC for EO IDs or SF IDs?

# unique.prefix <- "BCCDC2019:" 
# unique.suffix <- 1:nrow(BOLD.2021)

# Standardize columns

# BOLD.2021$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
# BOLD.2021$Geo_Ref <- NA
# BOLD.2021$HabitatRemarks <- NA
# BOLD.2021$PositionalAccuracy <- 80
# BOLD.2021$GeoPrivacy <- NA
# BOLD.2021$PrivateLatitude <- NA
# BOLD.2021$PrivateLongitude <- NA
# BOLD.2021$PrivateLatitude <- NA
# BOLD.2021$PrivateLongitude <- NA
# BOLD.2021$Prov_State <- "British Columbia"
# BOLD.2021$Region <- "Gulf Islands"
# BOLD.2021$Location <- "Galiano Island"

# Select key columns

# BOLD.2021 <- BOLD.2021 %>% select(ScientificName,InstitutionCode,CatalogueN,Collector,EventDate,Latitude,Longitude,Geo_Ref,
#       PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,
#       Locality,OccurrenceRemarks)

# Standardize column names to facilitate join

# names(BOLD.2021) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
#     'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe',
#       'HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

# BOLD.2021.names.matched <- left_join(BOLD.2021,summary, by = c('Taxon'))

# Unmatched records

# BOLD.2021.names.unmatched <- BOLD.2021.names.matched[is.na(BOLD.2021.names.matched$Taxon.Author),]

# Matched records

# BOLD.2021.names.matched <- anti_join(BOLD.2021.names.matched,BOLD.2021.names.unmatched)

# Standardize matched occcurrence records

# BOLD.2021.names.matched <- BOLD.2021.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,
#       Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
#       PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
#       National.Status)

# names(BOLD.2021.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
#       'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
#       'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks',
#       'Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

# nrow(BOLD.2021)
# nrow(BOLD.2021.names.matched)
# nrow(BOLD.2021.names.unmatched)
# nrow(BOLD.2021.names.matched)+nrow(BOLD.2021.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

# BOLD.2021.key <- read.csv("keys/vascular_plant_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(BOLD.2021.names.unmatched.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- BOLD.2021.names.unmatched.unmatched$Taxon

# unmatched.taxa$Genus <- word(BOLD.2021.names.unmatched.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(BOLD.2021.names.unmatched.unmatched$Taxon, 2)

# review.key <- rbind(BOLD.2021.key,unmatched.taxa)

# write.csv(review.key,"review.key.csv")

# Swap unmatched names using key

# BOLD.2021.names.unmatched.matched <- BOLD.2021.names.unmatched

# BOLD.2021.names.unmatched.matched$Taxon <- BOLD.2021.key$Matched.Taxon[match(unlist(BOLD.2021.names.unmatched.matched$Taxon), BOLD.2021.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

# BOLD.2021.names.unmatched.matched <- select(BOLD.2021.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

# BOLD.2021.names.unmatched.matched <- left_join(BOLD.2021.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

# BOLD.2021.names.unmatched.matched <- BOLD.2021.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

# BOLD.2021.names.unmatched.matched <- BOLD.2021.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,
#       Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
#       GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
#       National.Status)

# names(BOLD.2021.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
#       'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
#       'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
#       'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

# BOLD.2021.names.unmatched.unmatched <- anti_join(BOLD.2021.names.unmatched,BOLD.2021.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

# nrow(BOLD.2021)
# nrow(BOLD.2021.names.matched)
# nrow(BOLD.2021.names.unmatched)
# nrow(BOLD.2021.names.unmatched.matched)
# nrow(BOLD.2021.names.unmatched.unmatched)
# nrow(BOLD.2021.names.matched)+nrow(BOLD.2021.names.unmatched.matched)+nrow(BOLD.2021.names.unmatched.unmatched)

# Bind records

# BOLD.2021.records <- rbind(BOLD.2021.names.matched,BOLD.2021.names.unmatched.matched)

# Compare records in and out

# nrow(BOLD.2021)
# nrow(BOLD.2021.records) # Good: only five records discarded, accounted for above.

# Note: taxa unrecognized in summary, and hence excluded from catalog:
# Juncus effusus - infrataxonomic resolution required to meaningfully discriminate this taxon
# Epilobium cf ciliatum - could be E. ciliatum; could also be Epilobium leptophyllum, which has since documented at the bog
# Glyceria sp.

# Start record of unmatched names

# unmatched.vascular.plant.records <- BOLD.2021.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,
#       Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,
#       Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

# unmatched.vascular.plant.records



# Read Consortium of Pacific Northwest Herbaria records

CPNWH.2021 <- read.csv("digitized/CPNWH_Galiano_Island_macroalgae_2022-10-30.csv")

names(CPNWH.2021)

# Standardize columns

CPNWH.2021$Geo_Ref <- NA
CPNWH.2021$PositionalAccuracy <- NA
CPNWH.2021$GeoPrivacy <- NA
CPNWH.2021$PrivateLatitude <- NA
CPNWH.2021$PrivateLongitude <- NA
CPNWH.2021$PrivateLatitude <- NA
CPNWH.2021$PrivateLongitude <- NA
CPNWH.2021$Region <- "Gulf Islands"
CPNWH.2021$LocationDe <- NA

# Select key columns

CPNWH.2021 <- CPNWH.2021 %>% select(Scientific.Name,Herbarium,Accession,Collector,Date,Decimal.Latitude,Decimal.Longitude,Geo_Ref,
                                    PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,State.or.Province,Region,Locality,LocationDe,Site.Description)

# Standardize column names to facilitate join

names(CPNWH.2021) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
                       'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

CPNWH.2021.names.matched <- left_join(CPNWH.2021,summary, by = c('Taxon'))

# Unmatched records

CPNWH.2021.names.unmatched <- CPNWH.2021.names.matched[is.na(CPNWH.2021.names.matched$Taxon.Author),]

# Matched records

CPNWH.2021.names.matched <- anti_join(CPNWH.2021.names.matched,CPNWH.2021.names.unmatched)

# Standardize matched occcurence records

CPNWH.2021.names.matched <- CPNWH.2021.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,
        Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,
        PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(CPNWH.2021.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
        'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy',
        'GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks',
        'Origin','Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(CPNWH.2021)
nrow(CPNWH.2021.names.matched)
nrow(CPNWH.2021.names.unmatched)
nrow(CPNWH.2021.names.matched)+nrow(CPNWH.2021.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

CPNWH.2021.key <- read.csv("keys/algae_taxon_key_2022.csv") 

# Note: key updated based on this data set; code for generating key blotted out below

# Generate key to manually match unmatched names in CPNWH 2021
# Note: code no longer required once key was generated

# key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

# unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(CPNWH.2021.names.unmatched)))
# names(unmatched.taxa) <- key.field.names

# unmatched.taxa$Taxon <- CPNWH.2021.names.unmatched$Taxon

# unmatched.taxa$Genus <- word(CPNWH.2021.names.unmatched$Taxon, 1)

# unmatched.taxa$Species <- word(CPNWH.2021.names.unmatched$Taxon, 2)

# CPNWH.2021.key <- rbind(CPNWH.2021.key,unmatched.taxa)

# write.csv(CPNWH.2021.key,"CPNWH_2021_key.csv")

# Swap unmatched names using key

CPNWH.2021.names.unmatched.matched <- CPNWH.2021.names.unmatched

CPNWH.2021.names.unmatched.matched$Taxon <- CPNWH.2021.key$Matched.Taxon[match(unlist(CPNWH.2021.names.unmatched.matched$Taxon), CPNWH.2021.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

CPNWH.2021.names.unmatched.matched <- select(CPNWH.2021.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

CPNWH.2021.names.unmatched.matched <- left_join(CPNWH.2021.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

CPNWH.2021.names.unmatched.matched <- CPNWH.2021.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

CPNWH.2021.names.unmatched.matched <- CPNWH.2021.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,
        Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,
        GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,
        National.Status)

names(CPNWH.2021.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species',
        'Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
        'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
        'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

CPNWH.2021.names.unmatched.unmatched <- anti_join(CPNWH.2021.names.unmatched,CPNWH.2021.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(CPNWH.2021)
nrow(CPNWH.2021.names.matched)
nrow(CPNWH.2021.names.unmatched)
nrow(CPNWH.2021.names.unmatched.matched)
nrow(CPNWH.2021.names.unmatched.unmatched)
nrow(CPNWH.2021.names.matched)+nrow(CPNWH.2021.names.unmatched.matched)+nrow(CPNWH.2021.names.unmatched.unmatched)

# Bind records

CPNWH.2021.records <- rbind(CPNWH.2021.names.matched,CPNWH.2021.names.unmatched.matched)

# Compare records in and out

nrow(CPNWH.2021)
nrow(CPNWH.2021.records) # Good: only three records discarded: all taxa unidentified beyond genus

# Note: taxa unrecognized in summary, and hence excluded from catalog, are all species identified only to genus.

# Start record of unmatched names

unmatched.algae.records <- CPNWH.2021.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,Latitude,
        Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
        HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.algae.records



# Read Webber et al. 2022 records # TEMPORARY DATASET

Webber.et.al.2022 <- read.csv("digitized/Webber_et_al_2022_Taxonomy_table_epiphytic_diatoms_Zostera_2022-11-01.csv")

# Filter diatoms and algae (this step may be necessary for a future version of this list but not the current version which is already pruned)

# Webber.et.al.2022 <- DL63 %>% filter(Group == 'vascular')

# Create unique identifiers for observations

unique.prefix <- "WEBBERETAL2022:"
unique.suffix <- 1:nrow(Webber.et.al.2022)

# Standardize columns

Webber.et.al.2022$Source <- "Webber et al. 2022"
Webber.et.al.2022$Observer <- "Mark Webber, Siobhan Schenck & Arjan van Asselt"
Webber.et.al.2022$Date <- '2020-11-15' # temporary date, update
Webber.et.al.2022$CatalogueN <- paste(unique.prefix,unique.suffix, sep = "")
Webber.et.al.2022$Latitude <- 48.90071443043615
Webber.et.al.2022$Longitude <- -123.40790606380006
Webber.et.al.2022$Geo_Ref <- "Coordinates mapped based on precise location of study area"
Webber.et.al.2022$HabitatRemarks <- NA
Webber.et.al.2022$PositionalAccuracy <- 50
Webber.et.al.2022$GeoPrivacy <- NA
Webber.et.al.2022$PrivateLatitude <- NA
Webber.et.al.2022$PrivateLongitude <- NA
Webber.et.al.2022$PrivateLatitude <- NA
Webber.et.al.2022$PrivateLongitude <- NA
Webber.et.al.2022$Prov_State <- "British Columbia"
Webber.et.al.2022$Region <- "Gulf Islands"
Webber.et.al.2022$Location <- "Galiano Island"
Webber.et.al.2022$LocationDe <- "Montague Harbour Marine Provincial Park (Hulq'umi'num: 'Sumnuw')"

# Select names resolved as raw outputs of Lumina sequencing

Webber.et.al.2022.Lumina <- Webber.et.al.2022 %>% drop_na(Taxon_resolved_Lumina)
  
Webber.et.al.2022.Lumina$Taxon <- Webber.et.al.2022.Lumina$Taxon_resolved_Lumina

# Select names resolved with reference to BOLD

Webber.et.al.2022.BOLD <- Webber.et.al.2022 %>% drop_na(Taxon_BLAST)

Webber.et.al.2022.BOLD$Taxon <- Webber.et.al.2022.BOLD$Taxon_BLAST

# Combine taxa as resolved by Lumina and BOLD and reduce to one common Taxon ID field

Webber.et.al.2022 <- rbind(Webber.et.al.2022.Lumina,Webber.et.al.2022.BOLD)

Webber.et.al.2022 <- Webber.et.al.2022[ , -which(names(Webber.et.al.2022) %in% c("Taxon_resolved_Lumina","Taxon_BLAST"))]

# Remove '_'s from taxon names (replace with ' 's)

Webber.et.al.2022$Taxon <- gsub("_", " ", Webber.et.al.2022$Taxon)

# Select key columns

Webber.et.al.2022 <- Webber.et.al.2022 %>% select(Taxon,Source,CatalogueN,Observer,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,
        PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks)

# Standardize column names to facilitate join

names(Webber.et.al.2022) <- c('Taxon','Source','CatalogueN','Collector','Date','Latitude','Longitude','Geo_Ref',
                 'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location',
                 'LocationDe','HabitatRemarks')

# Merge with summary to standardize names and taxon metadata

Webber.et.al.2022.names.matched <- left_join(Webber.et.al.2022,summary, by = c('Taxon'))

# Unmatched records

Webber.et.al.2022.names.unmatched <- Webber.et.al.2022.names.matched[is.na(Webber.et.al.2022.names.matched$Taxon.Author),]

# Matched records

Webber.et.al.2022.names.matched <- anti_join(Webber.et.al.2022.names.matched,Webber.et.al.2022.names.unmatched)

# Standardize matched occcurence records

Webber.et.al.2022.names.matched <- Webber.et.al.2022.names.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,
                                                                              Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,
                                                                              PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Webber.et.al.2022.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid','Subspecies',
                                            'Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref','PositionalAccuracy','GeoPrivacy',
                                            'PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin',
                                            'Provincial.Status','National.Status')

# Confirm all records are represented 

nrow(Webber.et.al.2022)
nrow(Webber.et.al.2022.names.matched)
nrow(Webber.et.al.2022.names.unmatched)
nrow(Webber.et.al.2022.names.matched)+nrow(Webber.et.al.2022.names.unmatched)

# Generate key to reconcile mismatches based on previous keys modified with the inclusion of new reports to summary
# Note: some of the code below is not needed after reviewing and generating new key

Webber.et.al.2022.key <- read.csv("keys/algae_taxon_key_2022.csv") 

# Swap unmatched names using key

Webber.et.al.2022.names.unmatched.matched <- Webber.et.al.2022.names.unmatched

Webber.et.al.2022.names.unmatched.matched$Taxon <- Webber.et.al.2022.key$Matched.Taxon[match(unlist(Webber.et.al.2022.names.unmatched.matched$Taxon), Webber.et.al.2022.key$Taxon)]

# Remove unmatched fields prior to rejoining with summary

Webber.et.al.2022.names.unmatched.matched <- select(Webber.et.al.2022.names.unmatched.matched, c(1:17))

# Merge with newly matched records with  summary to standardize names and taxon metadata

Webber.et.al.2022.names.unmatched.matched <- left_join(Webber.et.al.2022.names.unmatched.matched,summary, by = c('Taxon'))

# Drop NAs (taxa not recognized in summary)

Webber.et.al.2022.names.unmatched.matched <- Webber.et.al.2022.names.unmatched.matched %>% drop_na(Taxon)

# Standardize matched occurrence records

Webber.et.al.2022.names.unmatched.matched <- Webber.et.al.2022.names.unmatched.matched %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,
                                                                                                  Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Date,Latitude,Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,
                                                                                                  PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,HabitatRemarks,Origin,Provincial.Status,National.Status)

names(Webber.et.al.2022.names.unmatched.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus','Species','Hybrid',
                                                      'Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude','Geo_Ref',
                                                      'PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','Prov_State','Region','Location','LocationDescription',
                                                      'HabitatRemarks','Origin','Provincial.Status','National.Status')

# Select names unmatched based on key

Webber.et.al.2022.names.unmatched.unmatched <- anti_join(Webber.et.al.2022.names.unmatched,Webber.et.al.2022.names.unmatched.matched,by='CatalogueN')

# Confirm all records are represented 

nrow(Webber.et.al.2022)
nrow(Webber.et.al.2022.names.matched)
nrow(Webber.et.al.2022.names.unmatched)
nrow(Webber.et.al.2022.names.unmatched.matched)
nrow(Webber.et.al.2022.names.unmatched.unmatched)
nrow(Webber.et.al.2022.names.matched)+nrow(Webber.et.al.2022.names.unmatched.matched)+nrow(Webber.et.al.2022.names.unmatched.unmatched)

# Revise key to patch remaining unmatched taxa
# Note: key updated based on this data set; code for generating key blotted out below

key.field.names <- c('Taxon', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety','Form','Matched.Taxon')

unmatched.taxa <- data.frame(matrix(ncol=length(key.field.names),nrow=nrow(Webber.et.al.2022.names.unmatched.unmatched)))
names(unmatched.taxa) <- key.field.names

unmatched.taxa$Taxon <- Webber.et.al.2022.names.unmatched.unmatched$Taxon

unmatched.taxa$Genus <- word(Webber.et.al.2022.names.unmatched.unmatched$Taxon, 1)

unmatched.taxa$Species <- word(Webber.et.al.2022.names.unmatched.unmatched$Taxon, 2)

unmatched.taxa <- distinct(unmatched.taxa)

review.key <- rbind(Webber.et.al.2022.key,unmatched.taxa)

write.csv(review.key,"keys/review.key.csv")

# Bind records

DL63.records <- rbind(DL63.names.matched,DL63.names.unmatched.matched)

# Compare records in and out

nrow(DL63)
nrow(DL63.records) # Good: only five records discarded, accounted for above.

# Note: 
# Proschkina not recognized as a genus by AlgaeBase or WoRMS (various other taxa to consider with that name in the species epithet)

# Add to record of unmatched names

DL63.names.unmatched.unmatched <- DL63.names.unmatched.unmatched %>% select(Taxon,Source,CatalogueN,Collector,Date,Latitude,
        Longitude,Geo_Ref,PositionalAccuracy,GeoPrivacy,PrivateLatitude,PrivateLongitude,Prov_State,Region,Location,LocationDe,
        HabitatRemarks,Origin,Provincial.Status,National.Status)

unmatched.vascular.plant.records <- rbind(unmatched.vascular.plant.records,DL63.names.unmatched.unmatched)




# Combine all source occurrence records

Algae.records <- rbind(CPNWH.2021.records,Webber.et.al.2022.records)

# Combine with iNaturalist observations

# iNaturalist.observations <- read.csv("digitized/iNaturalist_vascular_plant_observations.csv")

# names(Vascular.plant.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
#      'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
#      'Longitude','GeoRef','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','ProvState',
#      'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')
# names(iNaturalist.observations) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family','Genus',
#      'Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude',
#      'Longitude','GeoRef','PositionalAccuracy','GeoPrivacy','PrivateLatitude','PrivateLongitude','ProvState',
#      'Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Algae.records <- rbind(Algae.records,iNaturalist.observations)

# Output synthesized catalog of occurrence records

# write.csv(Vascular.plant.records,"Galiano_Island_vascular_plant_records_consolidated.csv")

# Tally records

nrow(Algae.records)

# Summary of records that remain unmatched

nrow(unmatched.algae.records)

unique(unmatched.algae.records$Taxon)
