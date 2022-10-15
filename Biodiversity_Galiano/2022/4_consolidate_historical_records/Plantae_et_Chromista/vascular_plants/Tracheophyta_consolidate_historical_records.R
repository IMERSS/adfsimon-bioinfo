# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary for standardizing species names

summary <- read.csv("Tracheophyta_review_summary_reviewed.csv")

# Read historical records 

# Read RBCM Records

RBCM.georeferencing.corrected <- read.csv("digitized/RBCM_vascular_plant_records_georeferencing_corrected_2021-12-05.csv") # Note: georeferencing still needs to be reviewed; at least one obs incorrectly mapped on Mount Galiano

# Select key columns

RBCM.georeferencing.corrected <-  RBCM.georeferencing.corrected %>% select(Genus, Species, CatalogueN, Collector, 
    Collecti_1, Prov_State, District, LocationNa, LocationDe, Latitude, Longitude, HabitatRem, Geo_Ref)

# Merge with summary to standardize names and taxon metadata

nrow(RBCM.georeferencing.corrected) # Confirm record #

RBCM.georeferencing.corrected.names.matched <- inner_join(summary, RBCM.georeferencing.corrected, by = c('Genus','Species'))

RBCM.georeferencing.corrected.names.matched$Source <- 'RBCM'

RBCM.georeferencing.corrected.names.matched <- RBCM.georeferencing.corrected.names.matched %>% select(Taxon,ID,Kingdom,
    Phylum,Class,Order,Family,Genus,Species,Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Collecti_1,Latitude,Longitude,
    Geo_Ref,Prov_State,District,LocationNa,LocationDe,HabitatRem,Origin,Provincial.Status,National.Status)

names(RBCM.georeferencing.corrected.names.matched) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
    'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
    'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

nrow(RBCM.georeferencing.corrected) # Confirm all records are retained after merging with summary columns

RBCM.vascular.plant.records <- RBCM.georeferencing.corrected.names.matched


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

key <- read.csv("Roemer_2004_unmatched_taxon_key.csv")

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

Roemer.2004.obs <- Roemer.2004.obs %>% select(Taxon,ID,Kingdom,Phylum,Class,Order,Family,Genus,Species,
      Hybrid,Subspecies,Variety,Source,CatalogueN,Collector,Collecti_1,latitude,longitude,Geo_Ref,Prov_State,
      District,LocationNa,LocationDe,HabitatRem,Origin,Provincial.Status,National.Status)

names(Roemer.2004.obs) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
                            'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
                            'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')


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

Simon.2018.matched.records <- Simon.2018.matched.records %>% select('Taxon','ID','Kingdom','Phylum','Class','Order','Family',
                            'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','Date','Latitude','Longitude',
                            'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status','id')

names(Simon.2018.matched.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
                            'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
                            'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status','id')

Simon.2018.matched.id <- Simon.2018.matched.records$id

Simon.2018.matched.records <- Simon.2018.matched.records %>% select(-c(id))

# Unmatched records

Simon.2018.unmatched.records <- Simon.2018[!Simon.2018$id %in% Simon.2018.matched.id, ]

# Apply key to match names from Simon 2018 with those from curated summary 
# Note: 39 records of domesticated plants are not integrated into the output catalog

key.Simon <- read.csv("Simon_2018_unmatched_taxon_key.csv")

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

Simon.2018.unmatched.records <- Simon.2018.unmatched.records %>% select('Taxon','ID','Kingdom','Phylum','Class','Order','Family',
                                                                    'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','Date','Latitude','Longitude',
                                                                    'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

names(Simon.2018.unmatched.records) <- c('Taxon','TaxonID','Kingdom','Phylum','Class','Order','Family',
                                       'Genus','Species','Hybrid','Subspecies','Variety','Source','CatalogueN','Collector','CollectionDate','Latitude','Longitude',
                                       'Geo_Ref','Prov_State','Region','Location','LocationDescription','HabitatRemarks','Origin','Provincial.Status','National.Status')

# Combine Simon 2018 records

Simon.2018.records <- rbind(Simon.2018.matched.records,Simon.2018.unmatched.records)

# Combine historical occurrence records

Historical.vascular.plant.records <- rbind(Roemer.2004.obs,RBCM.vascular.plant.records,Simon.2018.records)

