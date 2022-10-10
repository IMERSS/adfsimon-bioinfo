# Analyse distribution of native, exotic, and rare plant species
# 2022-10-01
# Andrew Simon

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNaturalist observations

obs <- read.csv("iNaturalist_Tracheophyta_observations_2022-10-09.csv")

# Drop cultivated taxa from observations

obs <- subset(obs, captive_cultivated != "true")

# Read summary

summary <- read.csv("Tracheophyta_review_summary_reviewed.csv")

# Standardize obs fields to facilitate join with summary

obs$taxon_species_name <- word(obs$taxon_species_name, 2)
obs$taxon_subspecies_name <- word(obs$taxon_subspecies_name, 3)
obs$taxon_variety_name <- word(obs$taxon_variety_name, 3)

obs <- rename(obs, Taxon = scientific_name)
obs <- rename(obs, Genus = taxon_genus_name)
obs <- rename(obs, Species = taxon_species_name)
obs <- rename(obs, Hybrid = taxon_hybrid_name)
obs <- rename(obs, Subspecies = taxon_subspecies_name)
obs <- rename(obs, Variety = taxon_variety_name)

# Merge catalog with summary based on genus, species, subspecies, hybrid, variety fields

matched.catalog.summary.genus.subspecies <- inner_join(summary, obs, by = c('Genus','Subspecies'))
matched.catalog.summary.genus.subspecies <- matched.catalog.summary.genus.subspecies[!(matched.catalog.summary.genus.subspecies$Subspecies==""), ]
matched.catalog.summary.genus.subspecies <- matched.catalog.summary.genus.subspecies %>% dplyr::select(Taxon.x,Genus,Species.x,Hybrid.x,Subspecies,Variety.x,Origin,Provincial.Status,National.Status,latitude,longitude,geoprivacy,private_latitude,private_longitude,id)
names(matched.catalog.summary.genus.subspecies) <- c('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')

matched.catalog.summary.genus.variety <- inner_join(summary, obs, by = c('Genus','Variety'))
matched.catalog.summary.genus.variety <- matched.catalog.summary.genus.variety[!(matched.catalog.summary.genus.variety$Variety==""), ]
matched.catalog.summary.genus.variety <- matched.catalog.summary.genus.variety %>% dplyr::select(Taxon.x,Genus,Species.x,Hybrid.x,Subspecies.x,Variety,Origin,Provincial.Status,National.Status,latitude,longitude,geoprivacy,private_latitude,private_longitude,id)
names(matched.catalog.summary.genus.variety) <- c('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')

matched.catalog.summary.hybrid <- inner_join(summary, obs, by = c('Genus','Hybrid'))
matched.catalog.summary.hybrid <- matched.catalog.summary.hybrid[!(matched.catalog.summary.hybrid$Hybrid==""), ]
matched.catalog.summary.hybrid <- matched.catalog.summary.hybrid %>% dplyr::select(Taxon.x,Genus,Species.x,Hybrid,Subspecies.x,Variety.x,Origin,Provincial.Status,National.Status,latitude,longitude,geoprivacy,private_latitude,private_longitude,id)
names(matched.catalog.summary.hybrid) <- c('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')

matched.catalog.summary.subspecies.variety.hybrid <- rbind(matched.catalog.summary.genus.subspecies,matched.catalog.summary.genus.variety, matched.catalog.summary.hybrid)

# Merge catalog of all species (not ssp, var, or hybrid) with summary (based on difference with other dataframes)

matched.catalog.summary.genus.species <- inner_join(summary, obs, by = c('Taxon'))
matched.catalog.summary.genus.species <- matched.catalog.summary.genus.species %>% dplyr::select(Taxon,Genus.x,Species.x,Hybrid.x,Subspecies.x,Variety.x,Origin,Provincial.Status,National.Status,latitude,longitude,geoprivacy,private_latitude,private_longitude,id)
names(matched.catalog.summary.genus.species) <- c('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')
matched.catalog.summary.genus.species <- anti_join(matched.catalog.summary.genus.species, matched.catalog.summary.subspecies.variety.hybrid, by = "id")

nrow(obs)
nrow(matched.catalog.summary.subspecies.variety.hybrid)
nrow(matched.catalog.summary.genus.species)
nrow(matched.catalog.summary.subspecies.variety.hybrid) + nrow(matched.catalog.summary.genus.species)

# Combine matched rows

obs.matched <- rbind(matched.catalog.summary.subspecies.variety.hybrid,matched.catalog.summary.genus.species)

# Determine unmatched rows

obs.unmatched <- obs %>% dplyr::select(Taxon,Genus,Species,Hybrid,Subspecies,Variety,latitude,longitude,geoprivacy,private_latitude,private_longitude,id)
other.summary.column.names <- c('Origin','Provincial.Status','National.Status')
other.summary.columns <- data.frame(matrix(ncol=length(other.summary.column.names),nrow=nrow(obs.unmatched)))
names(matched.catalog.summary.genus.subspecies) <- c('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')

obs.unmatched <- anti_join(obs.unmatched, obs.matched, by = "id")

nrow(obs)
nrow(obs.matched)
nrow(obs.unmatched)
nrow(obs.unmatched)+nrow(obs.matched)

# Write unmatched obs for review / key generation to facilitate join with summary

obs.unmatched.unique <- obs.unmatched %>% distinct(Taxon)

write.csv(obs.unmatched.unique, "obs_unmatched.unique.csv")

# Read unmatched taxon key

unmatched.taxon.key <- read.csv("unmatched_taxon_key.csv")

# Substitute names in ummatched dataframe with names from unmatched taxon key that correspond with summary

obs.unmatched$Taxon <- unmatched.taxon.key$Matched.Taxon[match(unlist(obs.unmatched$Taxon), unmatched.taxon.key$Taxon)]

obs.unmatched <- obs.unmatched %>% drop_na(Taxon)

obs.unmatched$Genus <- summary$Genus[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Species <- summary$Species[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Hybrid <- summary$Hybrid[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Subspecies <- summary$Subspecies[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Variety <- summary$Variety[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Origin <- summary$Origin[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$Provincial.Status <- summary$Provincial.Status[match(unlist(obs.unmatched$Taxon), summary$Taxon)]
obs.unmatched$National.Status <- summary$National.Status[match(unlist(obs.unmatched$Taxon), summary$Taxon)]

obs.unmatched <- obs.unmatched %>% select('Taxon', 'Genus', 'Species','Hybrid','Subspecies','Variety','Origin','Provincial.Status','National.Status','latitude','longitude','geoprivacy','private_latitude','private_longitude','id')

# Aggregate observations

obs.summary <- rbind(obs.matched, obs.unmatched)
obs.summary.exotic <- obs.summary %>% filter(Origin == 'exotic')
obs.summary.native <- obs.summary %>% filter(Origin == 'native')
obs.summary.rare <- obs.summary %>% filter(Provincial.Status == 'S3 (2018)' | Provincial.Status == 'S3 (2019)'| Provincial.Status == 'S2 (2019)'| Provincial.Status == 'S1S2 (2019)'| Provincial.Status == 'S2 (2021)')

# Summary species by observation frequency

exotic.obs.count <- obs.summary.exotic %>% count(Taxon)
native.obs.count <- obs.summary.native %>% count(Taxon)
rare.obs.count <- obs.summary.rare %>% count(Taxon)

# Write catalogs of exotic, native, and rare species 

write.csv(obs.summary.exotic, "obs_exotic.csv")
write.csv(obs.summary.native, "obs_native.csv")
write.csv(obs.summary.rare, "obs_rare.csv")
