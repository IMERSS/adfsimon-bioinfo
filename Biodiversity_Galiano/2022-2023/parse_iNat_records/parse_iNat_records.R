# Script to parse iNat records taxonomically

## NOTE: Request that future obs inc. common names and also the 'casual' field to filter out casual obs

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

iNat.obs <- read.csv("iNat_records/BioGaliano_iNat_data_2023-11-01.csv")

# Eliminate records without dates

iNat.obs <- iNat.obs[!(is.na(iNat.obs$Date.observed) | iNat.obs$Date.observed == ""), ]

# Eliminate captive/cultivated records

iNat.obs <- iNat.obs %>% filter(Captive.Cultivated == 'false')

# Eliminate extraneous time stamp from 'date observed' string

iNat.obs$Date.observed <- substr(iNat.obs$Date.observed,1,10)

# Parse taxa

## ALGAE

# Freshwater and terrestrial algae and protozoa

Desmids.etc.1.obs <- iNat.obs %>% filter(Phylum == 'Charophyta')
unique(Desmids.etc.1.obs$Taxon.name)
Desmids.etc.2.obs <- iNat.obs %>% filter(Phylum == 'Chlorophyta')
unique(Desmids.etc.2.obs$Taxon.name)
Desmids.etc.2.obs <- Desmids.etc.2.obs %>% filter(Order == 'Trentepohliales' | Class == 'Chlorophyceae')
Desmids.etc.3.obs <- iNat.obs %>% filter(Genus == 'Arcella')

Desmids.etc.obs <- rbind(Desmids.etc.1.obs, Desmids.etc.2.obs, Desmids.etc.3.obs)

# Marine algae and protozoa

Marine.algae.and.protozoa.1.obs <- iNat.obs %>% filter(Kingdom == 'Chromista' | Phylum == 'Rhodophyta')
Marine.algae.and.protozoa.2.obs <- iNat.obs %>% filter(Phylum == 'Chlorophyta')
Marine.algae.and.protozoa.2.obs <- subset(Marine.algae.and.protozoa.2.obs, Order != 'Trentepohliales' & Class != 'Chlorophyceae')

Marine.algae.and.protozoa.obs <- rbind(Marine.algae.and.protozoa.1.obs, Marine.algae.and.protozoa.2.obs)


## BACTERIA

Bacteria.obs <- iNat.obs %>% filter(Kingdom == 'Bacteria')


## FUNGI, LICHENS, MYXOGASTRIA

# Lichens

Ascomycota.obs <- iNat.obs %>% filter(Phylum == 'Ascomycota')

Lichenized.Ascomycota.obs <- Ascomycota.obs %>% filter(Class == 'Arthoniomycetes' | Class == 'Dothideomycetes' | Class == 'Eurotiomycetes' | Class == 'Lecanoromycetes' | Class == 'Lichinomycetes')

Lichenized.Ascomycota.obs<- subset(Lichenized.Ascomycota.obs, Order != 'Abrothallales' & Genus != 'Arthophacopsis' & Order != 'Pleosporales' & Order != 'Geoglossales' & Order != 'Venturiales' & Order != 'Sareales' & Family != 'Stictidaceae' & Order != 'Eurotiales' & Order != 'Chaetothyriales' & Order != 'Capnodiales' & Order != 'Botryosphaeriales')

Basidiomycota.obs <- iNat.obs %>% filter(Phylum == 'Basidiomycota')

Lichenized.Basidiomycota.obs <- Basidiomycota.obs %>% filter(Genus == 'Lichenomphalia' | Genus == 'Multiclavula')

Lichens.obs <- rbind(Lichenized.Ascomycota.obs, Lichenized.Basidiomycota.obs)

# Fungi (unlichenized Ascomycota and Basidiomycota, and Mucoromycota

Unlichenized.Ascomycota.obs <- anti_join(Ascomycota.obs,Lichenized.Ascomycota.obs)

Unlichenized.Basidiomycota.obs <- anti_join(Basidiomycota.obs,Lichenized.Basidiomycota.obs)

Mucoromycota.obs <- iNat.obs %>% filter(Phylum == 'Mucoromycota')

Fungi.obs <- rbind(Unlichenized.Ascomycota.obs,Unlichenized.Basidiomycota.obs,Mucoromycota.obs)

# Myxogastria

Myxogastria.obs <- iNat.obs %>% filter(Kingdom == 'Protozoa')
Myxogastria.obs <- subset(Myxogastria.obs, Phylum != 'Amoebozoa' & Phylum != 'Retaria' & Phylum != 'Sarcomastigophora')

## PLANTS

# Bryophyta, Marchantiophyta, Anthocerotophyta

Bryophyta.Marchantiophyta.Anthocerotophyta.obs <- iNat.obs %>% filter(Phylum == 'Bryophyta' | Phylum == 'Marchantiophyta' | Phylum == 'Anthocerotophyta')

# Tracheophyta

Tracheophyta.obs <- iNat.obs %>% filter(Phylum == 'Tracheophyta')

unique(Tracheophyta.obs$Phylum)


## TERRESTRIAL ANIMALS

# Birds

Aves.obs <- iNat.obs %>% filter(Class == 'Aves')

# Freshwater bryozoans

Freshwater.bryozoans.obs <- iNat.obs %>% filter(Taxon.name == 'Pectinatella magnifica')

# Herptiles

Herptiles.obs <- iNat.obs %>% filter(Class == 'Amphibia' | Class == 'Reptilia')

# Terrestrial Arthropods

Terrestrial.arthropods.1.obs <- iNat.obs %>% filter(Class == 'Insecta' | Class == 'Arachnida' | Class == 'Entognatha' | Class == 'Diplopoda' | Class == 'Chilopoda' | Class == 'Ostracoda')
Terrestrial.arthropods.2.obs <- iNat.obs %>% filter(Order == 'Isopoda')
Terrestrial.arthropods.2.obs <- subset(Terrestrial.arthropods.2.obs, Taxon.name != 'Bopyroides hippolytes' & Taxon.name != 'Gnorimosphaeroma oregonense' & Taxon.name != 'Pentidotea resecata' & Taxon.name != 'Pentidotea wosnesenskii')

Terrestrial.arthropods.obs <- rbind(Terrestrial.arthropods.1.obs, Terrestrial.arthropods.2.obs)

# Terrestrial Mammals

Terrestrial.mammals.obs <- iNat.obs %>% filter(Class == 'Mammalia')

Terrestrial.mammals.obs <- subset(Terrestrial.mammals.obs, Infraorder != 'Cetacea' & Taxon.name != 'Neogale vison' & Superfamily != 'Phocoidea' & Subfamily != 'Lutrinae')

# Terrestrial Molluscs

Molluscs <- iNat.obs %>% filter(Phylum == 'Mollusca')

Terrestrial.molluscs.1.obs <- Molluscs %>% filter(Superorder == 'Eupulmonata')

Terrestrial.molluscs.1.obs <- subset(Terrestrial.molluscs.1.obs, Order != 'Ellobiida')

Terrestrial.molluscs.2.obs <- Molluscs %>% filter(Order == 'Sphaeriida')

Terrestrial.molluscs.obs <- rbind(Terrestrial.molluscs.1.obs, Terrestrial.molluscs.2.obs)

# Terrestrial Annelids, Rotifers, etc.

Terrestrial.annelids.etc <- iNat.obs %>% filter(Genus == 'Amynthas' | Taxon.name == 'Octolasion cyaneum' | Genus == 'Habrotrocha' | Taxon.name == 'Lumbricus terrestris')

# Marine Animals

Animals <- iNat.obs %>% filter(Kingdom == 'Animalia')

Terrestrial.animals <- rbind(Aves.obs, Freshwater.bryozoans.obs, Herptiles.obs, Terrestrial.annelids.etc, Terrestrial.arthropods.obs, Terrestrial.mammals.obs, Terrestrial.molluscs.obs)

Marine.animals.obs <- anti_join(Animals, Terrestrial.animals)

# Check that all taxa are accounted for

taxa <- rbind(Aves.obs,Bacteria.obs,Bryophyta.Marchantiophyta.Anthocerotophyta.obs,Desmids.etc.obs,
              Freshwater.bryozoans.obs,Fungi.obs,Herptiles.obs,Marine.algae.and.protozoa.obs,Lichens.obs,
              Marine.animals.obs,Myxogastria.obs,Terrestrial.arthropods.obs,Terrestrial.mammals.obs,
              Terrestrial.molluscs.obs,Tracheophyta.obs)

missing.taxa <- anti_join(iNat.obs,taxa)

unique(missing.taxa$Taxon.name)

# Export catalogs

write.csv(Aves.obs, "outputs/iNat_obs_birds.csv", row.names = FALSE)

write.csv(Bacteria.obs, "outputs/iNat_obs_bacteria.csv", row.names = FALSE)

write.csv(Bryophyta.Marchantiophyta.Anthocerotophyta.obs, "outputs/iNat_obs_mosses_liverworts_and_hornworts.csv", row.names = FALSE)

write.csv(Desmids.etc.obs, "outputs/iNat_obs_freshwater_and_terrestrial_algae.csv", row.names = FALSE)

write.csv(Freshwater.bryozoans.obs, "outputs/iNat_obs_freshwater_bryozoans.csv", row.names = FALSE)

write.csv(Fungi.obs, "outputs/iNat_obs_fungi.csv", row.names = FALSE)

write.csv(Herptiles.obs, "outputs/iNat_obs_herptiles.csv", row.names = FALSE)

write.csv(Lichens.obs, "outputs/iNat_obs_lichens.csv", row.names = FALSE)

write.csv(Marine.algae.and.protozoa.obs, "outputs/iNat_obs_marine_algae_and_protozoa.csv", row.names = FALSE)

write.csv(Marine.animals.obs, "outputs/iNat_obs_marine_animals.csv", row.names = FALSE)

write.csv(Myxogastria.obs, "outputs/iNat_obs_Myxogastria.csv", row.names = FALSE)

write.csv(Terrestrial.annelids.etc, "outputs/iNat_obs_Terrestrial_annelids_etc.csv", row.names = FALSE)

write.csv(Terrestrial.arthropods.obs, "outputs/iNat_obs_terrestrial_arthropods.csv", row.names = FALSE)

write.csv(Terrestrial.mammals.obs, "outputs/iNat_obs_terrestrial_mammals.csv", row.names = FALSE)

write.csv(Terrestrial.molluscs.obs, "outputs/iNat_obs_terrestrial_molluscs.csv", row.names = FALSE)

write.csv(Tracheophyta.obs, "outputs/iNat_obs_Tracheophyta.csv", row.names = FALSE)
