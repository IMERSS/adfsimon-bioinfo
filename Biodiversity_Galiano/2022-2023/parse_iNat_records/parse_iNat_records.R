# Script to parse iNat records taxonomically

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records

iNat.obs <- read.csv("iNat_records/BioGaliano_iNat_data_2023-10-18.csv")

# Eliminate records without dates

iNat.obs <- iNat.obs[!(is.na(iNat.obs$Date.observed) | iNat.obs$Date.observed == ""), ]

# Eliminate captive/cultivated records

iNat.obs <- iNat.obs %>% filter(Captive.Cultivated == 'false')

# Eliminate extraneous time stamp from 'date observed' string

iNat.obs$Date.observed <- substr(iNat.obs$Date.observed,1,10)

# Parse taxa

# Ascomycota (lichenized)

Ascomycota.obs <- iNat.obs %>% filter(Phylum == 'Ascomycota')

# Filter to RG # Currently missing quality.grade field

Ascomycota.obs.RG <- Ascomycota.obs %>% filter(Quality.grade == 'research')

# Otherwise filter obs from trusted observers # This dataset is manageable without filtering by trusted observers

# Ascomycota.obs.non.RG <- anti_join(Ascomycota.obs, Ascomycota.obs.RG)

# unique(Ascomycota.obs.non.RG$Recorded.by)

# Ascomycota.obs.trusted <- Ascomycota.obs.non.RG %>% filter(Recorded.by == 'Randal' | Recorded.by == 'Andrew Simon')

# Combine RG obs w non-RG obs from trusted observers

Ascomycota.obs <- rbind(Ascomycota.obs.RG, Ascomycota.obs.trusted)

Lichenized.Ascomycota.obs <- Ascomycota.obs %>% filter(Class == 'Arthoniomycetes' | Class == 'Dothideomycetes' | Class == 'Eurotiomycetes' | Class == 'Lecanoromycetes' | Class == 'Lichinomycetes')

Lichenized.Ascomycota.obs<- subset(Lichenized.Ascomycota.obs, Order != 'Geoglossales' & Order != 'Venturiales' & Order != 'Sareales' & Family != 'Stictidaceae' & Order != 'Eurotiales' & Order != 'Chaetothyriales' & Order != 'Capnodiales' & Order != 'Botryosphaeriales')

# Ascomycota (unlichenized)

Unlichenized.Ascomycota.obs <- anti_join(Ascomycota.obs,Lichenized.Ascomycota.obs)


# Terrestrial Arthropods

Terrestrial.arthropods.1.obs <- iNat.obs %>% filter(Class == 'Insecta' | Class == 'Arachnida' | Class == 'Entognatha' | Class == 'Diplopoda' | Class == 'Chilopoda' | Class == 'Ostracoda')
Terrestrial.arthropods.2.obs <- iNat.obs %>% filter(Order == 'Isopoda')
Terrestrial.arthropods.2.obs<- subset(Terrestrial.arthropods.2.obs, Taxon.name != 'Bopyroides hippolytes' & Taxon.name != 'Gnorimosphaeroma oregonense' & Taxon.name != 'Pentidotea resecata' & Taxon.name != 'Pentidotea wosnesenskii')

Terrestrial.arthropods.obs <- rbind(Terrestrial.arthropods.1.obs, Terrestrial.arthropods.2.obs)

# Terrestrial Mammals

Terrestrial.mammals.obs <- iNat.obs %>% filter(Class == 'Mammalia')

Terrestrial.mammals.obs <- subset(Terrestrial.mammals.obs, Infraorder != 'Cetacea' & Taxon.name != 'Neogale vison' & Superfamily != 'Phocoidea' & Subfamily != 'Lutrinae')

# Tracheophyta

Tracheophyta.obs <- iNat.obs %>% filter(Phylum == 'Tracheophyta')

# Export taxonomically parsed catalogs

write.csv(Unlichenized.Ascomycota.obs, "outputs/iNat_obs_unlichenized_Ascomycota.csv")
write.csv(Lichenized.Ascomycota.obs, "outputs/iNat_obs_lichenized_Ascomycota.csv")
write.csv(Terrestrial.arthropods.obs, "outputs/iNat_obs_terrestrial_arthropods.csv")
write.csv(Terrestrial.mammals.obs, "outputs/iNat_obs_terrestrial_mammals.csv")
write.csv(Tracheophyta.obs, "outputs/iNat_obs_Tracheophyta.csv")

