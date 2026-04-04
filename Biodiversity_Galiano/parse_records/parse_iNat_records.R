# Parse iNat records taxonomically

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(stringr)
library(tidyr)

# Read iNat records
iNat.obs <- read.csv("iNat_records/Galiano_Union_Catalogue_2025_12_30-assigned.csv",
                     stringsAsFactors = FALSE)

# Harmonize field names
iNat.obs <- iNat.obs %>%
  mutate(
    Taxon.name   = scientific_name,
    Kingdom      = kingdom,
    Phylum       = phylum,
    Class        = class,
    Order        = order,
    Infraorder   = infraorder,
    Superfamily  = superfamily,
    Subfamily    = subfamily,
    Family       = family,
    Genus        = genus,
    Common.name  = common_name,
    Taxon.rank   = taxon_rank
  )

# Trim whitespace
iNat.obs <- iNat.obs %>%
  mutate(across(
    c(scientific_name, common_name, taxon_rank,
      kingdom, phylum, class, order, infraorder,
      superfamily, subfamily, family, genus,
      Taxon.name, Kingdom, Phylum, Class, Order,
      Infraorder, Superfamily, Subfamily, Family, Genus),
    ~ na_if(str_squish(.x), "")
  ))

# Remove records without dates
iNat.obs <- iNat.obs[!(is.na(iNat.obs$observed_on) | iNat.obs$observed_on == ""), ]

# Remove captive records
iNat.obs <- iNat.obs %>% filter(captive == "false")

# Clean date format
iNat.obs$observed_on <- substr(iNat.obs$observed_on, 1, 10)

# Replace coordinates with private values where available
iNat.obs.private <- iNat.obs[!is.na(iNat.obs$private_latitude), ]
iNat.obs.private$latitude <- iNat.obs.private$private_latitude
iNat.obs.private$longitude <- iNat.obs.private$private_longitude

iNat.obs.open <- iNat.obs[is.na(iNat.obs$private_latitude), ]

iNat.obs <- rbind(iNat.obs.private, iNat.obs.open)

iNat.obs$private_latitude <- NULL
iNat.obs$private_longitude <- NULL

# Fix observer names
iNat.obs.names <- iNat.obs[!is.na(iNat.obs$user_name) & iNat.obs$user_name != "", ]
iNat.obs.handles <- anti_join(iNat.obs, iNat.obs.names)

iNat.obs.handles$user_name <- iNat.obs.handles$user_login

iNat.obs <- rbind(iNat.obs.names, iNat.obs.handles)
iNat.obs$user_login <- NULL

# Algae
Desmids.etc.1.obs <- iNat.obs %>% filter(Phylum == "Charophyta")
Desmids.etc.2.obs <- iNat.obs %>% filter(Phylum == "Chlorophyta") %>%
  filter(Order == "Trentepohliales" | Class == "Chlorophyceae")
Desmids.etc.3.obs <- iNat.obs %>% filter(Genus == "Arcella")

Desmids.etc.obs <- rbind(Desmids.etc.1.obs, Desmids.etc.2.obs, Desmids.etc.3.obs)

Marine.algae.and.protozoa.1.obs <- iNat.obs %>%
  filter(Phylum %in% c("Ochrophyta", "Rhodophyta", "Miozoa", "Ciliophora",
                       "Bigyra", "Radiozoa", "Cercozoa", "Chlorophyta"))

Marine.algae.and.protozoa.2.obs <- iNat.obs %>% filter(Phylum == "Chlorophyta")

Marine.algae.and.protozoa.obs <- rbind(Marine.algae.and.protozoa.1.obs,
                                       Marine.algae.and.protozoa.2.obs)

# Bacteria
Bacteria.obs <- iNat.obs %>% filter(Kingdom == "Bacteria")

# Lichens
Ascomycota.obs <- iNat.obs %>% filter(Phylum == "Ascomycota")

Lichenized.Ascomycota.obs <- Ascomycota.obs %>%
  filter(Class %in% c("Arthoniomycetes", "Dothideomycetes", "Eurotiomycetes",
                      "Lecanoromycetes", "Lichinomycetes"))

Lichenized.Ascomycota.obs <- subset(
  Lichenized.Ascomycota.obs,
  Order != "Abrothallales" &
    Genus != "Arthophacopsis" &
    Order != "Pleosporales" &
    Order != "Geoglossales" &
    Order != "Venturiales" &
    Order != "Sareales" &
    Family != "Stictidaceae" &
    Order != "Eurotiales" &
    Order != "Chaetothyriales" &
    Order != "Capnodiales" &
    Order != "Botryosphaeriales"
)

Basidiomycota.obs <- iNat.obs %>% filter(Phylum == "Basidiomycota")

Lichenized.Basidiomycota.obs <- Basidiomycota.obs %>%
  filter(Genus %in% c("Lichenomphalia", "Multiclavula"))

Lichens.obs <- rbind(Lichenized.Ascomycota.obs, Lichenized.Basidiomycota.obs)

# Fungi
Unlichenized.Ascomycota.obs <- anti_join(Ascomycota.obs, Lichenized.Ascomycota.obs)
Unlichenized.Basidiomycota.obs <- anti_join(Basidiomycota.obs, Lichenized.Basidiomycota.obs)
Mucoromycota.obs <- iNat.obs %>% filter(Phylum == "Mucoromycota")

Fungi.obs <- rbind(Unlichenized.Ascomycota.obs,
                   Unlichenized.Basidiomycota.obs,
                   Mucoromycota.obs)

# Myxogastria
Myxogastria.obs <- iNat.obs %>% filter(Kingdom == "Protozoa")
Myxogastria.obs <- subset(Myxogastria.obs,
                          Phylum != "Amoebozoa" &
                            Phylum != "Retaria" &
                            Phylum != "Sarcomastigophora")

# Plants
Bryophyta.Marchantiophyta.Anthocerotophyta.obs <- iNat.obs %>%
  filter(Phylum %in% c("Bryophyta", "Marchantiophyta", "Anthocerotophyta"))

Tracheophyta.obs <- iNat.obs %>% filter(Phylum == "Tracheophyta")

# Animals
Aves.obs <- iNat.obs %>% filter(Class == "Aves")
Freshwater.bryozoans.obs <- iNat.obs %>% filter(Taxon.name == "Pectinatella magnifica")
Herptiles.obs <- iNat.obs %>% filter(Class %in% c("Amphibia", "Reptilia"))

Terrestrial.arthropods.1.obs <- iNat.obs %>%
  filter(Class %in% c("Insecta", "Arachnida", "Entognatha",
                      "Diplopoda", "Chilopoda", "Ostracoda"))

Terrestrial.arthropods.2.obs <- iNat.obs %>% filter(Order == "Isopoda")
Terrestrial.arthropods.2.obs <- subset(
  Terrestrial.arthropods.2.obs,
  Taxon.name != "Bopyroides hippolytes" &
    Taxon.name != "Gnorimosphaeroma oregonense" &
    Taxon.name != "Pentidotea resecata" &
    Taxon.name != "Pentidotea wosnesenskii"
)

Terrestrial.arthropods.obs <- rbind(Terrestrial.arthropods.1.obs,
                                    Terrestrial.arthropods.2.obs)

Terrestrial.mammals.obs <- iNat.obs %>% filter(Class == "Mammalia")
Terrestrial.mammals.obs <- subset(
  Terrestrial.mammals.obs,
  Infraorder != "Cetacea" &
    Taxon.name != "Neogale vison" &
    Superfamily != "Phocoidea" &
    Subfamily != "Lutrinae"
)

Molluscs <- iNat.obs %>% filter(Phylum == "Mollusca")

Terrestrial.molluscs.1.obs <- Molluscs %>% filter(Order != "Ellobiida")
Terrestrial.molluscs.2.obs <- Molluscs %>% filter(Order == "Sphaeriida")

Terrestrial.molluscs.obs <- rbind(Terrestrial.molluscs.1.obs,
                                  Terrestrial.molluscs.2.obs)

Terrestrial.annelids.etc <- iNat.obs %>%
  filter(Genus == "Amynthas" |
           Taxon.name == "Octolasion cyaneum" |
           Genus == "Habrotrocha" |
           Taxon.name == "Lumbricus terrestris")

Animals <- iNat.obs %>% filter(Kingdom == "Animalia")

Terrestrial.animals <- rbind(Aves.obs, Freshwater.bryozoans.obs, Herptiles.obs,
                             Terrestrial.annelids.etc, Terrestrial.arthropods.obs,
                             Terrestrial.mammals.obs, Terrestrial.molluscs.obs)

Marine.animals.obs <- anti_join(Animals, Terrestrial.animals)

# Check missing taxa
taxa <- rbind(Aves.obs, Bacteria.obs, Bryophyta.Marchantiophyta.Anthocerotophyta.obs,
              Desmids.etc.obs, Freshwater.bryozoans.obs, Fungi.obs, Herptiles.obs,
              Marine.algae.and.protozoa.obs, Lichens.obs, Marine.animals.obs,
              Myxogastria.obs, Terrestrial.arthropods.obs, Terrestrial.mammals.obs,
              Terrestrial.molluscs.obs, Tracheophyta.obs)

missing.taxa <- anti_join(iNat.obs, taxa)
unique(missing.taxa$Taxon.name)

# Export outputs
write.csv(Aves.obs, "outputs/iNat_obs_birds.csv", row.names = FALSE)
write.csv(Bacteria.obs, "outputs/iNat_obs_bacteria.csv", row.names = FALSE)
write.csv(Bryophyta.Marchantiophyta.Anthocerotophyta.obs,
          "outputs/iNat_obs_mosses_liverworts_and_hornworts.csv", row.names = FALSE)
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