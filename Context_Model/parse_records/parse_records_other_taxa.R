## ALGAE

# Freshwater and terrestrial algae and protozoa

Desmids.etc.1.obs <- obs %>% filter(Phylum == 'Charophyta')
unique(Desmids.etc.1.obs$Taxon.name)
Desmids.etc.2.obs <- obs %>% filter(Phylum == 'Chlorophyta')
unique(Desmids.etc.2.obs$Taxon.name)
Desmids.etc.2.obs <- Desmids.etc.2.obs %>% filter(Order == 'Trentepohliales' | Class == 'Chlorophyceae')
Desmids.etc.3.obs <- obs %>% filter(Genus == 'Arcella')

Desmids.etc.obs <- rbind(Desmids.etc.1.obs, Desmids.etc.2.obs, Desmids.etc.3.obs)

# Marine algae and protozoa

Marine.algae.and.protozoa.1.obs <- obs %>% filter(Kingdom == 'Chromista' | Phylum == 'Rhodophyta')
Marine.algae.and.protozoa.2.obs <- obs %>% filter(Phylum == 'Chlorophyta')
Marine.algae.and.protozoa.2.obs <- subset(Marine.algae.and.protozoa.2.obs, Order != 'Trentepohliales' & Class != 'Chlorophyceae')

Marine.algae.and.protozoa.obs <- rbind(Marine.algae.and.protozoa.1.obs, Marine.algae.and.protozoa.2.obs)


## BACTERIA

Bacteria.obs <- obs %>% filter(Kingdom == 'Bacteria')


## FUNGI, LICHENS, MYXOGASTRIA

# Lichens

Ascomycota.obs <- obs %>% filter(Phylum == 'Ascomycota')

Lichenized.Ascomycota.obs <- Ascomycota.obs %>% filter(Class == 'Arthoniomycetes' | Class == 'Dothideomycetes' | Class == 'Eurotiomycetes' | Class == 'Lecanoromycetes' | Class == 'Lichinomycetes')

Lichenized.Ascomycota.obs<- subset(Lichenized.Ascomycota.obs, Order != 'Abrothallales' & Genus != 'Arthophacopsis' & Order != 'Pleosporales' & Order != 'Geoglossales' & Order != 'Venturiales' & Order != 'Sareales' & Family != 'Stictidaceae' & Order != 'Eurotiales' & Order != 'Chaetothyriales' & Order != 'Capnodiales' & Order != 'Botryosphaeriales')

Basidiomycota.obs <- obs %>% filter(Phylum == 'Basidiomycota')

Lichenized.Basidiomycota.obs <- Basidiomycota.obs %>% filter(Genus == 'Lichenomphalia' | Genus == 'Multiclavula')

Lichens.obs <- rbind(Lichenized.Ascomycota.obs, Lichenized.Basidiomycota.obs)

# Fungi (unlichenized Ascomycota and Basidiomycota, and Mucoromycota

Unlichenized.Ascomycota.obs <- anti_join(Ascomycota.obs,Lichenized.Ascomycota.obs)

Unlichenized.Basidiomycota.obs <- anti_join(Basidiomycota.obs,Lichenized.Basidiomycota.obs)

Mucoromycota.obs <- obs %>% filter(Phylum == 'Mucoromycota')

Fungi.obs <- rbind(Unlichenized.Ascomycota.obs,Unlichenized.Basidiomycota.obs,Mucoromycota.obs)

# Myxogastria

Myxogastria.obs <- obs %>% filter(Kingdom == 'Protozoa')
Myxogastria.obs <- subset(Myxogastria.obs, Phylum != 'Amoebozoa' & Phylum != 'Retaria' & Phylum != 'Sarcomastigophora')

## PLANTS

# Bryophyta, Marchantiophyta, Anthocerotophyta

Bryophyta.Marchantiophyta.Anthocerotophyta.obs <- obs %>% filter(Phylum == 'Bryophyta' | Phylum == 'Marchantiophyta' | Phylum == 'Anthocerotophyta')

# Tracheophyta

Tracheophyta.obs <- obs %>% filter(Phylum == 'Tracheophyta')

unique(Tracheophyta.obs$Phylum)


animalia <- obs %>% filter(Kingdom == 'Animalia')


## TERRESTRIAL ANIMALS

# Birds

Aves.obs <- obs %>% filter(Class == 'Aves')

# Freshwater bryozoans

Freshwater.bryozoans.obs <- obs %>% filter(Taxon.name == 'Pectinatella magnifica')

# Terrestrial Arthropods

Terrestrial.arthropods.1.obs <- obs %>% filter(Class == 'Insecta' | Class == 'Arachnida' | Class == 'Entognatha' | Class == 'Diplopoda' | Class == 'Chilopoda' | Class == 'Ostracoda')
Terrestrial.arthropods.2.obs <- obs %>% filter(Order == 'Isopoda')
Terrestrial.arthropods.2.obs <- subset(Terrestrial.arthropods.2.obs, Taxon.name != 'Bopyroides hippolytes' & Taxon.name != 'Gnorimosphaeroma oregonense' & Taxon.name != 'Pentidotea resecata' & Taxon.name != 'Pentidotea wosnesenskii')

Terrestrial.arthropods.obs <- rbind(Terrestrial.arthropods.1.obs, Terrestrial.arthropods.2.obs)

# Terrestrial Molluscs

Molluscs <- obs %>% filter(Phylum == 'Mollusca')

Terrestrial.molluscs.1.obs <- Molluscs %>% filter(Superorder == 'Eupulmonata')

Terrestrial.molluscs.1.obs <- subset(Terrestrial.molluscs.1.obs, Order != 'Ellobiida')

Terrestrial.molluscs.2.obs <- Molluscs %>% filter(Order == 'Sphaeriida')

Terrestrial.molluscs.obs <- rbind(Terrestrial.molluscs.1.obs, Terrestrial.molluscs.2.obs)

# Terrestrial Annelids, Rotifers, etc.

Terrestrial.annelids.etc <- obs %>% filter(Genus == 'Amynthas' | Taxon.name == 'Octolasion cyaneum' | Genus == 'Habrotrocha' | Taxon.name == 'Lumbricus terrestris')

# Marine Animals

Animals <- obs %>% filter(Kingdom == 'Animalia')

Terrestrial.animals <- rbind(Aves.obs, Freshwater.bryozoans.obs, Herptiles.obs, Terrestrial.annelids.etc, Terrestrial.arthropods.obs, Terrestrial.mammals.obs, Terrestrial.molluscs.obs)

Marine.animals.obs <- anti_join(Animals, Terrestrial.animals)

# Export catalogs

write.csv(Aves.records, "outputs/records_birds.csv", row.names = FALSE)

write.csv(Bacteria.records, "outputs/records_bacteria.csv", row.names = FALSE)

write.csv(Bryophyta.Marchantiophyta.Anthocerotophyta.records, "outputs/records_mosses_liverworts_and_hornworts.csv", row.names = FALSE)

write.csv(Desmids.etc.records, "outputs/records_freshwater_and_terrestrial_algae.csv", row.names = FALSE)

write.csv(Freshwater.bryozoans.records, "outputs/records_freshwater_bryozoans.csv", row.names = FALSE)

write.csv(Fungi.records, "outputs/records_fungi.csv", row.names = FALSE)

write.csv(Lichens.records, "outputs/records_lichens.csv", row.names = FALSE)

write.csv(Marine.algae.and.protozoa.records, "outputs/records_marine_algae_and_protozoa.csv", row.names = FALSE)

write.csv(Marine.animals.records, "outputs/records_marine_animals.csv", row.names = FALSE)

write.csv(Myxogastria.records, "outputs/records_Myxogastria.csv", row.names = FALSE)

write.csv(Terrestrial.annelids.etc, "outputs/records_Terrestrial_annelids_etc.csv", row.names = FALSE)

write.csv(Terrestrial.arthropods.records, "outputs/records_terrestrial_arthropods.csv", row.names = FALSE)

write.csv(Terrestrial.molluscs.records, "outputs/records_terrestrial_molluscs.csv", row.names = FALSE)

write.csv(Tracheophyta.records, "outputs/records_Tracheophyta.csv", row.names = FALSE)
