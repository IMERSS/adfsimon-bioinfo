library(stringr)

Vascular.plant.records

historical.records <- Vascular.plant.records %>% filter(year < 2014)

iNat.pre.2014.records <- historical.records %>% filter(datasetName == 'iNaturalist')

historical.records <- anti_join(historical.records,iNat.pre.2014.records)

contemporary.records <- Vascular.plant.records %>% filter(year >= 2014)

contemporary.records <- rbind(contemporary.records,iNat.pre.2014.records)

nrow(Vascular.plant.records)

nrow(contemporary.records) + nrow(historical.records)

historical.voucher.specimens <- historical.records %>% filter(basisOfRecord == 'PreservedSpecimen')

historical.observations <- anti_join(historical.records,historical.voucher.specimens)

nrow(historical.voucher.specimens)/nrow(historical.records)

iNaturalist.observations <- Vascular.plant.records %>% filter(datasetName == 'iNaturalist')

nrow(iNaturalist.observations)/nrow(Vascular.plant.records)

head(summary)

confirmed.records <- summary %>% filter(Reporting.Status == 'confirmed')
reported.records <- summary %>% filter(Reporting.Status == 'reported')

historical.records.summary <- rbind(confirmed.records,reported.records)

nrow(historical.records.summary) 

# There were 607 vascular plant species historically reported for 
# Galiano Island when the BioGaliano project began...

historical.records.summary$year <-  str_sub(historical.records.summary$First.Observed, 1, 4)

historical.records.summary$year <- as.numeric(historical.records.summary$year)

historical.records.confirmed.pre.2020 <- historical.records.summary %>% filter(year < 2020)

nrow(historical.records.confirmed.pre.2020)

nrow(historical.records.confirmed.pre.2020)/nrow(historical.records.summary)

# By 2020,71% (435/607) of the historically reported vascular plant diversity had been verified
# through the Biodiversity Galiano project. Thus, there were still 172 historically reported 
# species that remained undocumented on the project.

historical.records.confirmed.pre.2020$year <- NULL

species.at.large.2020 <- anti_join(historical.records.summary,historical.records.confirmed.pre.2020)

native.secies.at.large.2020 <- species.at.large.2020 %>% filter(Origin == 'native')

nrow(native.secies.at.large.2020)

# Of the 172 species that remained at large, 110 were of native origin.

# How many were represented by voucher specimens?

# ...

# By 2024, how many species remain at large?

# To date, 82% (500/607) of the vascular plants historically reported
# for Galiano Island have been verified.

nrow(confirmed.records)/nrow(historical.records.summary)

nrow(reported.records) # A total of 107 species remain at large

native.species.at.large.2024 <- reported.records %>% filter(Origin == 'native')

nrow(native.species.at.large.2024) # Of the 1-7 species that remain at large, 68 are native.


