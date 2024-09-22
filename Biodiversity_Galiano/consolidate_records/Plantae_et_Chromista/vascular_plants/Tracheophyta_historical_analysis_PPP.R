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
