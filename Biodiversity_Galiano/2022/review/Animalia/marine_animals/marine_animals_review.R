# Script to synthesize occurrence records into baseline dataset

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Marine_animals_review_summary_2023-04-22.csv")

# Read catalog of consolidated occurrence records

records <- read.csv("../../../consolidate_records/Animalia/marine_animals/synthesized/Galiano_marine_animal_records_consolidated_2023-04-22.csv")

# Read resynthesized summary

synthesized.summary <- read.csv("outputs/marine_animal_summary_resynthesized.csv")

# Summarize unique taxa

unique.taxa.records <- distinct(records, scientificName)

unique.taxa.summary <- distinct(baseline, scientificName)

unique.taxa.resynthesized <- distinct(synthesized.summary, scientificName)

nrow(unique.taxa.records)
nrow(unique.taxa.summary)
nrow(unique.taxa.resynthesized)

intersect(unique.taxa.records, unique.taxa.summary)

setdiff(unique.taxa.records, unique.taxa.summary)
setdiff(unique.taxa.summary, unique.taxa.records)

# Tally records by taxon

taxon.record.count <- records %>% group_by(scientificName) %>% tally()

names(taxon.record.count) <- c('scientificName','count')

# Create summary of taxa observed vs unobserved on iNaturalist

iNat.observed <- records %>% filter(institutionCode == "iNaturalist")
iNat.observed <- distinct(iNat.observed, scientificName)

iNat.observed$iNatObservationStatus <- "observed"

iNat.unobserved <- anti_join(unique.taxa.records,iNat.observed)

iNat.unobserved$iNatObservationStatus <- "unobserved"

iNat.observation.status <- rbind(iNat.observed,iNat.unobserved)

# Summarize records without dates

dateless.records <- records[is.na(records$eventDate),]
dateless.records.unique <- distinct(dateless.records, scientificName)

dateless.records.unique$count <- taxon.record.count$count[match(unlist(dateless.records.unique$scientificName), taxon.record.count$scientificName)]

# Are there taxa unique represented by dateless records?

dateless.records.unique$count <- taxon.record.count$count[match(unlist(dateless.records.unique$scientificName), taxon.record.count$scientificName)]

# Filter unique records without dates to be reintegrated into historical data later

dateless.records.unique.n1 <- dateless.records.unique %>% filter(count == 1)

dateless.records.unique.n1$count <- NULL

dateless.records.unique.n1 <- left_join(dateless.records.unique.n1, records)

# Remove dateless records from records

records <- records %>% filter(!is.na(eventDate))

# Summarize first observed vs last observed

first.observed <- records %>% group_by(scientificName) %>% filter(eventDate == min(eventDate))
first.observed <- distinct(first.observed, scientificName, .keep_all = TRUE)

last.observed <- records %>% group_by(scientificName) %>% filter(eventDate == max(eventDate))
last.observed <- distinct(last.observed, scientificName, .keep_all = TRUE)

# Summarize observed since 2016 and not prior to 2016 ("new records")

records.post.2016 <- records %>% filter(year >= 2016)
records.pre.2016 <- records %>% filter(year < 2016)

nrow(records.post.2016)+nrow(records.pre.2016)+nrow(dateless.records) == nrow(records)

distinct.post.2016 <- distinct(records.post.2016, scientificName)
distinct.pre.2016 <- distinct(records.pre.2016, scientificName)

new.records <- anti_join(distinct.post.2016,distinct.pre.2016)

new.records$count <- taxon.record.count$count[match(unlist(new.records$scientificName), taxon.record.count$scientificName)]

new.records.years <- new.records

new.records.years$year <- first.observed$year[match(unlist(new.records.years$scientificName), first.observed$scientificName)]

# Identify taxa previously known

previously.known <- anti_join(taxon.record.count,new.records)

nrow(new.records)+nrow(previously.known)==nrow(unique.taxa.records) # All taxa accounted for

# Create vector to filter records of previously known species

previously.reported <- previously.known$scientificName

previously.reported <- previously.reported %>% paste(collapse = "|")

# Filter records for all previously reported species

past.records <- records %>% filter(str_detect(scientificName, previously.reported))

# Summarize confirmed records = records not new, observed more than once, and seen in the last twenty years

current.date <- max(last.observed$year)
recent <- current.date-20

# Remove list records that misrepresent the provenance of records

past.records <- past.records %>% filter(!str_detect(datasetName, "UNESCO Nomination"))

past.records$observationCount <- taxon.record.count$count[match(unlist(past.records$scientificName), taxon.record.count$scientificName)]

confirmed.records <- past.records %>% filter(observationCount > 1)

confirmed.records <- confirmed.records %>% filter(year >= recent)

confirmed.records  <- distinct(confirmed.records , scientificName)

confirmed.records$count <- taxon.record.count$count[match(unlist(confirmed.records$scientificName), taxon.record.count$scientificName)]

# Summarize historic records = records observed only once in the last twenty years, or otherwise not seen in the last twenty years

historic.records <- anti_join(taxon.record.count,new.records)

historic.records <- anti_join(historic.records,confirmed.records)

# Ensure all records are accounted for

nrow(new.records)+nrow(confirmed.records)+nrow(historic.records) == nrow(unique.taxa.records)

# Construct summary dataframe for new records

new.summary.col.no <- ncol(baseline)
new.summary.row.no <- nrow(new.records)

new.summary <- data.frame(matrix(ncol = new.summary.col.no, nrow = new.summary.row.no))
colnames(new.summary) <- names(baseline)

new.summary$scientificName <- new.records$scientificName

new.summary$firstReported <- first.observed$eventDate[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$firstReportedBy <- first.observed$recordedBy[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$firstReportedSource <- first.observed$institutionCode[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$firstReportedCollectionNumber <- first.observed$catalogNumber[match(unlist(new.summary$scientificName), first.observed$scientificName)]

new.summary$lastReported <- last.observed$eventDate[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$lastReportedBy <- last.observed$recordedBy[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$lastReportedSource <- last.observed$institutionCode[match(unlist(new.summary$scientificName), first.observed$scientificName)]
new.summary$lastReportedCollectionNumber <- last.observed$catalogNumber[match(unlist(new.summary$scientificName), first.observed$scientificName)]

new.summary$recordCount <- taxon.record.count$count[match(unlist(new.summary$scientificName), taxon.record.count$scientificName)]
new.summary$reportingStatus <- paste("new", new.records.years$year, sep = " ")
new.summary$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(new.summary$scientificName), iNat.observation.status$scientificName)]

# Add values where missing from source (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

new.summary.missing.source <- new.summary %>% filter(firstReportedSource == "")
new.summary.sourced <- anti_join(new.summary,new.summary.missing.source)
new.summary.missing.source$firstReportedSource <- first.observed$datasetName[match(unlist(new.summary.missing.source$scientificName), first.observed$scientificName)]
new.summary <- rbind(new.summary.missing.source,new.summary.sourced)

new.summary.missing.source <- new.summary %>% filter(lastReportedSource == "")
new.summary.sourced <- anti_join(new.summary,new.summary.missing.source)
new.summary.missing.source$lastReportedSource <- last.observed$datasetName[match(unlist(new.summary.missing.source$scientificName), first.observed$scientificName)]
new.summary <- rbind(new.summary.missing.source,new.summary.sourced)

# Construct summary dataframe for historic records (reported)

confirmed.summary.col.no <- ncol(baseline)
confirmed.summary.row.no <- nrow(confirmed.records)

confirmed.summary <- data.frame(matrix(ncol = confirmed.summary.col.no, nrow = confirmed.summary.row.no))
colnames(confirmed.summary) <- names(baseline)

confirmed.summary$scientificName <- confirmed.records$scientificName

confirmed.summary$firstReported <- first.observed$eventDate[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$firstReportedBy <- first.observed$recordedBy[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$firstReportedSource <- first.observed$institutionCode[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$firstReportedCollectionNumber <- first.observed$catalogNumber[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]

confirmed.summary$lastReported <- last.observed$eventDate[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$lastReportedBy <- last.observed$recordedBy[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$lastReportedSource <- last.observed$institutionCode[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]
confirmed.summary$lastReportedCollectionNumber <- last.observed$catalogNumber[match(unlist(confirmed.summary$scientificName), first.observed$scientificName)]

confirmed.summary$recordCount <- taxon.record.count$count[match(unlist(confirmed.summary$scientificName), taxon.record.count$scientificName)]
confirmed.summary$reportingStatus <- "confirmed"
confirmed.summary$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(confirmed.summary$scientificName), iNat.observation.status$scientificName)]

# Add values where missing from source (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

confirmed.summary.missing.source <- confirmed.summary %>% filter(firstReportedSource == "")
confirmed.summary.sourced <- anti_join(confirmed.summary,confirmed.summary.missing.source)
confirmed.summary.missing.source$firstReportedSource <- first.observed$datasetName[match(unlist(confirmed.summary.missing.source$scientificName), first.observed$scientificName)]
confirmed.summary <- rbind(confirmed.summary.missing.source,confirmed.summary.sourced)

confirmed.summary.missing.source <- confirmed.summary %>% filter(lastReportedSource == "")
confirmed.summary.sourced <- anti_join(confirmed.summary,confirmed.summary.missing.source)
confirmed.summary.missing.source$lastReportedSource <- last.observed$datasetName[match(unlist(confirmed.summary.missing.source$scientificName), first.observed$scientificName)]
confirmed.summary <- rbind(confirmed.summary.missing.source,confirmed.summary.sourced)

# Construct summary dataframe for historic records (reported)

historic.summary.col.no <- ncol(baseline)
historic.summary.row.no <- nrow(historic.records)

historic.summary <- data.frame(matrix(ncol = historic.summary.col.no, nrow = historic.summary.row.no))
colnames(historic.summary) <- names(baseline)

historic.summary$scientificName <- historic.records$scientificName

historic.summary$firstReported <- first.observed$eventDate[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$firstReportedBy <- first.observed$recordedBy[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$firstReportedSource <- first.observed$institutionCode[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$firstReportedCollectionNumber <- first.observed$catalogNumber[match(unlist(historic.summary$scientificName), first.observed$scientificName)]

historic.summary$lastReported <- last.observed$eventDate[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$lastReportedBy <- last.observed$recordedBy[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$lastReportedSource <- last.observed$institutionCode[match(unlist(historic.summary$scientificName), first.observed$scientificName)]
historic.summary$lastReportedCollectionNumber <- last.observed$catalogNumber[match(unlist(historic.summary$scientificName), first.observed$scientificName)]

historic.summary$recordCount <- taxon.record.count$count[match(unlist(historic.summary$scientificName), taxon.record.count$scientificName)]
historic.summary$reportingStatus <- "reported"
historic.summary$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(historic.summary$scientificName), iNat.observation.status$scientificName)]

# Replace values for dateless historical records 

# Note: all historical records have dates as of 2023-04-16

# historic.summary.dateless.col.no <- ncol(baseline)
# historic.summary.dateless.row.no <- nrow(dateless.records.unique.n1)

# historic.summary.dateless.records <- data.frame(matrix(ncol = historic.summary.dateless.col.no, nrow = historic.summary.dateless.row.no))
# colnames(historic.summary.dateless.records) <- names(baseline)

# historic.summary.dateless.records$scientificName <- dateless.records.unique.n1$scientificName

# historic.summary.dateless.records$firstReportedBy <- dateless.records.unique.n1$recordedBy[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historic.summary.dateless.records$firstReportedSource <- dateless.records.unique.n1$institutionCode[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historic.summary.dateless.records$firstReportedCollectionNumber <- dateless.records.unique.n1$catalogNumber[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]

# historic.summary.dateless.records$lastReportedBy <- dateless.records.unique.n1$recordedBy[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historic.summary.dateless.records$lastReportedSource <- dateless.records.unique.n1$institutionCode[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historic.summary.dateless.records$lastReportedCollectionNumber <- dateless.records.unique.n1$catalogNumber[match(unlist(historic.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]

# historic.summary.dateless.records$recordCount <- taxon.record.count$count[match(unlist(historic.summary.dateless.records$scientificName), taxon.record.count$scientificName)]
# historic.summary.dateless.records$reportingStatus <- "reported"
# historic.summary.dateless.records$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(historic.summary.dateless.records$scientificName), iNat.observation.status$scientificName)]

# dateless.record.names <- historic.summary.dateless.records$scientificName

# dateless.record.names <- dateless.record.names %>% paste(collapse = "|")

# past.records <- records %>% filter(str_detect(scientificName, previously.reported))

# historic.summary <- historic.summary %>% filter(!str_detect(scientificName, dateless.record.names))

# historic.summary <- rbind(historic.summary,historic.summary.dateless.records)

# Add values where missing from source (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

historic.summary.missing.source <- historic.summary %>% filter(firstReportedSource == "")
historic.summary.sourced <- anti_join(historic.summary,historic.summary.missing.source)
historic.summary.missing.source$firstReportedSource <- first.observed$datasetName[match(unlist(historic.summary.missing.source$scientificName), first.observed$scientificName)]
historic.summary <- rbind(historic.summary.missing.source,historic.summary.sourced)

historic.summary.missing.source <- historic.summary %>% filter(lastReportedSource == "")
historic.summary.sourced <- anti_join(historic.summary,historic.summary.missing.source)
historic.summary.missing.source$lastReportedSource <- last.observed$datasetName[match(unlist(historic.summary.missing.source$scientificName), first.observed$scientificName)]
historic.summary <- rbind(historic.summary.missing.source,historic.summary.sourced)

# Integrate new, confirmed, and historic records

summary <- rbind(new.summary,confirmed.summary,historic.summary)

summary$scientificNameAuthorship <- baseline$scientificNameAuthorship[match(unlist(summary$scientificName), baseline$scientificName)]
summary$subtaxonAuthorship <- baseline$subtaxonAuthorship[match(unlist(summary$scientificName), baseline$scientificName)]
summary$commonName <- baseline$commonName[match(unlist(summary$scientificName), baseline$scientificName)]
summary$kingdom <- baseline$kingdom[match(unlist(summary$scientificName), baseline$scientificName)]
summary$phylum <- baseline$phylum[match(unlist(summary$scientificName), baseline$scientificName)]
summary$subphylum <- baseline$subphylum[match(unlist(summary$scientificName), baseline$scientificName)]
summary$superclass <- baseline$superclass[match(unlist(summary$scientificName), baseline$scientificName)]
summary$class <- baseline$class[match(unlist(summary$scientificName), baseline$scientificName)]
summary$subclass <- baseline$subclass[match(unlist(summary$scientificName), baseline$scientificName)]
summary$superorder <- baseline$superorder[match(unlist(summary$scientificName), baseline$scientificName)]
summary$order <- baseline$order[match(unlist(summary$scientificName), baseline$scientificName)]
summary$suborder <- baseline$suborder[match(unlist(summary$scientificName), baseline$scientificName)]
summary$superfamily <- baseline$superfamily[match(unlist(summary$scientificName), baseline$scientificName)]
summary$family <- baseline$family[match(unlist(summary$scientificName), baseline$scientificName)]
summary$subfamily <- baseline$subfamily[match(unlist(summary$scientificName), baseline$scientificName)]
summary$tribe <- baseline$tribe[match(unlist(summary$scientificName), baseline$scientificName)]
summary$genus <- baseline$genus[match(unlist(summary$scientificName), baseline$scientificName)]
summary$specificEpithet <- baseline$specificEpithet[match(unlist(summary$scientificName), baseline$scientificName)]
summary$hybrid <- baseline$hybrid[match(unlist(summary$scientificName), baseline$scientificName)]
summary$subspecies <- baseline$subspecies[match(unlist(summary$scientificName), baseline$scientificName)]
summary$variety <- baseline$variety[match(unlist(summary$scientificName), baseline$scientificName)]
summary$establishmentMeans <- baseline$establishmentMeans[match(unlist(summary$scientificName), baseline$scientificName)]
summary$provincialStatus <- baseline$provincialStatus[match(unlist(summary$scientificName), baseline$scientificName)]
summary$nationalStatus <- baseline$nationalStatus[match(unlist(summary$scientificName), baseline$scientificName)]
summary$notes <- baseline$notes[match(unlist(summary$scientificName), baseline$scientificName)]
summary$ID <- baseline$ID[match(unlist(summary$scientificName), baseline$scientificName)]
summary$statsCode <- "VAS"

# Order and output summary

summary <- summary[order(summary$scientificName),] 

write.csv(summary, "outputs/marine_animal_summary_resynthesized.csv", row.names = FALSE, na = '')

# Output summaries for major clades

unique(summary$phylum)

sponge.summary <- summary %>% filter(phylum == 'Porifera')
write.csv(sponge.summary, "outputs/taxon_summaries/sponge_summary.csv", row.names = FALSE, na = '')

cnidarian.summary <- summary %>% filter(phylum == 'Cnidaria')
write.csv(cnidarian.summary, "outputs/taxon_summaries/cnidarian_summary.csv", row.names = FALSE, na = '')

ctenophore.summary <- summary %>% filter(phylum == 'Ctenophora')
write.csv(ctenophore.summary, "outputs/taxon_summaries/ctenophore_summary.csv", row.names = FALSE, na = '')

nemertean.summary <- summary %>% filter(phylum == 'Nemertea')
write.csv(nemertean.summary, "outputs/taxon_summaries/nemertean_summary.csv", row.names = FALSE, na = '')

platyhelminthe.summary <- summary %>% filter(phylum == 'Platyhelminthes')
write.csv(platyhelminthe.summary, "outputs/taxon_summaries/platyhelminthe_summary.csv", row.names = FALSE, na = '')

chaetognath.summary <- summary %>% filter(phylum == 'Chaetognatha')
write.csv(chaetognath.summary, "outputs/taxon_summaries/chaetognath_summary.csv", row.names = FALSE, na = '')

mollusc.summary <- summary %>% filter(phylum == 'Mollusca')
write.csv(mollusc.summary, "outputs/taxon_summaries/mollusc_summary.csv", row.names = FALSE, na = '')

annelid.summary <- summary %>% filter(phylum == 'Annelida')
write.csv(annelid.summary, "outputs/taxon_summaries/annelid_summary.csv", row.names = FALSE, na = '')

peanut.worm.summary <- summary %>% filter(phylum == 'Sipuncula')
write.csv(peanut.worm.summary, "outputs/taxon_summaries/peanut_worm_summary.csv", row.names = FALSE, na = '')

crustacean.summary <- summary %>% filter(subphylum == 'Crustacea')
write.csv(crustacean.summary, "outputs/taxon_summaries/crustacean_summary.csv", row.names = FALSE, na = '')

nodding.head.summary <- summary %>% filter(phylum == 'Entoprocta')
write.csv(nodding.head.summary, "outputs/taxon_summaries/nodding_head_summary.csv", row.names = FALSE, na = '')

lampshell.summary <- summary %>% filter(phylum == 'Brachiopoda')
write.csv(lampshell.summary, "outputs/taxon_summaries/lampshell_summary.csv", row.names = FALSE, na = '')

bryozoan.summary <- summary %>% filter(phylum == 'Bryozoa')
write.csv(bryozoan.summary, "outputs/taxon_summaries/bryozoan_summary.csv", row.names = FALSE, na = '')

horseshoe.worm.summary <- summary %>% filter(phylum == 'Phoronida')
write.csv(horseshoe.worm.summary, "outputs/taxon_summaries/horseshoe_worm_summary.csv", row.names = FALSE, na = '')

echinoderm.summary <- summary %>% filter(phylum == 'Echinodermata')
write.csv(echinoderm.summary, "outputs/taxon_summaries/echinoderm_summary.csv", row.names = FALSE, na = '')

tunicate.summary <- summary %>% filter(subphylum == 'Tunicata')
write.csv(tunicate.summary, "outputs/taxon_summaries/tunicate_summary.csv", row.names = FALSE, na = '')

fishes.summary <- summary %>% filter(class == 'Actinopterygii' | class == 'Elasmobranchii')
write.csv(fishes.summary, "outputs/taxon_summaries/fishes_summary.csv", row.names = FALSE, na = '')

mammal.summary <- summary %>% filter(class == 'Mammalia')
write.csv(mammal.summary, "outputs/taxon_summaries/mammal_summary.csv", row.names = FALSE, na = '')

