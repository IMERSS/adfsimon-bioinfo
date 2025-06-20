# Script to synthesize occurrence records into baseline dataset

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("../../../review/Plantae_et_Chromista/marine_algae_and_protozoa/summaries/Galiano_marine_algae_and_protozoa_review_summary_reviewed_2024-12-02.csv")

# Read catalog of consolidated occurrence records

records <- read.csv("../../../consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2024-12-02.csv")

# Read resynthesized summary

synthesized.summary <- read.csv("outputs/Galiano_marine_algae_and_protozoa_summary_resynthesized_2024-11-23.csv")

# Summarize unique taxa

unique.taxa <- distinct(records, scientificName)

unique.taxa.summary <- distinct(baseline, Taxon)

names(unique.taxa.summary) <- c("scientificName")

nrow(unique.taxa)
nrow(unique.taxa.summary)
#nrow(unique.taxa.resynthesized)

intersect(unique.taxa, unique.taxa.summary)

setdiff(unique.taxa, unique.taxa.summary)
setdiff(unique.taxa.summary, unique.taxa)

# Tally records by taxon

taxon.record.count <- records %>% group_by(scientificName) %>% tally()

names(taxon.record.count) <- c('scientificName','count')

# Create summary of taxa observed vs unobserved on iNaturalist

iNat.observed <- records %>% filter(institutionCode == "iNaturalist")
iNat.observed <- distinct(iNat.observed, scientificName)

iNat.observed$iNatObservationStatus <- "observed"

iNat.unobserved <- anti_join(unique.taxa,iNat.observed)

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

nrow(new.records)+nrow(previously.known)==nrow(unique.taxa) # All taxa accounted for

# Create vector to filter records of previously known species

previously.reported <- previously.known$scientificName

previously.reported <- previously.reported %>% paste(collapse = "|")

# Filter records for all previously reported species

past.records <- records %>% filter(str_detect(scientificName, previously.reported))

# Summarize confirmed records = records not new, observed more than once, and seen in the last ten years

current.date <- max(last.observed$year)
recent <- current.date-20

# Remove list records that misrepresent the provenance of records

past.records <- past.records %>% filter(!str_detect(datasetName, "UNESCO Nomination")) # Not a problem with this dataset

past.records$observationCount <- taxon.record.count$count[match(unlist(past.records$scientificName), taxon.record.count$scientificName)]

confirmed.records <- past.records %>% filter(observationCount > 1)

confirmed.records <- confirmed.records %>% filter(year >= recent)

confirmed.records  <- distinct(confirmed.records , scientificName)

confirmed.records$count <- taxon.record.count$count[match(unlist(confirmed.records$scientificName), taxon.record.count$scientificName)]

# Summarize historic records = records observed only once in the last ten years, or otherwise not seen in the last ten years

historic.records <- anti_join(taxon.record.count,new.records)

historic.records <- anti_join(historic.records,confirmed.records)

# Ensure all records are accounted for

nrow(new.records)+nrow(confirmed.records)+nrow(historic.records) == nrow(unique.taxa)

# Construct summary dataframe for new records

new.summary.col.no <- ncol(synthesized.summary)
new.summary.row.no <- nrow(new.records)

new.summary <- data.frame(matrix(ncol = new.summary.col.no, nrow = new.summary.row.no))
colnames(new.summary) <- names(synthesized.summary)

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

# Add values where missing from collection number (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

new.summary.missing.collectionNo <- new.summary %>% filter(firstReportedCollectionNumber == "")
new.summary.collectionNo <- anti_join(new.summary,new.summary.missing.collectionNo)
new.summary.missing.collectionNo$firstReportedCollectionNumber <- first.observed$occurrenceID[match(unlist(new.summary.missing.collectionNo$scientificName), first.observed$scientificName)]
new.summary <- rbind(new.summary.missing.collectionNo,new.summary.collectionNo)

new.summary.missing.collectionNo <- new.summary %>% filter(lastReportedCollectionNumber == "")
new.summary.collectionNo <- anti_join(new.summary,new.summary.missing.collectionNo)
new.summary.missing.collectionNo$lastReportedCollectionNumber <- last.observed$occurrenceID[match(unlist(new.summary.missing.collectionNo$scientificName), last.observed$scientificName)]
new.summary <- rbind(new.summary.missing.collectionNo,new.summary.collectionNo)

# Construct summary dataframe for historic records (reported)

confirmed.summary.col.no <- ncol(synthesized.summary)
confirmed.summary.row.no <- nrow(confirmed.records)

confirmed.summary <- data.frame(matrix(ncol = confirmed.summary.col.no, nrow = confirmed.summary.row.no))
colnames(confirmed.summary) <- names(synthesized.summary)

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

# Add values where missing from collection number (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

confirmed.summary.missing.collectionNo <- confirmed.summary %>% filter(firstReportedCollectionNumber == "")
confirmed.summary.collectionNo <- anti_join(confirmed.summary,confirmed.summary.missing.collectionNo)
confirmed.summary.missing.collectionNo$firstReportedCollectionNumber <- first.observed$occurrenceID[match(unlist(confirmed.summary.missing.collectionNo$scientificName), first.observed$scientificName)]
confirmed.summary <- rbind(confirmed.summary.missing.collectionNo,confirmed.summary.collectionNo)

confirmed.summary.missing.collectionNo <- confirmed.summary %>% filter(lastReportedCollectionNumber == "")
confirmed.summary.collectionNo <- anti_join(confirmed.summary,confirmed.summary.missing.collectionNo)
confirmed.summary.missing.collectionNo$lastReportedCollectionNumber <- last.observed$occurrenceID[match(unlist(confirmed.summary.missing.collectionNo$scientificName), last.observed$scientificName)]
confirmed.summary <- rbind(confirmed.summary.missing.collectionNo,confirmed.summary.collectionNo)

# Construct summary dataframe for historic records (reported)

historical.summary.col.no <- ncol(synthesized.summary)
historical.summary.row.no <- nrow(historic.records)

historical.summary <- data.frame(matrix(ncol = historical.summary.col.no, nrow = historical.summary.row.no))
colnames(historical.summary) <- names(synthesized.summary)

historical.summary$scientificName <- historic.records$scientificName

historical.summary$firstReported <- first.observed$eventDate[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$firstReportedBy <- first.observed$recordedBy[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$firstReportedSource <- first.observed$institutionCode[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$firstReportedCollectionNumber <- first.observed$catalogNumber[match(unlist(historical.summary$scientificName), first.observed$scientificName)]

historical.summary$lastReported <- last.observed$eventDate[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$lastReportedBy <- last.observed$recordedBy[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$lastReportedSource <- last.observed$institutionCode[match(unlist(historical.summary$scientificName), first.observed$scientificName)]
historical.summary$lastReportedCollectionNumber <- last.observed$catalogNumber[match(unlist(historical.summary$scientificName), first.observed$scientificName)]

historical.summary$recordCount <- taxon.record.count$count[match(unlist(historical.summary$scientificName), taxon.record.count$scientificName)]
historical.summary$reportingStatus <- "reported"
historical.summary$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(historical.summary$scientificName), iNat.observation.status$scientificName)]

# Replace values for dateless historical records 

# Note: all historical records have dates as of 2023-04-16

# historical.summary.dateless.col.no <- ncol(baseline)
# historical.summary.dateless.row.no <- nrow(dateless.records.unique.n1)

# historical.summary.dateless.records <- data.frame(matrix(ncol = historical.summary.dateless.col.no, nrow = historical.summary.dateless.row.no))
# colnames(historical.summary.dateless.records) <- names(baseline)

# historical.summary.dateless.records$scientificName <- dateless.records.unique.n1$scientificName

# historical.summary.dateless.records$firstReportedBy <- dateless.records.unique.n1$recordedBy[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historical.summary.dateless.records$firstReportedSource <- dateless.records.unique.n1$institutionCode[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historical.summary.dateless.records$firstReportedCollectionNumber <- dateless.records.unique.n1$catalogNumber[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]

# historical.summary.dateless.records$lastReportedBy <- dateless.records.unique.n1$recordedBy[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historical.summary.dateless.records$lastReportedSource <- dateless.records.unique.n1$institutionCode[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]
# historical.summary.dateless.records$lastReportedCollectionNumber <- dateless.records.unique.n1$catalogNumber[match(unlist(historical.summary.dateless.records$scientificName), dateless.records.unique.n1$scientificName)]

# historical.summary.dateless.records$recordCount <- taxon.record.count$count[match(unlist(historical.summary.dateless.records$scientificName), taxon.record.count$scientificName)]
# historical.summary.dateless.records$reportingStatus <- "reported"
# historical.summary.dateless.records$iNatObservationStatus <- iNat.observation.status$iNatObservationStatus[match(unlist(historical.summary.dateless.records$scientificName), iNat.observation.status$scientificName)]

# dateless.record.names <- historical.summary.dateless.records$scientificName

# dateless.record.names <- dateless.record.names %>% paste(collapse = "|")

# past.records <- records %>% filter(str_detect(scientificName, previously.reported))

# historical.summary <- historical.summary %>% filter(!str_detect(scientificName, dateless.record.names))

# historical.summary <- rbind(historical.summary,historical.summary.dateless.records)

# Add values where missing from source (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

historical.summary.missing.source <- historical.summary %>% filter(firstReportedSource == "")
historical.summary.sourced <- anti_join(historical.summary,historical.summary.missing.source)
historical.summary.missing.source$firstReportedSource <- first.observed$datasetName[match(unlist(historical.summary.missing.source$scientificName), first.observed$scientificName)]
historical.summary <- rbind(historical.summary.missing.source,historical.summary.sourced)

historical.summary.missing.source <- historical.summary %>% filter(lastReportedSource == "")
historical.summary.sourced <- anti_join(historical.summary,historical.summary.missing.source)
historical.summary.missing.source$lastReportedSource <- last.observed$datasetName[match(unlist(historical.summary.missing.source$scientificName), first.observed$scientificName)]
historical.summary <- rbind(historical.summary.missing.source,historical.summary.sourced)

# Add values where missing from collection number (first reported and last reported) by subsetting rows with missing values, adding values from datasetName, and remerging

historical.summary.missing.collectionNo <- historical.summary %>% filter(firstReportedCollectionNumber == "")
historical.summary.collectionNo <- anti_join(historical.summary,historical.summary.missing.collectionNo)
historical.summary.missing.collectionNo$firstReportedCollectionNumber <- first.observed$occurrenceID[match(unlist(historical.summary.missing.collectionNo$scientificName), first.observed$scientificName)]
historical.summary <- rbind(historical.summary.missing.collectionNo,historical.summary.collectionNo)

historical.summary.missing.collectionNo <- historical.summary %>% filter(lastReportedCollectionNumber == "")
historical.summary.collectionNo <- anti_join(historical.summary,historical.summary.missing.collectionNo)
historical.summary.missing.collectionNo$lastReportedCollectionNumber <- last.observed$occurrenceID[match(unlist(historical.summary.missing.collectionNo$scientificName), last.observed$scientificName)]
historical.summary <- rbind(historical.summary.missing.collectionNo,historical.summary.collectionNo)

# Integrate new, confirmed, and historic records

summary <- rbind(new.summary,confirmed.summary,historical.summary)

summary$scientificNameAuthorship <- baseline$Taxon.Author[match(unlist(summary$scientificName), baseline$Taxon)]
summary$subtaxonAuthorship <- baseline$Subtaxon.Author[match(unlist(summary$scientificName), baseline$Taxon)]
summary$commonName <- baseline$Common.Name[match(unlist(summary$scientificName), baseline$Taxon)]
summary$kingdom <- baseline$Kingdom[match(unlist(summary$scientificName), baseline$Taxon)]
summary$phylum <- baseline$Phylum[match(unlist(summary$scientificName), baseline$Taxon)]
summary$subphylum <- baseline$Subphylum[match(unlist(summary$scientificName), baseline$Taxon)]
summary$superclass <- baseline$Superclass[match(unlist(summary$scientificName), baseline$Taxon)]
summary$class <- baseline$Class[match(unlist(summary$scientificName), baseline$Taxon)]
summary$subclass <- baseline$Subclass[match(unlist(summary$scientificName), baseline$Taxon)]
summary$superorder <- baseline$Superorder[match(unlist(summary$scientificName), baseline$Taxon)]
summary$order <- baseline$Order[match(unlist(summary$scientificName), baseline$Taxon)]
summary$suborder <- baseline$Suborder[match(unlist(summary$scientificName), baseline$Taxon)]
summary$superfamily <- baseline$Superfamily[match(unlist(summary$scientificName), baseline$Taxon)]
summary$family <- baseline$Family[match(unlist(summary$scientificName), baseline$Taxon)]
summary$subfamily <- baseline$Subfamily[match(unlist(summary$scientificName), baseline$Taxon)]
summary$tribe <- baseline$Tribe[match(unlist(summary$scientificName), baseline$Taxon)]
summary$genus <- baseline$Genus[match(unlist(summary$scientificName), baseline$Taxon)]
summary$specificEpithet <- baseline$Species[match(unlist(summary$scientificName), baseline$Taxon)]
summary$hybrid <- baseline$Hybrid[match(unlist(summary$scientificName), baseline$Taxon)]
summary$subspecies <- baseline$Subspecies[match(unlist(summary$scientificName), baseline$Taxon)]
summary$variety <- baseline$Variety[match(unlist(summary$scientificName), baseline$Taxon)]
summary$establishmentMeans <- baseline$Origin[match(unlist(summary$scientificName), baseline$Taxon)]
summary$provincialStatus <- baseline$Provincial.Status[match(unlist(summary$scientificName), baseline$Taxon)]
summary$nationalStatus <- baseline$National.Status[match(unlist(summary$scientificName), baseline$Taxon)]
summary$notes <- baseline$Notes[match(unlist(summary$scientificName), baseline$Taxon)]
summary$ID <- baseline$ID[match(unlist(summary$scientificName), baseline$Taxon)]
summary$statsCode <- baseline$Stats.Code[match(unlist(summary$scientificName), baseline$Taxon)]

# Order and output summary

summary <- summary[order(summary$scientificName), ]

write.csv(summary, "outputs/Galiano_marine_algae_and_protozoa_summary_resynthesized.csv", row.names = FALSE, na = '')

# Output summaries for Chlorophyta, Rhodophyta, Phaeophyceae

Chlorophyta <- summary %>% filter(phylum == 'Chlorophyta')
Rhodophyta <- summary %>% filter(phylum == 'Rhodophyta')
Phaeophyceae <- summary %>% filter(class == 'Phaeophyceae')

write.csv(Chlorophyta, "Galiano_Island_Chlorophyta_summary.csv")
write.csv(Rhodophyta, "Galiano_Island_Rhodophyta_summary.csv")
write.csv(Chlorophyta, "Galiano_Island_Chlorophyta_summary.csv")

