# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Galiano_marine_animals_summary_2023-10-22.csv")

# Apply standardized field names to baseline

summary.fields <- c('scientificName','scientificNameAuthorship','subtaxonAuthorship','commonName','kingdom','phylum',
                    'subphylum','superclass','class','subclass','superorder','order','suborder',
                    'superfamily','family','subfamily','tribe','genus','specificEpithet','hybrid',
                    'subspecies','variety','establishmentMeans','provincialStatus','nationalStatus','reportingStatus',
                    'observation','firstReported','firstReportedBy','Collection.List',
                    'firstReportedCollectionNumber','firstReportedGBIF','firstObservediNat','firstObservedBy','firstObservedID','notes',
                    'ID','statsCode')

names(baseline) <- summary.fields

# Read iNaturalist obs

iNat.obs.summary <- read.csv("../../../parse_records/outputs/iNat_obs_marine_animals.csv")
names(iNat.obs.summary)

# Summarize by first observed

iNat.obs.summary <- iNat.obs.summary %>% group_by(iNaturalist.taxon.name) %>% filter(Date.observed == min(Date.observed))

# Drop extraneous fields

drop <- c("X")
iNat.obs.summary = iNat.obs.summary[,!(names(iNat.obs.summary) %in% drop)]

# Standardize 'scientificName', 'species', 'subspecies', 'variety' fields in observation summary to facilitate merges

iNat.obs.summary$taxon_subspecies_name <- word(iNat.obs.summary$Taxon.name, 3)
iNat.obs.summary$taxon_variety_name <- word(iNat.obs.summary$Taxon.name, 3)

# Add field for Hybrid

iNat.obs.summary$hybrid <- ""

# Rename fields in iNat observation summary to correspond with fields in baseline summary

iNat.obs.summary <- rename(iNat.obs.summary, scientificName = Taxon.name)
iNat.obs.summary <- rename(iNat.obs.summary, kingdom = Kingdom)
iNat.obs.summary <- rename(iNat.obs.summary, phylum = Phylum)
iNat.obs.summary <- rename(iNat.obs.summary, subphylum = Subphylum)
iNat.obs.summary <- rename(iNat.obs.summary, superclass = Superclass)
iNat.obs.summary <- rename(iNat.obs.summary, class = Class)
iNat.obs.summary <- rename(iNat.obs.summary, subclass = Subclass)
iNat.obs.summary <- rename(iNat.obs.summary, superorder = Superorder)
iNat.obs.summary <- rename(iNat.obs.summary, order = Order)
iNat.obs.summary <- rename(iNat.obs.summary, suborder = Suborder)
iNat.obs.summary <- rename(iNat.obs.summary, infraorder = Infraorder)
iNat.obs.summary <- rename(iNat.obs.summary, superfamily = Superfamily)
iNat.obs.summary <- rename(iNat.obs.summary, family = Family)
iNat.obs.summary <- rename(iNat.obs.summary, subfamily = Subfamily)
iNat.obs.summary <- rename(iNat.obs.summary, tribe = Tribe)
iNat.obs.summary <- rename(iNat.obs.summary, genus = Genus)
iNat.obs.summary <- rename(iNat.obs.summary, subspecies = taxon_subspecies_name)
iNat.obs.summary <- rename(iNat.obs.summary, variety = taxon_variety_name)
iNat.obs.summary <- rename(iNat.obs.summary, firstObservediNat = Date.observed)
iNat.obs.summary <- rename(iNat.obs.summary, firstObservedBy = Recorded.by)
iNat.obs.summary <- rename(iNat.obs.summary, firstObservedID = observationId)
iNat.obs.summary <- rename(iNat.obs.summary, ID = iNaturalist.taxon.ID)

# Create template dataframe for iNat obs summary that matches with baseline summary dataset

iNat.obs.summary.fields <- summary.fields

data.frame <- as.data.frame(matrix(ncol = length(iNat.obs.summary.fields), nrow = nrow(iNat.obs.summary)))
names(data.frame) <- iNat.obs.summary.fields

data.frame[names(iNat.obs.summary)] <- iNat.obs.summary

iNat.obs.summary <- select(data.frame, c(1:length(iNat.obs.summary.fields)))

# Replace NAs with empty strings "" and convert logical to character

iNat.obs.summary <- iNat.obs.summary %>% mutate_if(is.logical, as.character)
iNat.obs.summary <-  iNat.obs.summary %>% mutate_if(is.character, ~replace_na(.,""))

baseline <- baseline %>% mutate_if(is.logical, as.character)
baseline <- baseline %>% mutate_if(is.character, ~replace_na(.,""))

# Match observation summary against baseline summary by Genus, Species, Infrataxon

# First create common 'Infrataxon' field between data frames to facilitate join

baseline <- baseline %>% mutate(infrataxon = coalesce(subspecies,variety))
iNat.obs.summary$infrataxon <- iNat.obs.summary$subspecies

# Now match with inner_join

matched.iNat.obs.summary <- inner_join(baseline, iNat.obs.summary, by = c("ID"))
names(matched.iNat.obs.summary)
matched.iNat.obs.summary <- matched.iNat.obs.summary[,c(1,1:38)]

# Drop the field 'infrataxon' from summaries

baseline$infrataxon <- NULL
iNat.obs.summary$infrataxon <- NULL

# Match observation summary against baseline by Taxon and Date Observed

matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, scientificName = scientificName.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, firstObservediNat = firstObservediNat.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, firstObservedBy = firstObservedBy.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, firstObservedID = firstObservedID.x)

matched.iNat.obs.summary$firstObserved  <- iNat.obs.summary$firstObserved[match(unlist(matched.iNat.obs.summary$scientificName), iNat.obs.summary$scientificName)]
matched.iNat.obs.summary$firstObservedBy  <- iNat.obs.summary$firstObservedBy[match(unlist(matched.iNat.obs.summary$scientificName), iNat.obs.summary$scientificName)]
matched.iNat.obs.summary$firstObservedID  <- iNat.obs.summary$firstObservedID[match(unlist(matched.iNat.obs.summary$scientificName), iNat.obs.summary$scientificName)]

# Matched summary

summary.matched <- inner_join(baseline, matched.iNat.obs.summary, by = c('scientificName','firstObservediNat'))

summary.matched <- summary.matched[,c(1:38)]
colnames(summary.matched) <- colnames(baseline)
names(summary.matched) <- iNat.obs.summary.fields

names(iNat.obs.summary) <- iNat.obs.summary.fields

# Again convert logical to character

summary.matched <- summary.matched %>% mutate_if(is.logical, as.character)
summary.matched <-  summary.matched %>% mutate_if(is.character, ~replace_na(.,""))

# Unmatched summary

unmatched.iNat.obs.summary = anti_join(iNat.obs.summary, summary.matched, by = c("ID"))

# Add Stats Code to unmatched Taxa (note: this code is imperfect but will at least add codes for new records of species within previously recorded familes)

unmatched.iNat.obs.summary$Stats.Code  <- baseline$Stats.Code[match(unlist(unmatched.iNat.obs.summary$Family), baseline$Family)]

# Optional: trim unmatched summary to observations identified at least to genus

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary[!(is.na(unmatched.iNat.obs.summary$genus) | unmatched.iNat.obs.summary$genus == ""), ]

# Merge baseline and unmatched summary for review 

review.summary <- rbind(baseline, unmatched.iNat.obs.summary)

# Review dataframes

nrow(iNat.obs.summary)
nrow(baseline)
nrow(summary.matched)
nrow(unmatched.iNat.obs.summary)
nrow(review.summary)

# Replace NA values with ""

review.summary[is.na(review.summary)] <- ""

# Write review summary 

write.csv(review.summary, "outputs/Galiano_marine_animals_review_summary.csv", row.names = FALSE)
