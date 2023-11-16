# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Galiano_marine_algae_and_protozoa_review_summary_reviewed_2023-11-14.csv")

# Apply standardized field names to baseline

summary.fields <- c('Taxon','Taxon.Author','Subtaxon.Author','Common.Name','Kingdom','Phylum',
                    'Subphylum','Superclass','Class','Subclass','Superorder','Order','Suborder',
                    'Superfamily','Family','Subfamily','Tribe','Genus','Species','Hybrid',
                    'Subspecies','Variety','Origin','Provincial.Status','National.Status','Reporting.Status',
                    'Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List',
                    'Accession.Number','GBIF.ID','First.Observed','Observer','iNaturalist.Link','Notes',
                    'ID','Stats.Code')

names(baseline) <- summary.fields

# Read iNaturalist obs

iNat.obs.summary <- read.csv("../../../parse_iNat_records/outputs/iNat_obs_marine_algae_and_protozoa.csv")
names(iNat.obs.summary)

# Summarize by first observed

iNat.obs.summary <- iNat.obs.summary %>% group_by(iNaturalist.taxon.name) %>% filter(Date.observed == min(Date.observed))

# Drop extraneous fields

drop <- c("X")
iNat.obs.summary = iNat.obs.summary[,!(names(iNat.obs.summary) %in% drop)]

# Standardize 'Taxon', 'species', 'subspecies', 'variety' fields in observation summary to facilitate merges

iNat.obs.summary$taxon_subspecies_name <- word(iNat.obs.summary$Taxon.name, 3)
iNat.obs.summary$taxon_variety_name <- word(iNat.obs.summary$Taxon.name, 3)

# Add field for Hybrid

iNat.obs.summary$Hybrid <- ""

# Rename fields in iNat observation summary to correspond with fields in baseline summary

iNat.obs.summary <- rename(iNat.obs.summary, Taxon = Taxon.name)
iNat.obs.summary <- rename(iNat.obs.summary, Subspecies = taxon_subspecies_name)
iNat.obs.summary <- rename(iNat.obs.summary, Variety = taxon_variety_name)
iNat.obs.summary <- rename(iNat.obs.summary, First.Observed = Date.observed)
iNat.obs.summary <- rename(iNat.obs.summary, Observer = Recorded.by)
iNat.obs.summary <- rename(iNat.obs.summary, iNaturalist.Link = observationId)
iNat.obs.summary <- rename(iNat.obs.summary, ID = iNaturalist.taxon.ID)

# Create template dataframe for iNat obs summary that matches with baseline summary dataset

iNat.obs.summary.fields <- summary.fields

data.frame <- as.data.frame(matrix(ncol = length(iNat.obs.summary.fields), nrow = nrow(iNat.obs.summary)))
names(data.frame) <- iNat.obs.summary.fields

data.frame[names(iNat.obs.summary)] <- iNat.obs.summary

iNat.obs.summary <- select(data.frame, c(1:length(iNat.obs.summary.fields)))

# Replace NAs with empty strings ""

# First convert logical to character

iNat.obs.summary <- iNat.obs.summary %>% mutate_if(is.logical, as.character)
iNat.obs.summary <-  iNat.obs.summary %>% mutate_if(is.character, ~replace_na(.,""))

# Match observation summary against baseline summary by Taxon ID

matched.iNat.obs.summary <- inner_join(baseline, iNat.obs.summary, by = c("ID"))
names(matched.iNat.obs.summary)
matched.iNat.obs.summary <- matched.iNat.obs.summary[,c(1,1:38)]

# Drop the field 'Infrataxon' from summaries

baseline$Infrataxon <- NULL
iNat.obs.summary$Infrataxon <- NULL

# Match observation summary against baseline by Taxon and Date Observed

matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, Taxon = Taxon.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, First.Observed = First.Observed.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, Observer = Observer.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, iNaturalist.Link = iNaturalist.Link.x)

matched.iNat.obs.summary$First.Observed  <- iNat.obs.summary$First.Observed[match(unlist(matched.iNat.obs.summary$Taxon), iNat.obs.summary$Taxon)]
matched.iNat.obs.summary$Observer  <- iNat.obs.summary$Observer[match(unlist(matched.iNat.obs.summary$Taxon), iNat.obs.summary$Taxon)]
matched.iNat.obs.summary$iNaturalist.Link  <- iNat.obs.summary$iNaturalist.Link[match(unlist(matched.iNat.obs.summary$Taxon), iNat.obs.summary$Taxon)]

# Matched summary

summary.matched <- inner_join(baseline, matched.iNat.obs.summary, by = c('ID','First.Observed'))

summary.matched <- summary.matched[,c(1:38)]
colnames(summary.matched) <- colnames(baseline)
names(summary.matched) <- iNat.obs.summary.fields

names(iNat.obs.summary) <- iNat.obs.summary.fields

# Again convert logical to character

summary.matched <- summary.matched %>% mutate_if(is.logical, as.character)

summary.matched <-  summary.matched %>% mutate_if(is.character, ~replace_na(.,""))

# Unmatched summary

unmatched.iNat.obs.summary = anti_join(iNat.obs.summary, summary.matched, by = c("ID"))

# Add Stats Code to unmatched Taxa

unmatched.iNat.obs.summary$Stats.Code <- 'ALG'

# Optional: trim to unmatched summary to observations identified at least to genus

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary[!(is.na(unmatched.iNat.obs.summary$Genus) | unmatched.iNat.obs.summary$Genus == ""), ]

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

write.csv(review.summary, "outputs/Galiano_marine_algae_review_summary.csv", row.names = FALSE)
