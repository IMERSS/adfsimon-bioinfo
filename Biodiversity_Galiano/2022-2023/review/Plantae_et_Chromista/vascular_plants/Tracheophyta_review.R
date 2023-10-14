# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Tracheophyta_review_summary_reviewed_2023-02-25.csv")

Tracheophyta.baseline <- baseline %>% filter(Phylum == 'Tracheophyta')

# TEMP: Update Tracheophyta baseline iNaturalist observation id 

# Tracheophyta.baseline$iNaturalist.Link <- gsub("http://www.inaturalist.org/observations/", "", as.character(Tracheophyta.baseline$iNaturalist.Link))
# Tracheophyta.baseline$iNaturalist.Link <- gsub("https://www.inaturalist.org/observations/", "", as.character(Tracheophyta.baseline$iNaturalist.Link))
# Tracheophyta.baseline$iNaturalist.Link <- gsub("http://inaturalist.ca/observations/", "", as.character(Tracheophyta.baseline$iNaturalist.Link))
  
# Tracheophyta.baseline$iNaturalist.Link <- paste0("iNat:", Tracheophyta.baseline$iNaturalist.Link)

# Remove non-established species from iNaturalist observations:

# Geranium macrorrhizum
# Wisteria

# Read iNaturalist obs

iNat.obs.summary <- read.csv("../../../parse_iNat_records/outputs/iNat_obs_Tracheophyta.csv")
names(iNat.obs.summary)

# Summarize by first observed

iNat.obs.summary <- iNat.obs.summary %>% group_by(iNaturalist.taxon.name) %>% filter(Date.observed == min(Date.observed))

# Drop extraneous fields

drop <- c("X")
iNat.obs.summary = iNat.obs.summary[,!(names(iNat.obs.summary) %in% drop)]

# Drop cultivated species # Note: this field is currently missing

# iNat.obs.summary <- subset(iNat.obs.summary, captive_cultivated != "true")

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

# Create template dataframe for iNat obs summary that matches with baseline summary dataset

iNat.obs.summary.fields <- c('Taxon','Taxon.Author','Subtaxon.Author','Common.Name','Kingdom','Phylum',
                                    'Subphylum','Superclass','Class','Subclass','Superorder','Order','Suborder',
                                    'Superfamily','Family','Subfamily','Tribe','Genus','Species','Hybrid',
                                    'Subspecies','Variety','Origin','Provincial.Status','National.Status','Reporting.Status',
                                    'Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List',
                                    'Accession.Number','GBIF.ID','First.Observed','Observer','iNaturalist.Link','Notes',
                                    'ID','Stats.Code')

data.frame <- as.data.frame(matrix(ncol = length(iNat.obs.summary.fields), nrow = nrow(iNat.obs.summary)))
names(data.frame) <- iNat.obs.summary.fields

data.frame[names(iNat.obs.summary)] <- iNat.obs.summary

iNat.obs.summary <- select(data.frame, c(1:length(iNat.obs.summary.fields)))

# Replace NAs with empty strings ""

# First convert logical to character

iNat.obs.summary <- iNat.obs.summary %>% mutate_if(is.logical, as.character)

iNat.obs.summary <-  iNat.obs.summary %>% mutate_if(is.character, ~replace_na(.,""))

# Match observation summary against baseline summary by Genus, Species, Infrataxon

# First create common 'Infrataxon' field between data frames to facilitate join

Tracheophyta.baseline <- Tracheophyta.baseline %>% mutate(Infrataxon = coalesce(Subspecies,Variety))
iNat.obs.summary$Infrataxon <- iNat.obs.summary$Subspecies

# Now match with inner_join

matched.iNat.obs.summary <- inner_join(Tracheophyta.baseline, iNat.obs.summary, by = c("Genus","Species","Infrataxon"))
names(matched.iNat.obs.summary)
matched.iNat.obs.summary <- matched.iNat.obs.summary[,c(1,1:38)]

# Drop the field 'Infrataxon' from summaries

Tracheophyta.baseline$Infrataxon <- NULL
iNat.obs.summary$Infrataxon <- NULL

# Match observation summary against baseline by Taxon and Date Observed

matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, Taxon = Taxon.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, First.Observed = First.Observed.x)
matched.iNat.date.observed <- inner_join(Tracheophyta.baseline, matched.iNat.obs.summary, by = c('Taxon','First.Observed'))

# Matched summary

Tracheophyta.summary.matched <- inner_join(Tracheophyta.baseline, matched.iNat.date.observed, by = 'Taxon')
names(Tracheophyta.summary.matched)
Tracheophyta.summary.matched <- Tracheophyta.summary.matched[,c(1:38)]
colnames(Tracheophyta.summary.matched) <- colnames(Tracheophyta.baseline)
names(Tracheophyta.summary.matched) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed' , 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')
Tracheophyta.summary.matched <- Tracheophyta.summary.matched %>% select('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

# Unmatched summary

unmatched.iNat.obs.summary = anti_join(iNat.obs.summary, Tracheophyta.summary.matched, by = c("Genus", "Hybrid", "Species", "Subspecies", "Variety"))

# Add Stats Code to unmatched Taxa

unmatched.iNat.obs.summary$Stats.Code <- 'VAS'

# Merge baseline and unmatched summary for review 

Tracheophyta.review.summary <- rbind(Tracheophyta.baseline, unmatched.iNat.obs.summary)

# Review dataframes

nrow(iNat.obs.summary)
nrow(Tracheophyta.baseline)
nrow(Tracheophyta.summary.matched)
nrow(unmatched.iNat.obs.summary)
nrow(Tracheophyta.review.summary)
nrow(Tracheophyta.review.summary) - nrow(Tracheophyta.baseline) # observations for review

# Replace NA values with ""

Tracheophyta.review.summary[is.na(Tracheophyta.review.summary)] <- ""

# Write review summary 

write.csv(Tracheophyta.review.summary, "outputs/Tracheophyta_review_summary.csv", row.names = FALSE)