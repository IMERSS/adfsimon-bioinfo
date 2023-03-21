# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summary/Tracheophyta_review_summary_reviewed_2022-10-16.csv")

Tracheophyta.baseline <- baseline %>% filter(Phylum == 'Tracheophyta')

# Read iNaturalist obs summary

iNat.obs.summary <- read.csv("../../../1_split/vascular_plants/outputs/Tracheophyta_iNat_summary.csv")
names(iNat.obs.summary)

# Drop extraneous fields

drop <- c("X","X.Taxon.")
iNat.obs.summary = iNat.obs.summary[,!(names(iNat.obs.summary) %in% drop)]

# Drop cultivated species

iNat.obs.summary <- subset(iNat.obs.summary, captive_cultivated != "true")

# Standardize 'Taxon', 'species', 'subspecies', 'variety' fields in observation summary to facilitate merges

iNat.obs.summary$taxon_species_name <- word(iNat.obs.summary$taxon_species_name, 2)
iNat.obs.summary$taxon_subspecies_name <- word(iNat.obs.summary$taxon_subspecies_name, 3)
iNat.obs.summary$taxon_variety_name <- word(iNat.obs.summary$taxon_variety_name, 3)

iNat.obs.summary <- rename(iNat.obs.summary, Taxon = scientific_name)
iNat.obs.summary <- rename(iNat.obs.summary, Genus = taxon_genus_name)
iNat.obs.summary <- rename(iNat.obs.summary, Species = taxon_species_name)
iNat.obs.summary <- rename(iNat.obs.summary, Hybrid = taxon_hybrid_name)
iNat.obs.summary <- rename(iNat.obs.summary, Subspecies = taxon_subspecies_name)
iNat.obs.summary <- rename(iNat.obs.summary, Variety = taxon_variety_name)

iNat.obs.summary[is.na(iNat.obs.summary)] <- ""

# Match observation summary against baseline summary by Genus, Species, Subspecies, Variety

matched.iNat.obs.summary <- inner_join(Tracheophyta.baseline, iNat.obs.summary, by = c("Genus", "Hybrid", "Species", "Subspecies", "Variety"))
names(matched.iNat.obs.summary)
matched.iNat.obs.summary <- matched.iNat.obs.summary[,c(1,39:76)]

# Match observation summary against baseline by Taxon and Date Observed

matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, Taxon = Taxon.x)
matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, First.Observed = observed_on)
matched.iNat.date.observed <- inner_join(Tracheophyta.baseline, matched.iNat.obs.summary, by = c('Taxon','First.Observed'))

# Matched summary

Tracheophyta.summary.matched <- inner_join(Tracheophyta.baseline, matched.iNat.date.observed, by = 'Taxon')
names(Tracheophyta.summary.matched)
Tracheophyta.summary.matched <- Tracheophyta.summary.matched[,c(1:38)]
colnames(Tracheophyta.summary.matched) <- colnames(Tracheophyta.baseline)
names(Tracheophyta.summary.matched) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed' , 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')
Tracheophyta.summary.matched <- Tracheophyta.summary.matched %>% select('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

# Unmatched summary

unmatched.iNat.obs = anti_join(iNat.obs.summary, Tracheophyta.summary.matched, by = c("Genus", "Hybrid", "Species", "Subspecies", "Variety"))
unmatched.iNat.obs <- unmatched.iNat.obs %>% select(c('Taxon','common_name','observed_on', 'user_login', 'url','taxon_id', 'taxon_kingdom_name','taxon_phylum_name','taxon_subphylum_name','taxon_superclass_name','taxon_class_name','taxon_subclass_name','taxon_superorder_name','taxon_order_name','taxon_suborder_name','taxon_superfamily_name','taxon_family_name','taxon_subfamily_name','taxon_tribe_name','Genus','Species', 'Hybrid','Subspecies', 'Variety'))
other.summary.column.names <- c('Taxon.Author', 'Subtaxon.Author', 'Origin', 'Provincial.Status', 'National.Status', 'Reporting.Status','Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List','Accession.Number','GBIF.ID','Notes','Stats.Code')
other.summary.columns <- data.frame(matrix(ncol=length(other.summary.column.names),nrow=nrow(unmatched.iNat.obs)))
names(other.summary.columns) <- other.summary.column.names
unmatched.iNat.obs.summary <- cbind(unmatched.iNat.obs,other.summary.columns)
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>% select(Taxon, Taxon.Author, Subtaxon.Author, common_name, taxon_kingdom_name, taxon_phylum_name,taxon_subphylum_name,taxon_superclass_name,taxon_class_name,taxon_subclass_name,taxon_superorder_name,taxon_order_name,taxon_suborder_name,taxon_superfamily_name,taxon_family_name,taxon_subfamily_name,taxon_tribe_name,Genus,Species,Hybrid,Subspecies,Variety,Origin,Provincial.Status,National.Status,Reporting.Status,Observation,Collected.Reported..y.m.d.,Collector.Source,Collection.List,Accession.Number,GBIF.ID,observed_on,user_login,url,Notes,taxon_id,Stats.Code)

# Add Stats Code to unmatched Taxa

names(unmatched.iNat.obs.summary)
unmatched.iNat.obs.summary$Stats.Code <- 'VAS'

# Add standardized column headings

names(Tracheophyta.baseline) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status','Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')
names(unmatched.iNat.obs.summary) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status','Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

# Review dataframes

nrow(iNat.obs.summary)
nrow(Tracheophyta.baseline)
nrow(Tracheophyta.summary.matched)
nrow(unmatched.iNat.obs.summary)

# Optional: drop taxa identified to genus 
# Generally, it may be useful to review species determined only to genus to see if any novel taxa are present 
# Dropping these makes for a more efficient review, at the risk of overlooking novel taxa
# Best practice: include genera!

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary[!(is.na(unmatched.iNat.obs.summary$Species) | unmatched.iNat.obs.summary$Species==""), ]

# Merge baseline and unmatched summary for review 

Tracheophyta.review.summary <- rbind(Tracheophyta.baseline, unmatched.iNat.obs.summary)

# Replace NA values with ""

Tracheophyta.review.summary[is.na(Tracheophyta.review.summary)] <- ""

# Write review summary 
# Note: be careful not to overwrite reviewed summary in case you revise through another workflow
# This workflow needs to be improved to prevent any terrible errors!

# write.csv(Tracheophyta.review.summary, "outputs/Tracheophyta_review_summary.csv")