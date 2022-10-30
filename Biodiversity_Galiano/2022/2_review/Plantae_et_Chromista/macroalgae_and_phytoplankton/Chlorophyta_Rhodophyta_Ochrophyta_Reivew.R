# Script to compare iNaturalist observations against historical baseline

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("/Users/andrewsimon/GitHub/bioinformatics/adfsimon-bioinfo/Biodiversity_Galiano/2022/2_review/Plantae_et_Chromista/Plantae_et_Chromista_Baseline_Summary_2022-09-30.csv")

Tracheophyta.baseline <- baseline %>% filter(Phylum == 'Tracheophyta')

# Read iNaturalist obs summary

iNat.obs.summary <- read.csv("/Users/andrewsimon/GitHub/bioinformatics/adfsimon-bioinfo/Biodiversity_Galiano/2022/1_split/vascular_plants/Tracheophyta_iNat_summary.csv")
names(iNat.obs.summary)


# Standardize fields for Taxon and Date Observed to compare

iNat.obs.summary <- rename(iNat.obs.summary, Taxon = scientific_name)

# Match observation summary against baseline summary by Taxon

matched.iNat.obs.summary <- inner_join(Tracheophyta.baseline, iNat.obs.summary, by = 'Taxon')
matched.iNat.obs.summary <- matched.iNat.obs.summary %>% drop_na(X)
matched.iNat.obs.summary <- matched.iNat.obs.summary[,c(1,35:76)]

# Match observation summary against baseline by Taxon and Date Observed

matched.iNat.obs.summary <- rename(matched.iNat.obs.summary, First.Observed = observed_on)
matched.iNat.date.observed <- inner_join(Tracheophyta.baseline, matched.iNat.obs.summary, by = c('Taxon','First.Observed'))

# Matched summary

Tracheophyta.summary.matched <- inner_join(Tracheophyta.baseline, matched.iNat.date.observed, by = 'Taxon')
names(Tracheophyta.summary.matched)
Tracheophyta.summary.matched <- Tracheophyta.summary.matched[,c(1:33)]
colnames(Tracheophyta.summary.matched) <- colnames(Tracheophyta.baseline)
other.summary.column.names <- c('taxon_species_name', 'taxon_hybrid_name', 'taxon_subspecies_name','taxon_variety_name')
other.summary.columns <- data.frame(matrix(ncol=length(other.summary.column.names),nrow=nrow(Tracheophyta.summary.matched)))
names(other.summary.columns) <- other.summary.column.names
Tracheophyta.summary.matched <- cbind(Tracheophyta.summary.matched,other.summary.columns)
names(Tracheophyta.summary.matched)
names(Tracheophyta.summary.matched) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Origin', 'Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed' , 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code', 'Species', 'Hybrid', 'Subspecies','Variety')
Tracheophyta.summary.matched <- Tracheophyta.summary.matched %>% select('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

#  *TEMPORARY* Merge 'Species' epithet from iNat obs summary with Matched summary (Note: many unmatched taxa have not matched because of annotations like 'var.' etc. so fields for 'Species', 'Subspecies', 'Variety' will be important for matching and merging in the future)
# Note: an alternative approach for matching would be to use the Taxon ID. However, I am not confident in that approach because the Taxon ID columns may not be consistently accurate; hopefully Antranig will be able to generate a script to test this and create reliable summaries

matched.iNat.date.observed$taxon_species_name <- word(matched.iNat.date.observed$taxon_species_name, 2)
Tracheophyta.summary.matched$Species <- matched.iNat.date.observed$taxon_species_name[match(unlist(Tracheophyta.summary.matched$Taxon), matched.iNat.date.observed$Taxon)]

# Unmatched summary

unmatched.iNat.obs = anti_join(iNat.obs.summary, Tracheophyta.summary.matched, by = c("Taxon" = "Taxon"))
unmatched.iNat.obs <- unmatched.iNat.obs %>% select(c('Taxon','common_name','observed_on', 'user_login', 'url','taxon_id', 'taxon_kingdom_name','taxon_phylum_name','taxon_subphylum_name','taxon_superclass_name','taxon_class_name','taxon_subclass_name','taxon_superorder_name','taxon_order_name','taxon_suborder_name','taxon_superfamily_name','taxon_family_name','taxon_subfamily_name','taxon_tribe_name','taxon_genus_name','taxon_species_name', 'taxon_hybrid_name','taxon_subspecies_name', 'taxon_variety_name'))
other.summary.column.names <- c('Taxon.Author', 'Subtaxon.Author', 'Origin', 'Status', 'Reporting.Status','Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List','Accession.Number','GBIF.ID','Notes','Stats.Code')
other.summary.columns <- data.frame(matrix(ncol=length(other.summary.column.names),nrow=nrow(unmatched.iNat.obs)))
names(other.summary.columns) <- other.summary.column.names
unmatched.iNat.obs.summary <- cbind(unmatched.iNat.obs,other.summary.columns)
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>% select(Taxon, Taxon.Author, Subtaxon.Author, common_name, taxon_kingdom_name, taxon_phylum_name,taxon_subphylum_name,taxon_superclass_name,taxon_class_name,taxon_subclass_name,taxon_superorder_name,taxon_order_name,taxon_suborder_name,taxon_superfamily_name,taxon_family_name,taxon_subfamily_name,taxon_tribe_name,taxon_genus_name,taxon_species_name,taxon_hybrid_name,taxon_subspecies_name,taxon_variety_name,Origin,Status,Reporting.Status,Observation,Collected.Reported..y.m.d.,Collector.Source,Collection.List,Accession.Number,GBIF.ID,observed_on,user_login,url,Notes,taxon_id,Stats.Code)

# Add Stats Code to unmatched Taxa

names(unmatched.iNat.obs.summary)
unmatched.iNat.obs.summary$Stats.Code <- 'VAS'

# Add standardized column headings

names(unmatched.iNat.obs.summary) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

# *TEMPORARY* Modify 'Species' to just include species epithet and 'Subspecies' and 'Variety' to just include varietal name
# Note: These fields will permit more streamlined review by permitting a match between baseline and iNat obs based on these terms

unmatched.iNat.obs.summary$Species <- word(unmatched.iNat.obs.summary$Species, 2)

iNat.obs.summary$taxon_subspecies_name <- word(iNat.obs.summary$taxon_subspecies_name, 3)
iNat.obs.summary$taxon_variety_name <- word(iNat.obs.summary$taxon_variety_name, 3)

unmatched.iNat.obs.summary$Subspecies <- iNat.obs.summary$taxon_subspecies_name[match(unlist(unmatched.iNat.obs.summary$Taxon), iNat.obs.summary$Taxon)]
unmatched.iNat.obs.summary$Variety <- iNat.obs.summary$taxon_variety_name[match(unlist(unmatched.iNat.obs.summary$Taxon), iNat.obs.summary$Taxon)]

# Review dataframes

nrow(iNat.obs.summary)
nrow(Tracheophyta.baseline)
nrow(Tracheophyta.summary.matched)
nrow(unmatched.iNat.obs.summary)

# *TEMPORARY* Add 'Species', 'Hybrid', 'Subspecies', and 'Variety' columns to baseline dataframe (this won't be necessary in future iterations when the standard summary format carries these fields)

other.summary.column.names <- c('Species', 'Hybrid','Subspecies', 'Variety')
other.summary.columns <- data.frame(matrix(ncol=length(other.summary.column.names),nrow=nrow(Tracheophyta.baseline)))
names(other.summary.columns) <- other.summary.column.names
Tracheophyta.review.summary <- cbind(Tracheophyta.baseline,other.summary.columns)
names(Tracheophyta.review.summary)
Tracheophyta.review.summary <- Tracheophyta.review.summary %>% select(Taxon, Taxon.Author, Subtaxon.Author, Common.Name, Kingdom, Phylum, Subphylum, Superclass, Class, Subclass, Superorder, Order, Suborder, Superfamily, Family, Subfamily, Tribe, Genus, Species, Hybrid, Subspecies, Variety, Origin, Status, Reporting.Status, Observation, Collected.Reported..y.m.d., Collector.Source, Collection.List, Accession.Number, GBIF.ID, First.Observed, Observer, iNaturalist.Link, Notes, ID, Stats.Code)
names(Tracheophyta.review.summary) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

# *TEMPORARY* Modify 'Species' to just include species epithet 
# 'Subspecies' and 'Variety' names will be merged through review process
# Note: These fields will permit more streamlined review by permitting a match between baseline and iNat obs based on these terms

Tracheophyta.review.summary$Species <- Tracheophyta.summary.matched$Species[match(unlist(Tracheophyta.review.summary$Taxon), Tracheophyta.summary.matched$Taxon)]

# Merge baseline and unmatched summary for review 

Tracheophyta.review.summary <- rbind(Tracheophyta.review.summary, unmatched.iNat.obs.summary)

# Add column for National Status Ranking (and rename 'Status' to 'Provincial Status')

Tracheophyta.review.summary$National_Status <- NA

Tracheophyta.review.summary <- Tracheophyta.review.summary %>% select('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Status', 'National_Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

names(Tracheophyta.review.summary) <- c('Taxon', 'Taxon Author', 'Subtaxon Author', 'Common Name', 'Kingdom', 'Phylum', 'Subphylum', 'Superclass', 'Class', 'Subclass', 'Superorder', 'Order', 'Suborder', 'Superfamily', 'Family', 'Subfamily', 'Tribe', 'Genus', 'Species', 'Hybrid', 'Subspecies', 'Variety', 'Origin', 'Provincial Status', 'National Status', 'Reporting Status', 'Observation', 'Collected/Reported (y-m-d)', 'Collector/Source', 'Collection/List', 'Accession Number', 'GBIF ID', 'First Observed', 'Observer', 'iNaturalist Link', 'Notes', 'ID', 'Stats Code')

names(Tracheophyta.review.summary)

# Replace NA values with ""

Tracheophyta.review.summary[is.na(Tracheophyta.review.summary)] <- ""

write.csv(Tracheophyta.review.summary, "/Users/andrewsimon/GitHub/bioinformatics/adfsimon-bioinfo/Biodiversity_Galiano/2022/2_review/Plantae_et_Chromista/vascular_plants/Tracheophyta_review_summary.csv")