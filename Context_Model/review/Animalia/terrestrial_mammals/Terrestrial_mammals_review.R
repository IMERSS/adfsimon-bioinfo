# Script to compare iNaturalist observations against a historical baseline

# Set relative paths (https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load packages

library(dplyr)
library(stringr)
library(tidyr)

# Read baseline summary

baseline <- read.csv("summaries/Context_model_terrestrial_mammals_review_summary_2023-12-05.csv")

# Apply standardized field names to baseline

names(baseline)  <- c('Taxon','Taxon.Author','Subtaxon.Author','Common.Name','Kingdom','Phylum',
                      'Subphylum','Superclass','Class','Subclass','Superorder','Order','Suborder',
                      'Superfamily','Family','Subfamily','Tribe','Genus','Species','Hybrid',
                      'Subspecies','Variety','Origin','Provincial.Status','National.Status','Reporting.Status',
                      'Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List',
                      'Accession.Number','GBIF.ID','First.Observed','Observer','iNaturalist.Link','Notes',
                      'ID','Stats.Code')

# TEMP: Update baseline iNaturalist observation id 

# baseline$iNaturalist.Link <- gsub("http://www.inaturalist.org/observations/", "", as.character(baseline$iNaturalist.Link))
# baseline$iNaturalist.Link <- gsub("https://www.inaturalist.org/observations/", "", as.character(baseline$iNaturalist.Link))
# baseline$iNaturalist.Link <- gsub("http://inaturalist.ca/observations/", "", as.character(baseline$iNaturalist.Link))
  
# baseline$iNaturalist.Link <- paste0("iNat:", baseline$iNaturalist.Link)

# Read records

all.records <- read.csv("../../../parse_records/outputs/records_terrestrial_mammals.csv")

# Separate iNat obs vs other records

iNat.obs <- all.records %>% filter(institutionCode == 'iNaturalist')
records <- anti_join(all.records,iNat.obs)

# Summarize by first observed

iNat.obs <- iNat.obs %>% group_by(verbatimScientificName) %>% filter(eventDate == min(eventDate))
records <- records %>% group_by(verbatimScientificName) %>% filter(eventDate == min(eventDate))

# Rename fields in records to correspond with fields in baseline summary

# iNat observations

iNat.obs <- rename(iNat.obs, Taxon = verbatimScientificName)
iNat.obs <- rename(iNat.obs, First.Observed = eventDate)
iNat.obs <- rename(iNat.obs, Observer = recordedBy)
iNat.obs <- rename(iNat.obs, iNaturalist.Link = occurrenceID)

# Other records

records <- rename(records, Taxon = verbatimScientificName)
records <- rename(records, Collected.Reported..y.m.d. = eventDate)
records <- rename(records, Collector.Source = recordedBy)
records <- rename(records, Collection.List = institutionCode)
records <- rename(records, Accession.Number = catalogNumber)
records <- rename(records, GBIF.ID = gbifID)

# Create template dataframe for record / iNat obs summaries that matches with baseline summary dataset

records.fields <- c('Taxon','Taxon.Author','Subtaxon.Author','Common.Name','Kingdom','Phylum',
                                    'Subphylum','Superclass','Class','Subclass','Superorder','Order','Suborder',
                                    'Superfamily','Family','Subfamily','Tribe','Genus','Species','Hybrid',
                                    'Subspecies','Variety','Origin','Provincial.Status','National.Status','Reporting.Status',
                                    'Observation','Collected.Reported..y.m.d.','Collector.Source','Collection.List',
                                    'Accession.Number','GBIF.ID','First.Observed','Observer','iNaturalist.Link','Notes',
                                    'ID','Stats.Code')

# iNaturalist observations

data.frame <- as.data.frame(matrix(ncol = length(records.fields), nrow = nrow(iNat.obs)))
names(data.frame) <- records.fields

data.frame[names(iNat.obs)] <- iNat.obs

iNat.obs <- select(data.frame, c(1:length(records.fields)))

# Other records 

data.frame <- as.data.frame(matrix(ncol = length(records.fields), nrow = nrow(records)))
names(data.frame) <- records.fields

data.frame[names(records)] <- records

records <- select(data.frame, c(1:length(records.fields)))

# Replace NAs with empty strings ""

# First convert logical to character

iNat.obs <- iNat.obs %>% mutate_if(is.logical, as.character)
iNat.obs <- iNat.obs %>% mutate_if(is.character, ~replace_na(.,""))

records <- records %>% mutate_if(is.logical, as.character)
records <-  records %>% mutate_if(is.character, ~replace_na(.,""))

# Join dataframes for manual reconstitution

reconstituted <- rbind(baseline,records,iNat.obs)

# Write review summary 

write.csv(reconstituted, "outputs/Terrestrial_mammal_review_summary.csv", row.names = FALSE)