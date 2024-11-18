# Load packages

library(dplyr)

# Prepare matrix of Galiano Island phytoplankton based on morphological and dna evidence

phytoplankton.summary <- read.csv("/Users/andrewsimon/GitHub/adfsimon-bioinfo/Biodiversity_Galiano/consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/phytoplankton_review/Galiano_Island_marine_phytoplankton_summary_2024-11-17.csv")
records <- read.csv("/Users/andrewsimon/GitHub/adfsimon-bioinfo/Biodiversity_Galiano/consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2024-11-17.csv")

# Assuming 'name' is the column that contains the species names in both dataframes
phytoplankton.records <- records %>%
  filter(scientificName %in% phytoplankton.summary$scientificName)

# Prepare matrix of records from unique data sources

# Create a list of unique species names and datasets
species <- unique(phytoplankton.records$scientificName)
datasets <- unique(phytoplankton.records$datasetName)

# Initialize an empty matrix
presence_matrix <- matrix(0, nrow = length(species), ncol = length(datasets),
                          dimnames = list(species, datasets))

# Populate the matrix
for (i in seq_len(nrow(phytoplankton.records))) {
  species_name <- phytoplankton.records$scientificName[i]
  dataset_name <- phytoplankton.records$datasetName[i]
  presence_matrix[species_name, dataset_name] <- 1
}

# Convert the matrix to a data frame for easier viewing (optional)
table <- as.data.frame(presence_matrix)

# Replace a specific set of column names
colnames(table)[1:5] <- c("iNaturalist", "BioBlitz 2023", "Zostera 2020", "Miner's Bay 2022", "Plankton 2022-2023")

table <- table[, c(1, 3, 4, 5, 2)]

write.csv(phytoplankton.records, "/Users/andrewsimon/GitHub/adfsimon-bioinfo/Biodiversity_Galiano/consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/phytoplankton_review/Galiano_Island_phytoplankton_records.csv")
write.csv(table, "/Users/andrewsimon/GitHub/adfsimon-bioinfo/Biodiversity_Galiano/consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/phytoplankton_review/Galiano_Island_phytoplankton_taxon_x_sample_matrix.csv")

