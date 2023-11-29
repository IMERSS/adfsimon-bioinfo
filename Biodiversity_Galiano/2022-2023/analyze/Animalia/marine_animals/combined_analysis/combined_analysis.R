# Couldn't get rstudioapi method to work - instead https://rprojroot.r-lib.org/reference/find_root_file.html
library(rprojroot)
library(stringr)
library(sf)
library(tidyr)
library(dplyr)

projroot <- find_rstudio_root_file()

setwd(str_glue("{projroot}/Biodiversity_Galiano/2022-2023"))

# Load summary and catalog data

summary <- read.csv("review/Animalia/marine_animals/summaries/Galiano_marine_animals_summary_2023-11-01.csv")

hulq.summary <- read.csv("analyze/Animalia/marine_animals/combined_analysis/inputs/tabular_data/reintegrated-hulq.csv")

animals <- read.csv("consolidate_records/Animalia/marine_animals/synthesized/Galiano_marine_animal_records_consolidated-2023-11-03.csv")

algae <- read.csv("consolidate_records/Plantae_et_Chromista/macroalgae_zooplankton_and_phytoplankton/synthesized/Galiano_marine_algae_records_consolidated_2023-04-22.csv")

# Note: algae are called here to define a grid with an extent that includes all marine observations, to ensure a normalised extent
# for all marine diversity choropleths

# Create grid

# Create CRS object

EPSG.4326 <- st_crs(4326)

# Drop rows lacking coordinates

animals.points <- animals %>% drop_na(decimalLatitude)

algae.points <- algae %>% drop_na(decimalLatitude)

# Convert records to sf points

animals.points <- st_as_sf(animals.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

algae.points <- st_as_sf(algae.points, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)

# Transform to NAD84 UTM Zone 10

# Create CRS object

EPSG.32610 <- st_crs(32610)

# Reproject to NAD83 UTM Zone 10

# Point data (for gridded catalogs, with each record assigned cell_id)

animals.points <- st_transform(animals.points, crs = st_crs(EPSG.32610))

algae.points <- st_transform(algae.points, crs = st_crs(EPSG.32610))

# Combine points to create grid

points <- rbind(animals.points, algae.points)

# Set cell size

cs <- c(1000, 1000)

# Create grid

area.grid = st_make_grid(points, what = "polygons", square = TRUE, cellsize = cs)

sf::st_length(area.grid)
st_crs(area.grid)$proj4string

# Add grid ID

grid = st_sf(area.grid) %>%
  mutate(cell_id = 1:length(lengths(area.grid)))

annelida        <- list(summary = summary %>% filter(phylum == 'Annelida'), records = animals %>% filter(phylum == 'Annelida'))
brachiopoda     <- list(summary = summary %>% filter(phylum == 'Brachiopoda'), records = animals %>% filter(phylum == 'Brachiopoda'))
bryozoa         <- list(summary = summary %>% filter(phylum == 'Bryozoa'), records = animals %>% filter(phylum == 'Bryozoa'))
chaetognatha    <- list(summary = summary %>% filter(phylum == 'Chaetognatha'), records = animals %>% filter(phylum == 'Chaetognatha'))
cnidaria        <- list(summary = summary %>% filter(phylum == 'Cnidaria'), records = animals %>% filter(phylum == 'Cnidaria'))
crustacea       <- list(summary = summary %>% filter(subphylum == 'Crustacea'), records = animals %>% filter(subphylum == 'Crustacea'))
ctenophora      <- list(summary = summary %>% filter(phylum == 'Ctenophora'), records = animals %>% filter(phylum == 'Ctenophora'))
echinodermata   <- list(summary = summary %>% filter(phylum == 'Echinodermata'), records = animals %>% filter(phylum == 'Echinodermata'))
entoprocta      <- list(summary = summary %>% filter(phylum == 'Entoprocta'), records = animals %>% filter(phylum == 'Entoprocta'))
mammalia        <- list(summary = summary %>% filter(class == 'Mammalia'), records = animals %>% filter(class == 'Mammalia'))
mollusca        <- list(summary = summary %>% filter(phylum == 'Mollusca'), records = animals %>% filter(phylum == 'Mollusca'))
nemertea        <- list(summary = summary %>% filter(phylum == 'Nemertea'), records = animals %>% filter(phylum == 'Nemertea'))
phoronida       <- list(summary = summary %>% filter(phylum == 'Phoronida'), records = animals %>% filter(phylum == 'Phoronida'))
platyhelminthes <- list(summary = summary %>% filter(phylum == 'Platyhelminthes'), records = animals %>% filter(phylum == 'Platyhelminthes'))
porifera        <- list(summary = summary %>% filter(phylum == 'Porifera'), records = animals %>% filter(phylum == 'Porifera'))
sipuncula       <- list(summary = summary %>% filter(phylum == 'Sipuncula'), records = animals %>% filter(phylum == 'Sipuncula'))
tunicata        <- list(summary = summary %>% filter(subphylum == 'Tunicata'), records = animals %>% filter(subphylum == 'Tunicata'))
fishes          <- list(summary = summary %>% filter(class == 'Actinopterygii'| class == 'Elasmobranchii'), records = animals %>% filter(class == 'Actinopterygii'| class == 'Elasmobranchii'))


allTaxaRecords <- list(annelida = annelida, brachiopoda = brachiopoda, bryozoa = bryozoa, chaetognatha = chaetognatha, 
                       cnidaria = cnidaria, crustacea = crustacea, ctenophora = ctenophora, echinodermata = echinodermata,
                       entoprocta = entoprocta, mammalia = mammalia, mollusca = mollusca, nemertea = nemertea, platyhelminthes = platyhelminthes, 
                       phoronida = phoronida, porifera = porifera, sipuncula = sipuncula, tunicata = tunicata, fishes = fishes)

#allTaxaRecords <- list(bryozoa = bryozoa)

# Process one status (i.e. new, reported, confirmed) for one taxon
#   recs - The upstream records for the taxon, including elements `summary` and `records`
#   taxon - The taxon name
#   grid - The grid to which the records are to be binned
#   status - The status to be processed (new, reported, confirmed, hulq, all)
# Returns a structure containing all gridded data, and various other preparatory fields - unique taxa, counts, etc.
process_one_status <- function (recs, taxon, grid, status) {
    togo <- list()

    togo$summary <- recs$summary 
    if (status == "all") {
        togo$summary <- recs$summary 
    } else if (status == "hulq") {
        togo$summary <- togo$summary %>% filter(scientificName %in% hulq.summary$Taxon.name)
    } else {
        togo$summary <- recs$summary %>% filter(str_detect(reportingStatus, status))
    }
    # Rename this field back to what .Rmd in galiano-marine-atlas is expecting
    togo$summary <- togo$summary %>% rename(iNatObservationStatus = observation)

    togo$taxa <- unique(togo$summary$scientificName)

    togo$taxa.records <- recs$records %>% filter(scientificName %in% togo$taxa)
    togo$taxa.records <- togo$taxa.records %>% drop_na(decimalLatitude)
    
    # Convert records to sf points using WGS 84
    
    togo$taxa.points <- st_as_sf(togo$taxa.records, coords = c("decimalLongitude", "decimalLatitude"), crs = EPSG.4326)
    
    # Reproject to NAD83 UTM Zone 10

    togo$taxa.points <- st_transform(togo$taxa.points, crs = st_crs(EPSG.32610))

    # Generate gridded dataframes (each record assigned cell_id)
        
    togo$records.gridded <- togo$taxa.points %>% st_join(grid)
    
    togo$records.gridded <- as.data.frame(togo$records.gridded)
    
    togo$records.gridded$geometry <- NULL
    
    # Summarized points (for choropleths)

    # Method 2 from https://statisticsglobe.com/count-unique-values-by-group-in-r for computing richness
    # Generate species richness choropleths
    togo$taxa.points.sum <- togo$records.gridded %>% group_by(cell_id) %>% summarize(richness = n_distinct(scientificName))
    
    togo$grid <- merge(grid, togo$taxa.points.sum, by = "cell_id")

    if (nrow(togo$grid) > 0) {
        togo$grid$status <- status
    }
    
    return(togo)
}

# Write an SHP file, ensuring to delete anything at the target path first
write_grid <- function (grid, path) {
    unlink(path, recursive=TRUE)
    st_write(grid, path, driver = "ESRI Shapefile")
}

# Process one taxon's worth of records, calling process_one_status for each status, writing SHP and CSV files for each resulting status
process_one_taxon <- function (recs, taxon, grid) {
  print(str_glue("Processing {taxon}"))
  # Subset historic, confirmed and new records
  togo <- list(new = process_one_status(recs, taxon, grid, "new"),
               confirmed = process_one_status(recs, taxon, grid, "confirmed"), 
               reported = process_one_status(recs, taxon, grid, "reported"),
               hulq = process_one_status(recs, taxon, grid, "hulq"),
               all = process_one_status(recs, taxon, grid, "all"))
  
  #plot(togo$reported$grid)

  # Write choropleths
  
  write_grid(togo$confirmed$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_confirmed_grid"))
  write_grid(togo$new$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_new_grid"))
  write_grid(togo$reported$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_reported_grid"))
  write_grid(togo$hulq$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_hulq_grid"))
  write_grid(togo$all$grid, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/vectors/{taxon}_all_grid"))
  
  # Write gridded dataframes
  
  write.csv(togo$confirmed$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_confirmed_records_gridded.csv"), row.names = FALSE)
  write.csv(togo$new$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_new_records_gridded.csv"), row.names = FALSE)
  write.csv(togo$reported$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_reported_records_gridded.csv"), row.names = FALSE)
  write.csv(togo$hulq$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_hulq_records_gridded.csv"), row.names = FALSE)
  write.csv(togo$all$records.gridded, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_all_records_gridded.csv"), row.names = FALSE)

  # Write summary

  write.csv(togo$all$summary, str_glue("analyze/Animalia/marine_animals/combined_analysis/outputs/tabular_data/{taxon}_summary.csv"), row.names = FALSE)  
    
  return (togo)
}

allTaxaGridded = list()

for (taxon in names(allTaxaRecords)) {
  taxonGridded <- process_one_taxon(allTaxaRecords[[taxon]], taxon, grid)
  allTaxaGridded[[taxon]] <- taxonGridded
}
