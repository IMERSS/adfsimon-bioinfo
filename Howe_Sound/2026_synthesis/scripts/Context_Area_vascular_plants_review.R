# Build a Tracheophyta review summary by comparing incoming records
# against an existing baseline summary using the best available taxon ID
# in the records file matched to ID (or equivalent) in the baseline.
#
# This version is adapted to work with records tables like:
# - taxonID
# - scientificName
# - scientificNameAuthorship
# - commonName
# - order_   (instead of order)
# - firstReported / lastReported
# - Source
#
# Key changes in this version:
# - no longer requires iNaturalistTaxonId or iNaturalistTaxonName in records
# - detects the record taxon ID column automatically (prefers taxonID)
# - uses scientificName as the canonical incoming review name when no
#   assigned backbone name column exists
# - handles order_ -> order mapping
# - uses firstReported as the preferred incoming date when available
# - preserves unmatched rows for manual review
# - carries all original collapsed records metadata into unmatched/review outputs
#
# Outputs:
# - Howe_Sound_Tracheophyta_review_summary.csv
# - Howe_Sound_Tracheophyta_unmatched_for_review.csv
# - Howe_Sound_Tracheophyta_records_already_in_baseline.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ----------------------------
# User paths
# ----------------------------

baseline_path <- "/Users/andrewsimon/Desktop/Howe_Sound/summary/Howe_Sound_Tracheophyta_summary_2026-04-20.csv"
records_path  <- "/Users/andrewsimon/Desktop/Howe_Sound/inputs/vascular-plants_FullStudyRegion_curated_Feb2026.csv"
output_dir    <- "/Users/andrewsimon/Desktop/Howe_Sound/outputs"

review_summary_path <- file.path(output_dir, "Howe_Sound_Tracheophyta_review_summary.csv")
unmatched_only_path <- file.path(output_dir, "Howe_Sound_Tracheophyta_unmatched_for_review.csv")
matched_only_path   <- file.path(output_dir, "Howe_Sound_Tracheophyta_records_already_in_baseline.csv")

# ----------------------------
# Helpers
# ----------------------------

read_char_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
}

norm_chr <- function(x) {
  stringr::str_squish(ifelse(is.na(x), "", as.character(x)))
}

pick_first_existing <- function(df, candidates, default = "") {
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) {
    return(rep(default, nrow(df)))
  }
  norm_chr(df[[hit]])
}

detect_baseline_key_col <- function(df) {
  candidates <- c("ID", "taxonID", "TaxonID", "id", "iNaturalistTaxonId", "gbifTaxonID")
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) {
    stop(
      paste0(
        "Could not find a taxon identifier column in the baseline. ",
        "Expected one of: ", paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  hit
}

detect_record_key_col <- function(df) {
  candidates <- c(
    "taxonID", "iNaturalistTaxonId", "assigned_iNaturalistTaxonId",
    "TaxonID", "ID", "id", "TEIS_ID", "CPCAD_ID"
  )
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) {
    stop(
      paste0(
        "Could not find a taxon identifier column in records. ",
        "Expected one of: ", paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  hit
}

detect_record_review_name_col <- function(df) {
  candidates <- c(
    "iNaturalistTaxonName", "assignedScientificName", "acceptedScientificName",
    "scientificName", "Taxon", "scientific_name"
  )
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) {
    stop(
      paste0(
        "Could not find a scientific name column in records. ",
        "Expected one of: ", paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  hit
}

detect_name_col <- function(df) {
  candidates <- c("scientificName", "Taxon", "taxon_name", "scientific_name")
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

detect_genus_col <- function(df) {
  candidates <- c("genus", "Genus")
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

detect_family_col <- function(df) {
  candidates <- c("family", "Family")
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

detect_date_col <- function(df) {
  candidates <- c(
    "firstReported", "firstObserved", "First.Observed", "First_Observed",
    "Collected.Reported..y.m.d.", "eventDate", "observed_on"
  )
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

normalize_date_string <- function(x) {
  x <- norm_chr(x)
  ifelse(stringr::str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$"), x, x)
}

derive_species_field <- function(x) {
  x <- norm_chr(x)
  ifelse(str_count(x, "\\S+") >= 2, word(x, 2), "")
}

derive_subspecies_field <- function(x) {
  x <- norm_chr(x)
  case_when(
    str_detect(x, "\\bsubsp\\.\\b") ~ word(x, 4),
    str_detect(x, "\\bssp\\.\\b") ~ word(x, 4),
    str_count(x, "\\S+") >= 3 & !str_detect(x, "\\b(var\\.|subsp\\.|ssp\\.|f\\.)\\b") ~ word(x, 3),
    TRUE ~ ""
  )
}

derive_variety_field <- function(x) {
  x <- norm_chr(x)
  case_when(
    str_detect(x, "\\bvar\\.\\b") ~ word(x, 4),
    TRUE ~ ""
  )
}

derive_genus_field <- function(x) {
  x <- norm_chr(x)
  ifelse(x == "", "", word(x, 1))
}

# Populate ONLY baseline columns from incoming records.
# Helper review fields are added separately later so they are not lost when
# the baseline lacks them.
build_records_to_baseline <- function(records_df, baseline_cols, record_key_col, record_name_col) {
  out <- as.data.frame(
    matrix("", nrow = nrow(records_df), ncol = length(baseline_cols)),
    stringsAsFactors = FALSE
  )
  names(out) <- baseline_cols
  
  for (col in baseline_cols) {
    out[[col]] <- switch(
      col,
      
      # Core identity
      "ID"      = pick_first_existing(records_df, c(record_key_col, "taxonID", "TaxonID", "ID", "id")),
      "id"      = pick_first_existing(records_df, c(record_key_col, "taxonID", "TaxonID", "ID", "id")),
      "taxonID" = pick_first_existing(records_df, c(record_key_col, "taxonID", "TaxonID", "ID", "id")),
      "TaxonID" = pick_first_existing(records_df, c(record_key_col, "taxonID", "TaxonID", "ID", "id")),
      
      # Canonical review name
      "scientificName" = pick_first_existing(records_df, c(record_name_col, "scientificName")),
      "Taxon"          = pick_first_existing(records_df, c(record_name_col, "scientificName")),
      
      "scientificNameAuthorship" = pick_first_existing(records_df, c("scientificNameAuthorship")),
      "Taxon.Author"             = pick_first_existing(records_df, c("scientificNameAuthorship")),
      "subtaxonAuthorship"       = pick_first_existing(records_df, c("subtaxonAuthorship")),
      "Subtaxon.Author"          = pick_first_existing(records_df, c("subtaxonAuthorship")),
      
      "commonName"  = pick_first_existing(records_df, c("commonName")),
      "Common.Name" = pick_first_existing(records_df, c("commonName")),
      
      # Taxonomy
      "kingdom"     = pick_first_existing(records_df, c("kingdom")),
      "Kingdom"     = pick_first_existing(records_df, c("kingdom")),
      "phylum"      = pick_first_existing(records_df, c("phylum")),
      "Phylum"      = pick_first_existing(records_df, c("phylum")),
      "subphylum"   = pick_first_existing(records_df, c("subphylum")),
      "Subphylum"   = pick_first_existing(records_df, c("subphylum")),
      "superclass"  = pick_first_existing(records_df, c("superclass")),
      "Superclass"  = pick_first_existing(records_df, c("superclass")),
      "class"       = pick_first_existing(records_df, c("class")),
      "Class"       = pick_first_existing(records_df, c("class")),
      "subclass"    = pick_first_existing(records_df, c("subclass")),
      "Subclass"    = pick_first_existing(records_df, c("subclass")),
      "superorder"  = pick_first_existing(records_df, c("superorder")),
      "Superorder"  = pick_first_existing(records_df, c("superorder")),
      "order"       = pick_first_existing(records_df, c("order", "order_")),
      "Order"       = pick_first_existing(records_df, c("order", "order_")),
      "suborder"    = pick_first_existing(records_df, c("suborder")),
      "Suborder"    = pick_first_existing(records_df, c("suborder")),
      "infraorder"  = pick_first_existing(records_df, c("infraorder")),
      "superfamily" = pick_first_existing(records_df, c("superfamily")),
      "Superfamily" = pick_first_existing(records_df, c("superfamily")),
      "family"      = pick_first_existing(records_df, c("family")),
      "Family"      = pick_first_existing(records_df, c("family")),
      "subfamily"   = pick_first_existing(records_df, c("subfamily")),
      "Subfamily"   = pick_first_existing(records_df, c("subfamily")),
      "tribe"       = pick_first_existing(records_df, c("tribe")),
      "Tribe"       = pick_first_existing(records_df, c("tribe")),
      "genus"       = pick_first_existing(records_df, c("genus")),
      "Genus"       = pick_first_existing(records_df, c("genus")),
      "species"          = pick_first_existing(records_df, c("specificEpithet", "species")),
      "Species"          = pick_first_existing(records_df, c("specificEpithet", "species")),
      "specificEpithet"  = pick_first_existing(records_df, c("specificEpithet", "species")),
      "hybrid"      = pick_first_existing(records_df, c("hybrid"), default = ""),
      "Hybrid"      = pick_first_existing(records_df, c("hybrid"), default = ""),
      "subspecies"  = pick_first_existing(records_df, c("subspecies")),
      "Subspecies"  = pick_first_existing(records_df, c("subspecies")),
      "variety"     = pick_first_existing(records_df, c("variety")),
      "Variety"     = pick_first_existing(records_df, c("variety")),
      
      # Status / metadata
      "nameStatus"             = pick_first_existing(records_df, c("nameStatus")),
      "Origin"                 = pick_first_existing(records_df, c("establishmentMeans", "origin")),
      "origin"                 = pick_first_existing(records_df, c("establishmentMeans", "origin")),
      "Provincial.Status"      = pick_first_existing(records_df, c("provincialStatus", "provincial_status", "Provincial.Status")),
      "provincial_status"      = pick_first_existing(records_df, c("provincialStatus", "provincial_status", "Provincial.Status")),
      "provincial_status_norm" = pick_first_existing(records_df, c("provincial_status_norm")),
      "bc_list"                = pick_first_existing(records_df, c("bc_list")),
      "residencyStatus"        = pick_first_existing(records_df, c("residencyStatus")),
      "National.Status"        = pick_first_existing(records_df, c("nationalStatus", "national_status", "National.Status")),
      "national_status"        = pick_first_existing(records_df, c("nationalStatus", "national_status", "National.Status")),
      "national_status_clean"  = pick_first_existing(records_df, c("national_status_clean")),
      "national_status_norm"   = pick_first_existing(records_df, c("national_status_norm")),
      "national_status_check"  = pick_first_existing(records_df, c("national_status_check")),
      
      # Observation metadata mapped into common baseline field variants
      "Reporting.Status" = pick_first_existing(records_df, c("reportingStatus"), default = "new"),
      "reportingStatus"  = pick_first_existing(records_df, c("reportingStatus"), default = "new"),
      "Observation"      = "context record",
      "observation"      = "context record",
      
      "Collected.Reported..y.m.d." = pick_first_existing(records_df, c("firstReported", "eventDate", "dateIdentified", "firstObserved")),
      "First.Observed"             = pick_first_existing(records_df, c("firstReported", "eventDate", "dateIdentified", "firstObserved")),
      "First_Observed"             = pick_first_existing(records_df, c("firstReported", "eventDate", "dateIdentified", "firstObserved")),
      "firstObserved"              = pick_first_existing(records_df, c("firstReported", "eventDate", "dateIdentified", "firstObserved")),
      "firstReported"              = pick_first_existing(records_df, c("firstReported", "eventDate", "dateIdentified", "firstObserved")),
      
      "Collector.Source" = pick_first_existing(records_df, c("firstReportedSource", "Source", "Source_Ref")),
      "collectorSource"  = pick_first_existing(records_df, c("firstReportedSource", "Source", "Source_Ref")),
      "sources"          = pick_first_existing(records_df, c("Source"), default = "records"),
      "source"           = pick_first_existing(records_df, c("Source"), default = "records"),
      
      "Collection.List" = pick_first_existing(records_df, c("firstReportedSource", "Source_Ref")),
      "collectionList"  = pick_first_existing(records_df, c("firstReportedSource", "Source_Ref")),
      
      "Accession.Number" = pick_first_existing(records_df, c("firstReportedCollectionNumber")),
      "accessionNumber"  = pick_first_existing(records_df, c("firstReportedCollectionNumber")),
      
      "GBIF.ID" = pick_first_existing(records_df, c("firstReportedGBIF", "lastReportedGBIF")),
      "gbifID"  = pick_first_existing(records_df, c("firstReportedGBIF", "lastReportedGBIF")),
      
      "Observer" = pick_first_existing(records_df, c("firstReportedBy", "lastReportedBy")),
      "observer" = pick_first_existing(records_df, c("firstReportedBy", "lastReportedBy")),
      
      "iNaturalist.Link" = pick_first_existing(records_df, c("iNaturalistLink", "inaturalistLink", "linkName")),
      "linkName"         = pick_first_existing(records_df, c("linkName", "iNaturalistLink", "inaturalistLink")),
      
      "Notes" = pick_first_existing(records_df, c("notes")),
      "notes" = pick_first_existing(records_df, c("notes")),
      
      "Stats.Code" = pick_first_existing(records_df, c("statsCode"), default = "VAS"),
      "stats_code" = pick_first_existing(records_df, c("statsCode"), default = "VAS"),
      
      "iNaturalistTaxonId"   = pick_first_existing(records_df, c("iNaturalistTaxonId", "taxonID")),
      "iNaturalistTaxonName" = pick_first_existing(records_df, c("iNaturalistTaxonName", record_name_col, "scientificName")),
      
      rep("", nrow(records_df))
    )
  }
  
  sci <- if ("scientificName" %in% names(out)) norm_chr(out$scientificName) else if ("Taxon" %in% names(out)) norm_chr(out$Taxon) else rep("", nrow(out))
  
  if ("Genus" %in% names(out) && all(norm_chr(out$Genus) == "")) out$Genus <- derive_genus_field(sci)
  if ("genus" %in% names(out) && all(norm_chr(out$genus) == "")) out$genus <- derive_genus_field(sci)
  
  if ("Species" %in% names(out) && all(norm_chr(out$Species) == "")) out$Species <- derive_species_field(sci)
  if ("species" %in% names(out) && all(norm_chr(out$species) == "")) out$species <- derive_species_field(sci)
  if ("specificEpithet" %in% names(out) && all(norm_chr(out$specificEpithet) == "")) out$specificEpithet <- derive_species_field(sci)
  
  if ("Subspecies" %in% names(out) && all(norm_chr(out$Subspecies) == "")) out$Subspecies <- derive_subspecies_field(sci)
  if ("subspecies" %in% names(out) && all(norm_chr(out$subspecies) == "")) out$subspecies <- derive_subspecies_field(sci)
  
  if ("Variety" %in% names(out) && all(norm_chr(out$Variety) == "")) out$Variety <- derive_variety_field(sci)
  if ("variety" %in% names(out) && all(norm_chr(out$variety) == "")) out$variety <- derive_variety_field(sci)
  
  out[] <- lapply(out, norm_chr)
  out
}

# ----------------------------
# Read inputs
# ----------------------------

cat("Reading baseline: ", baseline_path, "\n", sep = "")
cat("Reading records:  ", records_path, "\n", sep = "")

baseline <- read_char_csv(baseline_path)
records  <- read_char_csv(records_path)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

baseline[] <- lapply(baseline, norm_chr)
records[]  <- lapply(records, norm_chr)

baseline_key_col   <- detect_baseline_key_col(baseline)
baseline_name_col  <- detect_name_col(baseline)
baseline_genus_col <- detect_genus_col(baseline)
baseline_family_col <- detect_family_col(baseline)
baseline_date_col  <- detect_date_col(baseline)

record_key_col  <- detect_record_key_col(records)
record_name_col <- detect_record_review_name_col(records)

cat("Baseline key column detected: ", baseline_key_col, "\n", sep = "")
if (!is.na(baseline_name_col))   cat("Baseline name column detected: ", baseline_name_col, "\n", sep = "")
if (!is.na(baseline_genus_col))  cat("Baseline genus column detected: ", baseline_genus_col, "\n", sep = "")
if (!is.na(baseline_family_col)) cat("Baseline family column detected: ", baseline_family_col, "\n", sep = "")
if (!is.na(baseline_date_col))   cat("Baseline date column detected: ", baseline_date_col, "\n", sep = "")
cat("Records key column detected: ", record_key_col, "\n", sep = "")
cat("Records review name column detected: ", record_name_col, "\n", sep = "")

required_record_fields <- c(record_key_col, "scientificName")
missing_required <- setdiff(required_record_fields, names(records))
if (length(missing_required) > 0) {
  stop(
    paste0(
      "Records file is missing required column(s): ",
      paste(missing_required, collapse = ", ")
    ),
    call. = FALSE
  )
}

if ("phylum" %in% names(records)) {
  records <- records %>% filter(phylum == "Tracheophyta" | phylum == "")
}

# ----------------------------
# Collapse to one earliest row per taxon ID
# ----------------------------

records_collapsed <- records %>%
  mutate(
    record_taxonID = norm_chr(.data[[record_key_col]]),
    assigned_name = norm_chr(.data[[record_name_col]]),
    verbatim_name = norm_chr(scientificName),
    rank_for_review = tolower(pick_first_existing(., c("taxonRank", "taxon_rank"))),
    eventDate_review = normalize_date_string(
      pick_first_existing(., c("firstReported", "eventDate", "dateIdentified", "firstObserved"))
    ),
    gbifID_sort = pick_first_existing(., c("firstReportedGBIF", "lastReportedGBIF"))
  ) %>%
  filter(record_taxonID != "") %>%
  arrange(record_taxonID, eventDate_review, gbifID_sort, assigned_name, verbatim_name) %>%
  group_by(record_taxonID) %>%
  slice(1) %>%
  ungroup()

cat("Unique incoming taxonIDs after collapsing: ", nrow(records_collapsed), "\n", sep = "")

# ----------------------------
# Convert collapsed records into baseline schema
# ----------------------------

incoming_std <- build_records_to_baseline(
  records_df = records_collapsed,
  baseline_cols = names(baseline),
  record_key_col = record_key_col,
  record_name_col = record_name_col
)

incoming_std[[baseline_key_col]] <- norm_chr(records_collapsed$record_taxonID)

incoming_std$verbatimScientificName <- norm_chr(records_collapsed$verbatim_name)
incoming_std$assignedNameForReview  <- norm_chr(records_collapsed$assigned_name)
incoming_std$originalScientificName <- norm_chr(records_collapsed$verbatim_name)
incoming_std$taxonRank_review       <- norm_chr(records_collapsed$rank_for_review)
incoming_std$firstReported_review   <- norm_chr(records_collapsed$eventDate_review)
incoming_std$recordedBy_review      <- pick_first_existing(records_collapsed, c("firstReportedBy", "lastReportedBy"))
incoming_std$source_review          <- pick_first_existing(records_collapsed, c("Source"), default = "records")
incoming_std$gbifID_review          <- pick_first_existing(records_collapsed, c("firstReportedGBIF", "lastReportedGBIF"))

# Carry forward the full collapsed incoming record metadata for unmatched/new taxa.
# This ensures that truly new rows in the review summary retain scientificName and
# all parallel source metadata from the incoming records table.
for (col in names(records_collapsed)) {
  if (!(col %in% names(incoming_std))) {
    incoming_std[[col]] <- norm_chr(records_collapsed[[col]])
  }
}

# Preserve a verbatim copy of the incoming scientific name even if the baseline
# schema also contains a scientificName field that may be harmonized above.
if (!("records_scientificName" %in% names(incoming_std)) && "scientificName" %in% names(records_collapsed)) {
  incoming_std$records_scientificName <- norm_chr(records_collapsed$scientificName)
}

if ("scientificName" %in% names(incoming_std)) incoming_std$scientificName <- norm_chr(incoming_std$assignedNameForReview)
if ("Taxon" %in% names(incoming_std)) incoming_std$Taxon <- norm_chr(incoming_std$assignedNameForReview)

if ("firstReported" %in% names(incoming_std) && all(norm_chr(incoming_std$firstReported) == "")) {
  incoming_std$firstReported <- norm_chr(incoming_std$firstReported_review)
}
if ("firstObserved" %in% names(incoming_std) && all(norm_chr(incoming_std$firstObserved) == "")) {
  incoming_std$firstObserved <- norm_chr(incoming_std$firstReported_review)
}
if ("First.Observed" %in% names(incoming_std) && all(norm_chr(incoming_std$First.Observed) == "")) {
  incoming_std$First.Observed <- norm_chr(incoming_std$firstReported_review)
}
if ("Collected.Reported..y.m.d." %in% names(incoming_std) && all(norm_chr(incoming_std[["Collected.Reported..y.m.d."]]) == "")) {
  incoming_std[["Collected.Reported..y.m.d."]] <- norm_chr(incoming_std$firstReported_review)
}
if ("sources" %in% names(incoming_std) && all(norm_chr(incoming_std$sources) == "")) {
  incoming_std$sources <- norm_chr(incoming_std$source_review)
}
if ("source" %in% names(incoming_std) && all(norm_chr(incoming_std$source) == "")) {
  incoming_std$source <- norm_chr(incoming_std$source_review)
}

incoming_std[] <- lapply(incoming_std, norm_chr)

# ----------------------------
# Match baseline vs incoming by ID / taxon key
# ----------------------------

baseline_keys <- baseline %>%
  transmute(match_taxonID = norm_chr(.data[[baseline_key_col]])) %>%
  distinct() %>%
  filter(match_taxonID != "")

incoming_std <- incoming_std %>%
  mutate(match_taxonID = norm_chr(.data[[baseline_key_col]]))

matched_records <- incoming_std %>%
  semi_join(baseline_keys, by = "match_taxonID")

unmatched_records <- incoming_std %>%
  anti_join(baseline_keys, by = "match_taxonID")

cat("Matched by taxonID: ", nrow(matched_records), "\n", sep = "")
cat("Unmatched by taxonID (retained as new review rows): ", nrow(unmatched_records), "\n", sep = "")

# ----------------------------
# Build baseline lookups for context flags
# ----------------------------

if (!is.na(baseline_name_col)) {
  baseline_taxon_lookup <- baseline %>%
    transmute(
      baseline_name = norm_chr(.data[[baseline_name_col]]),
      baseline_first_observed = if (!is.na(baseline_date_col)) norm_chr(.data[[baseline_date_col]]) else ""
    ) %>%
    distinct() %>%
    filter(baseline_name != "")
} else {
  baseline_taxon_lookup <- data.frame(
    baseline_name = character(),
    baseline_first_observed = character(),
    stringsAsFactors = FALSE
  )
}

if (!is.na(baseline_genus_col)) {
  baseline_genus_lookup <- baseline %>%
    transmute(genus_lookup = norm_chr(.data[[baseline_genus_col]])) %>%
    distinct() %>%
    filter(genus_lookup != "") %>%
    mutate(genus_in_baseline = TRUE)
} else {
  baseline_genus_lookup <- data.frame(
    genus_lookup = character(),
    genus_in_baseline = logical(),
    stringsAsFactors = FALSE
  )
}

if (!is.na(baseline_family_col)) {
  baseline_family_lookup <- baseline %>%
    transmute(family_lookup = norm_chr(.data[[baseline_family_col]])) %>%
    distinct() %>%
    filter(family_lookup != "") %>%
    mutate(family_in_baseline = TRUE)
} else {
  baseline_family_lookup <- data.frame(
    family_lookup = character(),
    family_in_baseline = logical(),
    stringsAsFactors = FALSE
  )
}

incoming_name_col <- "assignedNameForReview"
incoming_genus_col <- if (!is.na(detect_genus_col(unmatched_records))) detect_genus_col(unmatched_records) else NA_character_
incoming_family_col <- if (!is.na(detect_family_col(unmatched_records))) detect_family_col(unmatched_records) else NA_character_

if (is.na(incoming_genus_col)) {
  unmatched_records$.__genus_tmp__ <- derive_genus_field(unmatched_records[[incoming_name_col]])
  incoming_genus_col <- ".__genus_tmp__"
}
if (is.na(incoming_family_col)) {
  unmatched_records$.__family_tmp__ <- ""
  incoming_family_col <- ".__family_tmp__"
}

# ----------------------------
# Review helper / triage columns
# ----------------------------

unmatched_records <- unmatched_records %>%
  left_join(
    baseline_taxon_lookup,
    by = c("assignedNameForReview" = "baseline_name")
  ) %>%
  left_join(
    baseline_genus_lookup,
    by = setNames("genus_lookup", incoming_genus_col)
  ) %>%
  left_join(
    baseline_family_lookup,
    by = setNames("family_lookup", incoming_family_col)
  ) %>%
  mutate(
    genus_in_baseline = ifelse(is.na(genus_in_baseline), FALSE, genus_in_baseline),
    family_in_baseline = ifelse(is.na(family_in_baseline), FALSE, family_in_baseline),
    resolution_level = case_when(
      originalScientificName == "" ~ "unresolved",
      taxonRank_review %in% c("species", "subspecies", "variety", "form", "hybrid") ~ "species_or_lower",
      taxonRank_review == "genus" ~ "genus_only",
      TRUE ~ "higher_taxon"
    ),
    novelty_flag = case_when(
      resolution_level == "species_or_lower" & !genus_in_baseline ~ "novel_genus",
      resolution_level == "species_or_lower" & genus_in_baseline ~ "novel_species_in_known_genus",
      resolution_level == "genus_only" & !genus_in_baseline ~ "novel_genus",
      resolution_level == "genus_only" & genus_in_baseline ~ "known_genus_only",
      resolution_level == "higher_taxon" & !family_in_baseline ~ "novel_higher_taxon",
      resolution_level == "higher_taxon" & family_in_baseline ~ "known_higher_taxon_only",
      TRUE ~ ""
    ),
    date_flag = case_when(
      baseline_first_observed == "" ~ "new_taxon",
      firstReported_review == "" ~ "",
      firstReported_review < baseline_first_observed ~ "earlier_than_baseline_name_match",
      firstReported_review == baseline_first_observed ~ "same_date_as_baseline_name_match",
      firstReported_review > baseline_first_observed ~ "later_than_baseline_name_match",
      TRUE ~ ""
    ),
    review_group = case_when(
      resolution_level == "species_or_lower" ~ "deep_review",
      date_flag %in% c("earlier_than_baseline_name_match", "later_than_baseline_name_match", "same_date_as_baseline_name_match") ~ "date_precedence_check",
      resolution_level == "genus_only" & novelty_flag == "novel_genus" ~ "review_genus_novelty",
      resolution_level == "genus_only" ~ "batch_genus_only",
      TRUE ~ "low_priority"
    ),
    auto_exclusion_note = case_when(
      review_group == "batch_genus_only" ~ "genus",
      review_group == "low_priority" & resolution_level == "higher_taxon" ~ "higher",
      review_group == "low_priority" & resolution_level == "unresolved" ~ "unresolved",
      review_group == "low_priority" ~ "other",
      TRUE ~ ""
    ),
    Curator.Decision = "",
    Curator.Notes = ""
  )

unmatched_review_targets <- unmatched_records

# ----------------------------
# Combine with baseline
# ----------------------------

review_cols <- c(
  "verbatimScientificName",
  "assignedNameForReview",
  "originalScientificName",
  "taxonRank_review",
  "firstReported_review",
  "recordedBy_review",
  "source_review",
  "gbifID_review",
  "baseline_first_observed",
  "genus_in_baseline",
  "family_in_baseline",
  "resolution_level",
  "novelty_flag",
  "date_flag",
  "review_group",
  "auto_exclusion_note",
  "Curator.Decision",
  "Curator.Notes"
)

baseline_review <- baseline
for (col in review_cols) baseline_review[[col]] <- ""

common_cols <- union(names(baseline_review), names(unmatched_review_targets))

for (col in setdiff(common_cols, names(baseline_review))) {
  baseline_review[[col]] <- ""
}
for (col in setdiff(common_cols, names(unmatched_review_targets))) {
  unmatched_review_targets[[col]] <- ""
}

baseline_review <- baseline_review[, common_cols, drop = FALSE]
unmatched_review_targets <- unmatched_review_targets[, common_cols, drop = FALSE]

baseline_review[] <- lapply(baseline_review, as.character)
unmatched_review_targets[] <- lapply(unmatched_review_targets, as.character)

review_summary <- dplyr::bind_rows(baseline_review, unmatched_review_targets)
review_summary[] <- lapply(review_summary, norm_chr)
review_summary[is.na(review_summary)] <- ""

# ----------------------------
# Write outputs
# ----------------------------

write.csv(review_summary, review_summary_path, row.names = FALSE)
write.csv(unmatched_review_targets, unmatched_only_path, row.names = FALSE)
write.csv(matched_records, matched_only_path, row.names = FALSE)

# ----------------------------
# Diagnostics
# ----------------------------

cat("\nDiagnostics\n")
cat("-----------\n")
cat("Baseline rows: ", nrow(baseline), "\n", sep = "")
cat("Unique incoming taxonIDs after collapsing: ", nrow(records_collapsed), "\n", sep = "")
cat("Matched by taxonID: ", nrow(matched_records), "\n", sep = "")
cat("Unmatched by taxonID retained for review: ", nrow(unmatched_review_targets), "\n", sep = "")
cat("Combined review summary rows: ", nrow(review_summary), "\n", sep = "")

cat("\nReview groups among unmatched taxa:\n")
print(table(unmatched_review_targets$review_group, useNA = "ifany"))

cat("\nResolution levels among unmatched taxa:\n")
print(table(unmatched_review_targets$resolution_level, useNA = "ifany"))

cat("\nWrote:\n")
cat(review_summary_path, "\n")
cat(unmatched_only_path, "\n")
cat(matched_only_path, "\n")


length(unique(records$scientificName))
