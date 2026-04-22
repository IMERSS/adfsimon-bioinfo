# ============================================================
# LOAD REVIEWED SUMMARIES, PERFORM QC ON OCCURRENCE DATA, 
# MERGE CONSERVATION RANKINGS, WRITE RANKED WORKING FILES, 
# THEN LOAD UPDATED CURATED FILES
# AND WRITE FINAL STANDARDIZED CHECKLIST CSVs
# ============================================================

# Set relative path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)
library(readr)
library(fs)

# ============================================================
# PATHS
# ============================================================

reviewed_dir <- "/Users/andrewsimon/Desktop/Howe_Sound/reviewed"
rankings_file <- "/Users/andrewsimon/Desktop/Howe_Sound/rankings/bc-cdc_full_2026-04-10-assigned.csv"
ranked_dir <- "/Users/andrewsimon/Desktop/Howe_Sound/ranked"

lists_dir <- "/Users/andrewsimon/Desktop/Howe_Sound/lists"
final_csv_dir <- file.path(lists_dir, "curated_csv")
qc_dir <- file.path(lists_dir, "qc")

raw_catalog_file <- "/Users/andrewsimon/Desktop/Howe_Sound/inputs/gbif-howe-project-all-category-2026-03-06-assigned-full.csv"
raw_locality_qc_dir <- file.path(qc_dir, "raw_locality_qc")

# ============================================================
# STANDARDIZED FINAL CURATED CSV FIELDS
# ============================================================

base_export_fields <- c(
  "scientificName",
  "scientificNameAuthorship",
  "commonName",
  "kingdom",
  "phylum",
  "subphylum",
  "superclass",
  "class",
  "subclass",
  "superorder",
  "order",
  "suborder",
  "infraorder",
  "superfamily",
  "family",
  "subfamily",
  "tribe",
  "genus",
  "species",
  "variety",
  "subspecies",
  "hybrid",
  "provincial_status",
  "Prov Status Review Date",
  "bc_list",
  "national_status",
  "COSEWIC Date",
  "SARA Schedule",
  "SARA_status",
  "SARA Date"
)

bird_extra_fields <- c("residencyStatus")

report_field_map <- list(
  "BIRDS" = c(base_export_fields, bird_extra_fields),
  "TERRESTRIAL MAMMALS" = base_export_fields,
  "AMPHIBIANS" = base_export_fields,
  "REPTILES" = base_export_fields,
  "TERRESTRIAL INVERTS" = base_export_fields,
  "FRESHWATER ALGAE" = base_export_fields,
  "MARINE ALGAE" = base_export_fields,
  "MARINE ANIMALS" = base_export_fields,
  "LICHENS" = base_export_fields,
  "MACROFUNGI" = base_export_fields,
  "VASCULAR PLANTS" = base_export_fields,
  "NON-VASCULAR PLANTS" = base_export_fields,
  "SLIME MOLDS" = base_export_fields,
  "UNASSIGNED" = base_export_fields
)

# ============================================================
# REPORT LOOKUP (AUTHORITATIVE MAPPING)
# ============================================================

report_lookup <- tribble(
  ~report_group,         ~report_supergroup,      ~report_detail,
  "BIRDS",               "TERRESTRIAL WILDLIFE",  NA_character_,
  "TERRESTRIAL MAMMALS", "TERRESTRIAL WILDLIFE",  NA_character_,
  "AMPHIBIANS",          "TERRESTRIAL WILDLIFE",  NA_character_,
  "REPTILES",            "TERRESTRIAL WILDLIFE",  NA_character_,
  "TERRESTRIAL INVERTS", "TERRESTRIAL WILDLIFE",  NA_character_,
  "FRESHWATER ALGAE",    "AQUATIC LIFE",          "FRESHWATER ALGAE",
  "MARINE ALGAE",        "AQUATIC LIFE",          "MARINE ALGAE (UNSPLIT)",
  "MARINE ANIMALS",      "AQUATIC LIFE",          "MARINE ANIMALS",
  "LICHENS",             "LICHENS",               NA_character_,
  "MACROFUNGI",          "MACROFUNGI",            NA_character_,
  "VASCULAR PLANTS",     "VASCULAR PLANTS",       NA_character_,
  "NON-VASCULAR PLANTS", "NON-VASCULAR PLANTS",   NA_character_,
  "SLIME MOLDS",         "SLIME MOLDS",           NA_character_,
  "UNASSIGNED",          "UNASSIGNED",            NA_character_
)

expected_report_groups <- report_lookup$report_group

# ============================================================
# HELPERS
# ============================================================

clean_taxon <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "Na", "N/A")] <- NA_character_
  x
}

norm_key <- function(x) {
  x |>
    clean_taxon() |>
    stringr::str_to_lower()
}

read_csv_utf8 <- function(file, ...) {
  readr::read_csv(
    file,
    locale = readr::locale(encoding = "UTF-8"),
    ...
  )
}

read_csv_character_utf8 <- function(file, ...) {
  readr::read_csv(
    file,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    ...
  )
}

safe_write_csv <- function(x, path, excel_compatible = TRUE, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(excel_compatible)) {
    readr::write_excel_csv(x, path, ...)
  } else {
    readr::write_csv(x, path, ...)
  }
}

string_match_bidirectional <- function(a, b) {
  if (is.na(a) || is.na(b) || a == "" || b == "") return(FALSE)
  stringr::str_detect(a, stringr::fixed(b)) ||
    stringr::str_detect(b, stringr::fixed(a))
}

ensure_columns <- function(df, cols) {
  for (nm in cols) {
    if (!nm %in% names(df)) {
      df[[nm]] <- NA_character_
    }
  }
  df
}

get_first_existing_col <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

group_slug <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

normalize_export_strings <- function(df) {
  df |>
    mutate(
      across(
        where(is.character),
        ~ {
          y <- as.character(.x)
          y <- stringr::str_squish(y)
          dplyr::na_if(y, "")
        }
      )
    )
}

coerce_cols_to_character <- function(df, cols) {
  df <- ensure_columns(df, cols)
  df |>
    mutate(across(any_of(cols), as.character))
}

assign_source_code <- function(filename) {
  x <- basename(filename) |>
    str_to_lower() |>
    str_replace("\\.csv$", "")
  
  case_when(
    # terrestrial vertebrates / wildlife
    str_detect(x, "mammal") ~ "SRC_MAMMALS",
    str_detect(x, "amphib|herptile_amphib|herp_amphib") ~ "SRC_AMPHIBIANS",
    str_detect(x, "reptile|herptile_rept|herp_rept") ~ "SRC_REPTILES",
    str_detect(x, "bird|aves") ~ "SRC_BIRDS",
    
    # terrestrial invertebrates
    str_detect(x, "terrestrial.*invert|invert.*terrestrial|arthropod|annelid|terrestrial.*mollusc|terrestrial.*mollusk") ~ "SRC_TERR_INVERTS",
    
    # aquatic algae / life
    str_detect(x, "freshwater.*alga|freshwater.*algae") ~ "SRC_FRESHWATER_ALGAE",
    str_detect(x, "marine.*alga|marine.*algae") ~ "SRC_MARINE_ALGAE",
    str_detect(x, "green.*alga|green.*algae|chlorophyta") ~ "SRC_GREEN_ALGAE",
    str_detect(x, "red.*alga|red.*algae|rhodophyta") ~ "SRC_RED_ALGAE",
    str_detect(x, "kelp|phaeophy|ochrophyta") ~ "SRC_KELP",
    str_detect(x, "marine.*animal|marine.*invertebrate|marine.*species|porifera|cnidaria|echinoderm|brachiopod|bryozoa") ~ "SRC_MARINE_ANIMALS",
    
    # fungi / lichens / plants / slime molds
    str_detect(x, "lichen") ~ "SRC_LICHENS",
    str_detect(x, "macrofung|fungi|fungus") ~ "SRC_MACROFUNGI",
    str_detect(x, "vascular.*plant|vascular") ~ "SRC_VASCULAR_PLANTS",
    str_detect(x, "non.*vascular|bryophyte|moss|liverwort|hornwort") ~ "SRC_NONVASC_PLANTS",
    str_detect(x, "slime.*mold|slime.*mould|myxomyc") ~ "SRC_SLIME_MOLDS",
    
    TRUE ~ "SRC_UNASSIGNED"
  )
}

assign_report_group <- function(source_code) {
  case_when(
    source_code == "SRC_MAMMALS" ~ "TERRESTRIAL MAMMALS",
    source_code == "SRC_AMPHIBIANS" ~ "AMPHIBIANS",
    source_code == "SRC_REPTILES" ~ "REPTILES",
    source_code == "SRC_BIRDS" ~ "BIRDS",
    source_code == "SRC_TERR_INVERTS" ~ "TERRESTRIAL INVERTS",
    
    source_code %in% c("SRC_MARINE_ALGAE", "SRC_GREEN_ALGAE", "SRC_RED_ALGAE", "SRC_KELP") ~ "MARINE ALGAE",
    source_code == "SRC_FRESHWATER_ALGAE" ~ "FRESHWATER ALGAE",
    source_code == "SRC_MARINE_ANIMALS" ~ "MARINE ANIMALS",
    
    source_code == "SRC_LICHENS" ~ "LICHENS",
    source_code == "SRC_MACROFUNGI" ~ "MACROFUNGI",
    source_code == "SRC_VASCULAR_PLANTS" ~ "VASCULAR PLANTS",
    source_code == "SRC_NONVASC_PLANTS" ~ "NON-VASCULAR PLANTS",
    source_code == "SRC_SLIME_MOLDS" ~ "SLIME MOLDS",
    
    TRUE ~ "UNASSIGNED"
  )
}

initialize_ranking_columns <- function(df) {
  cols_chr <- c(
    "commonName_rankings",
    "provincial_status",
    "Prov Status Review Date",
    "bc_list",
    "national_status",
    "COSEWIC Date",
    "SARA Schedule",
    "SARA_status",
    "SARA Date",
    "match_pass",
    "matched_on_source_field",
    "matched_on_ranking_field",
    "matched_value_source",
    "matched_value_ranking"
  )
  
  for (nm in cols_chr) {
    if (!nm %in% names(df)) {
      df[[nm]] <- NA_character_
    }
  }
  
  df
}

assign_ranking_fields <- function(df, row_id, rankings_tbl, rank_id) {
  df$commonName_rankings[row_id]        <- rankings_tbl$commonName[rank_id]
  df$provincial_status[row_id]          <- rankings_tbl$provincial_status[rank_id]
  df$`Prov Status Review Date`[row_id]  <- rankings_tbl$`Prov Status Review Date`[rank_id]
  df$bc_list[row_id]                    <- rankings_tbl$bc_list[rank_id]
  df$national_status[row_id]            <- rankings_tbl$national_status[rank_id]
  df$`COSEWIC Date`[row_id]             <- rankings_tbl$`COSEWIC Date`[rank_id]
  df$`SARA Schedule`[row_id]            <- rankings_tbl$`SARA Schedule`[rank_id]
  df$SARA_status[row_id]                <- rankings_tbl$SARA_status[rank_id]
  df$`SARA Date`[row_id]                <- rankings_tbl$`SARA Date`[rank_id]
  df
}

apply_fuzzy_pass <- function(df_current,
                             rankings_tbl,
                             pass_label,
                             source_field,
                             ranking_field) {
  
  if (!source_field %in% names(df_current)) {
    message("Skipping ", pass_label, ": source field '", source_field, "' not found.")
    return(list(
      data = df_current,
      log = tibble(
        pass = pass_label,
        source_field = source_field,
        ranking_field = ranking_field,
        newly_matched = 0L,
        ambiguous = 0L,
        still_unmatched_before_pass = sum(is.na(df_current$match_pass))
      ),
      details = tibble()
    ))
  }
  
  if (!ranking_field %in% names(rankings_tbl)) {
    message("Skipping ", pass_label, ": ranking field '", ranking_field, "' not found.")
    return(list(
      data = df_current,
      log = tibble(
        pass = pass_label,
        source_field = source_field,
        ranking_field = ranking_field,
        newly_matched = 0L,
        ambiguous = 0L,
        still_unmatched_before_pass = sum(is.na(df_current$match_pass))
      ),
      details = tibble()
    ))
  }
  
  unmatched_idx <- which(is.na(df_current$match_pass))
  
  if (length(unmatched_idx) == 0) {
    return(list(
      data = df_current,
      log = tibble(
        pass = pass_label,
        source_field = source_field,
        ranking_field = ranking_field,
        newly_matched = 0L,
        ambiguous = 0L,
        still_unmatched_before_pass = 0L
      ),
      details = tibble()
    ))
  }
  
  src_vals <- norm_key(df_current[[source_field]][unmatched_idx])
  rank_vals <- norm_key(rankings_tbl[[ranking_field]])
  
  details_list <- vector("list", length(unmatched_idx))
  n_new <- 0L
  n_ambig <- 0L
  
  for (i in seq_along(unmatched_idx)) {
    row_id <- unmatched_idx[i]
    sv <- src_vals[i]
    
    if (is.na(sv) || sv == "") next
    
    cand_idx <- which(vapply(rank_vals, function(rv) {
      string_match_bidirectional(sv, rv)
    }, logical(1)))
    
    if (length(cand_idx) == 1) {
      j <- cand_idx[1]
      
      df_current <- assign_ranking_fields(df_current, row_id, rankings_tbl, j)
      
      df_current$match_pass[row_id] <- pass_label
      df_current$matched_on_source_field[row_id] <- source_field
      df_current$matched_on_ranking_field[row_id] <- ranking_field
      df_current$matched_value_source[row_id] <- df_current[[source_field]][row_id]
      df_current$matched_value_ranking[row_id] <- rankings_tbl[[ranking_field]][j]
      
      src_file_val <- if ("source_file" %in% names(df_current)) df_current$source_file[row_id] else NA_character_
      sci_val <- if ("scientificName" %in% names(df_current)) df_current$scientificName[row_id] else NA_character_
      
      details_list[[i]] <- tibble(
        scientificName = sci_val,
        source_file = src_file_val,
        pass = pass_label,
        source_field = source_field,
        ranking_field = ranking_field,
        matched_value_source = df_current[[source_field]][row_id],
        matched_value_ranking = rankings_tbl[[ranking_field]][j],
        matched_taxonName = rankings_tbl$taxonName[j]
      )
      
      n_new <- n_new + 1L
      
    } else if (length(cand_idx) > 1) {
      n_ambig <- n_ambig + 1L
    }
  }
  
  details_tbl <- bind_rows(details_list)
  
  log_tbl <- tibble(
    pass = pass_label,
    source_field = source_field,
    ranking_field = ranking_field,
    newly_matched = n_new,
    ambiguous = n_ambig,
    still_unmatched_before_pass = length(unmatched_idx)
  )
  
  list(
    data = df_current,
    log = log_tbl,
    details = details_tbl
  )
}

build_curated_csv <- function(df, fields) {
  df |>
    normalize_export_strings() |>
    select(any_of(fields)) |>
    distinct()
}

build_qc_csv <- function(df) {
  qc_fields <- c(
    "scientificName",
    "commonName",
    "source_file",
    "source_code",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status",
    "match_pass",
    "matched_on_source_field",
    "matched_on_ranking_field",
    "matched_value_source",
    "matched_value_ranking",
    "report_group",
    "report_supergroup",
    "report_detail",
    "n_catalog_rows",
    "n_inaturalist_rows",
    "n_non_inaturalist_rows",
    "n_distinct_locality_blobs",
    "n_inside_term_rows",
    "n_outside_term_rows",
    "n_mixed_term_rows",
    "n_review_rows",
    "n_missing_locality_rows",
    "locality_qc_flag",
    "locality_qc_suspect",
    "locality_examples"
  )
  
  df |>
    normalize_export_strings() |>
    select(any_of(qc_fields), everything()) |>
    distinct()
}

get_report_table <- function(x, grp) {
  if (grp %in% names(x)) x[[grp]] else tibble()
}


# ============================================================
# RAW LOCALITY QC HELPERS
# QUESTIONABLE-LOCALITY REVIEW + CURATORIAL REMOVAL WORKFLOW
# ============================================================

collapse_nonempty_unique <- function(x, sep = " | ") {
  x <- as.character(x)
  x <- clean_taxon(x)
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  paste(x, collapse = sep)
}

paste_row_nonempty <- function(...) {
  vals <- c(...)
  vals <- as.character(vals)
  vals <- clean_taxon(vals)
  vals <- vals[!is.na(vals) & vals != ""]
  vals <- unique(vals)
  if (length(vals) == 0) return(NA_character_)
  paste(vals, collapse = " | ")
}

summarize_distinct_values <- function(x, n_max = 10) {
  x <- clean_taxon(x)
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  shown <- head(sort(x), n_max)
  out <- paste(shown, collapse = " || ")
  if (length(x) > n_max) {
    out <- paste0(out, " || ... [", length(x), " distinct]")
  }
  out
}

collapse_examples <- function(x, n_max = 8) {
  x <- clean_taxon(x)
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  shown <- head(x, n_max)
  out <- paste(shown, collapse = " || ")
  if (length(x) > n_max) {
    out <- paste0(out, " || ... [", length(x), " distinct]")
  }
  out
}

collapse_ids <- function(x, n_max = Inf) {
  x <- as.character(x)
  x <- clean_taxon(x)
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  shown <- head(x, n_max)
  out <- paste(shown, collapse = " | ")
  if (length(x) > n_max && is.finite(n_max)) {
    out <- paste0(out, " | ... [", length(x), " unique]")
  }
  out
}

first_matching_questionable_term <- function(blob, terms_lower) {
  blob <- clean_taxon(blob)
  if (is.na(blob) || blob == "" || length(terms_lower) == 0) return(NA_character_)
  hits <- terms_lower[stringr::str_detect(stringr::str_to_lower(blob), stringr::fixed(terms_lower))]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

is_yes_value <- function(x) {
  x <- stringr::str_to_lower(clean_taxon(x))
  dplyr::coalesce(x %in% c("y", "yes", "true", "t", "1", "remove", "drop"), FALSE)
}

build_raw_locality_qc <- function(records_df, questionable_localities_tbl = NULL) {
  required_cols <- c("scientificName", "locality", "stateProvince", "countryCode")
  missing_required <- setdiff(required_cols, names(records_df))
  if (length(missing_required) > 0) {
    stop(
      "Raw catalog is missing required locality QC column(s): ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  locality_cols <- intersect(c("locality", "stateProvince", "countryCode"), names(records_df))
  context_cols <- intersect(
    c(
      "gbifID", "occurrenceID", "catalogNumber", "institutionCode",
      "collectionCode", "recordedBy", "eventDate", "basisOfRecord",
      "issue", "datasetKey", "scientificNameAuthorship",
      "verbatimScientificName", "verbatimScientificNameAuthorship",
      "iNaturalistTaxonId", "iNaturalistTaxonName", "nameStatus"
    ),
    names(records_df)
  )
  
  q_tbl <- if (is.null(questionable_localities_tbl)) {
    tibble(term = character(), qc_flag = character(), reason = character())
  } else {
    questionable_localities_tbl |>
      rename(term = 1, qc_flag = 2, reason = 3) |>
      mutate(across(everything(), clean_taxon)) |>
      filter(!is.na(term), term != "") |>
      mutate(term_lower = stringr::str_to_lower(term))
  }
  
  institution_code_vec <- if ("institutionCode" %in% names(records_df)) records_df$institutionCode else rep(NA_character_, nrow(records_df))
  collection_code_vec  <- if ("collectionCode" %in% names(records_df)) records_df$collectionCode else rep(NA_character_, nrow(records_df))
  
  raw_locality_record_qc <- records_df |>
    mutate(across(where(is.character), clean_taxon)) |>
    mutate(
      institutionCode = if ("institutionCode" %in% names(records_df)) institutionCode else institution_code_vec,
      collectionCode = if ("collectionCode" %in% names(records_df)) collectionCode else collection_code_vec,
      is_inaturalist_record = dplyr::coalesce(
        (!is.na(institutionCode) & institutionCode == "iNaturalist") |
          (!is.na(collectionCode) & collectionCode == "Observations"),
        FALSE
      ),
      locality_blob = purrr::pmap_chr(
        pick(any_of(locality_cols)),
        \(...) paste_row_nonempty(...)
      ),
      locality_blob_lower = stringr::str_to_lower(dplyr::coalesce(locality_blob, "")),
      questionable_locality_match = purrr::map_chr(
        locality_blob,
        ~ first_matching_questionable_term(.x, q_tbl$term_lower)
      )
    )
  
  if (nrow(q_tbl) > 0) {
    raw_locality_record_qc <- raw_locality_record_qc |>
      left_join(
        q_tbl |>
          select(term_lower, qc_flag, reason),
        by = c("questionable_locality_match" = "term_lower")
      )
  } else {
    raw_locality_record_qc <- raw_locality_record_qc |>
      mutate(qc_flag = NA_character_, reason = NA_character_)
  }
  
  raw_locality_record_qc <- raw_locality_record_qc |>
    mutate(
      locality_signal = case_when(
        is.na(locality) | locality == "" ~ "no_locality_metadata",
        is_inaturalist_record ~ "ignored_inaturalist",
        !is.na(questionable_locality_match) ~ "questionable_locality_string",
        TRUE ~ "ok_locality_string"
      ),
      suspect_locality_flag = !is_inaturalist_record & !is.na(questionable_locality_match),
      questionable_locality_flag = suspect_locality_flag,
      questionable_locality_term = questionable_locality_match,
      questionable_locality_reason = reason,
      questionable_locality_qc_flag = qc_flag
    )
  
  raw_locality_distinct <- raw_locality_record_qc |>
    select(
      any_of(c(
        "scientificName", "scientificNameAuthorship", "locality",
        "stateProvince", "countryCode", "locality_blob",
        "locality_signal", "suspect_locality_flag", "is_inaturalist_record",
        "questionable_locality_flag", "questionable_locality_term",
        "questionable_locality_reason", "questionable_locality_qc_flag"
      )),
      any_of(context_cols)
    ) |>
    distinct() |>
    arrange(scientificName, desc(suspect_locality_flag), locality, gbifID)
  
  raw_locality_summary <- raw_locality_record_qc |>
    filter(!is_inaturalist_record, !is.na(locality), locality != "") |>
    group_by(locality, stateProvince, countryCode) |>
    summarise(
      n_records = n(),
      n_taxa = n_distinct(scientificName, na.rm = TRUE),
      n_questionable_records = sum(questionable_locality_flag, na.rm = TRUE),
      example_taxa = collapse_examples(scientificName, n_max = 12),
      locality_blob = dplyr::first(locality_blob),
      questionable_locality_term = collapse_examples(questionable_locality_term[questionable_locality_flag], n_max = 10),
      questionable_locality_reason = collapse_examples(questionable_locality_reason[questionable_locality_flag], n_max = 10),
      .groups = "drop"
    ) |>
    arrange(desc(n_questionable_records), desc(n_records), locality)
  
  questionable_records <- raw_locality_record_qc |>
    filter(questionable_locality_flag) |>
    select(
      scientificName,
      any_of(c(
        "scientificNameAuthorship", "gbifID", "occurrenceID", "catalogNumber",
        "locality", "stateProvince", "countryCode", "locality_blob",
        "questionable_locality_term", "questionable_locality_reason",
        "questionable_locality_qc_flag", "institutionCode", "collectionCode",
        "recordedBy", "eventDate", "basisOfRecord", "datasetKey", "source_file"
      ))
    ) |>
    arrange(scientificName, locality, gbifID)
  
  taxon_qc <- raw_locality_record_qc |>
    group_by(scientificName) |>
    summarise(
      scientificNameAuthorship = if ("scientificNameAuthorship" %in% names(records_df)) {
        vals <- na.omit(scientificNameAuthorship)
        if (length(vals) == 0) NA_character_ else dplyr::first(vals)
      } else {
        NA_character_
      },
      n_catalog_rows = n(),
      n_inaturalist_rows = sum(is_inaturalist_record, na.rm = TRUE),
      n_non_inaturalist_rows = sum(!is_inaturalist_record, na.rm = TRUE),
      n_distinct_locality_blobs = n_distinct(locality_blob[!is.na(locality_blob) & locality_blob != ""], na.rm = TRUE),
      n_inside_term_rows = 0L,
      n_outside_term_rows = sum(questionable_locality_flag, na.rm = TRUE),
      n_mixed_term_rows = 0L,
      n_review_rows = sum(!is_inaturalist_record & !is.na(locality) & locality != "", na.rm = TRUE),
      n_missing_locality_rows = sum(is.na(locality) | locality == "", na.rm = TRUE),
      n_questionable_records = sum(questionable_locality_flag, na.rm = TRUE),
      n_non_questionable_records = sum(!is_inaturalist_record & !questionable_locality_flag & !is.na(locality) & locality != "", na.rm = TRUE),
      all_non_inat_records_questionable = dplyr::if_else(
        n_non_inaturalist_rows > 0,
        n_questionable_records == n_non_inaturalist_rows,
        FALSE
      ),
      any_questionable_records = n_questionable_records > 0,
      locality_examples = collapse_examples(locality[!is.na(locality) & locality != ""], n_max = 15),
      suspect_locality_examples = collapse_examples(locality[questionable_locality_flag], n_max = 15),
      suspect_locality_terms = collapse_examples(questionable_locality_term[questionable_locality_flag], n_max = 15),
      suspect_locality_reasons = collapse_examples(questionable_locality_reason[questionable_locality_flag], n_max = 15),
      suspect_gbifIDs = collapse_ids(gbifID[questionable_locality_flag]),
      suspect_occurrenceIDs = collapse_ids(occurrenceID[questionable_locality_flag]),
      locality_qc_flag = case_when(
        n_non_inaturalist_rows == 0 ~ "IGNORED_INATURALIST_ONLY",
        n_missing_locality_rows == n_catalog_rows ~ "FLAG_NO_LOCALITY_METADATA",
        any_questionable_records & all_non_inat_records_questionable ~ "ALL_NON_INAT_RECORDS_QUESTIONABLE",
        any_questionable_records ~ "HAS_QUESTIONABLE_RECORDS",
        TRUE ~ "NO_QUESTIONABLE_LOCALITY_STRINGS"
      ),
      locality_qc_suspect = any_questionable_records,
      .groups = "drop"
    ) |>
    arrange(desc(all_non_inat_records_questionable), desc(n_questionable_records), scientificName)
  
  taxa_subject_to_removal_review <- taxon_qc |>
    filter(all_non_inat_records_questionable) |>
    transmute(
      scientificName,
      scientificNameAuthorship,
      n_catalog_rows,
      n_non_inaturalist_rows,
      n_questionable_records,
      n_non_questionable_records,
      all_non_inat_records_questionable,
      suspect_locality_examples,
      suspect_locality_terms,
      suspect_locality_reasons,
      suspect_gbifIDs,
      suspect_occurrenceIDs,
      curator_remove_taxon = "",
      curator_remove_basis = "suggest_remove",
      curator_notes = ""
    ) |>
    arrange(desc(n_questionable_records), scientificName)
  
  list(
    locality_cols = locality_cols,
    context_cols = context_cols,
    record_qc = raw_locality_record_qc,
    distinct_localities = raw_locality_distinct,
    locality_summary = raw_locality_summary,
    questionable_records = questionable_records,
    taxon_qc = taxon_qc,
    taxa_subject_to_removal_review = taxa_subject_to_removal_review
  )
}

# ============================================================
# CREATE OUTPUT DIRECTORIES
# ============================================================

dir.create(ranked_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(lists_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(final_csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_locality_qc_dir, recursive = TRUE, showWarnings = FALSE)

read_csv_utf8 <- function(file, ...) {
  readr::read_csv(
    file,
    locale = readr::locale(encoding = "UTF-8"),
    ...
  )
}

read_csv_character_utf8 <- function(file, ...) {
  readr::read_csv(
    file,
    locale = readr::locale(encoding = "UTF-8"),
    col_types = readr::cols(.default = readr::col_character()),
    ...
  )
}

# Use Excel-compatible UTF-8 CSV writing (BOM) because some manually curated
# files are opened in spreadsheet apps before being re-read in R.
safe_write_csv <- function(x, path, excel_compatible = TRUE, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(excel_compatible)) {
    readr::write_excel_csv(x, path, ...)
  } else {
    readr::write_csv(x, path, ...)
  }
}


# QUESTIONABLE LOCALITY STRINGS FOR REVIEW:
# NOTE: these are matched as lowercase literal substrings against locality_blob.
# Keep this list conservative to avoid false positives (for example, do not use plain "vancouver"
# because it would also match North Vancouver and West Vancouver, which may be valid Howe Sound records).

out_of_region_terms <- tribble(
  ~term, ~qc_flag, ~reason,
  "victoria", "OUT_OF_REGION", "Vancouver Island",
  "oak bay", "OUT_OF_REGION", "Vancouver Island",
  "saanich", "OUT_OF_REGION", "Vancouver Island",
  "sidney", "OUT_OF_REGION", "Vancouver Island",
  "goldstream", "OUT_OF_REGION", "Vancouver Island",
  "saanich inlet", "OUT_OF_REGION", "Vancouver Island",
  "nanaimo", "OUT_OF_REGION", "Vancouver Island",
  "duncan", "OUT_OF_REGION", "Vancouver Island",
  "tofino", "OUT_OF_REGION", "Vancouver Island",
  "ucluelet", "OUT_OF_REGION", "Vancouver Island",
  "comox", "OUT_OF_REGION", "Vancouver Island",
  "courtenay", "OUT_OF_REGION", "Vancouver Island",
  "campbell river", "OUT_OF_REGION", "Vancouver Island",
  "port hardy", "OUT_OF_REGION", "Vancouver Island",
  "haida gwaii", "OUT_OF_REGION", "Haida Gwaii",
  "seattle", "OUT_OF_REGION", "United States",
  "washington", "OUT_OF_REGION", "United States",
  "oregon", "OUT_OF_REGION", "United States",
  "california", "OUT_OF_REGION", "United States",
  "alaska", "OUT_OF_REGION", "United States",
  "mexico", "OUT_OF_REGION", "Outside BC/Howe Sound"
)

questionable_terms <- tribble(
  ~term, ~qc_flag, ~reason,
  "burnaby", "QUESTIONABLE", "Metro Vancouver; requires review",
  "richmond", "QUESTIONABLE", "Metro Vancouver; requires review",
  "delta", "QUESTIONABLE", "Metro Vancouver; requires review",
  "surrey", "QUESTIONABLE", "Metro Vancouver; requires review",
  "langley", "QUESTIONABLE", "Fraser Valley / Lower Mainland; requires review",
  "abbotsford", "QUESTIONABLE", "Fraser Valley; requires review",
  "chilliwack", "QUESTIONABLE", "Fraser Valley; requires review",
  "mission", "QUESTIONABLE", "Fraser Valley; requires review",
  "gibsons", "QUESTIONABLE", "Sunshine Coast; requires review",
  "sechelt", "QUESTIONABLE", "Sunshine Coast; requires review",
  "powell river", "QUESTIONABLE", "Sunshine Coast; requires review",
  "fraser valley", "QUESTIONABLE", "Outside core Howe Sound; requires review",
  "okanagan", "QUESTIONABLE", "Interior BC; requires review",
  "kelowna", "QUESTIONABLE", "Interior BC; requires review",
  "kamloops", "QUESTIONABLE", "Interior BC; requires review",
  "prince george", "QUESTIONABLE", "Interior BC; requires review",
  "whistler", "QUESTIONABLE", "Outside core Howe Sound; requires review",
  "pemberton", "QUESTIONABLE", "Outside core Howe Sound; requires review",
  "lillooet", "QUESTIONABLE", "Outside core Howe Sound; requires review"
)

questionable_localities <- bind_rows(
  out_of_region_terms,
  questionable_terms
) |>
  distinct(term, .keep_all = TRUE) |>
  mutate(term = str_to_lower(str_squish(term)))

questionable_taxa_review_file <- file.path(raw_locality_qc_dir, "raw_catalog_questionable_taxa_curatorial_review.csv")

# ============================================================
# RAW CATALOG LOCALITY QC
# ============================================================

raw_catalog <- read_csv_character_utf8(
  raw_catalog_file,
  show_col_types = FALSE
) |>
  mutate(across(everything(), clean_taxon))

if (!"scientificName" %in% names(raw_catalog)) {
  stop("Raw catalog file must contain a scientificName column.")
}

raw_locality_qc <- build_raw_locality_qc(
  raw_catalog,
  questionable_localities_tbl = questionable_localities
)

message("Raw locality QC fields used:")
print(raw_locality_qc$locality_cols)

safe_write_csv(
  tibble(locality_field = raw_locality_qc$locality_cols),
  file.path(raw_locality_qc_dir, "raw_catalog_locality_fields_used.csv")
)

safe_write_csv(
  tibble(context_field = raw_locality_qc$context_cols),
  file.path(raw_locality_qc_dir, "raw_catalog_context_fields_used.csv")
)

safe_write_csv(
  raw_locality_qc$record_qc,
  file.path(raw_locality_qc_dir, "raw_catalog_record_locality_qc.csv")
)

safe_write_csv(
  raw_locality_qc$distinct_localities,
  file.path(raw_locality_qc_dir, "raw_catalog_distinct_localities_by_taxon.csv")
)

safe_write_csv(
  raw_locality_qc$locality_summary,
  file.path(raw_locality_qc_dir, "raw_catalog_unique_locality_summary.csv")
)

safe_write_csv(
  raw_locality_qc$questionable_records,
  file.path(raw_locality_qc_dir, "raw_catalog_questionable_locality_records.csv")
)

if (file.exists(questionable_taxa_review_file)) {
  existing_review <- read_csv_character_utf8(
    questionable_taxa_review_file,
    show_col_types = FALSE
  ) |>
    mutate(across(everything(), clean_taxon))
  
  review_export <- raw_locality_qc$taxa_subject_to_removal_review |>
    select(-any_of(c("curator_remove_taxon", "curator_remove_basis", "curator_notes"))) |>
    left_join(
      existing_review |>
        select(any_of(c("scientificName", "curator_remove_taxon", "curator_remove_basis", "curator_notes"))),
      by = "scientificName"
    ) |>
    mutate(
      curator_remove_taxon = coalesce(curator_remove_taxon, ""),
      curator_remove_basis = coalesce(curator_remove_basis, ""),
      curator_notes = coalesce(curator_notes, "")
    )
} else {
  review_export <- raw_locality_qc$taxa_subject_to_removal_review
}

safe_write_csv(
  review_export,
  questionable_taxa_review_file
)

safe_write_csv(
  raw_locality_qc$taxon_qc,
  file.path(raw_locality_qc_dir, "raw_catalog_taxon_locality_qc_summary.csv")
)

safe_write_csv(
  raw_locality_qc$taxon_qc |>
    filter(all_non_inat_records_questionable) |>
    arrange(desc(n_questionable_records), scientificName),
  file.path(raw_locality_qc_dir, "raw_catalog_taxa_flagged_for_locality_review.csv")
)

safe_write_csv(
  raw_locality_qc$taxon_qc |>
    filter(any_questionable_records) |>
    arrange(desc(all_non_inat_records_questionable), desc(n_questionable_records), scientificName),
  file.path(raw_locality_qc_dir, "raw_catalog_taxa_with_any_questionable_records.csv")
)

safe_write_csv(
  raw_locality_qc$distinct_localities |>
    filter(suspect_locality_flag) |>
    arrange(scientificName, locality_signal, locality_blob),
  file.path(raw_locality_qc_dir, "raw_catalog_suspect_locality_records.csv")
)

selected_taxa_for_removal <- character()

if (file.exists(questionable_taxa_review_file)) {
  questionable_taxa_review <- read_csv_character_utf8(
    questionable_taxa_review_file,
    show_col_types = FALSE
  ) |>
    mutate(across(everything(), clean_taxon))
  
  selected_taxa_for_removal <- questionable_taxa_review |>
    filter(is_yes_value(curator_remove_taxon)) |>
    pull(scientificName) |>
    unique()
  
  safe_write_csv(
    tibble(scientificName = selected_taxa_for_removal),
    file.path(raw_locality_qc_dir, "raw_catalog_selected_taxa_for_removal.csv")
  )
}

# ============================================================
# READ REVIEWED SUMMARY FILES
# ============================================================

reviewed_files <- list.files(
  path = reviewed_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

reviewed_list <- reviewed_files |>
  set_names(basename(reviewed_files)) |>
  map(function(f) {
    
    dat <- read_csv_utf8(f, show_col_types = FALSE) |>
      mutate(across(where(is.character), clean_taxon))
    
    sci_col <- get_first_existing_col(dat, c("scientificName", "taxonName", "Taxon"))
    if (is.na(sci_col)) {
      stop(paste("No common taxon field found in:", basename(f)))
    }
    
    if (!"scientificName" %in% names(dat)) {
      dat$scientificName <- dat[[sci_col]]
    }
    
    dat <- ensure_columns(dat, c("variety", "subspecies", "genus", "species"))
    
    dat |>
      mutate(
        scientificName = clean_taxon(scientificName),
        variety = clean_taxon(variety),
        subspecies = clean_taxon(subspecies),
        genus = clean_taxon(genus),
        species = clean_taxon(species),
        source_file = basename(f),
        source_code = assign_source_code(f)
      )
  })

howe_sound_all <- bind_rows(reviewed_list) |>
  mutate(
    genus_species = if_else(
      !is.na(genus) & genus != "" & !is.na(species) & species != "",
      paste(genus, species),
      NA_character_
    )
  )

howe_sound_all <- howe_sound_all |>
  left_join(
    raw_locality_qc$taxon_qc |>
      select(
        scientificName,
        n_catalog_rows,
        n_inaturalist_rows,
        n_non_inaturalist_rows,
        n_distinct_locality_blobs,
        n_inside_term_rows,
        n_outside_term_rows,
        n_mixed_term_rows,
        n_review_rows,
        n_missing_locality_rows,
        locality_qc_flag,
        locality_qc_suspect,
        locality_examples
      ),
    by = "scientificName"
  )


if (length(selected_taxa_for_removal) > 0) {
  message("Removing ", length(selected_taxa_for_removal), " taxa selected in curatorial locality review.")
  howe_sound_all <- howe_sound_all |>
    filter(!scientificName %in% selected_taxa_for_removal)
}

# ============================================================
# READ CONSERVATION RANKINGS
# ============================================================

rankings <- read_csv_utf8(rankings_file, show_col_types = FALSE) |>
  mutate(across(where(is.character), clean_taxon))

rank_col <- get_first_existing_col(rankings, c("taxonName", "scientificName", "Taxon"))
if (is.na(rank_col)) {
  stop("No common taxon field found in rankings file.")
}
if (!"taxonName" %in% names(rankings)) {
  rankings$taxonName <- rankings[[rank_col]]
}

rankings <- ensure_columns(
  rankings,
  c(
    "variety", "subspecies", "genus", "species",
    "commonName", "provincial_status", "Prov Status Review Date",
    "bc_list", "national_status", "COSEWIC Date",
    "SARA Schedule", "SARA_status", "SARA Date"
  )
)

rankings <- rankings |>
  mutate(
    taxonName = clean_taxon(taxonName),
    variety = clean_taxon(variety),
    subspecies = clean_taxon(subspecies),
    genus = clean_taxon(genus),
    species = clean_taxon(species),
    genus_species = if_else(
      !is.na(genus) & genus != "" & !is.na(species) & species != "",
      paste(genus, species),
      NA_character_
    )
  )

rankings_sub <- rankings |>
  select(
    taxonName,
    commonName,
    provincial_status,
    `Prov Status Review Date`,
    bc_list,
    national_status,
    `COSEWIC Date`,
    `SARA Schedule`,
    SARA_status,
    `SARA Date`,
    any_of(c("variety", "subspecies", "genus", "species", "genus_species"))
  ) |>
  distinct()

rankings_sub <- rankings_sub |>
  mutate(taxonName_norm = norm_key(taxonName)) |>
  distinct(taxonName_norm, .keep_all = TRUE) |>
  select(-taxonName_norm)

# ============================================================
# MERGE RANKINGS ON TAXON NAME (MULTI-PASS; NO left_join)
# ============================================================

howe_sound_ranked <- howe_sound_all |>
  initialize_ranking_columns()

# -----------------------------
# pass 1: exact scientificName -> taxonName
# -----------------------------
exact_idx <- match(
  norm_key(howe_sound_ranked$scientificName),
  norm_key(rankings_sub$taxonName)
)

matched_rows <- which(!is.na(exact_idx))

if (length(matched_rows) > 0) {
  howe_sound_ranked$commonName_rankings[matched_rows]       <- rankings_sub$commonName[exact_idx[matched_rows]]
  howe_sound_ranked$provincial_status[matched_rows]         <- rankings_sub$provincial_status[exact_idx[matched_rows]]
  howe_sound_ranked$`Prov Status Review Date`[matched_rows] <- rankings_sub$`Prov Status Review Date`[exact_idx[matched_rows]]
  howe_sound_ranked$bc_list[matched_rows]                   <- rankings_sub$bc_list[exact_idx[matched_rows]]
  howe_sound_ranked$national_status[matched_rows]           <- rankings_sub$national_status[exact_idx[matched_rows]]
  howe_sound_ranked$`COSEWIC Date`[matched_rows]            <- rankings_sub$`COSEWIC Date`[exact_idx[matched_rows]]
  howe_sound_ranked$`SARA Schedule`[matched_rows]           <- rankings_sub$`SARA Schedule`[exact_idx[matched_rows]]
  howe_sound_ranked$SARA_status[matched_rows]               <- rankings_sub$SARA_status[exact_idx[matched_rows]]
  howe_sound_ranked$`SARA Date`[matched_rows]               <- rankings_sub$`SARA Date`[exact_idx[matched_rows]]
  
  howe_sound_ranked$match_pass[matched_rows] <- "PASS_1_EXACT_SCIENTIFICNAME"
  howe_sound_ranked$matched_on_source_field[matched_rows] <- "scientificName"
  howe_sound_ranked$matched_on_ranking_field[matched_rows] <- "taxonName"
  howe_sound_ranked$matched_value_source[matched_rows] <- howe_sound_ranked$scientificName[matched_rows]
  howe_sound_ranked$matched_value_ranking[matched_rows] <- rankings_sub$taxonName[exact_idx[matched_rows]]
}

match_log <- tibble(
  pass = "PASS_1_EXACT_SCIENTIFICNAME",
  source_field = "scientificName",
  ranking_field = "taxonName",
  newly_matched = length(matched_rows),
  ambiguous = 0L,
  still_unmatched_before_pass = nrow(howe_sound_ranked)
)

tmp_match_details <- howe_sound_ranked |>
  filter(match_pass == "PASS_1_EXACT_SCIENTIFICNAME")

if (!"source_file" %in% names(tmp_match_details)) {
  tmp_match_details$source_file <- NA_character_
}

match_details <- tmp_match_details |>
  transmute(
    scientificName,
    source_file,
    pass = match_pass,
    source_field = matched_on_source_field,
    ranking_field = matched_on_ranking_field,
    matched_value_source,
    matched_value_ranking,
    matched_taxonName = matched_value_ranking
  )

# -----------------------------
# pass 2: variety -> variety
# -----------------------------
res2 <- apply_fuzzy_pass(
  df_current = howe_sound_ranked,
  rankings_tbl = rankings_sub,
  pass_label = "PASS_2_VARIETY_STRING",
  source_field = "variety",
  ranking_field = "variety"
)

howe_sound_ranked <- res2$data
match_log <- bind_rows(match_log, res2$log)
match_details <- bind_rows(match_details, res2$details)

# -----------------------------
# pass 3: subspecies -> subspecies
# -----------------------------
res3 <- apply_fuzzy_pass(
  df_current = howe_sound_ranked,
  rankings_tbl = rankings_sub,
  pass_label = "PASS_3_SUBSPECIES_STRING",
  source_field = "subspecies",
  ranking_field = "subspecies"
)

howe_sound_ranked <- res3$data
match_log <- bind_rows(match_log, res3$log)
match_details <- bind_rows(match_details, res3$details)

# -----------------------------
# pass 4: genus+species -> genus+species
# -----------------------------
res4 <- apply_fuzzy_pass(
  df_current = howe_sound_ranked,
  rankings_tbl = rankings_sub,
  pass_label = "PASS_4_GENUS_SPECIES_STRING",
  source_field = "genus_species",
  ranking_field = "genus_species"
)

howe_sound_ranked <- res4$data
match_log <- bind_rows(match_log, res4$log)
match_details <- bind_rows(match_details, res4$details)

# ============================================================
# FINAL STANDARDIZED CURATED CSV EXPORT FIELDS
# ============================================================

base_export_fields <- c(
  "scientificName",
  "scientificNameAuthorship",
  "commonName",
  "kingdom",
  "phylum",
  "subphylum",
  "superclass",
  "class",
  "subclass",
  "superorder",
  "order",
  "suborder",
  "infraorder",
  "superfamily",
  "family",
  "subfamily",
  "tribe",
  "genus",
  "species",
  "variety",
  "subspecies",
  "hybrid",
  "provincial_status",
  "Prov Status Review Date",
  "bc_list",
  "national_status",
  "COSEWIC Date",
  "SARA Schedule",
  "SARA_status",
  "SARA Date"
)

bird_extra_fields <- c("residencyStatus")

report_field_map <- list(
  "BIRDS" = c(base_export_fields, bird_extra_fields),
  "TERRESTRIAL MAMMALS" = base_export_fields,
  "AMPHIBIANS" = base_export_fields,
  "REPTILES" = base_export_fields,
  "TERRESTRIAL INVERTS" = base_export_fields,
  "FRESHWATER ALGAE" = base_export_fields,
  "MARINE ALGAE" = base_export_fields,
  "MARINE ANIMALS" = base_export_fields,
  "LICHENS" = base_export_fields,
  "MACROFUNGI" = base_export_fields,
  "VASCULAR PLANTS" = base_export_fields,
  "NON-VASCULAR PLANTS" = base_export_fields,
  "SLIME MOLDS" = base_export_fields,
  "UNASSIGNED" = base_export_fields
)

# ============================================================
# HELPERS FOR FINAL EXPORT VARIANTS
# ============================================================

split_flag_variants <- function(df) {
  has_flag <- "FLAG" %in% names(df)
  
  if (!has_flag) {
    return(list(
      all = df,
      unlisted_removed = df,
      unlisted = df[0, , drop = FALSE],
      incorrect_locality_removed = df[0, , drop = FALSE],
      has_flag = FALSE
    ))
  }
  
  list(
    all = df,
    unlisted_removed = df |>
      filter(is.na(FLAG) | !FLAG %in% c("!", "!!")),
    unlisted = df |>
      filter(!is.na(FLAG) & FLAG == "!"),
    incorrect_locality_removed = df |>
      filter(!is.na(FLAG) & FLAG == "!!"),
    has_flag = TRUE
  )
}

get_variant_table <- function(x, grp, variant = c("all", "unlisted_removed", "unlisted", "incorrect_locality_removed")) {
  variant <- match.arg(variant)
  
  if (!grp %in% names(x)) return(tibble())
  if (!variant %in% names(x[[grp]])) return(tibble())
  
  x[[grp]][[variant]]
}

# ============================================================
# LOAD MANUALLY CURATED RANKED FILES
# ============================================================

curated_files <- list.files(
  path = ranked_dir,
  pattern = "^Howe_Sound_.*_ranked_curated\\.csv$",
  full.names = TRUE
)

if (length(curated_files) == 0) {
  stop("No curated ranked files found in ranked_dir.")
}

howe_sound_curated_all <- curated_files |>
  set_names(basename(curated_files)) |>
  purrr::map(function(f) {
    dat <- read_csv_character_utf8(
      f,
      show_col_types = FALSE
    ) |>
      mutate(across(everything(), clean_taxon))
    
    if (!"source_file" %in% names(dat)) {
      dat$source_file <- basename(f)
    }
    
    if (!"source_code" %in% names(dat)) {
      dat$source_code <- assign_source_code(f)
    }
    
    if (!"report_group" %in% names(dat)) {
      dat$report_group <- NA_character_
    }
    
    if (!"report_supergroup" %in% names(dat)) {
      dat$report_supergroup <- NA_character_
    }
    
    if (!"report_detail" %in% names(dat)) {
      dat$report_detail <- NA_character_
    }
    
    dat |>
      mutate(
        across(everything(), as.character),
        report_group = coalesce(clean_taxon(report_group), assign_report_group(source_code))
      )
  }) |>
  bind_rows() |>
  left_join(report_lookup, by = "report_group", suffix = c("", ".lookup")) |>
  mutate(
    report_supergroup = coalesce(report_supergroup, report_supergroup.lookup),
    report_detail = coalesce(report_detail, report_detail.lookup)
  ) |>
  select(-any_of(c("report_supergroup.lookup", "report_detail.lookup"))) |>
  left_join(
    raw_locality_qc$taxon_qc |>
      select(
        scientificName,
        n_catalog_rows,
        n_inaturalist_rows,
        n_non_inaturalist_rows,
        n_distinct_locality_blobs,
        n_inside_term_rows,
        n_outside_term_rows,
        n_mixed_term_rows,
        n_review_rows,
        n_missing_locality_rows,
        locality_qc_flag,
        locality_qc_suspect,
        locality_examples
      ),
    by = "scientificName"
  )

# ============================================================
# SPLIT CURATED DATA INTO FINAL REPORTING TABLES
# ============================================================

tmp_curated <- howe_sound_curated_all |>
  filter(!is.na(report_group))

report_tables <- split(tmp_curated, tmp_curated$report_group)

# validation
present_groups <- sort(names(report_tables))
missing_expected_groups <- setdiff(expected_report_groups, present_groups)
extra_groups <- setdiff(present_groups, expected_report_groups)

message("Report groups present:")
print(present_groups)

if (length(missing_expected_groups) > 0) {
  message("Expected report groups not present in curated data:")
  print(missing_expected_groups)
}

if (length(extra_groups) > 0) {
  message("Unexpected report groups present in curated data:")
  print(extra_groups)
}

# ============================================================
# CREATE ALL / UNLISTEDREMOVED / UNLISTED / INCORRECT-LOCALITY VARIANTS FOR EACH GROUP
# ============================================================

report_table_variants <- purrr::map(
  report_tables,
  split_flag_variants
)

flag_variant_summary <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    tibble(
      report_group = grp,
      has_flag_column = variants$has_flag,
      n_all = nrow(variants$all),
      n_unlisted_removed = nrow(variants$unlisted_removed),
      n_unlisted = nrow(variants$unlisted),
      n_incorrect_locality_removed = nrow(variants$incorrect_locality_removed),
      check_all_equals_sum = nrow(variants$all) == (
        nrow(variants$unlisted_removed) +
          nrow(variants$unlisted) +
          nrow(variants$incorrect_locality_removed)
      )
    )
  }
)

print(flag_variant_summary, n = Inf)

# ============================================================
# CONVENIENT OBJECTS FOR FORMATTED TABLE SECTION
# ============================================================

# ALL
terrestrial_mammals_all <- get_variant_table(report_table_variants, "TERRESTRIAL MAMMALS", "all")
amphibians_all <- get_variant_table(report_table_variants, "AMPHIBIANS", "all")
reptiles_all <- get_variant_table(report_table_variants, "REPTILES", "all")
birds_all <- get_variant_table(report_table_variants, "BIRDS", "all")
terrestrial_inverts_all <- get_variant_table(report_table_variants, "TERRESTRIAL INVERTS", "all")
freshwater_algae_all <- get_variant_table(report_table_variants, "FRESHWATER ALGAE", "all")
marine_algae_all <- get_variant_table(report_table_variants, "MARINE ALGAE", "all")
marine_animals_all <- get_variant_table(report_table_variants, "MARINE ANIMALS", "all")
lichens_all <- get_variant_table(report_table_variants, "LICHENS", "all")
macrofungi_all <- get_variant_table(report_table_variants, "MACROFUNGI", "all")
vascular_plants_all <- get_variant_table(report_table_variants, "VASCULAR PLANTS", "all")
nonvascular_plants_all <- get_variant_table(report_table_variants, "NON-VASCULAR PLANTS", "all")
slime_molds_all <- get_variant_table(report_table_variants, "SLIME MOLDS", "all")
unassigned_all <- get_variant_table(report_table_variants, "UNASSIGNED", "all")

# UNLISTED REMOVED
terrestrial_mammals_unlistedRemoved <- get_variant_table(report_table_variants, "TERRESTRIAL MAMMALS", "unlisted_removed")
amphibians_unlistedRemoved <- get_variant_table(report_table_variants, "AMPHIBIANS", "unlisted_removed")
reptiles_unlistedRemoved <- get_variant_table(report_table_variants, "REPTILES", "unlisted_removed")
birds_unlistedRemoved <- get_variant_table(report_table_variants, "BIRDS", "unlisted_removed")
terrestrial_inverts_unlistedRemoved <- get_variant_table(report_table_variants, "TERRESTRIAL INVERTS", "unlisted_removed")
freshwater_algae_unlistedRemoved <- get_variant_table(report_table_variants, "FRESHWATER ALGAE", "unlisted_removed")
marine_algae_unlistedRemoved <- get_variant_table(report_table_variants, "MARINE ALGAE", "unlisted_removed")
marine_animals_unlistedRemoved <- get_variant_table(report_table_variants, "MARINE ANIMALS", "unlisted_removed")
lichens_unlistedRemoved <- get_variant_table(report_table_variants, "LICHENS", "unlisted_removed")
macrofungi_unlistedRemoved <- get_variant_table(report_table_variants, "MACROFUNGI", "unlisted_removed")
vascular_plants_unlistedRemoved <- get_variant_table(report_table_variants, "VASCULAR PLANTS", "unlisted_removed")
nonvascular_plants_unlistedRemoved <- get_variant_table(report_table_variants, "NON-VASCULAR PLANTS", "unlisted_removed")
slime_molds_unlistedRemoved <- get_variant_table(report_table_variants, "SLIME MOLDS", "unlisted_removed")
unassigned_unlistedRemoved <- get_variant_table(report_table_variants, "UNASSIGNED", "unlisted_removed")

# INCORRECT LOCALITY ONLY
terrestrial_mammals_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "TERRESTRIAL MAMMALS", "incorrect_locality_removed")
amphibians_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "AMPHIBIANS", "incorrect_locality_removed")
reptiles_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "REPTILES", "incorrect_locality_removed")
birds_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "BIRDS", "incorrect_locality_removed")
terrestrial_inverts_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "TERRESTRIAL INVERTS", "incorrect_locality_removed")
freshwater_algae_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "FRESHWATER ALGAE", "incorrect_locality_removed")
marine_algae_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "MARINE ALGAE", "incorrect_locality_removed")
marine_animals_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "MARINE ANIMALS", "incorrect_locality_removed")
lichens_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "LICHENS", "incorrect_locality_removed")
macrofungi_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "MACROFUNGI", "incorrect_locality_removed")
vascular_plants_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "VASCULAR PLANTS", "incorrect_locality_removed")
nonvascular_plants_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "NON-VASCULAR PLANTS", "incorrect_locality_removed")
slime_molds_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "SLIME MOLDS", "incorrect_locality_removed")
unassigned_incorrectLocalityRemoved <- get_variant_table(report_table_variants, "UNASSIGNED", "incorrect_locality_removed")

# UNLISTED ONLY
terrestrial_mammals_unlisted <- get_variant_table(report_table_variants, "TERRESTRIAL MAMMALS", "unlisted")
amphibians_unlisted <- get_variant_table(report_table_variants, "AMPHIBIANS", "unlisted")
reptiles_unlisted <- get_variant_table(report_table_variants, "REPTILES", "unlisted")
birds_unlisted <- get_variant_table(report_table_variants, "BIRDS", "unlisted")
terrestrial_inverts_unlisted <- get_variant_table(report_table_variants, "TERRESTRIAL INVERTS", "unlisted")
freshwater_algae_unlisted <- get_variant_table(report_table_variants, "FRESHWATER ALGAE", "unlisted")
marine_algae_unlisted <- get_variant_table(report_table_variants, "MARINE ALGAE", "unlisted")
marine_animals_unlisted <- get_variant_table(report_table_variants, "MARINE ANIMALS", "unlisted")
lichens_unlisted <- get_variant_table(report_table_variants, "LICHENS", "unlisted")
macrofungi_unlisted <- get_variant_table(report_table_variants, "MACROFUNGI", "unlisted")
vascular_plants_unlisted <- get_variant_table(report_table_variants, "VASCULAR PLANTS", "unlisted")
nonvascular_plants_unlisted <- get_variant_table(report_table_variants, "NON-VASCULAR PLANTS", "unlisted")
slime_molds_unlisted <- get_variant_table(report_table_variants, "SLIME MOLDS", "unlisted")
unassigned_unlisted <- get_variant_table(report_table_variants, "UNASSIGNED", "unlisted")

# ============================================================
# WRITE FINAL STANDARDIZED CURATED CHECKLIST CSVs
# ============================================================

purrr::iwalk(
  report_table_variants,
  function(variants, grp) {
    slug <- group_slug(grp)
    export_fields <- report_field_map[[grp]]
    
    if (is.null(export_fields)) {
      warning(
        "No export field map defined for group: ",
        grp,
        ". Using base_export_fields."
      )
      export_fields <- base_export_fields
    }
    
    build_curated_csv(variants$all, export_fields) |>
      safe_write_csv(file.path(final_csv_dir, paste0(slug, "_all.csv")))
    
    build_curated_csv(variants$unlisted_removed, export_fields) |>
      safe_write_csv(file.path(final_csv_dir, paste0(slug, "_unlistedRemoved.csv")))
    
    build_curated_csv(variants$unlisted, export_fields) |>
      safe_write_csv(file.path(final_csv_dir, paste0(slug, "_unlisted.csv")))
    
    build_curated_csv(variants$incorrect_locality_removed, export_fields) |>
      safe_write_csv(file.path(final_csv_dir, paste0(slug, "_incorrectLocalityRemoved.csv")))
  }
)

# ============================================================
# OPTIONAL FINAL CHECKS
# ============================================================

curated_counts_all <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$all |>
      count(report_supergroup, report_group, report_detail, sort = TRUE)
  }
)

curated_counts_unlistedRemoved <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$unlisted_removed |>
      count(report_supergroup, report_group, report_detail, sort = TRUE)
  }
)

curated_counts_unlisted <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$unlisted |>
      count(report_supergroup, report_group, report_detail, sort = TRUE)
  }
)

curated_counts_incorrectLocalityRemoved <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$incorrect_locality_removed |>
      count(report_supergroup, report_group, report_detail, sort = TRUE)
  }
)

print(curated_counts_all, n = Inf)
print(curated_counts_unlistedRemoved, n = Inf)
print(curated_counts_unlisted, n = Inf)
print(curated_counts_incorrectLocalityRemoved, n = Inf)

curated_missing_rankings_all <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$all |>
      filter(is.na(provincial_status)) |>
      distinct(scientificName, source_file, report_group)
  }
)

curated_missing_rankings_unlistedRemoved <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$unlisted_removed |>
      filter(is.na(provincial_status)) |>
      distinct(scientificName, source_file, report_group)
  }
)

curated_missing_rankings_unlisted <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$unlisted |>
      filter(is.na(provincial_status)) |>
      distinct(scientificName, source_file, report_group)
  }
)

curated_missing_rankings_incorrectLocalityRemoved <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    variants$incorrect_locality_removed |>
      filter(is.na(provincial_status)) |>
      distinct(scientificName, source_file, report_group)
  }
)

print(curated_missing_rankings_all, n = 100)
print(curated_missing_rankings_unlistedRemoved, n = 100)
print(curated_missing_rankings_unlisted, n = 100)
print(curated_missing_rankings_incorrectLocalityRemoved, n = 100)

safe_write_csv(curated_counts_all, file.path(qc_dir, "curated_report_group_counts_all.csv"))
safe_write_csv(curated_counts_unlistedRemoved, file.path(qc_dir, "curated_report_group_counts_unlistedRemoved.csv"))
safe_write_csv(curated_counts_unlisted, file.path(qc_dir, "curated_report_group_counts_unlisted.csv"))
safe_write_csv(curated_counts_incorrectLocalityRemoved, file.path(qc_dir, "curated_report_group_counts_incorrectLocalityRemoved.csv"))

safe_write_csv(curated_missing_rankings_all, file.path(qc_dir, "curated_missing_rankings_all.csv"))
safe_write_csv(curated_missing_rankings_unlistedRemoved, file.path(qc_dir, "curated_missing_rankings_unlistedRemoved.csv"))
safe_write_csv(curated_missing_rankings_unlisted, file.path(qc_dir, "curated_missing_rankings_unlisted.csv"))
safe_write_csv(curated_missing_rankings_incorrectLocalityRemoved, file.path(qc_dir, "curated_missing_rankings_incorrectLocalityRemoved.csv"))

safe_write_csv(flag_variant_summary, file.path(qc_dir, "curated_flag_variant_summary.csv"))

# ============================================================
# DIAGNOSTICS
# ============================================================

message("Observed report_group values in curated data:")
print(sort(unique(howe_sound_curated_all$report_group)))

howe_sound_curated_all |>
  count(report_group, sort = TRUE) |>
  print(n = Inf)

howe_sound_curated_all |>
  distinct(source_file, source_code, report_group, report_supergroup) |>
  arrange(report_group, source_file) |>
  print(n = Inf)

purrr::imap_dfr(
  report_table_variants,
  ~ tibble(
    report_group = .y,
    n_all = nrow(.x$all),
    n_unlistedRemoved = nrow(.x$unlisted_removed),
    n_unlisted = nrow(.x$unlisted),
    n_incorrectLocalityRemoved = nrow(.x$incorrect_locality_removed)
  )
) |>
  arrange(report_group) |>
  print(n = Inf)

# ============================================================
# PREVIEW
# ============================================================

names(howe_sound_curated_all)
dim(howe_sound_curated_all)
names(report_tables)
names(report_table_variants)

names(howe_sound_curated_all)






# ============================================================
# APPENDIX-FORMATTED TABLE EXPORTS
# FINAL MAPPED VERSION
# ============================================================

library(gt)
library(htmltools)

# ============================================================
# APPENDIX OUTPUT DIRECTORIES (USER-DEFINED)
# ============================================================

appendix_dir <- "/Users/andrewsimon/Desktop/Howe_Sound/lists/tables"

appendix_html_dir <- file.path(appendix_dir, "html")
appendix_csv_dir  <- file.path(appendix_dir, "csv")

dir.create(appendix_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(appendix_html_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(appendix_csv_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# GROUPING RULES
# ------------------------------------------------------------

appendix_grouping_rules <- list(
  "BIRDS" = "family",
  "TERRESTRIAL MAMMALS" = "order",
  "AMPHIBIANS" = "order",
  "REPTILES" = "order",
  "TERRESTRIAL INVERTS" = "class",
  "FRESHWATER ALGAE" = "phylum",
  "MARINE ALGAE" = "phylum",
  "MARINE ANIMALS" = "phylum",
  "LICHENS" = "family",
  "MACROFUNGI" = "family",
  "VASCULAR PLANTS" = "family",
  "NON-VASCULAR PLANTS" = "family",
  "SLIME MOLDS" = "family",
  "UNASSIGNED" = NA_character_
)

# ------------------------------------------------------------
# COLUMN RULES
# scientificNameAuthorship only for:
# vascular plants, non-vascular plants, marine algae,
# lichens, macrofungi, slime molds
# ------------------------------------------------------------

appendix_field_map <- list(
  "BIRDS" = c(
    "family",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status",
    "residencyStatus"
  ),
  "TERRESTRIAL MAMMALS" = c(
    "order",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "AMPHIBIANS" = c(
    "order",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "REPTILES" = c(
    "order",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "TERRESTRIAL INVERTS" = c(
    "order",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "FRESHWATER ALGAE" = c(
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "MARINE ALGAE" = c(
    "phylum",
    "scientificName",
    "scientificNameAuthorship",
    "commonName"
  ),
  "MARINE ANIMALS" = c(
    "phylum",
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "LICHENS" = c(
    "family",
    "scientificName",
    "scientificNameAuthorship",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "MACROFUNGI" = c(
    "family",
    "scientificName",
    "scientificNameAuthorship",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "VASCULAR PLANTS" = c(
    "family",
    "scientificName",
    "scientificNameAuthorship",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "NON-VASCULAR PLANTS" = c(
    "family",
    "scientificName",
    "scientificNameAuthorship",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  ),
  "SLIME MOLDS" = c(
    "family",
    "scientificName",
    "scientificNameAuthorship",
    "commonName",
    "provincial_status",
    "bc_list"
  ),
  "UNASSIGNED" = c(
    "scientificName",
    "commonName",
    "provincial_status",
    "bc_list",
    "national_status",
    "SARA_status"
  )
)

# ------------------------------------------------------------
# OPTIONAL: HUMAN-READABLE TITLES
# ------------------------------------------------------------

appendix_titles <- c(
  "BIRDS" = "Birds",
  "TERRESTRIAL MAMMALS" = "Terrestrial mammals",
  "AMPHIBIANS" = "Amphibians",
  "REPTILES" = "Reptiles",
  "TERRESTRIAL INVERTS" = "Terrestrial invertebrates",
  "FRESHWATER ALGAE" = "Freshwater algae",
  "MARINE ALGAE" = "Marine algae",
  "MARINE ANIMALS" = "Marine animals",
  "LICHENS" = "Lichens",
  "MACROFUNGI" = "Macrofungi",
  "VASCULAR PLANTS" = "Vascular plants",
  "NON-VASCULAR PLANTS" = "Non-vascular plants",
  "SLIME MOLDS" = "Slime molds",
  "UNASSIGNED" = "Unassigned"
)

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

clean_appendix_value <- function(x, blank = "") {
  x <- as.character(x)
  x <- str_trim(x)
  x[is.na(x)] <- blank
  x[x %in% c("NA", "Na", "N/A", "na")] <- blank
  x
}

extract_year_from_string <- function(x) {
  x <- clean_appendix_value(x)
  yr <- str_extract(x, "(19|20)\\d{2}")
  yr[is.na(yr)] <- ""
  yr
}

remove_not_at_risk <- function(x) {
  x <- clean_appendix_value(x)
  x <- str_replace_all(x, regex("\\bNot\\s+at\\s+Risk\\b", ignore_case = TRUE), "")
  x <- str_replace_all(x, "\\s*;\\s*", "; ")
  x <- str_replace_all(x, "\\s*,\\s*", ", ")
  x <- str_replace_all(x, "\\(\\s*\\)", "")
  x <- str_replace_all(x, "\\s{2,}", " ")
  x <- str_replace_all(x, "^[:;,-]+\\s*", "")
  x <- str_replace_all(x, "\\s*[:;,-]+$", "")
  x <- str_trim(x)
  x
}

append_year_to_status <- function(status, date_field) {
  status <- remove_not_at_risk(status)
  year   <- extract_year_from_string(date_field)
  
  ifelse(
    status != "" & year != "",
    paste0(status, " (", year, ")"),
    status
  )
}

format_sara_status <- function(schedule, status, sara_date) {
  schedule <- clean_appendix_value(schedule)
  schedule_num <- str_extract(schedule, "\\d+")
  schedule_num[is.na(schedule_num)] <- ""
  
  status <- remove_not_at_risk(status)
  year   <- extract_year_from_string(sara_date)
  
  prefix <- ifelse(schedule_num != "", paste0("S", schedule_num), "")
  
  body <- dplyr::case_when(
    prefix != "" & status != "" ~ paste0(prefix, ": ", status),
    prefix != "" & status == "" ~ prefix,
    prefix == "" & status != "" ~ status,
    TRUE ~ ""
  )
  
  ifelse(
    body != "" & year != "",
    paste0(body, " (", year, ")"),
    body
  )
}

get_appendix_title <- function(grp) {
  if (grp %in% names(appendix_titles)) {
    appendix_titles[[grp]]
  } else {
    grp
  }
}

# ------------------------------------------------------------
# PRINT ORGANIZING CLASSIFICATION VALUES
# ------------------------------------------------------------

clean_lookup_value <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[is.na(x)] <- ""
  x[x %in% c("NA", "Na", "N/A", "na")] <- ""
  x
}

organizing_values <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    
    df <- variants$unlisted_removed
    grouping_field <- appendix_grouping_rules[[grp]]
    
    if (is.null(df) || nrow(df) == 0 || is.na(grouping_field) || !grouping_field %in% names(df)) {
      return(NULL)
    }
    
    tibble::tibble(
      report_group = grp,
      classification_rank = grouping_field,
      classification_value = clean_lookup_value(df[[grouping_field]])
    ) |>
      dplyr::filter(classification_value != "") |>
      dplyr::distinct() |>
      dplyr::arrange(report_group, classification_rank, classification_value)
  }
)

organizing_values_terrestrial_invert_orders <- {
  df <- report_table_variants[["TERRESTRIAL INVERTS"]]$unlisted_removed
  
  if (is.null(df) || nrow(df) == 0 || !"order" %in% names(df)) {
    tibble::tibble(
      report_group = character(),
      classification_rank = character(),
      classification_value = character()
    )
  } else {
    tibble::tibble(
      report_group = "TERRESTRIAL INVERTS",
      classification_rank = "order",
      classification_value = clean_lookup_value(df[["order"]])
    ) |>
      dplyr::filter(classification_value != "") |>
      dplyr::distinct() |>
      dplyr::arrange(report_group, classification_rank, classification_value)
  }
}

organizing_values_all <- dplyr::bind_rows(
  organizing_values,
  organizing_values_terrestrial_invert_orders
) |>
  dplyr::distinct() |>
  dplyr::arrange(report_group, classification_rank, classification_value)

print(organizing_values_all, n = Inf)

safe_write_csv(
  organizing_values_all,
  file.path(appendix_dir, "organizing_values_for_friendly_names.csv")
)

# ------------------------------------------------------------
# FRIENDLY-NAME LOOKUP
# Common names first, Latin in parentheses
# ------------------------------------------------------------

make_display_label <- function(friendly_name, latin_name) {
  friendly_name <- clean_appendix_value(friendly_name)
  latin_name <- clean_appendix_value(latin_name)
  
  ifelse(
    friendly_name != "",
    paste0(friendly_name, " (", latin_name, ")"),
    latin_name
  )
}

classification_friendly_overrides <- tribble(
  ~report_group, ~classification_rank, ~classification_value, ~friendly_name,
  
  # BIRDS
  "BIRDS", "family", "Accipitridae", "hawks, eagles, and kites",
  "BIRDS", "family", "Aegithalidae", "long-tailed tits",
  "BIRDS", "family", "Alaudidae", "larks",
  "BIRDS", "family", "Alcedinidae", "kingfishers",
  "BIRDS", "family", "Alcidae", "auks",
  "BIRDS", "family", "Anatidae", "ducks, geese, and swans",
  "BIRDS", "family", "Apodidae", "swifts",
  "BIRDS", "family", "Ardeidae", "herons, egrets, and bitterns",
  "BIRDS", "family", "Bombycillidae", "waxwings",
  "BIRDS", "family", "Calcariidae", "longspurs and snow buntings",
  "BIRDS", "family", "Caprimulgidae", "nightjars",
  "BIRDS", "family", "Cardinalidae", "cardinals, grosbeaks, and buntings",
  "BIRDS", "family", "Cathartidae", "New World vultures",
  "BIRDS", "family", "Certhiidae", "treecreepers",
  "BIRDS", "family", "Charadriidae", "plovers",
  "BIRDS", "family", "Cinclidae", "dippers",
  "BIRDS", "family", "Columbidae", "pigeons and doves",
  "BIRDS", "family", "Corvidae", "crows, jays, and magpies",
  "BIRDS", "family", "Cuculidae", "cuckoos",
  "BIRDS", "family", "Falconidae", "falcons and caracaras",
  "BIRDS", "family", "Fringillidae", "finches",
  "BIRDS", "family", "Gaviidae", "loons",
  "BIRDS", "family", "Gruidae", "cranes",
  "BIRDS", "family", "Haematopodidae", "oystercatchers",
  "BIRDS", "family", "Hirundinidae", "swallows and martins",
  "BIRDS", "family", "Hydrobatidae", "storm-petrels",
  "BIRDS", "family", "Icteridae", "blackbirds and orioles",
  "BIRDS", "family", "Icteriidae", "yellow-breasted chats",
  "BIRDS", "family", "Laniidae", "shrikes",
  "BIRDS", "family", "Laridae", "gulls, terns, and skimmers",
  "BIRDS", "family", "Mimidae", "mockingbirds and thrashers",
  "BIRDS", "family", "Motacillidae", "wagtails and pipits",
  "BIRDS", "family", "Odontophoridae", "New World quail",
  "BIRDS", "family", "Pandionidae", "ospreys",
  "BIRDS", "family", "Paridae", "chickadees and tits",
  "BIRDS", "family", "Parulidae", "New World warblers",
  "BIRDS", "family", "Passerellidae", "New World sparrows",
  "BIRDS", "family", "Passeridae", "Old World sparrows",
  "BIRDS", "family", "Pelecanidae", "pelicans",
  "BIRDS", "family", "Phalacrocoracidae", "cormorants and shags",
  "BIRDS", "family", "Phasianidae", "pheasants, grouse, and allies",
  "BIRDS", "family", "Picidae", "woodpeckers",
  "BIRDS", "family", "Podicipedidae", "grebes",
  "BIRDS", "family", "Polioptilidae", "gnatcatchers",
  "BIRDS", "family", "Procellariidae", "shearwaters and petrels",
  "BIRDS", "family", "Rallidae", "rails, gallinules, and coots",
  "BIRDS", "family", "Recurvirostridae", "stilts and avocets",
  "BIRDS", "family", "Regulidae", "kinglets",
  "BIRDS", "family", "Scolopacidae", "sandpipers and allies",
  "BIRDS", "family", "Sittidae", "nuthatches",
  "BIRDS", "family", "Stercorariidae", "skuas and jaegers",
  "BIRDS", "family", "Strigidae", "typical owls",
  "BIRDS", "family", "Sturnidae", "starlings",
  "BIRDS", "family", "Threskiornithidae", "ibises and spoonbills",
  "BIRDS", "family", "Trochilidae", "hummingbirds",
  "BIRDS", "family", "Troglodytidae", "wrens",
  "BIRDS", "family", "Turdidae", "thrushes",
  "BIRDS", "family", "Tyrannidae", "tyrant flycatchers",
  "BIRDS", "family", "Tytonidae", "barn owls",
  "BIRDS", "family", "Vireonidae", "vireos",
  
  # TERRESTRIAL MAMMALS
  "TERRESTRIAL MAMMALS", "order", "Artiodactyla", "even-toed ungulates",
  "TERRESTRIAL MAMMALS", "order", "Carnivora", "carnivorans",
  "TERRESTRIAL MAMMALS", "order", "Chiroptera", "bats",
  "TERRESTRIAL MAMMALS", "order", "Didelphimorphia", "opossums",
  "TERRESTRIAL MAMMALS", "order", "Eulipotyphla", "shrews, moles, and hedgehog allies",
  "TERRESTRIAL MAMMALS", "order", "Lagomorpha", "hares, rabbits, and pikas",
  "TERRESTRIAL MAMMALS", "order", "Rodentia", "rodents",
  
  # AMPHIBIANS
  "AMPHIBIANS", "order", "Anura", "frogs and toads",
  "AMPHIBIANS", "order", "Caudata", "salamanders",
  
  # REPTILES
  "REPTILES", "order", "Squamata", "lizards and snakes",
  "REPTILES", "order", "Testudines", "turtles",
  
  # TERRESTRIAL INVERTS - CLASS
  "TERRESTRIAL INVERTS", "class", "Arachnida", "arachnids",
  "TERRESTRIAL INVERTS", "class", "Bivalvia", "bivalves",
  "TERRESTRIAL INVERTS", "class", "Chilopoda", "centipedes",
  "TERRESTRIAL INVERTS", "class", "Clitellata", "clitellates",
  "TERRESTRIAL INVERTS", "class", "Diplopoda", "millipedes",
  "TERRESTRIAL INVERTS", "class", "Entognatha", "springtails and allies",
  "TERRESTRIAL INVERTS", "class", "Gastropoda", "snails and slugs",
  "TERRESTRIAL INVERTS", "class", "Insecta", "insects",
  "TERRESTRIAL INVERTS", "class", "Malacostraca", "malacostracans",
  
  # TERRESTRIAL INVERTS - ORDER
  "TERRESTRIAL INVERTS", "order", "Araneae", "spiders",
  "TERRESTRIAL INVERTS", "order", "Archaeognatha", "jumping bristletails",
  "TERRESTRIAL INVERTS", "order", "Blattodea", "cockroaches and termites",
  "TERRESTRIAL INVERTS", "order", "Chordeumatida", "millipedes",
  "TERRESTRIAL INVERTS", "order", "Coleoptera", "beetles",
  "TERRESTRIAL INVERTS", "order", "Crassiclitellata", "earthworms and allies",
  "TERRESTRIAL INVERTS", "order", "Dermaptera", "earwigs",
  "TERRESTRIAL INVERTS", "order", "Diptera", "flies",
  "TERRESTRIAL INVERTS", "order", "Ellobiida", "air-breathing snails",
  "TERRESTRIAL INVERTS", "order", "Entomobryomorpha", "springtails",
  "TERRESTRIAL INVERTS", "order", "Ephemeroptera", "mayflies",
  "TERRESTRIAL INVERTS", "order", "Hemiptera", "true bugs",
  "TERRESTRIAL INVERTS", "order", "Hymenoptera", "bees, wasps, ants, and sawflies",
  "TERRESTRIAL INVERTS", "order", "Isopoda", "woodlice and allies",
  "TERRESTRIAL INVERTS", "order", "Ixodida", "ticks",
  "TERRESTRIAL INVERTS", "order", "Julida", "millipedes",
  "TERRESTRIAL INVERTS", "order", "Lepidoptera", "butterflies and moths",
  "TERRESTRIAL INVERTS", "order", "Mecoptera", "scorpionflies",
  "TERRESTRIAL INVERTS", "order", "Megaloptera", "alderflies, dobsonflies, and fishflies",
  "TERRESTRIAL INVERTS", "order", "Mesostigmata", "mesostigmatid mites",
  "TERRESTRIAL INVERTS", "order", "Neuroptera", "lacewings and allies",
  "TERRESTRIAL INVERTS", "order", "Notoptera", "ice crawlers and rock crawlers",
  "TERRESTRIAL INVERTS", "order", "Odonata", "dragonflies and damselflies",
  "TERRESTRIAL INVERTS", "order", "Opiliones", "harvestmen",
  "TERRESTRIAL INVERTS", "order", "Orthoptera", "grasshoppers, crickets, and katydids",
  "TERRESTRIAL INVERTS", "order", "Plecoptera", "stoneflies",
  "TERRESTRIAL INVERTS", "order", "Poduromorpha", "springtails",
  "TERRESTRIAL INVERTS", "order", "Polydesmida", "flat-backed millipedes",
  "TERRESTRIAL INVERTS", "order", "Psocodea", "barklice, booklice, and parasitic lice",
  "TERRESTRIAL INVERTS", "order", "Sarcoptiformes", "oribatid and astigmatid mites",
  "TERRESTRIAL INVERTS", "order", "Scolopendromorpha", "large centipedes",
  "TERRESTRIAL INVERTS", "order", "Siphonaptera", "fleas",
  "TERRESTRIAL INVERTS", "order", "Sphaeriida", "pea clams",
  "TERRESTRIAL INVERTS", "order", "Stylommatophora", "land snails and slugs",
  "TERRESTRIAL INVERTS", "order", "Symphypleona", "globular springtails",
  "TERRESTRIAL INVERTS", "order", "Trichoptera", "caddisflies",
  "TERRESTRIAL INVERTS", "order", "Trochida", "top snails and allies",
  "TERRESTRIAL INVERTS", "order", "Trombidiformes", "prostigmatid mites",
  "TERRESTRIAL INVERTS", "order", "Zygentoma", "silverfish and firebrats",
  
  # MARINE ALGAE
  "MARINE ALGAE", "phylum", "Chlorophyta", "green algae",
  "MARINE ALGAE", "phylum", "Ochrophyta", "brown algae",
  "MARINE ALGAE", "phylum", "Rhodophyta", "red algae",
  
  # MARINE ANIMALS
  "MARINE ANIMALS", "phylum", "Annelida", "segmented worms",
  "MARINE ANIMALS", "phylum", "Arthropoda", "arthropods",
  "MARINE ANIMALS", "phylum", "Brachiopoda", "lamp shells",
  "MARINE ANIMALS", "phylum", "Bryozoa", "moss animals",
  "MARINE ANIMALS", "phylum", "Chaetognatha", "arrow worms",
  "MARINE ANIMALS", "phylum", "Chordata", "chordates",
  "MARINE ANIMALS", "phylum", "Cnidaria", "cnidarians",
  "MARINE ANIMALS", "phylum", "Ctenophora", "comb jellies",
  "MARINE ANIMALS", "phylum", "Echinodermata", "echinoderms",
  "MARINE ANIMALS", "phylum", "Entoprocta", "goblet worms",
  "MARINE ANIMALS", "phylum", "Mollusca", "molluscs",
  "MARINE ANIMALS", "phylum", "Nemertea", "ribbon worms",
  "MARINE ANIMALS", "phylum", "Nemertera", "ribbon worms",
  "MARINE ANIMALS", "phylum", "Phoronida", "horseshoe worms",
  "MARINE ANIMALS", "phylum", "Platyhelminthes", "flatworms",
  "MARINE ANIMALS", "phylum", "Porifera", "sponges",
  "MARINE ANIMALS", "phylum", "Rotifera", "rotifers",
  "MARINE ANIMALS", "phylum", "Sipuncula", "peanut worms",
  
  # SLIME MOLDS
  "SLIME MOLDS", "family", "Amaurochaetaceae", "sooty slime molds",
  "SLIME MOLDS", "family", "Arcyriaceae", "netted slime molds",
  "SLIME MOLDS", "family", "Ceratiomyxaceae", "coral slime molds",
  "SLIME MOLDS", "family", "Cribrariaceae", "basket slime molds",
  "SLIME MOLDS", "family", "Dianemataceae", "slime molds",
  "SLIME MOLDS", "family", "Didymiaceae", "lime-capped slime molds",
  "SLIME MOLDS", "family", "Hemitrichiaceae", "golden slime molds",
  "SLIME MOLDS", "family", "Physaraceae", "bubble slime molds",
  "SLIME MOLDS", "family", "Reticulariaceae", "net slime molds",
  "SLIME MOLDS", "family", "Stemonitidaceae", "thread slime molds",
  "SLIME MOLDS", "family", "Trichiaceae", "hair-cap slime molds",
  
  # VASCULAR PLANTS
  "VASCULAR PLANTS", "family", "Alismataceae", "water-plantains",
  "VASCULAR PLANTS", "family", "Amaranthaceae", "amaranths",
  "VASCULAR PLANTS", "family", "Amaryllidaceae", "amaryllis family",
  "VASCULAR PLANTS", "family", "Anacardiaceae", "sumac family",
  "VASCULAR PLANTS", "family", "Apiaceae", "carrot family",
  "VASCULAR PLANTS", "family", "Apocynaceae", "dogbane family",
  "VASCULAR PLANTS", "family", "Aquifoliaceae", "holly family",
  "VASCULAR PLANTS", "family", "Araceae", "arum family",
  "VASCULAR PLANTS", "family", "Araliaceae", "ginseng family",
  "VASCULAR PLANTS", "family", "Aristolochiaceae", "birthwort family",
  "VASCULAR PLANTS", "family", "Asparagaceae", "asparagus family",
  "VASCULAR PLANTS", "family", "Aspleniaceae", "spleenwort family",
  "VASCULAR PLANTS", "family", "Asteraceae", "aster family",
  "VASCULAR PLANTS", "family", "Athyriaceae", "lady-fern family",
  "VASCULAR PLANTS", "family", "Balsaminaceae", "touch-me-not family",
  "VASCULAR PLANTS", "family", "Berberidaceae", "barberry family",
  "VASCULAR PLANTS", "family", "Betulaceae", "birch family",
  "VASCULAR PLANTS", "family", "Blechnaceae", "hard-fern family",
  "VASCULAR PLANTS", "family", "Boraginaceae", "borage family",
  "VASCULAR PLANTS", "family", "Brassicaceae", "mustard family",
  "VASCULAR PLANTS", "family", "Cabombaceae", "water-shield family",
  "VASCULAR PLANTS", "family", "Campanulaceae", "bellflower family",
  "VASCULAR PLANTS", "family", "Cannabaceae", "hemp family",
  "VASCULAR PLANTS", "family", "Caprifoliaceae", "honeysuckle family",
  "VASCULAR PLANTS", "family", "Caryophyllaceae", "pink family",
  "VASCULAR PLANTS", "family", "Celastraceae", "staff-vine family",
  "VASCULAR PLANTS", "family", "Ceratophyllaceae", "hornwort family",
  "VASCULAR PLANTS", "family", "Convolvulaceae", "morning-glory family",
  "VASCULAR PLANTS", "family", "Cornaceae", "dogwood family",
  "VASCULAR PLANTS", "family", "Crassulaceae", "stonecrop family",
  "VASCULAR PLANTS", "family", "Cupressaceae", "cypress family",
  "VASCULAR PLANTS", "family", "Cyperaceae", "sedge family",
  "VASCULAR PLANTS", "family", "Cystopteridaceae", "bladder-fern family",
  "VASCULAR PLANTS", "family", "Dennstaedtiaceae", "bracken family",
  "VASCULAR PLANTS", "family", "Droseraceae", "sundew family",
  "VASCULAR PLANTS", "family", "Dryopteridaceae", "wood-fern family",
  "VASCULAR PLANTS", "family", "Elaeagnaceae", "oleaster family",
  "VASCULAR PLANTS", "family", "Equisetaceae", "horsetail family",
  "VASCULAR PLANTS", "family", "Ericaceae", "heath family",
  "VASCULAR PLANTS", "family", "Euphorbiaceae", "spurge family",
  "VASCULAR PLANTS", "family", "Fabaceae", "pea family",
  "VASCULAR PLANTS", "family", "Fagaceae", "beech family",
  "VASCULAR PLANTS", "family", "Gentianaceae", "gentian family",
  "VASCULAR PLANTS", "family", "Geraniaceae", "geranium family",
  "VASCULAR PLANTS", "family", "Grossulariaceae", "currant family",
  "VASCULAR PLANTS", "family", "Haloragaceae", "water-milfoil family",
  "VASCULAR PLANTS", "family", "Hydrangeaceae", "hydrangea family",
  "VASCULAR PLANTS", "family", "Hypericaceae", "St. John\'s-wort family",
  "VASCULAR PLANTS", "family", "Iridaceae", "iris family",
  "VASCULAR PLANTS", "family", "Isoetaceae", "quillwort family",
  "VASCULAR PLANTS", "family", "Juglandaceae", "walnut family",
  "VASCULAR PLANTS", "family", "Juncaceae", "rush family",
  "VASCULAR PLANTS", "family", "Juncaginaceae", "arrow-grass family",
  "VASCULAR PLANTS", "family", "Lamiaceae", "mint family",
  "VASCULAR PLANTS", "family", "Lentibulariaceae", "bladderwort family",
  "VASCULAR PLANTS", "family", "Liliaceae", "lily family",
  "VASCULAR PLANTS", "family", "Linaceae", "flax family",
  "VASCULAR PLANTS", "family", "Lycopodiaceae", "clubmoss family",
  "VASCULAR PLANTS", "family", "Lythraceae", "loosestrife family",
  "VASCULAR PLANTS", "family", "Malvaceae", "mallow family",
  "VASCULAR PLANTS", "family", "Melanthiaceae", "bunchflower family",
  "VASCULAR PLANTS", "family", "Menyanthaceae", "bogbean family",
  "VASCULAR PLANTS", "family", "Montiaceae", "miner's-lettuce family",
  "VASCULAR PLANTS", "family", "Myricaceae", "bayberry family",
  "VASCULAR PLANTS", "family", "Nymphaeaceae", "water-lily family",
  "VASCULAR PLANTS", "family", "Oleaceae", "olive family",
  "VASCULAR PLANTS", "family", "Onagraceae", "evening-primrose family",
  "VASCULAR PLANTS", "family", "Ophioglossaceae", "adder's-tongue family",
  "VASCULAR PLANTS", "family", "Orchidaceae", "orchid family",
  "VASCULAR PLANTS", "family", "Orobanchaceae", "broomrape family",
  "VASCULAR PLANTS", "family", "Oxalidaceae", "wood-sorrel family",
  "VASCULAR PLANTS", "family", "Papaveraceae", "poppy family",
  "VASCULAR PLANTS", "family", "Phrymaceae", "lopseed family",
  "VASCULAR PLANTS", "family", "Pinaceae", "pine family",
  "VASCULAR PLANTS", "family", "Plantaginaceae", "plantain family",
  "VASCULAR PLANTS", "family", "Poaceae", "grass family",
  "VASCULAR PLANTS", "family", "Polemoniaceae", "phlox family",
  "VASCULAR PLANTS", "family", "Polygonaceae", "knotweed family",
  "VASCULAR PLANTS", "family", "Polypodiaceae", "polypody family",
  "VASCULAR PLANTS", "family", "Portulacaceae", "purslane family",
  "VASCULAR PLANTS", "family", "Potamogetonaceae", "pondweed family",
  "VASCULAR PLANTS", "family", "Primulaceae", "primrose family",
  "VASCULAR PLANTS", "family", "Pteridaceae", "brake family",
  "VASCULAR PLANTS", "family", "Ranunculaceae", "buttercup family",
  "VASCULAR PLANTS", "family", "Rhamnaceae", "buckthorn family",
  "VASCULAR PLANTS", "family", "Rosaceae", "rose family",
  "VASCULAR PLANTS", "family", "Rubiaceae", "madder family",
  "VASCULAR PLANTS", "family", "Ruppiaceae", "ditch-grass family",
  "VASCULAR PLANTS", "family", "Salicaceae", "willow family",
  "VASCULAR PLANTS", "family", "Santalaceae", "sandalwood family",
  "VASCULAR PLANTS", "family", "Sapindaceae", "soapberry family",
  "VASCULAR PLANTS", "family", "Saxifragaceae", "saxifrage family",
  "VASCULAR PLANTS", "family", "Scheuchzeriaceae", "Scheuchzeria family",
  "VASCULAR PLANTS", "family", "Scrophulariaceae", "figwort family",
  "VASCULAR PLANTS", "family", "Selaginellaceae", "spikemoss family",
  "VASCULAR PLANTS", "family", "Simaroubaceae", "quassia family",
  "VASCULAR PLANTS", "family", "Solanaceae", "nightshade family",
  "VASCULAR PLANTS", "family", "Taxaceae", "yew family",
  "VASCULAR PLANTS", "family", "Thelypteridaceae", "marsh-fern family",
  "VASCULAR PLANTS", "family", "Thymelaeaceae", "mezereum family",
  "VASCULAR PLANTS", "family", "Tofieldiaceae", "false asphodel family",
  "VASCULAR PLANTS", "family", "Typhaceae", "cattail family",
  "VASCULAR PLANTS", "family", "Urticaceae", "nettle family",
  "VASCULAR PLANTS", "family", "Viburnaceae", "viburnum family",
  "VASCULAR PLANTS", "family", "Violaceae", "violet family",
  "VASCULAR PLANTS", "family", "Vitaceae", "grape family",
  "VASCULAR PLANTS", "family", "Woodsiaceae", "cliff-fern family",
  "VASCULAR PLANTS", "family", "Zosteraceae", "eelgrass family"
)

classification_display_lookup <- organizing_values_all |>
  left_join(
    classification_friendly_overrides,
    by = c("report_group", "classification_rank", "classification_value")
  ) |>
  mutate(
    friendly_name = if_else(is.na(friendly_name), "", friendly_name),
    display_label = make_display_label(friendly_name, classification_value)
  ) |>
  arrange(report_group, classification_rank, classification_value)

print(classification_display_lookup, n = Inf)

safe_write_csv(
  classification_display_lookup,
  file.path(appendix_dir, "classification_display_lookup.csv")
)

appendix_taxon_group_value <- function(df, field, grp, display_lookup = NULL) {
  if (is.na(field) || !field %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }
  
  out <- clean_appendix_value(df[[field]])
  out[out == ""] <- "Unassigned"
  
  if (grp %in% c("LICHENS", "MACROFUNGI", "NON-VASCULAR PLANTS")) {
    return(out)
  }
  
  if (is.null(display_lookup)) {
    return(out)
  }
  
  lookup_tbl <- display_lookup |>
    filter(
      report_group == grp,
      classification_rank == field
    ) |>
    select(classification_value, display_label) |>
    distinct()
  
  matched <- tibble(classification_value = out) |>
    left_join(lookup_tbl, by = "classification_value") |>
    pull(display_label)
  
  ifelse(is.na(matched) | matched == "", out, matched)
}

prepare_appendix_df <- function(df, grp) {
  group_field <- appendix_grouping_rules[[grp]]
  keep_fields <- appendix_field_map[[grp]]
  
  if (is.null(keep_fields)) {
    keep_fields <- c(
      "scientificName",
      "commonName",
      "provincial_status",
      "bc_list",
      "national_status",
      "SARA_status"
    )
  }
  
  n_rows <- nrow(df)
  
  get_col_or_blank <- function(dat, col) {
    if (col %in% names(dat)) {
      clean_appendix_value(dat[[col]])
    } else {
      rep("", n_rows)
    }
  }
  
  scientific_name <- get_col_or_blank(df, "scientificName")
  scientific_name_authorship <- get_col_or_blank(df, "scientificNameAuthorship")
  common_name <- get_col_or_blank(df, "commonName")
  provincial_status_raw <- get_col_or_blank(df, "provincial_status")
  bc_list_vec <- get_col_or_blank(df, "bc_list")
  national_status_raw <- get_col_or_blank(df, "national_status")
  sara_status_raw <- get_col_or_blank(df, "SARA_status")
  residency_status <- get_col_or_blank(df, "residencyStatus")
  phylum_vec <- get_col_or_blank(df, "phylum")
  class_vec <- get_col_or_blank(df, "class")
  order_vec <- get_col_or_blank(df, "order")
  family_vec <- get_col_or_blank(df, "family")
  
  prov_status_review_date <- get_col_or_blank(df, "Prov Status Review Date")
  cosewic_date <- get_col_or_blank(df, "COSEWIC Date")
  sara_schedule <- get_col_or_blank(df, "SARA Schedule")
  sara_date <- get_col_or_blank(df, "SARA Date")
  
  out <- tibble(
    scientificName = scientific_name,
    scientificNameAuthorship = scientific_name_authorship,
    commonName = common_name,
    provincial_status = append_year_to_status(
      provincial_status_raw,
      prov_status_review_date
    ),
    bc_list = bc_list_vec,
    national_status = append_year_to_status(
      national_status_raw,
      cosewic_date
    ),
    SARA_status = format_sara_status(
      sara_schedule,
      sara_status_raw,
      sara_date
    ),
    residencyStatus = residency_status,
    phylum = phylum_vec,
    class = class_vec,
    order = order_vec,
    family = family_vec
  )
  
  out <- out |>
    mutate(
      appendix_grouping_value = appendix_taxon_group_value(
        out,
        field = group_field,
        grp = grp,
        display_lookup = classification_display_lookup
      ),
      order = if ("order" %in% names(out)) {
        appendix_taxon_group_value(
          out,
          field = "order",
          grp = grp,
          display_lookup = classification_display_lookup
        )
      } else {
        order
      }
    ) |>
    select(any_of(c(
      "appendix_grouping_value",
      keep_fields
    ))) |>
    distinct()
  
  if (!is.na(group_field) && group_field %in% names(out)) {
    out <- out |>
      select(-all_of(group_field))
  }
  
  if (grp == "TERRESTRIAL INVERTS") {
    out <- out |>
      arrange(appendix_grouping_value, order, scientificName, commonName)
  } else if (!is.na(group_field)) {
    out <- out |>
      arrange(appendix_grouping_value, scientificName, commonName)
  } else {
    out <- out |>
      arrange(scientificName, commonName)
  }
  
  out
}

rename_appendix_columns <- function(df) {
  nm <- names(df)
  
  nm[nm == "appendix_grouping_value"] <- "Taxonomic group"
  nm[nm == "phylum"] <- "Phylum"
  nm[nm == "class"] <- "Class"
  nm[nm == "order"] <- "Order"
  nm[nm == "family"] <- "Family"
  nm[nm == "scientificName"] <- "Scientific name"
  nm[nm == "scientificNameAuthorship"] <- "Authority"
  nm[nm == "commonName"] <- "Common name"
  nm[nm == "provincial_status"] <- "Provincial status"
  nm[nm == "bc_list"] <- "BC list"
  nm[nm == "national_status"] <- "National status"
  nm[nm == "SARA_status"] <- "SARA status"
  nm[nm == "residencyStatus"] <- "Residency status"
  
  names(df) <- nm
  df
}

build_appendix_gt <- function(df, grp) {
  group_field <- appendix_grouping_rules[[grp]]
  dat <- prepare_appendix_df(df, grp)
  
  display_df <- rename_appendix_columns(dat)
  
  if (!is.na(group_field)) {
    gt_tbl <- display_df |>
      gt(groupname_col = "Taxonomic group")
  } else {
    if ("Taxonomic group" %in% names(display_df)) {
      display_df <- display_df |>
        select(-`Taxonomic group`)
    }
    gt_tbl <- display_df |>
      gt()
  }
  
  gt_tbl |>
    tab_header(
      title = md(get_appendix_title(grp))
    ) |>
    cols_align(
      align = "left",
      columns = everything()
    ) |>
    opt_row_striping() |>
    tab_options(
      table.width = pct(100),
      table.font.size = px(11),
      heading.title.font.size = px(14),
      data_row.padding = px(3),
      row_group.font.weight = "bold"
    ) |>
    cols_width(
      everything() ~ px(140)
    )
}

# ------------------------------------------------------------
# BUILD APPENDIX TABLES FROM UNLISTED-REMOVED VARIANTS
# ------------------------------------------------------------

appendix_tables <- purrr::imap(
  report_table_variants,
  function(variants, grp) {
    df <- variants$unlisted_removed
    if (is.null(df) || nrow(df) == 0) return(NULL)
    build_appendix_gt(df, grp)
  }
)

appendix_data_export <- purrr::imap(
  report_table_variants,
  function(variants, grp) {
    df <- variants$unlisted_removed
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    dat <- prepare_appendix_df(df, grp)
    
    if ("appendix_grouping_value" %in% names(dat)) {
      dat <- dat |>
        select(-appendix_grouping_value)
    }
    
    dat
  }
)

save_gt_html_utf8 <- function(gt_tbl, filename) {
  
  html_out <- gt::as_raw_html(gt_tbl, inline_css = FALSE)
  
  html_doc <- htmltools::tagList(
    htmltools::tags$html(
      lang = "en",
      htmltools::tags$head(
        htmltools::tags$meta(charset = "utf-8")
      ),
      htmltools::tags$body(
        htmltools::HTML(html_out)
      )
    )
  )
  
  htmltools::save_html(
    html_doc,
    file = filename,
    background = "white"
  )
}

# ------------------------------------------------------------
# EXPORT HTML TABLES
# ------------------------------------------------------------

purrr::iwalk(
  appendix_tables,
  function(tbl, grp) {
    if (is.null(tbl)) return(NULL)
    
    save_gt_html_utf8(
      gt_tbl = tbl,
      filename = file.path(
        appendix_html_dir,
        paste0(group_slug(grp), "_appendix_table.html")
      )
    )
  }
)

# ------------------------------------------------------------
# EXPORT CLEAN CSV TABLES USED FOR APPENDIX FORMATTING
# ------------------------------------------------------------

purrr::iwalk(
  appendix_data_export,
  function(df, grp) {
    if (is.null(df)) return(NULL)
    
    safe_write_csv(
      rename_appendix_columns(df),
      file.path(
        appendix_csv_dir,
        paste0(group_slug(grp), "_appendix_table.csv")
      )
    )
  }
)

# ------------------------------------------------------------
# SUMMARY OF APPENDIX TABLE OUTPUTS
# ------------------------------------------------------------

appendix_summary <- purrr::imap_dfr(
  report_table_variants,
  function(variants, grp) {
    df <- variants$unlisted_removed
    
    tibble(
      report_group = grp,
      grouping_rule = appendix_grouping_rules[[grp]],
      n_rows_source = if (is.null(df)) 0 else nrow(df),
      n_rows_appendix = if (is.null(df) || nrow(df) == 0) {
        0
      } else {
        nrow(prepare_appendix_df(df, grp))
      }
    )
  }
)

print(appendix_summary, n = Inf)

safe_write_csv(
  appendix_summary,
  file.path(qc_dir, "appendix_table_summary.csv")
)

# ------------------------------------------------------------
# PREVIEW
# ------------------------------------------------------------

print(names(appendix_tables))


names(raw_locality_qc$distinct_localities)