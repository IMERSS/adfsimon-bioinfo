# Script to compare iNaturalist observations against a historical baseline

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(stringr)
library(tidyr)

# Read CSV as character
read_char_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE
  )
}

# Normalize character values
norm_chr <- function(x) {
  str_squish(ifelse(is.na(x), "", as.character(x)))
}

# Extract synonym target from notes like "Syn. Name"
extract_synonym_target <- function(x) {
  out <- str_match(norm_chr(x), "^Syn\\.\\s*(.+)$")[, 2]
  norm_chr(out)
}

# Derive species from taxon string
derive_species_field <- function(x) {
  x <- norm_chr(x)
  ifelse(str_count(x, "\\S+") >= 2, word(x, 2), "")
}

# Derive subspecies from taxon string
derive_subspecies_field <- function(x) {
  x <- norm_chr(x)
  case_when(
    str_detect(x, "\\bsubsp\\.\\b") ~ word(x, 4),
    str_detect(x, "\\bssp\\.\\b") ~ word(x, 4),
    str_count(x, "\\S+") >= 3 & !str_detect(x, "\\b(var\\.|subsp\\.|ssp\\.|f\\.)\\b") ~ word(x, 3),
    TRUE ~ ""
  )
}

# Derive variety from taxon string
derive_variety_field <- function(x) {
  x <- norm_chr(x)
  case_when(
    str_detect(x, "\\bvar\\.\\b") ~ word(x, 4),
    TRUE ~ ""
  )
}

# Derive genus from taxon string
derive_genus_field <- function(x) {
  x <- norm_chr(x)
  ifelse(x == "", "", word(x, 1))
}

# Replace existing rows by key with new rows
upsert_rows <- function(existing_df, new_df, key_cols) {
  if (nrow(new_df) == 0) return(existing_df)
  if (nrow(existing_df) == 0) return(new_df)
  
  existing_trimmed <- anti_join(
    existing_df,
    new_df %>% select(all_of(key_cols)) %>% distinct(),
    by = key_cols
  )
  
  bind_rows(new_df, existing_trimmed)
}

# Check required columns
require_cols <- function(df, required, df_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      paste0(
        df_name, " is missing required column(s): ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# Empty rule log
empty_rule_log <- function() {
  data.frame(
    Taxon = character(),
    Curator.Decision = character(),
    Curator.Notes = character(),
    stringsAsFactors = FALSE
  )
}

# Empty change log
empty_change_log <- function() {
  data.frame(
    Taxon = character(),
    ID = character(),
    Curator.Decision = character(),
    Curator.Notes = character(),
    stringsAsFactors = FALSE
  )
}

# File paths
baseline_path <- "summaries/Galiano_Tracheophyta_review_summary_reviewed_2026-04-04.csv"
inat_path <- "../../../parse_records/outputs/iNat_obs_Tracheophyta.csv"
review_notes_path <- "outputs/Galiano_Tracheophyta_review_summary_review_notes.csv"
taxon_rule_log_path <- "outputs/Galiano_Tracheophyta_taxon_rule_log.csv"
change_log_path <- "outputs/Galiano_Tracheophyta_exclusion_change_log.csv"
harvested_rules_path <- "outputs/Galiano_Tracheophyta_manual_rules_harvested_this_cycle.csv"

# Print paths
cat("Working directory:", getwd(), "\n")
cat("Baseline path:", baseline_path, "\n")
cat("iNat path:", inat_path, "\n")
cat("Review notes path:", review_notes_path, "\n")
cat("Taxon rule log path:", taxon_rule_log_path, "\n")
cat("Change log path:", change_log_path, "\n")
cat("Harvested rules path:", harvested_rules_path, "\n\n")

# Check required files
if (!file.exists(baseline_path)) {
  stop("Baseline file not found: ", baseline_path, call. = FALSE)
}
if (!file.exists(inat_path)) {
  stop("iNaturalist observations file not found: ", inat_path, call. = FALSE)
}
if (!file.exists(review_notes_path)) {
  stop(
    paste0(
      "Review notes file not found: ", review_notes_path, "\n",
      "Aborting to avoid overwriting manual rule outputs with empty files."
    ),
    call. = FALSE
  )
}

# Read baseline
baseline <- read.csv(
  baseline_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

baseline <- baseline %>%
  filter(Phylum == "Tracheophyta")

summary.fields <- c(
  "Taxon","Taxon.Author","Subtaxon.Author","Common.Name","Kingdom","Phylum",
  "Subphylum","Superclass","Class","Subclass","Superorder","Order","Suborder",
  "Superfamily","Family","Subfamily","Tribe","Genus","Species","Hybrid",
  "Subspecies","Variety","Origin","Provincial.Status","National.Status","Reporting.Status",
  "Observation","Collected.Reported..y.m.d.","Collector.Source","Collection.List",
  "Accession.Number","GBIF.ID","First.Observed","Observer","iNaturalist.Link","Notes",
  "ID","Stats.Code"
)

if (ncol(baseline) != length(summary.fields)) {
  stop(
    paste0(
      "Baseline column count mismatch.\n",
      "Expected ", length(summary.fields), " columns, found ", ncol(baseline), "."
    ),
    call. = FALSE
  )
}

names(baseline) <- summary.fields

baseline <- baseline %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>%
  mutate(
    Taxon = norm_chr(Taxon),
    `Taxon.Author` = norm_chr(`Taxon.Author`),
    `Subtaxon.Author` = norm_chr(`Subtaxon.Author`),
    `Common.Name` = norm_chr(`Common.Name`),
    Genus = norm_chr(Genus),
    Family = norm_chr(Family),
    First.Observed = norm_chr(First.Observed),
    Observer = norm_chr(Observer),
    iNaturalist.Link = norm_chr(iNaturalist.Link),
    Notes = norm_chr(Notes),
    ID = as.character(ID)
  )

# Read iNaturalist observations
iNat.obs.summary <- read.csv(
  inat_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

require_cols(
  iNat.obs.summary,
  c("scientific_name", "observed_on", "user_name", "taxon_rank", "genus", "family", "taxon_id", "id"),
  "iNaturalist observation file"
)

iNat.obs.summary <- iNat.obs.summary %>%
  mutate(
    scientific_name = norm_chr(scientific_name),
    observed_on = norm_chr(observed_on),
    user_name = norm_chr(user_name),
    taxon_rank = tolower(norm_chr(taxon_rank)),
    genus = norm_chr(genus),
    family = norm_chr(family),
    taxon_id = as.character(taxon_id),
    id = as.character(id)
  )

# Keep earliest observation per raw taxon
iNat.obs.summary <- iNat.obs.summary %>%
  arrange(scientific_name, observed_on, id) %>%
  group_by(scientific_name) %>%
  slice(1) %>%
  ungroup()

# Add helper fields
iNat.obs.summary <- iNat.obs.summary %>%
  mutate(
    taxon_subspecies_name = word(scientific_name, 3),
    taxon_variety_name = word(scientific_name, 3),
    hybrid = ""
  )

# Match baseline-style fields
iNat.obs.summary <- iNat.obs.summary %>%
  mutate(
    Taxon = scientific_name,
    Subspecies = taxon_subspecies_name,
    Variety = taxon_variety_name,
    First.Observed = observed_on,
    Observer = user_name,
    iNaturalist.Link = paste0("iNat:", id),
    ID = taxon_id,
    Genus = genus,
    Family = family,
    Taxon.rank = taxon_rank
  )

# Build template
template.df <- as.data.frame(
  matrix(ncol = length(summary.fields), nrow = nrow(iNat.obs.summary))
)
names(template.df) <- summary.fields

common.cols <- intersect(names(iNat.obs.summary), names(template.df))
template.df[common.cols] <- iNat.obs.summary[common.cols]

iNat.obs.summary.std <- template.df %>%
  select(all_of(summary.fields)) %>%
  mutate(across(where(is.logical), as.character)) %>%
  mutate(across(where(is.character), ~ replace_na(., ""))) %>%
  mutate(
    Taxon = norm_chr(Taxon),
    `Taxon.Author` = norm_chr(`Taxon.Author`),
    `Subtaxon.Author` = norm_chr(`Subtaxon.Author`),
    `Common.Name` = norm_chr(`Common.Name`),
    Genus = norm_chr(Genus),
    Family = norm_chr(Family),
    First.Observed = norm_chr(First.Observed),
    Observer = norm_chr(Observer),
    iNaturalist.Link = norm_chr(iNaturalist.Link),
    Notes = norm_chr(Notes),
    ID = as.character(ID)
  ) %>%
  mutate(
    Taxon.rank = iNat.obs.summary$Taxon.rank
  )

# Read taxon rule log
if (file.exists(taxon_rule_log_path)) {
  taxon_rule_log <- read_char_csv(taxon_rule_log_path)
} else {
  taxon_rule_log <- empty_rule_log()
}

require_cols(
  taxon_rule_log,
  c("Taxon", "Curator.Decision", "Curator.Notes"),
  "taxon rule log"
)

taxon_rule_log <- taxon_rule_log %>%
  mutate(
    Taxon = norm_chr(Taxon),
    Curator.Decision = toupper(norm_chr(Curator.Decision)),
    Curator.Notes = norm_chr(Curator.Notes)
  ) %>%
  filter(
    Taxon != "",
    Curator.Decision %in% c("DELETE", "SYNONYM")
  ) %>%
  distinct(Taxon, .keep_all = TRUE)

# Read review notes
review_notes <- read_char_csv(review_notes_path)

require_cols(
  review_notes,
  c("Taxon", "Curator.Decision", "Curator.Notes"),
  "review notes file"
)

review_notes <- review_notes %>%
  mutate(
    Taxon = norm_chr(Taxon),
    Curator.Decision = toupper(norm_chr(Curator.Decision)),
    Curator.Notes = norm_chr(Curator.Notes)
  )

valid_delete_notes <- c(
  "non-established",
  "unresolved infrataxon"
)

# Harvest valid persistent rules
new_manual_rules <- review_notes %>%
  mutate(
    synonym_target = ifelse(
      Curator.Decision == "SYNONYM",
      extract_synonym_target(Curator.Notes),
      ""
    ),
    valid_persistent_rule = case_when(
      Taxon == "" ~ FALSE,
      Curator.Decision == "DELETE" & Curator.Notes %in% valid_delete_notes ~ TRUE,
      Curator.Decision == "SYNONYM" & synonym_target != "" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(valid_persistent_rule) %>%
  transmute(
    Taxon = Taxon,
    Curator.Decision = Curator.Decision,
    Curator.Notes = Curator.Notes
  ) %>%
  distinct(Taxon, .keep_all = TRUE)

# Save harvested rules
write.csv(
  new_manual_rules,
  harvested_rules_path,
  row.names = FALSE
)

# Update cumulative rule log
taxon_rule_log <- upsert_rows(
  existing_df = taxon_rule_log,
  new_df = new_manual_rules,
  key_cols = c("Taxon")
)

# Keep only valid rule rows
taxon_rule_log <- taxon_rule_log %>%
  mutate(
    synonym_target = ifelse(
      Curator.Decision == "SYNONYM",
      extract_synonym_target(Curator.Notes),
      ""
    )
  ) %>%
  filter(
    Taxon != "",
    (
      Curator.Decision == "DELETE" & Curator.Notes %in% valid_delete_notes
    ) |
      (
        Curator.Decision == "SYNONYM" & synonym_target != ""
      )
  ) %>%
  select(-synonym_target) %>%
  distinct(Taxon, .keep_all = TRUE)

write.csv(
  taxon_rule_log,
  taxon_rule_log_path,
  row.names = FALSE
)

# Build manual rule lookup
manual_rule_lookup <- taxon_rule_log %>%
  transmute(
    rule_taxon = Taxon,
    manual_rule = Curator.Decision,
    manual_note = Curator.Notes
  )

# Apply manual rules
iNat.obs.summary.std <- iNat.obs.summary.std %>%
  mutate(
    Original.Taxon = Taxon
  ) %>%
  left_join(manual_rule_lookup, by = c("Taxon" = "rule_taxon")) %>%
  mutate(
    manual_rule = norm_chr(manual_rule),
    manual_note = norm_chr(manual_note),
    synonym_target = case_when(
      manual_rule == "SYNONYM" ~ extract_synonym_target(manual_note),
      TRUE ~ ""
    ),
    synonym_parse_failed = case_when(
      manual_rule == "SYNONYM" & synonym_target == "" ~ TRUE,
      TRUE ~ FALSE
    ),
    Taxon = case_when(
      manual_rule == "SYNONYM" & synonym_target != "" ~ synonym_target,
      TRUE ~ Taxon
    ),
    Genus = derive_genus_field(Taxon),
    Species = derive_species_field(Taxon),
    Subspecies = derive_subspecies_field(Taxon),
    Variety = derive_variety_field(Taxon)
  )

manual_delete_count <- sum(iNat.obs.summary.std$manual_rule == "DELETE", na.rm = TRUE)
manual_synonym_count <- sum(iNat.obs.summary.std$manual_rule == "SYNONYM", na.rm = TRUE)
manual_synonym_parse_fail_count <- sum(iNat.obs.summary.std$synonym_parse_failed, na.rm = TRUE)

# Remove manual DELETE taxa
iNat.obs.summary.std <- iNat.obs.summary.std %>%
  filter(manual_rule != "DELETE")

# Collapse after synonym replacement
iNat.obs.summary.std <- iNat.obs.summary.std %>%
  arrange(Taxon, First.Observed, iNaturalist.Link) %>%
  group_by(Taxon) %>%
  slice(1) %>%
  ungroup()

# Compare synonym-resolved taxa against baseline
baseline_compare <- baseline %>%
  transmute(
    Taxon = norm_chr(Taxon),
    baseline_First_Observed = norm_chr(First.Observed),
    baseline_Observer = norm_chr(Observer),
    baseline_iNaturalist_Link = norm_chr(iNaturalist.Link)
  ) %>%
  distinct(Taxon, .keep_all = TRUE)

iNat.obs.summary.std <- iNat.obs.summary.std %>%
  left_join(baseline_compare, by = "Taxon") %>%
  mutate(
    taxon_in_baseline = !is.na(baseline_First_Observed) & baseline_First_Observed != "",
    earlier_than_baseline_for_taxon = case_when(
      !taxon_in_baseline ~ FALSE,
      First.Observed == "" ~ FALSE,
      baseline_First_Observed == "" ~ FALSE,
      First.Observed < baseline_First_Observed ~ TRUE,
      TRUE ~ FALSE
    ),
    metadata_differs_from_baseline = case_when(
      !taxon_in_baseline ~ FALSE,
      norm_chr(First.Observed) != baseline_First_Observed ~ TRUE,
      norm_chr(Observer) != baseline_Observer ~ TRUE,
      norm_chr(iNaturalist.Link) != baseline_iNaturalist_Link ~ TRUE,
      TRUE ~ FALSE
    ),
    keep_after_baseline_taxon_check = case_when(
      !taxon_in_baseline ~ TRUE,
      earlier_than_baseline_for_taxon & metadata_differs_from_baseline ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(keep_after_baseline_taxon_check) %>%
  select(
    -baseline_First_Observed,
    -baseline_Observer,
    -baseline_iNaturalist_Link,
    -taxon_in_baseline,
    -earlier_than_baseline_for_taxon,
    -metadata_differs_from_baseline,
    -keep_after_baseline_taxon_check
  )

# Match against baseline by ID
summary.matched <- inner_join(
  baseline,
  iNat.obs.summary.std %>% select(ID) %>% distinct(),
  by = "ID"
) %>%
  distinct(ID, .keep_all = TRUE)

unmatched.iNat.obs.summary <- anti_join(
  iNat.obs.summary.std,
  summary.matched %>% select(ID),
  by = "ID"
)

unmatched.iNat.obs.summary$Stats.Code <- "VAS"

# Baseline lookup tables
baseline_taxon_lookup <- baseline %>%
  select(Taxon, First.Observed) %>%
  distinct() %>%
  mutate(
    Taxon = norm_chr(Taxon),
    baseline_First_Observed = First.Observed
  ) %>%
  select(Taxon, baseline_First_Observed)

baseline_genus_lookup <- baseline %>%
  filter(Genus != "") %>%
  distinct(Genus) %>%
  mutate(genus_in_baseline = TRUE)

baseline_family_lookup <- baseline %>%
  filter(Family != "") %>%
  distinct(Family) %>%
  mutate(family_in_baseline = TRUE)

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  left_join(baseline_taxon_lookup, by = "Taxon") %>%
  left_join(baseline_genus_lookup, by = "Genus") %>%
  left_join(baseline_family_lookup, by = "Family") %>%
  mutate(
    genus_in_baseline = ifelse(is.na(genus_in_baseline), FALSE, genus_in_baseline),
    family_in_baseline = ifelse(is.na(family_in_baseline), FALSE, family_in_baseline)
  )

# Resolution categories
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    resolution_level = case_when(
      Taxon == "" | is.na(Taxon) ~ "unresolved",
      Taxon.rank %in% c("species", "subspecies", "variety", "form", "hybrid") ~ "species_or_lower",
      Taxon.rank == "genus" ~ "genus_only",
      TRUE ~ "higher_taxon"
    )
  )

# Novelty categories
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    novelty_flag = case_when(
      resolution_level == "species_or_lower" & !genus_in_baseline ~ "novel_genus",
      resolution_level == "species_or_lower" & genus_in_baseline ~ "novel_species_in_known_genus",
      resolution_level == "genus_only" & !genus_in_baseline ~ "novel_genus",
      resolution_level == "genus_only" & genus_in_baseline ~ "known_genus_only",
      resolution_level == "higher_taxon" & !family_in_baseline ~ "novel_higher_taxon",
      resolution_level == "higher_taxon" & family_in_baseline ~ "known_higher_taxon_only",
      TRUE ~ ""
    )
  )

# Date categories
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    date_flag = case_when(
      is.na(baseline_First_Observed) | baseline_First_Observed == "" ~ "new_taxon",
      First.Observed < baseline_First_Observed ~ "earlier_than_baseline",
      First.Observed == baseline_First_Observed ~ "same_date_as_baseline",
      First.Observed > baseline_First_Observed ~ "later_than_baseline",
      TRUE ~ ""
    )
  )

# Review grouping
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    review_group = case_when(
      resolution_level == "species_or_lower" ~ "deep_review",
      date_flag %in% c("earlier_than_baseline", "later_than_baseline", "same_date_as_baseline") ~ "date_precedence_check",
      resolution_level == "genus_only" & novelty_flag == "novel_genus" ~ "review_genus_novelty",
      resolution_level == "genus_only" ~ "batch_genus_only",
      TRUE ~ "low_priority"
    )
  )

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    Curator.Decision = "",
    Curator.Notes = ""
  )

# Auto triage
unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    auto_exclusion_note = case_when(
      review_group == "batch_genus_only" ~ "genus",
      review_group == "low_priority" & resolution_level == "higher_taxon" ~ "higher",
      review_group == "low_priority" & resolution_level == "unresolved" ~ "unresolved",
      review_group == "low_priority" ~ "other",
      TRUE ~ ""
    )
  )

unmatched.iNat.obs.summary <- unmatched.iNat.obs.summary %>%
  mutate(
    Curator.Decision = case_when(
      review_group %in% c("batch_genus_only", "low_priority") ~ "DELETE",
      TRUE ~ Curator.Decision
    ),
    Curator.Notes = case_when(
      Curator.Decision == "DELETE" & auto_exclusion_note != "" ~ auto_exclusion_note,
      TRUE ~ Curator.Notes
    )
  )

# Read change log
if (file.exists(change_log_path)) {
  change_log <- read_char_csv(change_log_path)
} else {
  change_log <- empty_change_log()
}

require_cols(
  change_log,
  c("Taxon", "ID", "Curator.Decision", "Curator.Notes"),
  "exclusion change log"
)

change_log <- change_log %>%
  mutate(
    Taxon = norm_chr(Taxon),
    ID = norm_chr(ID),
    Curator.Decision = toupper(norm_chr(Curator.Decision)),
    Curator.Notes = norm_chr(Curator.Notes)
  )

# Current auto-exclusions
current_auto_exclusions <- unmatched.iNat.obs.summary %>%
  filter(Curator.Decision == "DELETE") %>%
  transmute(
    Taxon = Taxon,
    ID = norm_chr(ID),
    Curator.Decision = "DELETE",
    Curator.Notes = Curator.Notes
  ) %>%
  distinct(Taxon, ID, .keep_all = TRUE)

# Update change log
change_log <- upsert_rows(
  existing_df = change_log,
  new_df = current_auto_exclusions,
  key_cols = c("Taxon", "ID")
)

write.csv(
  change_log,
  change_log_path,
  row.names = FALSE
)

# Keep only manual review targets
unmatched.review.targets <- unmatched.iNat.obs.summary %>%
  filter(Curator.Decision != "DELETE")

# Add extra review columns to baseline
baseline.review <- baseline %>%
  mutate(
    Taxon.rank = "",
    Original.Taxon = "",
    manual_rule = "",
    manual_note = "",
    synonym_target = "",
    synonym_parse_failed = "",
    baseline_First_Observed = "",
    genus_in_baseline = "",
    family_in_baseline = "",
    resolution_level = "",
    novelty_flag = "",
    date_flag = "",
    review_group = "",
    Curator.Decision = "",
    Curator.Notes = "",
    auto_exclusion_note = ""
  )

review.cols <- union(names(baseline.review), names(unmatched.review.targets))

for (col in setdiff(review.cols, names(baseline.review))) {
  baseline.review[[col]] <- ""
}

for (col in setdiff(review.cols, names(unmatched.review.targets))) {
  unmatched.review.targets[[col]] <- ""
}

baseline.review <- baseline.review[, review.cols]
unmatched.review.targets <- unmatched.review.targets[, review.cols]

review.summary <- rbind(baseline.review, unmatched.review.targets)
review.summary[is.na(review.summary)] <- ""

# Print diagnostics
cat("Review notes path exists:", file.exists(review_notes_path), "\n")
cat("Rows in review notes:", nrow(review_notes), "\n")
cat("Rows harvested from review notes:", nrow(new_manual_rules), "\n")
cat("Rows in taxon rule log:", nrow(taxon_rule_log), "\n")
cat("Manual DELETE rules applied:", manual_delete_count, "\n")
cat("Manual SYNONYM rules applied:", manual_synonym_count, "\n")
cat("Manual SYNONYM parse failures:", manual_synonym_parse_fail_count, "\n")
cat("Rows in iNat summary after manual rules and baseline-taxon screening:", nrow(iNat.obs.summary.std), "\n")
cat("Rows matched by ID:", nrow(summary.matched), "\n")
cat("Rows unmatched before auto DELETE filter:", nrow(unmatched.iNat.obs.summary), "\n")
cat("Rows auto-excluded and logged:", nrow(current_auto_exclusions), "\n")
cat("Rows retained for manual review:", nrow(unmatched.review.targets), "\n")
cat("Rows in combined review summary:", nrow(review.summary), "\n")

cat("\nReview note decision counts:\n")
print(table(review_notes$Curator.Decision, useNA = "ifany"))

cat("\nHarvested manual rules this cycle:\n")
print(new_manual_rules)

cat("\nTaxon rule log decisions:\n")
print(table(taxon_rule_log$Curator.Decision, useNA = "ifany"))

cat("\nAuto-exclusion note counts:\n")
print(table(current_auto_exclusions$Curator.Notes, useNA = "ifany"))

# Write output files
write.csv(
  review.summary,
  "outputs/Galiano_Tracheophyta_review_summary.csv",
  row.names = FALSE
)