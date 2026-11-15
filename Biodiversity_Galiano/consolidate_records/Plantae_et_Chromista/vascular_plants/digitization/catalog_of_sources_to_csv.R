# Convert the GCA "Galiano Island Vascular Plant Data: Catalog of Sources" workbook to CSV
# Excel serial dates (> 10000) are converted to ISO dates; year-only values (e.g. 2023.0) are kept as years.
# Run from this script's directory.

library(readxl)

archive <- "../../../records/raw/Galiano_Island_Vascular_Plant_Data_compilation_2026-11-14"

catalog <- read_excel(file.path(archive, "Galiano Island Vascular Plant Data_ Catalog of Sources.xlsx"),
                      col_types = "text")

fix_date <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  out <- x
  serial <- !is.na(n) & n > 10000
  out[serial] <- format(as.Date(n[serial], origin = "1899-12-30"))
  yr <- !is.na(n) & n <= 10000
  out[yr] <- as.character(as.integer(n[yr]))
  out
}

catalog$date <- fix_date(catalog$date)
catalog$lastUpdated <- fix_date(catalog$lastUpdated)
catalog[] <- lapply(catalog, function(x) trimws(ifelse(is.na(x), "", x)))

write.csv(catalog, "../../../records/Galiano_Island_vascular_plant_data_catalog_of_sources.csv", row.names = FALSE)
