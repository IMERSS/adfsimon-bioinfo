# Merge curator decisions from keys/review_key_GCA_compilation_2026-11-14_reviewed.csv into keys/vascular_plant_taxon_key_2024.csv
#
# Rules (2026-11-14):
#   - a name with a Matched.Taxon is added as a synonym rule
#   - a name with no Matched.Taxon that the curator rejected (or a genus-level report, or a non-vascular legacy name)
#     is added with Critical.Note "DELETE: <reason>", so it is recorded as resolved and no longer offered for review
#   - a name with no Matched.Taxon and no decision yet ("verify", "potential new record", tentatively accepted pending a
#     summary entry) is NOT added, so it remains in keys/review_key.csv for review
# Run from this script's directory.

library(stringr)

reviewed <- read.csv("../keys/review_key_GCA_compilation_2026-11-14_reviewed.csv", colClasses = "character")
key <- read.csv("../keys/vascular_plant_taxon_key_2024.csv", colClasses = "character")
summary <- read.csv("../../../../review/Plantae_et_Chromista/vascular_plants/summaries/Tracheophyta_summary_2026-04-04.csv")

# Targets corrected to names present in the summary
reviewed$Matched.Taxon[reviewed$Taxon == "Fragaria virginiana var. glauca"] <- "Fragaria vesca ssp. bracteata"
luzula <- reviewed$Taxon == "Luzula fastigiata"
reviewed$Matched.Taxon[luzula] <- ""
reviewed$Critical.Note[luzula] <- paste(reviewed$Critical.Note[luzula], "(genus-level only)")

reviewed$Critical.Note <- str_squish(reviewed$Critical.Note)
delete <- reviewed$Matched.Taxon == "" &
  grepl("^(Rejected|Non-established|Cannot accept|misreport|genus-level|legacy transcription)", reviewed$Critical.Note, ignore.case = TRUE)
reviewed$Critical.Note[delete] <- paste0("DELETE: ", reviewed$Critical.Note[delete])

add <- reviewed[(reviewed$Matched.Taxon != "" | delete) & !(reviewed$Taxon %in% key$Taxon), c("Taxon", "Matched.Taxon", "Critical.Note")]
stopifnot(all(add$Matched.Taxon[add$Matched.Taxon != ""] %in% summary$Taxon))

key <- rbind(key, add)
key <- key[order(key$Taxon), ]
write.csv(key, "../keys/vascular_plant_taxon_key_2024.csv", row.names = FALSE)

cat("Added", nrow(add), "rules:", sum(add$Matched.Taxon != ""), "synonyms,", sum(add$Matched.Taxon == ""), "DELETE\n")
cat("Left for review:", paste(reviewed$Taxon[!(reviewed$Taxon %in% add$Taxon)], collapse = " | "), "\n")
