# Clean the GCA vascular plant digitization (archived 2026-11-14) into upstream sources for the
# BioGaliano consolidation workflow.
#
# Input:  ../../../records/raw/Galiano_Island_Vascular_Plant_Data_compilation_2026-11-14/  (verbatim archive; never modified)
# Output: ../../../records/digitized/<source>.csv               cleaned transcription (original columns, names corrected,
#                                                               Excel serial dates converted, verbatimScientificName kept)
#         ../../../records/digitized/DarwinCore/<source>_DwC.csv  DwC-formatted copy read by Tracheophyta_consolidate_occurrence_records_DwC.R
#         ./corrections_log_2026-11-14.csv                       every verbatim -> corrected name change
#
# Cleaning policy: fix orthography, capitalization, stray punctuation and qualifiers only. The source author's
# taxonomic concept is kept (e.g. 'Montia perfoliata' is not changed to 'Claytonia perfoliata'); reconciliation to the
# curated summary is left to the taxon key / review key in the consolidation workflow.
# Non-vascular taxa (bryophytes, lichens, fungi, slime moulds, algae) are retained, tagged by kingdom/phylum,
# so the vascular workflow can filter on phylum == 'Tracheophyta' and other workflows can reuse them.
#
# Run from this script's directory.

suppressMessages({
  library(readxl); library(dplyr); library(stringr); library(sf)
})

archive <- "../../../records/raw/Galiano_Island_Vascular_Plant_Data_compilation_2026-11-14"
dig.dir <- "../../../records/digitized"
dwc.dir <- "../../../records/digitized/DarwinCore"
dc <- file.path(archive, "Digitizing_compiling data")

read_sheet <- function(path, sheet = 1) {
  x <- read_excel(file.path(dc, path), sheet = sheet, col_types = "text", .name_repair = "minimal")
  names(x)[names(x) == "" | is.na(names(x))] <- paste0("unnamed_", which(names(x) == "" | is.na(names(x))))
  x[] <- lapply(x, function(v) str_squish(ifelse(is.na(v), "", v)))
  x[x[[names(x)[1]]] != "" | x$scientificName != "", ]
}

excel_date <- function(x) format(as.Date(as.numeric(x), origin = "1899-12-30"))

# ---- Name cleaning ---------------------------------------------------------

# Specific corrections, applied after generic normalization. Value: "name|identificationQualifier|remark"
fixes <- c(
  "Carex rostrate" = "Carex rostrata",
  "Daucus pusillu" = "Daucus pusillus",
  "Fritilaria lanceolata" = "Fritillaria lanceolata",
  "Frittilaria affinis" = "Fritillaria affinis",
  "Goldenback fern" = "Pentagramma triangularis||vernacular name 'goldenback fern' given in scientificName field",
  "Hylocomium splenden" = "Hylocomium splendens",
  "Hypochaeris radica" = "Hypochaeris radicata",
  "Menzies’ tree moss" = "Leucolepis acanthoneuron||vernacular name 'Menzies' tree moss' given in scientificName field",
  "Moehringia macrophyla" = "Moehringia macrophylla",
  "Moehringia macrophyllum" = "Moehringia macrophylla",
  "Rhyzomnium glabrescens" = "Rhizomnium glabrescens",
  "Rubus parviflora" = "Rubus parviflorus",
  "Rubus parviflorum" = "Rubus parviflorus",
  "Sanicula crassicaulus" = "Sanicula crassicaulis",
  "Sanicula crasssicaulis" = "Sanicula crassicaulis",
  "Unknown grass" = "Poaceae||unidentified grass",
  "Acmispon parviflora" = "Acmispon parviflorus",
  "Anthoxanthum odorata" = "Anthoxanthum odoratum",
  "Idahoa scapigera idaho" = "Idahoa scapigera",
  "Lepidum virginicum" = "Lepidium virginicum",
  "Torilis japonica upright" = "Torilis japonica",
  "Clarkia amoena vars." = "Clarkia amoena||reported as 'Clarkia amoena vars.'",
  "Triteleia hyacinthine" = "Triteleia hyacinthina",
  "Bromus carrinatus" = "Bromus carinatus",
  "Centaurium erythraea aka. c. umbellatum" = "Centaurium erythraea||reported as 'Centaurium erythraea aka. C. umbellatum'",
  "Circium arvense" = "Cirsium arvense",
  "Corallorhiza maculata sp. mertensiana" = "Corallorhiza maculata ssp. mertensiana",
  "Corallorhiza maculata spp. mertensiana" = "Corallorhiza maculata ssp. mertensiana",
  "Cytusus scoparius" = "Cytisus scoparius",
  "Lysichitum americanum" = "Lysichiton americanum",
  "Polupodium glycyrrhiza" = "Polypodium glycyrrhiza",
  "Ribes sanguinteum" = "Ribes sanguineum",
  "Ribes sanguinuum" = "Ribes sanguineum",
  "Rosa cf pisocarpa" = "Rosa pisocarpa|cf.",
  "Rumex ? occedentalis" = "Rumex occidentalis|?",
  "Senecio jacobei" = "Senecio jacobaea",
  "Tuhja plicata" = "Thuja plicata",
  "Vicia giganteo" = "Vicia gigantea",
  "Narcissus pseudo-narcissus" = "Narcissus pseudonarcissus",
  "Pteridium aqulinum" = "Pteridium aquilinum",
  "Symphytum x uplandicum" = "Symphytum × uplandicum",
  "Bellis perrenis" = "Bellis perennis",
  "Linnea borealis" = "Linnaea borealis",
  "Verbascus thapsus" = "Verbascum thapsus",
  "Stachys colleyae" = "Stachys cooleyae",
  "Boshniakia hookeri" = "Boschniakia hookeri",
  "Icmadophila ericetorus" = "Icmadophila ericetorum",
  "Leucolepsis acanthoneuron" = "Leucolepis acanthoneuron",
  "Lobaria pulmoneria" = "Lobaria pulmonaria",
  "Peltigera membrenacea" = "Peltigera membranacea",
  "Luzula subcessilis" = "Luzula subsessilis",
  "Guepiniopsis alpinus" = "Guepiniopsis alpina",
  "Gamochaeta purpurea? g. ustulata?" = "Gamochaeta sp.||reported as 'Gamochaeta purpurea? G. ustulata?'",
  "Taraxacum spp. (section ruderalia)" = "Taraxacum officinale agg.||reported as 'Taraxacum spp. (section Ruderalia)'",
  "Bromus hordaceus" = "Bromus hordeaceus",
  "Trifolium wildenovii" = "Trifolium willdenovii",
  "Anisocarpos madioides" = "Anisocarpus madioides",
  "Arbutus menziesii - seedlings" = "Arbutus menziesii||seedlings",
  "Quercus garryana (large)" = "Quercus garryana||large trees",
  "Holodiscus discolour" = "Holodiscus discolor",
  "Ranunculus acer" = "Ranunculus acris",
  "Eurhynchium oregana" = "Eurhynchium oreganum",
  "Peltigera (gray)" = "Peltigera sp.||grey Peltigera",
  "Juncus tenuis s.l." = "Juncus tenuis|s.l.",
  "Luzula multiflora s.l." = "Luzula multiflora|s.l.",
  "Populus trichocarpa x deltoides" = "Populus trichocarpa × deltoides",
  # Crystal Mountain 2010, Collins 2009, Retreat Island 2011 transcriptions
  "Adenocaulon bicolour" = "Adenocaulon bicolor",
  "Arbutus menzieseii" = "Arbutus menziesii", "Arubutus menziessii" = "Arbutus menziesii", "Arubutus menziesii" = "Arbutus menziesii",
  "Atherium felix-femina" = "Athyrium filix-femina",
  "Carex opnupta" = "Carex obnupta",
  "Circium sp." = "Cirsium sp.",
  "Cornus nuttalli" = "Cornus nuttallii",
  "Equisitum arvense" = "Equisetum arvense",
  "Gallium aparine" = "Galium aparine",
  "Gaultheria shalon" = "Gaultheria shallon",
  "Grass sp." = "Poaceae||reported as 'grass spp.'",
  "Hylocomnium splendens" = "Hylocomium splendens",
  "Hypocaeris radicata" = "Hypochaeris radicata",
  "Juncos effusus" = "Juncus effusus",
  "Kinbergia oregana" = "Kindbergia oregana", "Kindberia oregana" = "Kindbergia oregana", "Kindberia oregona" = "Kindbergia oregana",
  "Kinderbergia oregana" = "Kindbergia oregana", "Kindvergia oregano" = "Kindbergia oregana",
  "Kinbergia praelonga" = "Kindbergia praelonga", "Kindberia praelonga" = "Kindbergia praelonga",
  "Lonicera cilosia" = "Lonicera ciliosa",
  "Lonicera hispadula" = "Lonicera hispidula",
  "Menthe arvensis" = "Mentha arvensis",
  "Paxistima mirsinites" = "Paxistima myrsinites", "Pachistima myrsinites" = "Paxistima myrsinites",
  "Pseudotsuga menzieseii" = "Pseudotsuga menziesii", "Psuedotsuga menziesii" = "Pseudotsuga menziesii",
  "Racomitrium" = "Racomitrium sp.",
  "Rhytideadelphus lorius" = "Rhytidiadelphus loreus",
  "Rhytideadelphus triquetrus" = "Rhytidiadelphus triquetrus", "Rhytididiadelphus triquetrus" = "Rhytidiadelphus triquetrus",
  "Rubus parvaflorum" = "Rubus parviflorus", "Rubus parvaflorus" = "Rubus parviflorus",
  "Rubus spectabalis" = "Rubus spectabilis",
  "Salix lucida spp laciandra" = "Salix lucida ssp. lasiandra",
  "Senecio jacobeae" = "Senecio jacobaea",
  "Symphoricarpus albus" = "Symphoricarpos albus", "Symphorocapus alba" = "Symphoricarpos albus",
  "Symphorocarpus alba" = "Symphoricarpos albus", "Symphorocarpus albus" = "Symphoricarpos albus",
  "Timmia austiaca" = "Timmia austriaca",
  "Vaccinium parvaflorum" = "Vaccinium parvifolium", "Vaccinium parviflorum" = "Vaccinium parvifolium",
  "Vaccinnium ovatum" = "Vaccinium ovatum",
  "Taraxicum officinale" = "Taraxacum officinale",
  "Populus balsamifera trichocarpa" = "Populus balsamifera ssp. trichocarpa",
  "Grindelia" = "Grindelia sp."
)

normalize_name <- function(x) {
  x <- str_squish(x)
  x <- ifelse(grepl("\\(", x), x, str_remove(x, "\\)$"))
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  x <- str_replace(x, "\\bsubsp\\.", "ssp.")
  x <- str_replace(x, " spp?\\.?$", " sp.")
  x
}

corrections <- data.frame()

clean_names <- function(verbatim, source) {
  n <- normalize_name(verbatim)
  f <- unname(fixes[n])
  parts <- str_split_fixed(ifelse(is.na(f), n, f), "\\|", 3)
  out <- data.frame(verbatimScientificName = verbatim, scientificName = parts[, 1],
                    identificationQualifier = parts[, 2], identificationRemarks = parts[, 3],
                    stringsAsFactors = FALSE)
  changed <- unique(out[out$verbatimScientificName != out$scientificName |
                        out$identificationQualifier != "" | out$identificationRemarks != "", ])
  if (nrow(changed) > 0) corrections <<- rbind(corrections, cbind(source = source, changed))
  out
}

# ---- Coarse classification so the vascular workflow can filter non-vascular taxa ----

nonvascular <- list(
  "Plantae|Bryophyta" = c("Alsia","Climacium","Neckera","Plagiothecium","Pseudotaxiphyllum","Timmia","Antitrichia","Atrichum","Brachythecium","Bryum","Buckiella","Ceratodon","Claopodium",
    "Dendroalsia","Dicranoweisia","Dicranum","Didymodon","Dryptodon","Eurhynchium","Fissidens","Grimmia","Hedwigia",
    "Homalothecium","Hylocomium","Hypnum","Isothecium","Kindbergia","Leucolepis","Metaneckera","Nogopterium","Orthotrichum",
    "Plagiomnium","Polytrichum","Pterogonium","Racomitrium","Rhizomnium","Rhytidiadelphus","Schistidium","Scleropodium",
    "Syntrichia","Timmiella","Tortella","Weissia"),
  "Plantae|Marchantiophyta" = c("Calypogeia","Cephaloziella","Lepidozia","Porella","Riccia","Scapania"),
  "Fungi|Ascomycota" = c("Caloplaca","Chrysothrix","Cladina","Cladonia","Evernia","Geoglossum","Hypogymnia","Icmadophila",
    "Lecanora","Lobaria","Melanelixia","Mollisia","Parmelia","Peltigera","Pertusaria","Physcia","Platismatia","Rhytisma",
    "Tuckermannopsis","Usnea","Verpa","Xanthoria"),
  "Fungi|Basidiomycota" = c("Agrocybe","Amanita","Antrodia","Cantharellus","Conocybe","Dacrymyces","Fomitopsis","Galerina",
    "Guepiniopsis","Hebeloma","Hygrocybe","Hypholoma","Inocybe","Laccaria","Lichenomphalia","Lycoperdon","Mycena","Nidula",
    "Nolanea","Phaeolus","Phellinus","Pholiota","Pleurotus","Pluteus","Polyporus","Stereum","Trametes","Tremella","Tubaria"),
  "Protozoa|Mycetozoa" = c("Brefeldia","Leocarpus"),
  "Chromista|Ochrophyta" = c("Fucus"),
  "Plantae|Chlorophyta" = c("Ulva"),
  "Plantae|Rhodophyta" = c("Porphyra")
)
genus_group <- unlist(lapply(names(nonvascular), function(g) setNames(rep(g, length(nonvascular[[g]])), nonvascular[[g]])))

classify <- function(name) {
  g <- unname(genus_group[word(name, 1)])
  g[is.na(g)] <- "Plantae|Tracheophyta"
  str_split_fixed(g, "\\|", 2)
}

# ---- DwC assembly ----------------------------------------------------------

dwc.cols <- c("scientificName","verbatimScientificName","identificationQualifier","identificationRemarks","vernacularName",
  "kingdom","phylum","datasetName","recordedBy","eventDate","verbatimEventDate","eventRemarks","fieldNumber","locality",
  "locationRemarks","habitat","decimalLatitude","decimalLongitude","geodeticDatum","coordinateUncertaintyInMeters",
  "georeferencedBy","georeferencedDate","georeferenceProtocol","georeferenceSources","georeferenceVerificationStatus","georeferenceRemarks","basisOfRecord","samplingProtocol","occurrenceRemarks","bibliographicCitation")

# Point-radius georeferences for named sites (see build_site_gazetteer_2026-11-14.R); applied only to records without
# coordinates of their own
gazetteer <- read.csv(file.path(dig.dir, "Galiano_vascular_plant_site_gazetteer_2026-11-14.csv"), colClasses = "character")
source.sites <- c(
  "Searle_2001_Trincomali_Nature_Sanctuary_plant_list" = "Trincomali Nature Sanctuary",
  "Huggins_Thompson_2023_Quadra_Hill_baseline_survey" = "Quadra Hill",
  "Turner_Erickson_Canfield_1998_Pebble_Beach_vegetation_inventory" = "Pebble Beach",
  "Huggins_2019_DL64_Cable_Bay_preliminary_survey" = "Cable Bay (DL64)",
  "DL57_Millard_Learning_Centre_flora_inventory_2012-2015" = "Millard Learning Centre (DL57)",
  "Erickson_2016_DL21_DL22_Mount_Sutil_baseline_plot_vegetation" = "Mount Sutil (DL21, DL22, REM Lot 23)",
  "Fairbarns_2016_Rock_Bluff_System_vascular_plant_survey" = "Mount Sutil (DL21, DL22, REM Lot 23)",
  "Scholz_Janszen_2004_Mount_Sutil_REM_Lot_23_baseline" = "Mount Sutil (DL21, DL22, REM Lot 23)",
  "Erickson_2022_DL21_DL22_BC_CDC_rare_element_occurrences" = "Mount Sutil (DL21, DL22, REM Lot 23)",
  "Taylor_2012_Galiano_Island_vascular_plant_record" = "Bluffs Park; Matthews Point; Bellhouse Park",
  "Crystal_Mountain_Species" = "Crystal Mountain (DL89, DL90)"
)

georeference_site <- function(name, dwc) {
  if (!(name %in% names(source.sites))) return(dwc)
  g <- gazetteer[gazetteer$site == source.sites[[name]], ]
  for (col in c("decimalLatitude","decimalLongitude","georeferenceRemarks")) if (is.null(dwc[[col]])) dwc[[col]] <- ""
  todo <- is.na(dwc$decimalLatitude) | dwc$decimalLatitude == ""
  dwc$decimalLatitude[todo] <- g$decimalLatitude
  dwc$decimalLongitude[todo] <- g$decimalLongitude
  dwc$geodeticDatum[todo] <- g$geodeticDatum
  dwc$coordinateUncertaintyInMeters[todo] <- g$coordinateUncertaintyInMeters
  dwc$georeferencedBy[todo] <- "automated point-radius georeference prepared for Andrew Simon"
  dwc$georeferencedDate[todo] <- "2026-11-14"
  dwc$georeferenceProtocol[todo] <- "point-radius method (Chapman & Wieczorek 2020, Georeferencing Best Practices); point on surface of site polygon(s), radius to furthest extent"
  dwc$georeferenceSources[todo] <- g$georeferenceSources
  dwc$georeferenceVerificationStatus[todo] <- "requires verification"
  dwc$georeferenceRemarks[todo] <- str_squish(paste0("site-level georeference for '", g$site, "'; record not located more precisely by the source. ",
                                                     dwc$georeferenceRemarks[todo]))
  dwc
}

write_source <- function(name, digitized, dwc) {
  for (col in setdiff(dwc.cols, names(dwc))) dwc[[col]] <- ""
  dwc <- georeference_site(name, dwc)
  cls <- classify(dwc$scientificName)
  dwc$kingdom <- cls[, 1]; dwc$phylum <- cls[, 2]
  dwc$country <- NULL
  for (col in setdiff(dwc.cols, names(dwc))) dwc[[col]] <- ""
  dwc <- dwc[, dwc.cols]
  dwc[is.na(dwc)] <- ""
  digitized[is.na(digitized)] <- ""
  write.csv(digitized, file.path(dig.dir, paste0(name, ".csv")), row.names = FALSE)
  write.csv(dwc, file.path(dwc.dir, paste0(name, "_DwC.csv")), row.names = FALSE)
  cat(sprintf("%-75s %4d records (%d vascular)\n", name, nrow(dwc), sum(dwc$phylum == "Tracheophyta")))
}

with_names <- function(x, source) {
  cl <- clean_names(x$scientificName, source)
  x$verbatimScientificName <- x$scientificName
  x$scientificName <- cl$scientificName
  list(x = x, cl = cl)
}

# ---- 1. Erickson 2016: Baseline Ecological Documentation of DL21 and DL22 (Mount Sutil) plots ----

src <- "Erickson_2016_DL21_DL22_Mount_Sutil_baseline_plot_vegetation"
x <- read_sheet("DL22 Mt. Sutil/2016 baseline report DL21 &DL22/DL21_DL22 baseline report species list 2016.xlsx", "Digitized data")
names(x)[names(x) == "occouranceRemarks"] <- "occurrenceRemarks"
# Plot IDs such as '4-1' were auto-converted by Excel to dates (serials 45748 etc. = 2025-04-01); restore 'month-day'
serial <- grepl("^[0-9]{5}(\\.0)?$", x$plot)
d <- as.Date(as.numeric(x$plot[serial]), origin = "1899-12-30")
x$plot[serial] <- paste0(as.integer(format(d, "%m")), "-", as.integer(format(d, "%d")))
x$eventDate <- ifelse(x$eventDate == "04.28/2016", "2016-04-28", excel_date(x$eventDate))
x$recordedBy <- "Keith Erickson"   # 'Erikson' in transcription; report author is Keith Erickson
r <- with_names(x, src); x <- r$x; cl <- r$cl
pts <- x$easting != ""
ll <- st_coordinates(st_transform(st_as_sf(data.frame(e = as.numeric(x$easting[pts]), n = as.numeric(x$northing[pts])),
                     coords = c("e", "n"), crs = 26910), 4326))
x$decimalLatitude <- ""; x$decimalLongitude <- ""
x$decimalLatitude[pts] <- round(ll[, 2], 6); x$decimalLongitude[pts] <- round(ll[, 1], 6)

dwc <- data.frame(cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, fieldNumber = paste0("plot ", x$plot), locality = "Galiano Island; Mount Sutil; DL21 & DL22",
  habitat = ifelse(x$siteSeries == "", "", paste0("site series: ", x$siteSeries, ifelse(x$aspect == "", "", paste0("; aspect: ", x$aspect)))),
  decimalLatitude = x$decimalLatitude, decimalLongitude = x$decimalLongitude,
  geodeticDatum = ifelse(pts, "WGS84", ""),
  georeferenceProtocol = ifelse(pts, "plot GPS position (Trimble GeoXH 6000, NAD83 UTM 10N) converted to WGS84", ""),
  georeferenceRemarks = ifelse(pts, "", "no plot coordinates recorded in source"),
  basisOfRecord = "HumanObservation", samplingProtocol = "vegetation plot",
  occurrenceRemarks = x$notes, stringsAsFactors = FALSE)

# Appendix 2 (complete species list): add taxa not recorded in plots
app <- read.csv(file.path(archive, "Raw Data/2016-09-05_Baseline_Ecological_Documentation_of_DL21_and_DL22_Galiano_Island_Comprehensive_Species_List.csv"),
                colClasses = "character")
app$scientificName <- str_squish(app$scientificName)
app.cl <- clean_names(app$scientificName, src)
app.new <- !(app.cl$scientificName %in% dwc$scientificName)
if (any(app.new)) {
  dwc <- bind_rows(dwc, data.frame(app.cl[app.new, ], datasetName = unique(x$datasetName), recordedBy = "Keith Erickson",
    eventDate = "2016-04-25/2016-05-12", locality = "Galiano Island; Mount Sutil; DL21 & DL22",
    basisOfRecord = "HumanObservation", samplingProtocol = "species list (report Appendix 2: complete species list)",
    occurrenceRemarks = app$note[app.new], stringsAsFactors = FALSE))
}
dwc$bibliographicCitation <- "Erickson, K. (2016). Baseline Ecological Documentation of DL21 and DL22, Galiano Island. 2016-09-05."
write_source(src, x, dwc)

# ---- 2. Fairbarns 2016: Vascular Plant Survey of a Rock Bluff System (Aruncus Consulting) ----

src <- "Fairbarns_2016_Rock_Bluff_System_vascular_plant_survey"
# Source: Andrew Simon's own digitization (Raw Data/Aruncus_Consulting_...csv); the GCA (S. Poleyko) xlsx transcription
# of the same list is kept in the archive only, to avoid redundancy.
x <- read.csv(file.path(archive, "Raw Data/Aruncus_Consulting_July_2016_Vascular_Plant_Survey_of_a_Rock_Bluff_System_on_Galiano_Island.csv"),
              colClasses = "character", check.names = FALSE)
x[] <- lapply(x, str_squish)
# Two taxa were merged into one row in the source CSV (line 101); split them (confirmed against the GCA transcription)
merged <- which(x$scientificName == "Erythranthe alsinoides Erythranthe guttata")
x <- rbind(x[seq_len(merged - 1), ], x[merged, ], x[merged:nrow(x), ])
x$scientificName[merged:(merged + 1)] <- c("Erythranthe alsinoides", "Erythranthe guttata")
# Amelanchier alnifolia and Crataegus monogyna are each listed twice in the source CSV (once per group); keep one row each
x <- x[!duplicated(x$scientificName), ]
x$recordedBy <- "Matt Fairbarns"
x$datasetName <- "Vascular Plant Survey of a Rock Bluff System on Galiano Island"
x$locality <- "Galiano Island; Mount Sutil; The Galiano Bluffs; southeast slopes of Galiano Island"
x$notes <- x$note
x$vernacularName <- ""
x$eventDate <- "2016-07"
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, eventRemarks = "report dated July 2016; survey dates not given",
  locality = x$locality, basisOfRecord = "HumanObservation", samplingProtocol = "species list",
  occurrenceRemarks = x$notes,
  bibliographicCitation = "Fairbarns, M. (2016). Vascular Plant Survey of a Rock Bluff System on Galiano Island. Aruncus Consulting, July 2016.",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 3. Scholz & Janszen 2004: Mount Sutil Baseline Report, REM Lot 23 ----

src <- "Scholz_Janszen_2004_Mount_Sutil_REM_Lot_23_baseline"
x <- read_sheet("DL22 Mt. Sutil/REM lot 23/Galiano Island Mount Sutil Baseline Report REM Lot 23.xlsx")
names(x)[2:3] <- c("provincialRank", "provincialList")
hab.cols <- c("SO (sandstone)","GR (grass)","RO (Conglomerate bluff)","WDbd (Garry Oak woodland)","WDmx (Doug-fir Garry Oak Woodland Forest)")
hab <- apply(x[, hab.cols], 1, function(v) paste(hab.cols[v != ""], collapse = "; "))
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, datasetName = x$datasetName, recordedBy = x$recordedBy, eventDate = "2003-07/2004-03",
  verbatimEventDate = x$eventDate, locality = "Galiano Island; Mount Sutil; Remainder of Lot 23",
  habitat = hab, basisOfRecord = "HumanObservation", samplingProtocol = "species list by habitat unit",
  occurrenceRemarks = ifelse(x$provincialRank == "", "", paste0("provincial rank in source: ", x$provincialRank,
                      ifelse(x$provincialList == "", "", paste0(" (", x$provincialList, ")")))),
  bibliographicCitation = "Scholz, O. & Janszen, H. (2004). Galiano Island Mount Sutil Baseline Report, REM Lot 23.",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 4. Erickson 2022: Ecological Assessment for DL21 and 22 — BC CDC rare element occurrences ----

src <- "Erickson_2022_DL21_DL22_BC_CDC_rare_element_occurrences"
x <- read_sheet("DL22 Mt. Sutil/sutil rare species/Sutil - BC Conservation Data Centre Rare Element Occurrences.xlsx")
x$eventDate <- "2022-06-13"
x$vernacularName[x$vernacularName == "Gary Oak"] <- "Garry oak"
r <- with_names(x, src); x <- r$x
community <- grepl("woodland occurrence|forest occurrence", x$occurrenceRemarks)
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, eventRemarks = "date of report; element occurrences compiled from BC CDC records of various dates",
  locality = "Galiano Island; Mount Galiano / Mount Sutil; DL21 & DL22", basisOfRecord = "HumanObservation",
  samplingProtocol = "BC Conservation Data Centre element occurrence",
  occurrenceRemarks = paste0(ifelse(community, "ECOLOGICAL COMMUNITY element occurrence (not a species occurrence). ", ""),
                             x$occurrenceRemarks, " [BC list: ", x$`BC list`, "; S-rank ", x$`provincial rank`,
                             "; G-rank ", x$`global rank`, "]"),
  bibliographicCitation = "Erickson, K. (2022). Ecological Assessment for District Lot 21 and 22, Galiano Island. 2022-06-13.",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 5. Turner, Erickson & Canfield 1998: Vegetation Species Inventory, Pebble Beach Area ----
# Note: 'Vegetation Inventory Pebble Beach Area.xlsx' is an earlier, undated transcription of the same list (archived only).

src <- "Turner_Erickson_Canfield_1998_Pebble_Beach_vegetation_inventory"
x <- read_sheet("DL63 Pebble Beach/Pebble Beach Plant List 1998.xlsx")
x$eventDate <- excel_date(x$eventDate)
x$vernacularName <- recode(x$vernacularName, "Western uew" = "Western yew")
x$notes <- recode(x$notes, "costal" = "coastal", "forest nr. each" = "forest near beach")
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, locality = x$locality, basisOfRecord = "HumanObservation", samplingProtocol = "species list",
  occurrenceRemarks = x$notes, stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 6. Huggins 2019: DL 64 (Cable Bay) Preliminary Survey ----

src <- "Huggins_2019_DL64_Cable_Bay_preliminary_survey"
x <- read_sheet("DL64 Cable Bay/DL 64 Preliminary Survey.xlsx")
# eventDate values were drag-filled in Excel (2019-01-01, 2019-01-02, ...); only the year is reliable
x$eventDate <- "2019"
x$locality <- "Galiano Island; DL64; Cable Bay; Qwxwulwi’s"
x$locationRemarks <- str_replace_all(x$locationRemarks, c("^DL 64; ploly " = "poly ", "\\.0$" = ""))
x$locationRemarks <- ifelse(grepl("^[0-9, ]+$", x$locationRemarks), paste0("poly ", x$locationRemarks), x$locationRemarks)
x$name <- recode(x$name, "costal strip" = "coastal strip", "costal buffer" = "coastal buffer", "rockey shoreline" = "rocky shoreline")
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, datasetName = x$datasetName, recordedBy = x$recordedBy, eventDate = x$eventDate,
  eventRemarks = "year only; dates in transcription were drag-filled and are not survey dates",
  locality = x$locality, locationRemarks = paste0("map unit ", x$locationRemarks, ": ", x$name),
  habitat = paste0("site series: ", x$siteSeries), basisOfRecord = "HumanObservation",
  samplingProtocol = "ecosystem map unit survey", occurrenceRemarks = x$notes, stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 7. Taylor 2012: Vascular Plants of Galiano Island ----

src <- "Taylor_2012_Galiano_Island_vascular_plant_record"
x <- read_sheet("Galiano Island General/Terry Taylor Galiano Vascular Plant Record 2012.xlsx")
x$eventDate <- excel_date(x$eventDate)
gen.only <- x$idQualifier == "sp."
x$scientificName[gen.only] <- paste(x$scientificName[gen.only], "sp.")
x$idQualifier[gen.only] <- ""
cf <- x$idQualifier == "cf. coronaria"
x$scientificName[cf] <- paste(x$scientificName[cf], "coronaria"); x$idQualifier[cf] <- "cf."
r <- with_names(x, src); x <- r$x
r$cl$identificationQualifier <- ifelse(x$idQualifier != "", x$idQualifier, r$cl$identificationQualifier)
dwc <- data.frame(r$cl, datasetName = x$datasetName, recordedBy = x$recordedBy, eventDate = x$eventDate,
  eventRemarks = "date of list", locality = "Galiano Island; Bluffs Park; Matthews Point; Bellhouse Park",
  basisOfRecord = "HumanObservation", samplingProtocol = "species list", stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 8. Galiano Species Master List (GCA, 2009; author unknown) ----

src <- "GCA_2009_Galiano_Species_Master_List"
x <- read_sheet("Galiano Island General/Galiano Species Master List_June_9.xlsx")
x <- x[, c("scientificName", "vernacularName", "Notes", "eventDate")]
x$eventDate <- "2009"
r <- with_names(x, src); x <- r$x
r$cl$identificationRemarks <- str_squish(paste(r$cl$identificationRemarks, x$Notes))
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = "Galiano Species Master List",
  eventDate = x$eventDate, verbatimEventDate = "June 9 (file name); 2009-06-01 (spreadsheet)",
  eventRemarks = "list found in GCA records with no further context or author; year from spreadsheet",
  locality = "Galiano Island", basisOfRecord = "HumanObservation", samplingProtocol = "species list",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 9. DL57 Millard Learning Centre flora inventory (2012-2015) ----

src <- "DL57_Millard_Learning_Centre_flora_inventory_2012-2015"
x <- read_sheet("Millard Learning Centre/Digitized Flora Inventory DL57.xlsx", "Sheet1")
x$year <- str_extract(x$recordedBy, "[0-9]{4}")
x$observer <- str_squish(str_remove(x$recordedBy, "[0-9]{4}"))
x$observer[x$observer == "?"] <- ""
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = "DL57 Millard Learning Centre Flora Inventory",
  recordedBy = x$observer, eventDate = ifelse(is.na(x$year), "2012/2015", x$year),
  locality = "Galiano Island; Millard Learning Centre; DL57",
  basisOfRecord = ifelse(grepl("collected", x$basisOfRecord, ignore.case = TRUE), "PreservedSpecimen", "HumanObservation"),
  samplingProtocol = "species inventory",
  occurrenceRemarks = str_squish(paste0("growth form: ", x$habitat, "; origin in source: ", x$degreeOfEstablishment,
                       "; basis in source: ", x$basisOfRecord, ifelse(x$fieldNotes == "", "", paste0("; ", x$fieldNotes)))),
  stringsAsFactors = FALSE)
dwc$basisOfRecord[dwc$basisOfRecord == "PreservedSpecimen"] <- "HumanObservation"  # specimen repository unknown; keep as observation
write_source(src, x, dwc)

# ---- 10. Huggins & Thompson 2023: Quadra Hill Baseline Survey ----

src <- "Huggins_Thompson_2023_Quadra_Hill_baseline_survey"
x <- read_sheet("Quadra Hill/Quadra Hill Baseline Survey Species List.xlsx")
# eventDate values were drag-filled (2023-01-01 ... ); coordinates (45.855064, -54.342583) are not on Galiano Island
x$eventDate <- "2023"; x$decimalLatitude <- ""; x$decimalLongitude <- ""
names(x)[names(x) == "verbatiumCoordinates"] <- "verbatimCoordinates"
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, eventRemarks = "year only; dates in transcription were drag-filled",
  locality = x$locality, locationRemarks = "species list from the entire baseline survey (19 plots)",
  georeferenceRemarks = "verbatim coordinates in source ('45°51’18.23”E; 54°20’33.30”N') are invalid and were disregarded",
  basisOfRecord = "HumanObservation", samplingProtocol = "baseline survey species list", stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- 11. Searle & Associates 2001: Trincomali Nature Sanctuary ----

src <- "Searle_2001_Trincomali_Nature_Sanctuary_plant_list"
x <- read_sheet("Trincomali Nature Reserve/Plant Species List Trincomali 2001.xlsx")
x <- x[, names(x) != "unnamed_2"]
names(x)[names(x) == "datasetname"] <- "datasetName"
x$eventDate <- excel_date(x$eventDate)
# Single coordinate in source (48.55276, -123.27399) lies off Galiano Island (Saanich Peninsula); removed
x$decimalLat <- ""; x$decimalLong <- ""
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, datasetName = x$datasetName, recordedBy = x$recordedBy, eventDate = x$eventDate,
  locality = "Galiano Island; Trincomali Nature Sanctuary", locationRemarks = x$locationRemarks,
  georeferenceRemarks = "coordinate in source transcription (48.55276, -123.27399) falls off Galiano Island; removed",
  basisOfRecord = "HumanObservation", samplingProtocol = "species list by habitat",
  bibliographicCitation = "Searle & Associates (2001). Management Plan for Trincomali Nature Sanctuary, Galiano Island, BC.",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)


# ---- 12-14. Previously installed GCA (S. Poleyko) transcriptions, regenerated from the archive with the same cleaning ----
# (approved by A. Simon 2026-11-14; verbatim originals are the archive workbooks and the pre-2026-11-14 repo CSVs in git history)

src <- "Crystal_Mountain_Species"
x <- read_sheet("Crystal Mountain/Crystal Mountain Species.xlsx")
names(x)[names(x) == "unnamed_7"] <- "locationRemarks"
x$polygon <- sub("\\.0$", "", x$polygon)
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, datasetName = x$datasetName, recordedBy = "Keith Erickson", eventDate = "2009-09/2009-12",
  verbatimEventDate = x$eventDate, locality = x$locality,
  locationRemarks = str_squish(paste0("polygon ", x$polygon, ifelse(x$locationRemarks == "", "", paste0("; ", x$locationRemarks)))),
  basisOfRecord = "HumanObservation", samplingProtocol = "ecosystem polygon survey",
  bibliographicCitation = "Erickson, K. (2010). Crystal Mountain Ecosystem-based Land-use Plan. March 2010.", stringsAsFactors = FALSE)
write_source(src, x, dwc)

src <- "Collins_Westbourne_Covanent_Baseline Report_Consolidated_data"
x <- read_sheet("Collins baseline report data/Collins Westbourne Covanent Baseline Report.xlsx", "Consolidated data")
x$eventDate <- excel_date(x$eventDate)
# Latitude and longitude were transposed in the transcription
lat <- x$decimalLongitude; x$decimalLongitude <- x$decimalLatitude; x$decimalLatitude <- lat
x$decimalLatitude <- as.character(round(as.numeric(x$decimalLatitude), 6)); x$decimalLongitude <- as.character(round(as.numeric(x$decimalLongitude), 6))
r <- with_names(x, src); x <- r$x
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = "Collins Westbourne Covenant Baseline Report",
  recordedBy = "Keith Erickson", eventDate = x$eventDate, eventRemarks = x$eventRemarks,
  locality = "Galiano Island; Collins Westbourne Covenant; DL26", locationRemarks = x$locationRemarks,
  habitat = paste0("site series: ", x$habitat), decimalLatitude = x$decimalLatitude, decimalLongitude = x$decimalLongitude,
  georeferenceRemarks = "coordinates approximate", basisOfRecord = "HumanObservation",
  samplingProtocol = "baseline report species list", stringsAsFactors = FALSE)
write_source(src, x, dwc)

src <- "Celeste_Thompson_2011_Plants_of_Retreat_Island"
x <- read_sheet("Plans of Retreat Island/Plants of Retreat Island 2011.xlsx")
# Latitude and longitude column labels were transposed in the transcription
names(x)[names(x) == "decmalLat"] <- "decimalLongitude"; names(x)[names(x) == "decimalLong"] <- "decimalLatitude"
x$eventDate <- "2011-06/2011-10"
r <- with_names(x, src); x <- r$x
r$cl$identificationQualifier <- ifelse(x$idQualifier != "", x$idQualifier, r$cl$identificationQualifier)
dwc <- data.frame(r$cl, vernacularName = x$vernacularName, datasetName = x$datasetName, recordedBy = x$recordedBy,
  eventDate = x$eventDate, locality = "Galiano Island; Retreat Island", decimalLatitude = "48.940414", decimalLongitude = "-123.504297",
  georeferenceRemarks = "general point for Retreat Island", basisOfRecord = "HumanObservation", samplingProtocol = "species list",
  stringsAsFactors = FALSE)
write_source(src, x, dwc)

# ---- Corrections log ----

write.csv(unique(corrections), "corrections_log_2026-11-14.csv", row.names = FALSE)
cat("\nName corrections logged:", nrow(unique(corrections)), "\n")
