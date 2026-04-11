#### Streamlined R script for visualizing marine animal reporting
#### Historical reporters/collectors vs contemporary observers
#### Includes historical-baseline accounting, contributor summaries,
#### cumulative knowledge by phylum, and example outputs

# Set relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ------------------------------------------------------------
## LOAD PACKAGES
## ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(igraph)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)
library(tibble)
library(viridis)
library(scales)
library(rstudioapi)

## ------------------------------------------------------------
## USER CONTROLS
## ------------------------------------------------------------

# Network layout and filtering
EXPANSION_MULTIPLIER <- 1.4
REFERENCE_EXPANSION_FOR_WINDOW <- 1.00
PLOT_WINDOW_PADDING <- 0.20
MAX_HISTORICAL_CONTRIBUTORS <- 35
MAX_CONTEMPORARY_CONTRIBUTORS <- 63
MAX_TAXA <- 686
LAYOUT_NITER <- 2000
PRINT_RANGE_CHECK <- TRUE

# Filter which "new YYYY" records are shown
FILTER_NEW_RECORD_YEARS <- TRUE
NEW_RECORD_YEAR_MIN <- 2015
NEW_RECORD_YEAR_MAX <- 2026

# Style
BASE_FONT_FAMILY <- "sans"
PLOT_TITLE <- "Biodiversity Galiano 2026: marine animals"
PLOT_SUBTITLE <- "Relational network of historical reporting and contemporary biodiversity observations"
TITLE_CEX <- 1.60
SUBTITLE_CEX <- 1
TITLE_FONT <- 2
BG_COLOR <- "black"
FG_COLOR <- "white"
SUBTLE_LINE_COLOR <- "gray70"
PERSON_LABEL_CEX <- 0.72
LABEL_COLOR <- "white"
USE_LABEL_JITTER <- TRUE
LABEL_JITTER_X <- 0.03
LABEL_JITTER_Y <- 0.03
LABEL_ONLY_PEOPLE <- TRUE
LEGEND_TITLE_CEX <- 1.00
LEGEND_TEXT_CEX <- 0.88

# Outputs
OUTPUT_DIR <- "outputs"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

RECENT_YEARS_BACK <- 1
TOP_N_GAINERS <- 5
TOP_N_ATTENTION <- 5
TOP_N_CONTRIBUTORS <- 10
TOP_N_HISTORICAL_SOURCES <- 10

## ------------------------------------------------------------
## HELPERS
## ------------------------------------------------------------

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

first_existing_col <- function(data, candidates) {
  hits <- candidates[candidates %in% names(data)]
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

pull_or_na <- function(data, colname) {
  if (is.na(colname) || !(colname %in% names(data))) {
    return(rep(NA_character_, nrow(data)))
  }
  as.character(data[[colname]])
}

extract_year <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x), NA_character_, x)
  yr <- stringr::str_extract(x, "(?<![0-9])(18|19|20)[0-9]{2}(?![0-9])")
  suppressWarnings(as.integer(yr))
}

pct_fmt <- function(x, digits = 1) {
  ifelse(is.na(x), "0%", paste0(round(100 * x, digits), "%"))
}

n_fmt <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

collapse_and <- function(x) {
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

normalize_inat_link <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x[x == ""] <- NA_character_
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    stringr::str_detect(x, "^https?://www\\.inaturalist\\.org/observations/[0-9]+/?$") ~
      stringr::str_remove(x, "/$"),
    stringr::str_detect(x, "^https?://inaturalist\\.org/observations/[0-9]+/?$") ~
      sub("^https?://inaturalist\\.org", "https://www.inaturalist.org", stringr::str_remove(x, "/$")),
    stringr::str_detect(x, "^iNat:[0-9]+$") ~
      paste0("https://www.inaturalist.org/observations/", stringr::str_extract(x, "[0-9]+")),
    stringr::str_detect(x, "^[0-9]+$") ~
      paste0("https://www.inaturalist.org/observations/", x),
    TRUE ~ NA_character_
  )
}

extract_inat_id <- function(x) {
  x <- as.character(x)
  out <- stringr::str_extract(x, "(?<=/observations/)[0-9]+")
  out[out == ""] <- NA_character_
  out
}

format_historical_metadata <- function(collection, accession) {
  collection <- stringr::str_trim(as.character(collection))
  accession  <- stringr::str_trim(as.character(accession))
  
  collection[is.na(collection) | collection == ""] <- NA_character_
  accession[is.na(accession) | accession == ""] <- NA_character_
  
  dplyr::case_when(
    !is.na(collection) & !is.na(accession) ~ paste0(" (", collection, ", ", accession, ")"),
    !is.na(collection) ~ paste0(" (", collection, ")"),
    !is.na(accession) ~ paste0(" (", accession, ")"),
    TRUE ~ ""
  )
}

choose_group_phrase <- function(phylum_label, phylum_latin = NA_character_) {
  if (!is.na(phylum_label) && nzchar(phylum_label)) {
    out <- stringr::str_remove(phylum_label, "\\s*\\([^\\)]+\\)")
    out <- stringr::str_trim(out)
    return(tolower(out))
  }
  if (!is.na(phylum_latin) && nzchar(phylum_latin)) return(phylum_latin)
  "animals"
}

phylum_label_map <- c(
  "Annelida"        = "Segmented worms (Annelida)",
  "Arthropoda"      = "Arthropods (Arthropoda)",
  "Brachiopoda"     = "Lamp shells (Brachiopoda)",
  "Bryozoa"         = "Moss animals (Bryozoa)",
  "Chaetognatha"    = "Arrow worms (Chaetognatha)",
  "Chordata"        = "Chordates (Chordata)",
  "Cnidaria"        = "Jellyfish, corals & anemones (Cnidaria)",
  "Ctenophora"      = "Comb jellies (Ctenophora)",
  "Echinodermata"   = "Sea stars, urchins & kin (Echinodermata)",
  "Entoprocta"      = "Goblet worms (Entoprocta)",
  "Mollusca"        = "Molluscs (Mollusca)",
  "Nemertea"        = "Ribbon worms (Nemertea)",
  "Phoronida"       = "Horseshoe worms (Phoronida)",
  "Platyhelminthes" = "Flatworms (Platyhelminthes)",
  "Porifera"        = "Sponges (Porifera)",
  "Sipuncula"       = "Peanut worms (Sipuncula)"
)

get_phylum_label <- function(x) {
  dplyr::recode(x, !!!phylum_label_map, .default = x)
}

## ------------------------------------------------------------
## READ AND ALIGN DATA
## ------------------------------------------------------------

MARINE_ANIMALS <- read.csv(
  "../review/Animalia/marine_animals/summaries/Galiano_marine_animals_summary_2026-04-05.csv",
  stringsAsFactors = FALSE
)

updated_names <- c(
  "Taxon","Taxon.Author","Subtaxon.Author","Common.Name","Kingdom","Phylum",
  "Subphylum","Superclass","Class","Subclass","Superorder","Order","Suborder",
  "Superfamily","Family","Subfamily","Tribe","Genus","Species","Hybrid",
  "Subspecies","Variety","Origin","Provincial.Status","National.Status",
  "Reporting.Status","Observation","Collected.Reported..y.m.d.",
  "Collector.Source","Collection.List","Accession.Number","GBIF.ID",
  "First.Observed","Observer","iNaturalist.Link","Notes","ID","Stats.Code"
)

names(MARINE_ANIMALS) <- updated_names

## ------------------------------------------------------------
## CLEAN KEY FIELDS
## ------------------------------------------------------------

df <- MARINE_ANIMALS %>%
  mutate(
    Taxon = str_trim(as.character(Taxon)),
    Reporting.Status = str_to_lower(str_trim(as.character(Reporting.Status))),
    Collector.Source = na_if(str_trim(as.character(Collector.Source)), ""),
    Observer = na_if(str_trim(as.character(Observer)), ""),
    Collected.Reported..y.m.d. = na_if(str_trim(as.character(Collected.Reported..y.m.d.)), ""),
    First.Observed = na_if(str_trim(as.character(First.Observed)), ""),
    Collection.List = na_if(str_trim(as.character(Collection.List)), ""),
    Accession.Number = na_if(str_trim(as.character(Accession.Number)), ""),
    iNaturalist.Link = na_if(str_trim(as.character(iNaturalist.Link)), "")
  ) %>%
  mutate(
    Observer = case_when(
      Observer == "Lauren Magner & Andrew Simon" ~ "Lauren Magner",
      TRUE ~ Observer
    )
  ) %>%
  filter(!is.na(Taxon), Taxon != "") %>%
  mutate(
    new_year = case_when(
      str_detect(Reporting.Status, "^new\\s+[0-9]{4}$") ~ as.integer(str_extract(Reporting.Status, "[0-9]{4}")),
      TRUE ~ NA_integer_
    ),
    Reporting.Status.simple = case_when(
      str_detect(Reporting.Status, "^new") ~ "new",
      Reporting.Status %in% c("reported", "confirmed") ~ Reporting.Status,
      TRUE ~ NA_character_
    )
  )

if (FILTER_NEW_RECORD_YEARS) {
  df <- df %>%
    filter(
      Reporting.Status.simple != "new" |
        (!is.na(new_year) &
           new_year >= NEW_RECORD_YEAR_MIN &
           new_year <= NEW_RECORD_YEAR_MAX)
    )
}

## ------------------------------------------------------------
## CENTRAL ACCOUNTING TABLE
## Everything else is derived from this
## ------------------------------------------------------------

dat <- df %>%
  mutate(
    Taxon_std = Taxon,
    Phylum_std = na_if(str_trim(as.character(Phylum)), ""),
    status_std = Reporting.Status.simple,
    historical_year = extract_year(Collected.Reported..y.m.d.),
    observed_year = extract_year(First.Observed),
    new_year_std = new_year,
    contribution_year = case_when(
      status_std == "confirmed" ~ observed_year,
      status_std == "new" ~ new_year_std,
      TRUE ~ NA_integer_
    ),
    Observer_std = Observer,
    Collector_std = Collector.Source,
    iNat_link = normalize_inat_link(iNaturalist.Link),
    iNat_ID = extract_inat_id(normalize_inat_link(iNaturalist.Link)),
    Historical_metadata = format_historical_metadata(Collection.List, Accession.Number),
    Phylum_label = get_phylum_label(Phylum_std)
  ) %>%
  filter(!is.na(Taxon_std), !is.na(status_std))

all_recent_years <- c(dat$new_year_std, dat$observed_year)
all_recent_years <- all_recent_years[!is.na(all_recent_years)]
RECENT_YEAR_MAX <- if (length(all_recent_years) > 0) max(all_recent_years, na.rm = TRUE) else as.integer(format(Sys.Date(), "%Y"))
RECENT_YEAR_MIN <- RECENT_YEAR_MAX - RECENT_YEARS_BACK

## ------------------------------------------------------------
## TAXON STATUS FOR PLOTTING
## Keep existing labels unchanged
## ------------------------------------------------------------

taxon_status <- dat %>%
  group_by(Taxon_std) %>%
  summarise(
    has_confirmed = any(status_std == "confirmed", na.rm = TRUE),
    has_new = any(status_std == "new", na.rm = TRUE),
    has_reported = any(status_std == "reported", na.rm = TRUE),
    new_year_min = suppressWarnings(min(new_year_std, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    new_year_min = ifelse(is.infinite(new_year_min), NA, new_year_min),
    taxon_status = case_when(
      has_confirmed ~ "confirmed",
      has_new ~ "new",
      has_reported ~ "reported",
      TRUE ~ NA_character_
    ),
    taxon_year = case_when(
      taxon_status == "new" ~ new_year_min,
      TRUE ~ NA_real_
    )
  ) %>%
  select(Taxon = Taxon_std, taxon_status, taxon_year)

## ------------------------------------------------------------
## EDGES FOR NETWORK
## ------------------------------------------------------------

historical_edges <- dat %>%
  filter(status_std %in% c("reported", "confirmed"), !is.na(Collector_std)) %>%
  transmute(
    from = Collector_std,
    to = Taxon_std,
    person_name = Collector_std,
    person_class = "historical",
    edge_class = status_std,
    weight = 1
  ) %>%
  distinct() %>%
  group_by(from, to, person_name, person_class, edge_class) %>%
  summarise(weight = sum(weight), .groups = "drop")

contemporary_edges <- dat %>%
  filter(status_std %in% c("new", "confirmed"), !is.na(Observer_std)) %>%
  transmute(
    from = Observer_std,
    to = Taxon_std,
    person_name = Observer_std,
    person_class = "contemporary",
    edge_class = status_std,
    weight = 1
  ) %>%
  distinct() %>%
  group_by(from, to, person_name, person_class, edge_class) %>%
  summarise(weight = sum(weight), .groups = "drop")

edges_all <- bind_rows(historical_edges, contemporary_edges)

historical_rank <- historical_edges %>%
  group_by(person_name) %>%
  summarise(total_links = sum(weight), n_taxa = n_distinct(to), .groups = "drop") %>%
  arrange(desc(total_links), desc(n_taxa), person_name)

contemporary_rank <- contemporary_edges %>%
  group_by(person_name) %>%
  summarise(total_links = sum(weight), n_taxa = n_distinct(to), .groups = "drop") %>%
  arrange(desc(total_links), desc(n_taxa), person_name)

top_historical <- if (is.infinite(MAX_HISTORICAL_CONTRIBUTORS)) {
  historical_rank$person_name
} else {
  historical_rank %>% slice_head(n = min(MAX_HISTORICAL_CONTRIBUTORS, nrow(historical_rank))) %>% pull(person_name)
}

top_contemporary <- if (is.infinite(MAX_CONTEMPORARY_CONTRIBUTORS)) {
  contemporary_rank$person_name
} else {
  contemporary_rank %>% slice_head(n = min(MAX_CONTEMPORARY_CONTRIBUTORS, nrow(contemporary_rank))) %>% pull(person_name)
}

edges_people_filtered <- edges_all %>%
  filter(
    (person_class == "historical" & person_name %in% top_historical) |
      (person_class == "contemporary" & person_name %in% top_contemporary)
  )

taxon_rank <- edges_people_filtered %>%
  group_by(to) %>%
  summarise(total_links = sum(weight), n_people = n_distinct(from), .groups = "drop") %>%
  arrange(desc(total_links), desc(n_people), to)

top_taxa <- if (is.infinite(MAX_TAXA)) {
  taxon_rank$to
} else {
  taxon_rank %>% slice_head(n = min(MAX_TAXA, nrow(taxon_rank))) %>% pull(to)
}

edges_top <- edges_people_filtered %>% filter(to %in% top_taxa)

cat("Plotting with:\n")
cat("  Historical contributors retained:   ", length(unique(edges_top$from[edges_top$person_class == "historical"])), "\n")
cat("  Contemporary contributors retained: ", length(unique(edges_top$from[edges_top$person_class == "contemporary"])), "\n")
cat("  Taxa retained:                      ", length(unique(edges_top$to)), "\n")
cat("  Total edges retained:               ", nrow(edges_top), "\n\n")

## ------------------------------------------------------------
## NODES AND GRAPH
## ------------------------------------------------------------

historical_nodes <- data.frame(name = unique(edges_top$from[edges_top$person_class == "historical"]), node_class = "historical", stringsAsFactors = FALSE)
contemporary_nodes <- data.frame(name = unique(edges_top$from[edges_top$person_class == "contemporary"]), node_class = "contemporary", stringsAsFactors = FALSE)
taxon_nodes <- data.frame(name = unique(edges_top$to), node_class = "taxon", stringsAsFactors = FALSE)

nodes_top <- bind_rows(historical_nodes, contemporary_nodes, taxon_nodes) %>%
  distinct(name, .keep_all = TRUE) %>%
  left_join(taxon_status, by = c("name" = "Taxon"))

g_top <- graph_from_data_frame(
  d = edges_top %>% select(from, to, weight, edge_class, person_class),
  vertices = nodes_top,
  directed = FALSE
)

## ------------------------------------------------------------
## STYLING
## ------------------------------------------------------------

base_node_colors <- c(
  "historical" = "#CC79A7",
  "contemporary" = "#E69F00",
  "reported" = "gray60",
  "confirmed" = "#009E73"
)

new_years_present <- sort(unique(na.omit(V(g_top)$taxon_year)))

if (length(new_years_present) > 1) {
  new_palette <- colorRampPalette(c("#FDE725", "#D55E00"))(length(new_years_present))
  names(new_palette) <- as.character(new_years_present)
} else if (length(new_years_present) == 1) {
  new_palette <- "#D55E00"
  names(new_palette) <- as.character(new_years_present)
} else {
  new_palette <- character(0)
}

V(g_top)$color <- case_when(
  V(g_top)$node_class == "historical" ~ base_node_colors["historical"],
  V(g_top)$node_class == "contemporary" ~ base_node_colors["contemporary"],
  V(g_top)$node_class == "taxon" & V(g_top)$taxon_status == "reported" ~ base_node_colors["reported"],
  V(g_top)$node_class == "taxon" & V(g_top)$taxon_status == "confirmed" ~ base_node_colors["confirmed"],
  V(g_top)$node_class == "taxon" & V(g_top)$taxon_status == "new" &
    !is.na(V(g_top)$taxon_year) &
    as.character(V(g_top)$taxon_year) %in% names(new_palette) ~ new_palette[as.character(V(g_top)$taxon_year)],
  V(g_top)$node_class == "taxon" & V(g_top)$taxon_status == "new" ~ "#D55E00",
  TRUE ~ "gray80"
)

deg_top <- degree(g_top)
V(g_top)$size <- case_when(
  V(g_top)$node_class == "historical" ~ rescale(deg_top, to = c(2.0, 3.0)),
  V(g_top)$node_class == "contemporary" ~ rescale(deg_top, to = c(2.0, 3.0)),
  V(g_top)$node_class == "taxon" ~ rescale(deg_top, to = c(1.0, 1.8))
)

V(g_top)$label <- ifelse(V(g_top)$node_class %in% c("historical", "contemporary"), V(g_top)$name, NA)

edge_colors <- c("reported" = "gray60", "confirmed" = "#009E73", "new" = "#D55E00")
E(g_top)$color <- edge_colors[E(g_top)$edge_class]
E(g_top)$color[is.na(E(g_top)$color)] <- "gray80"
E(g_top)$width <- rescale(E(g_top)$weight, to = c(0.5, 2.2))

## ------------------------------------------------------------
## LAYOUT AND NETWORK PLOT
## ------------------------------------------------------------

set.seed(123)
lay_base <- layout_with_fr(g_top, niter = LAYOUT_NITER)
lay_center <- lay_base
lay_center[, 1] <- lay_center[, 1] - mean(lay_center[, 1])
lay_center[, 2] <- lay_center[, 2] - mean(lay_center[, 2])
lay_top <- lay_center * EXPANSION_MULTIPLIER

x_range_ref <- range(lay_center[, 1] * REFERENCE_EXPANSION_FOR_WINDOW, na.rm = TRUE)
y_range_ref <- range(lay_center[, 2] * REFERENCE_EXPANSION_FOR_WINDOW, na.rm = TRUE)
x_span_ref <- diff(x_range_ref)
y_span_ref <- diff(y_range_ref)
if (!is.finite(x_span_ref) || x_span_ref == 0) x_span_ref <- 1
if (!is.finite(y_span_ref) || y_span_ref == 0) y_span_ref <- 1
x_pad <- x_span_ref * PLOT_WINDOW_PADDING
y_pad <- y_span_ref * PLOT_WINDOW_PADDING
xlim_use <- c(x_range_ref[1] - x_pad, x_range_ref[2] + x_pad)
ylim_use <- c(y_range_ref[1] - y_pad, y_range_ref[2] + y_pad)

label_coords <- lay_top
if (USE_LABEL_JITTER) {
  set.seed(123)
  label_coords[, 1] <- label_coords[, 1] + rnorm(nrow(label_coords), mean = 0, sd = LABEL_JITTER_X)
  label_coords[, 2] <- label_coords[, 2] + rnorm(nrow(label_coords), mean = 0, sd = LABEL_JITTER_Y)
}

vertex_labels <- V(g_top)$label
if (LABEL_ONLY_PEOPLE) {
  vertex_labels[!(V(g_top)$node_class %in% c("historical", "contemporary"))] <- NA
}

old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(family = BASE_FONT_FAMILY, bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
layout(matrix(c(1, 2), nrow = 1), widths = c(5.2, 1.9))

par(mar = c(1.2, 1.2, 3.2, 0.8), bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
plot(
  g_top,
  layout = lay_top,
  rescale = FALSE,
  xlim = xlim_use,
  ylim = ylim_use,
  vertex.shape = "none",
  vertex.label = NA,
  edge.color = E(g_top)$color,
  edge.width = E(g_top)$width,
  main = NA,
  asp = 0,
  axes = FALSE
)

coords <- lay_top
node_df <- data.frame(
  x = coords[, 1],
  y = coords[, 2],
  name = V(g_top)$name,
  node_class = V(g_top)$node_class,
  color = V(g_top)$color,
  size = V(g_top)$size,
  stringsAsFactors = FALSE
)

tax_idx <- which(node_df$node_class == "taxon")
hist_idx <- which(node_df$node_class == "historical")
cont_idx <- which(node_df$node_class == "contemporary")

points(node_df$x[tax_idx], node_df$y[tax_idx], pch = 16, col = node_df$color[tax_idx], cex = node_df$size[tax_idx])
points(node_df$x[hist_idx], node_df$y[hist_idx], pch = 15, col = node_df$color[hist_idx], cex = node_df$size[hist_idx])
points(node_df$x[cont_idx], node_df$y[cont_idx], pch = 15, col = node_df$color[cont_idx], cex = node_df$size[cont_idx])

title(main = PLOT_TITLE, cex.main = TITLE_CEX, font.main = TITLE_FONT, family = BASE_FONT_FAMILY, col.main = FG_COLOR, line = 1)
mtext(text = PLOT_SUBTITLE, side = 3, line = -0.2, cex = SUBTITLE_CEX, col = FG_COLOR, family = BASE_FONT_FAMILY, font = 1)

label_idx <- which(!is.na(vertex_labels) & vertex_labels != "")
text(label_coords[label_idx, 1], label_coords[label_idx, 2], labels = vertex_labels[label_idx], cex = PERSON_LABEL_CEX, col = LABEL_COLOR, family = BASE_FONT_FAMILY, xpd = TRUE)

par(mar = c(1.2, 0.2, 3.2, 1.2), bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
text(x = 0.08, y = 0.96, labels = "Legend", adj = c(0, 1), cex = LEGEND_TITLE_CEX, font = 2, family = BASE_FONT_FAMILY, col = FG_COLOR)

legend(
  x = 0.08, y = 0.90,
  legend = c(
    "Historical reporter / collector",
    "Contemporary observer",
    "Reported species",
    "Confirmed species",
    "New species (year gradient)",
    "Reported",
    "Confirmed",
    "New"
  ),
  pch = c(15, 15, 16, 16, 16, NA, NA, NA),
  pt.cex = c(1.25, 1.25, 1.05, 1.05, 1.05, NA, NA, NA),
  col = c("#CC79A7", "#E69F00", "gray60", "#009E73", "#D55E00", "gray60", "#009E73", "#D55E00"),
  lty = c(NA, NA, NA, NA, NA, 1, 1, 1),
  lwd = c(NA, NA, NA, NA, NA, 2, 2, 2),
  text.col = FG_COLOR,
  bty = "n",
  cex = LEGEND_TEXT_CEX,
  xjust = 0,
  yjust = 1,
  seg.len = 2.2,
  text.font = 1,
  x.intersp = 0.8,
  y.intersp = 1.1
)

if (length(new_years_present) > 1) {
  y0 <- 0.18
  x_left <- 0.10
  x_right <- 0.86
  xs <- seq(x_left, x_right, length.out = length(new_years_present))
  points(xs, rep(y0, length(xs)), pch = 16, cex = 1.05, col = new_palette)
  segments(x_left, y0, x_right, y0, col = SUBTLE_LINE_COLOR, lwd = 1)
  text(x = x_left, y = y0 - 0.07, labels = min(new_years_present), adj = c(0, 1), cex = 0.78, family = BASE_FONT_FAMILY, col = FG_COLOR)
  text(x = x_right, y = y0 - 0.07, labels = max(new_years_present), adj = c(1, 1), cex = 0.78, family = BASE_FONT_FAMILY, col = FG_COLOR)
  text(x = x_left, y = y0 + 0.07, labels = "New taxa year gradient", adj = c(0, 0), cex = 0.82, family = BASE_FONT_FAMILY, col = FG_COLOR)
} else if (length(new_years_present) == 1) {
  text(x = 0.10, y = 0.16, labels = paste0("New taxa year: ", new_years_present), adj = c(0, 0), cex = 0.82, family = BASE_FONT_FAMILY, col = FG_COLOR)
}


if (PRINT_RANGE_CHECK) {
  cat("\n================ RANGE CHECK ================\n")
  cat("Historical contributors available:   ", nrow(historical_rank), "\n")
  cat("Contemporary contributors available: ", nrow(contemporary_rank), "\n")
  cat("Unique taxa available overall:       ", n_distinct(df$Taxon), "\n")
  cat("Historical edges available:          ", nrow(historical_edges), "\n")
  cat("Contemporary edges available:        ", nrow(contemporary_edges), "\n")
  cat("Total edges available:               ", nrow(edges_all), "\n")
  cat("---------------------------------------------\n")
  cat("Suggested bounds:\n")
  cat("  MAX_HISTORICAL_CONTRIBUTORS: 1 to ", nrow(historical_rank), "\n", sep = "")
  cat("  MAX_CONTEMPORARY_CONTRIBUTORS: 1 to ", nrow(contemporary_rank), "\n", sep = "")
  cat("  MAX_TAXA: 1 to ", n_distinct(df$Taxon), "\n", sep = "")
  if (sum(df$Reporting.Status.simple == "new", na.rm = TRUE) > 0) {
    cat("  Available 'new' record years: ", paste(range(df$new_year[df$Reporting.Status.simple == "new"], na.rm = TRUE), collapse = " to "), "\n", sep = "")
  }
  if (FILTER_NEW_RECORD_YEARS) {
    cat("  Active new-year filter: ", NEW_RECORD_YEAR_MIN, " to ", NEW_RECORD_YEAR_MAX, "\n", sep = "")
  } else {
    cat("  Active new-year filter: OFF\n")
  }
  cat("  Expansion multiplier: ", EXPANSION_MULTIPLIER, "\n", sep = "")
  cat("  Reference expansion for window: ", REFERENCE_EXPANSION_FOR_WINDOW, "\n", sep = "")
  cat("  Plot window padding: ", PLOT_WINDOW_PADDING, "\n", sep = "")
  cat("  Label jitter enabled: ", USE_LABEL_JITTER, "\n", sep = "")
  cat("=============================================\n\n")
}


## ------------------------------------------------------------
## CUMULATIVE KNOWLEDGE BY PHYLUM
## Labels unchanged
## ------------------------------------------------------------

if (!"Order" %in% names(df)) {
  stop("The data do not contain a column named 'Order'.", call. = FALSE)
}

df <- df %>% mutate(Order = str_trim(as.character(Order)), Order = na_if(Order, ""))

phylum_names_present <- df %>%
  filter(!is.na(Phylum)) %>%
  distinct(Phylum) %>%
  arrange(Phylum) %>%
  pull(Phylum)

cat("\nPhyla present in the dataset:\n")
print(phylum_names_present)

taxon_phylum_status <- dat %>%
  filter(!is.na(Phylum_std), !is.na(Taxon_std), Taxon_std != "") %>%
  group_by(Phylum_std, Taxon_std) %>%
  summarise(
    has_confirmed = any(status_std == "confirmed", na.rm = TRUE),
    has_new = any(status_std == "new", na.rm = TRUE),
    has_reported = any(status_std == "reported", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    knowledge_category = case_when(
      has_confirmed ~ "Observed historical reports",
      has_new ~ "New records",
      has_reported ~ "Unobserved historical records",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(knowledge_category))

phylum_diversity_summary <- taxon_phylum_status %>%
  count(Phylum_std, knowledge_category, name = "n_taxa") %>%
  group_by(Phylum_std) %>%
  mutate(total_taxa = sum(n_taxa)) %>%
  ungroup()

phylum_order <- phylum_diversity_summary %>%
  distinct(Phylum_std, total_taxa) %>%
  arrange(desc(total_taxa), Phylum_std) %>%
  pull(Phylum_std)

phylum_diversity_summary$Phylum_std <- factor(phylum_diversity_summary$Phylum_std, levels = phylum_order)
phylum_diversity_summary$knowledge_category <- factor(
  phylum_diversity_summary$knowledge_category,
  levels = c("Unobserved historical records", "Observed historical reports", "New records")
)

phylum_diversity_summary <- phylum_diversity_summary %>%
  mutate(Phylum_label = recode(as.character(Phylum_std), !!!phylum_label_map, .default = as.character(Phylum_std)))

phylum_label_levels <- phylum_diversity_summary %>%
  distinct(Phylum_std, Phylum_label) %>%
  mutate(Phylum_std = factor(Phylum_std, levels = phylum_order)) %>%
  arrange(Phylum_std) %>%
  pull(Phylum_label)

phylum_diversity_summary$Phylum_label <- factor(phylum_diversity_summary$Phylum_label, levels = phylum_label_levels)

cat("\nCumulative knowledge summary by phylum:\n")
print(phylum_diversity_summary)

vir_cols <- viridis::viridis(3, option = "D", direction = 1)
names(vir_cols) <- c("Unobserved historical records", "Observed historical reports", "New records")

p_cumulative_knowledge <- ggplot(
  phylum_diversity_summary,
  aes(x = Phylum_label, y = n_taxa, fill = knowledge_category)
) +
  geom_col(width = 0.82, color = NA) +
  scale_fill_manual(values = vir_cols) +
  scale_x_discrete(expand = expansion(mult = c(0.10, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Biodiversity Galiano 2026: cumulative knowledge by phylum",
    subtitle = "Number of taxa represented by historical-only, confirmed, and new records",
    x = NULL,
    y = "Number of taxa",
    fill = NULL
  ) +
  theme_minimal(base_family = BASE_FONT_FAMILY) +
  theme(
    plot.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
    panel.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 60),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray30", linewidth = 0.3),
    axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1, color = FG_COLOR, size = 10.5),
    axis.text.y = element_text(color = FG_COLOR, size = 11),
    axis.title.y = element_text(color = FG_COLOR, size = 12),
    plot.title = element_text(color = FG_COLOR, face = "bold", size = 16),
    plot.subtitle = element_text(color = FG_COLOR, size = 11),
    legend.text = element_text(color = FG_COLOR, size = 11),
    legend.background = element_rect(fill = BG_COLOR, color = BG_COLOR),
    legend.key = element_rect(fill = BG_COLOR, color = BG_COLOR)
  )

print(p_cumulative_knowledge)

## ------------------------------------------------------------
## STORY EXAMPLES BY PHYLUM × KNOWLEDGE CATEGORY
## Self-contained: does not depend on example_pool
## ------------------------------------------------------------

set.seed(123)

RECENT_NEW_YEAR_MAX <- if ("new_year" %in% names(df) && any(!is.na(df$new_year))) {
  max(df$new_year, na.rm = TRUE)
} else {
  2026
}
RECENT_NEW_YEAR_MIN <- RECENT_NEW_YEAR_MAX - 1

story_pool <- dat %>%
  mutate(
    knowledge_category = case_when(
      status_std == "reported" ~ "Remaining historical report",
      status_std == "confirmed" ~ "Confirmed record",
      status_std == "new" ~ "New record",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(Phylum_std),
    !is.na(Taxon_std),
    !is.na(knowledge_category)
  ) %>%
  filter(
    knowledge_category != "New record" |
      (!is.na(new_year_std) &
         new_year_std >= RECENT_NEW_YEAR_MIN &
         new_year_std <= RECENT_NEW_YEAR_MAX)
  )

marine_animal_story_examples <- story_pool %>%
  group_by(Phylum_std, Phylum_label, knowledge_category) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(
    category_sort = case_when(
      knowledge_category == "Remaining historical report" ~ 1L,
      knowledge_category == "Confirmed record" ~ 2L,
      knowledge_category == "New record" ~ 3L,
      TRUE ~ 99L
    ),
    historical_year = extract_year(Collected.Reported..y.m.d.),
    observed_year = extract_year(First.Observed),
    group_phrase = mapply(
      choose_group_phrase,
      Phylum_label,
      Phylum_std,
      USE.NAMES = FALSE
    ),
    story_text = case_when(
      
      # ------------------------------
      # Remaining historical
      # ------------------------------
      knowledge_category == "Remaining historical report" &
        !is.na(historical_year) & !is.na(Collector_std) ~
        paste0(
          "In ", historical_year, ", ",
          Collector_std, Historical_metadata,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", which has not been seen since."
        ),
      
      knowledge_category == "Remaining historical report" &
        !is.na(historical_year) ~
        paste0(
          "In ", historical_year,
          ", ", Taxon_std,
          ", a species of ", group_phrase,
          ", was historically reported",
          Historical_metadata,
          ", but it has not been seen since."
        ),
      
      knowledge_category == "Remaining historical report" &
        !is.na(Collector_std) ~
        paste0(
          Collector_std, Historical_metadata,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", which has not been seen since."
        ),
      
      knowledge_category == "Remaining historical report" ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was historically reported",
          Historical_metadata,
          ", but has not been seen since."
        ),
      
      # ------------------------------
      # Confirmed records (WITH LINK)
      # ------------------------------
      knowledge_category == "Confirmed record" &
        !is.na(historical_year) & !is.na(Collector_std) &
        !is.na(observed_year) & !is.na(Observer_std) & !is.na(iNat_link) ~
        paste0(
          "In ", historical_year, ", ",
          Collector_std, Historical_metadata,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", which was later confirmed by ",
          Observer_std,
          " in ", observed_year,
          ". Observation: ", iNat_link
        ),
      
      knowledge_category == "Confirmed record" &
        !is.na(historical_year) & !is.na(Collector_std) &
        !is.na(observed_year) & !is.na(Observer_std) ~
        paste0(
          "In ", historical_year, ", ",
          Collector_std, Historical_metadata,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", which was later confirmed by ",
          Observer_std,
          " in ", observed_year, "."
        ),
      
      knowledge_category == "Confirmed record" &
        !is.na(Observer_std) & !is.na(observed_year) & !is.na(iNat_link) ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was later confirmed by ",
          Observer_std,
          " in ", observed_year,
          ". Observation: ", iNat_link
        ),
      
      knowledge_category == "Confirmed record" &
        !is.na(Observer_std) & !is.na(observed_year) ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was later confirmed by ",
          Observer_std,
          " in ", observed_year, "."
        ),
      
      knowledge_category == "Confirmed record" & !is.na(iNat_link) ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was historically reported",
          Historical_metadata,
          " and later confirmed. Observation: ",
          iNat_link
        ),
      
      knowledge_category == "Confirmed record" ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was historically reported",
          Historical_metadata,
          " and later confirmed."
        ),
      
      # ------------------------------
      # New records (WITH LINK)
      # ------------------------------
      knowledge_category == "New record" &
        !is.na(new_year_std) & !is.na(Observer_std) & !is.na(iNat_link) ~
        paste0(
          "In ", new_year_std, ", ",
          Observer_std,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", as new for Galiano Island. Observation: ",
          iNat_link
        ),
      
      knowledge_category == "New record" &
        !is.na(new_year_std) & !is.na(Observer_std) ~
        paste0(
          "In ", new_year_std, ", ",
          Observer_std,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", as new for Galiano Island."
        ),
      
      knowledge_category == "New record" &
        !is.na(new_year_std) & !is.na(iNat_link) ~
        paste0(
          "In ", new_year_std,
          ", ", Taxon_std,
          ", a species of ", group_phrase,
          ", was reported as new for Galiano Island. Observation: ",
          iNat_link
        ),
      
      knowledge_category == "New record" &
        !is.na(Observer_std) & !is.na(iNat_link) ~
        paste0(
          Observer_std,
          " reported ", Taxon_std,
          ", a species of ", group_phrase,
          ", as new for Galiano Island. Observation: ",
          iNat_link
        ),
      
      knowledge_category == "New record" ~
        paste0(
          Taxon_std,
          ", a species of ", group_phrase,
          ", was reported as new for Galiano Island."
        ),
      
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Phylum_label, category_sort, Taxon_std) %>%
  select(
    Phylum = Phylum_label,
    Phylum_Latin = Phylum_std,
    Knowledge_category = knowledge_category,
    Taxon = Taxon_std,
    Collector_Source = Collector_std,
    Observer = Observer_std,
    Collection_List = Collection.List,
    Accession_Number = Accession.Number,
    Historical_metadata,
    Historical_date = Collected.Reported..y.m.d.,
    First_observed = First.Observed,
    New_record_year = new_year_std,
    iNat_ID,
    iNat_link,
    story_text
  )

write.csv(marine_animal_story_examples, file.path(OUTPUT_DIR, "Marine_animal_reports_by_phylum.csv"), row.names = FALSE)
cat("\nSaved file: ", file.path(OUTPUT_DIR, "Marine_animal_reports_by_phylum.csv"), "\n", sep = "")

## ------------------------------------------------------------
## STREAMLINED REPORTING FRAMEWORK
## Historical baseline + rediscovery + additions + contributors
## ------------------------------------------------------------

taxon_level <- dat %>%
  group_by(Phylum_std, Phylum_label, Taxon_std) %>%
  summarise(
    in_historical_baseline = any(status_std %in% c("reported", "confirmed"), na.rm = TRUE),
    rediscovered_from_history = any(status_std == "confirmed", na.rm = TRUE),
    added_new_since_project = any(status_std == "new", na.rm = TRUE),
    first_rediscovery_year = suppressWarnings(min(observed_year[status_std == "confirmed"], na.rm = TRUE)),
    first_new_year = suppressWarnings(min(new_year_std[status_std == "new"], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    first_rediscovery_year = ifelse(is.infinite(first_rediscovery_year), NA, first_rediscovery_year),
    first_new_year = ifelse(is.infinite(first_new_year), NA, first_new_year),
    historical_still_missing = in_historical_baseline & !rediscovered_from_history,
    recent_rediscovery = rediscovered_from_history & !is.na(first_rediscovery_year) & first_rediscovery_year >= RECENT_YEAR_MIN & first_rediscovery_year <= RECENT_YEAR_MAX,
    recent_new_addition = added_new_since_project & !is.na(first_new_year) & first_new_year >= RECENT_YEAR_MIN & first_new_year <= RECENT_YEAR_MAX
  )

phylum_summary <- taxon_level %>%
  group_by(Phylum_std, Phylum_label) %>%
  summarise(
    total_known_now = n_distinct(Taxon_std),
    historical_baseline = sum(in_historical_baseline, na.rm = TRUE),
    historical_rediscovered = sum(rediscovered_from_history, na.rm = TRUE),
    historical_still_missing = sum(historical_still_missing, na.rm = TRUE),
    new_since_project = sum(added_new_since_project, na.rm = TRUE),
    recent_rediscoveries = sum(recent_rediscovery, na.rm = TRUE),
    recent_new_additions = sum(recent_new_addition, na.rm = TRUE),
    recent_total_progress = recent_rediscoveries + recent_new_additions,
    rediscovery_share = ifelse(historical_baseline > 0, historical_rediscovered / historical_baseline, NA_real_),
    still_missing_share = ifelse(historical_baseline > 0, historical_still_missing / historical_baseline, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(historical_baseline), desc(total_known_now), Phylum_label)

overall <- taxon_level %>%
  summarise(
    total_known_now = n_distinct(Taxon_std),
    historical_baseline = sum(in_historical_baseline, na.rm = TRUE),
    historical_rediscovered = sum(rediscovered_from_history, na.rm = TRUE),
    historical_still_missing = sum(historical_still_missing, na.rm = TRUE),
    new_since_project = sum(added_new_since_project, na.rm = TRUE),
    recent_rediscoveries = sum(recent_rediscovery, na.rm = TRUE),
    recent_new_additions = sum(recent_new_addition, na.rm = TRUE)
  ) %>%
  mutate(
    recent_total_progress = recent_rediscoveries + recent_new_additions,
    rediscovery_share = ifelse(historical_baseline > 0, historical_rediscovered / historical_baseline, NA_real_),
    still_missing_share = ifelse(historical_baseline > 0, historical_still_missing / historical_baseline, NA_real_)
  )

contemporary_confirmed_rank <- dat %>%
  filter(status_std == "confirmed", !is.na(Observer_std)) %>%
  distinct(Observer_std, Taxon_std) %>%
  count(Observer_std, name = "n_confirmed_taxa") %>%
  arrange(desc(n_confirmed_taxa), Observer_std)

contemporary_new_rank <- dat %>%
  filter(status_std == "new", !is.na(Observer_std)) %>%
  distinct(Observer_std, Taxon_std) %>%
  count(Observer_std, name = "n_new_taxa") %>%
  arrange(desc(n_new_taxa), Observer_std)

contemporary_total_rank <- dat %>%
  filter(status_std %in% c("confirmed", "new"), !is.na(Observer_std)) %>%
  distinct(Observer_std, Taxon_std) %>%
  count(Observer_std, name = "n_total_taxa") %>%
  arrange(desc(n_total_taxa), Observer_std)

contemporary_contributor_summary <- contemporary_total_rank %>%
  full_join(contemporary_confirmed_rank, by = "Observer_std") %>%
  full_join(contemporary_new_rank, by = "Observer_std") %>%
  mutate(
    n_total_taxa = dplyr::coalesce(n_total_taxa, 0L),
    n_confirmed_taxa = dplyr::coalesce(n_confirmed_taxa, 0L),
    n_new_taxa = dplyr::coalesce(n_new_taxa, 0L)
  ) %>%
  arrange(desc(n_total_taxa), desc(n_confirmed_taxa), desc(n_new_taxa), Observer_std)

historical_source_rank <- dat %>%
  filter(status_std %in% c("reported", "confirmed"), !is.na(Collector_std)) %>%
  distinct(Collector_std, Taxon_std) %>%
  count(Collector_std, name = "n_historical_taxa") %>%
  arrange(desc(n_historical_taxa), Collector_std)

historical_source_confirmed_rank <- dat %>%
  filter(status_std == "confirmed", !is.na(Collector_std)) %>%
  distinct(Collector_std, Taxon_std) %>%
  count(Collector_std, name = "n_confirmed_from_source") %>%
  arrange(desc(n_confirmed_from_source), Collector_std)

historical_source_summary <- historical_source_rank %>%
  full_join(historical_source_confirmed_rank, by = "Collector_std") %>%
  mutate(
    n_historical_taxa = dplyr::coalesce(n_historical_taxa, 0L),
    n_confirmed_from_source = dplyr::coalesce(n_confirmed_from_source, 0L)
  ) %>%
  arrange(desc(n_historical_taxa), desc(n_confirmed_from_source), Collector_std)

top_rediscovered <- phylum_summary %>% filter(historical_rediscovered > 0) %>% arrange(desc(historical_rediscovered), desc(recent_rediscoveries), Phylum_label) %>% slice_head(n = TOP_N_GAINERS)
top_new <- phylum_summary %>% filter(new_since_project > 0) %>% arrange(desc(new_since_project), desc(recent_new_additions), Phylum_label) %>% slice_head(n = TOP_N_GAINERS)
top_attention <- phylum_summary %>% filter(historical_still_missing > 0) %>% arrange(desc(historical_still_missing), desc(still_missing_share), desc(historical_baseline), Phylum_label) %>% slice_head(n = TOP_N_ATTENTION)
top_recent <- phylum_summary %>% filter(recent_total_progress > 0) %>% arrange(desc(recent_total_progress), desc(recent_rediscoveries), desc(recent_new_additions), Phylum_label) %>% slice_head(n = TOP_N_GAINERS)
top_confirmers <- contemporary_contributor_summary %>% filter(n_confirmed_taxa > 0) %>% slice_head(n = TOP_N_CONTRIBUTORS)
top_new_contributors <- contemporary_contributor_summary %>% filter(n_new_taxa > 0) %>% slice_head(n = TOP_N_CONTRIBUTORS)
top_total_contributors <- contemporary_contributor_summary %>% filter(n_total_taxa > 0) %>% slice_head(n = TOP_N_CONTRIBUTORS)
top_historical_sources <- historical_source_summary %>% filter(n_historical_taxa > 0) %>% slice_head(n = TOP_N_HISTORICAL_SOURCES)
top_historical_sources_confirmed <- historical_source_summary %>% filter(n_confirmed_from_source > 0) %>% slice_head(n = TOP_N_HISTORICAL_SOURCES)

write_csv(
  phylum_summary %>%
    mutate(
      rediscovery_share_pct = round(100 * rediscovery_share, 1),
      still_missing_share_pct = round(100 * still_missing_share, 1)
    ) %>%
    select(
      Phylum = Phylum_label,
      Phylum_Latin = Phylum_std,
      total_known_now,
      historical_baseline,
      historical_rediscovered,
      historical_still_missing,
      new_since_project,
      recent_rediscoveries,
      recent_new_additions,
      recent_total_progress,
      rediscovery_share_pct,
      still_missing_share_pct
    ),
  file.path(OUTPUT_DIR, "Marine_animal_diversity_summary_by_phylum.csv")
)

write_csv(contemporary_contributor_summary, file.path(OUTPUT_DIR, "Marine_animal_contemporary_contributors_summary.csv"))
write_csv(historical_source_summary, file.path(OUTPUT_DIR, "Marine_animal_historical_sources_summary.csv"))

## ------------------------------------------------------------
## TEXT OUTPUT SUMMARY
## ------------------------------------------------------------

cat("
")
cat("============================================================
")
cat("MARINE ANIMAL DIVERSITY: HISTORICAL BASELINE AND PROJECT PROGRESS
")
cat("============================================================

")

cat(
  paste0(
    "There are ", n_fmt(overall$total_known_now),
    " species of marine animals known to occur in waters around Galiano Island. Of these, ",
    n_fmt(overall$historical_baseline),
    " reports are based on the historical baseline established by earlier collectors and reporters, while ",
    n_fmt(overall$new_since_project),
    " have been added new to the island life list since the project began.

"
  )
)

cat(
  paste0(
    "Of the ", n_fmt(overall$historical_baseline),
    " taxa known from historical sources, ",
    n_fmt(overall$historical_rediscovered), " (",
    pct_fmt(overall$rediscovery_share),
    ") have now been observed by community members, while ",
    n_fmt(overall$historical_still_missing), " (",
    pct_fmt(overall$still_missing_share),
    ") remain unobserved since this project began in 2015. Over the last ",
    RECENT_YEARS_BACK + 1,
    " years (", RECENT_YEAR_MIN, "–", RECENT_YEAR_MAX,
    "), our community has confirmed ",
    n_fmt(overall$recent_rediscoveries),
    " historical ", ifelse(overall$recent_rediscoveries == 1, "report", "reports"),
    " based on observations added to iNaturalist, and added ",
    n_fmt(overall$recent_new_additions),
    " new ", ifelse(overall$recent_new_additions == 1, "species", "species"),
    " to the list.

"
  )
)

if (nrow(top_rediscovered) > 0) {
  cat(
    paste0(
      "So far, the greatest progress toward documenting the historically reported fauna has been made in: ",
      paste0(
        paste0(
          top_rediscovered$Phylum_label,
          " (", top_rediscovered$historical_rediscovered,
          ifelse(top_rediscovered$historical_rediscovered == 1, " re-observed species", " re-observed species"),
          ")"
        ),
        collapse = ", "
      ),
      ". "
    )
  )
} else {
  cat("So far, no historically reported taxa have yet been re-observed. ")
}

if (nrow(top_new) > 0) {
  cat(
    paste0(
      "The most new additions to the list have come from ",
      paste0(
        paste0(top_new$Phylum_label, " (", top_new$new_since_project, " added)"),
        collapse = ", "
      ),
      ". "
    )
  )
}

if (nrow(top_recent) > 0) {
  cat(
    paste0(
      "Most recently, new reports have been made in ",
      paste0(
        paste0(top_recent$Phylum_label, " (", top_recent$recent_total_progress, ")"),
        collapse = ", "
      ),
      ".

"
    )
  )
} else {
  cat("

")
}

if (nrow(top_attention) > 0) {
  cat(
    paste0(
      "Looking back at the historical record, the biggest opportunities for confirming historical records lie in ",
      paste0(
        paste0(
          top_attention$Phylum_label,
          " (", top_attention$historical_still_missing,
          ' "species at large")'
        ),
        collapse = ", "
      ),
      ". Each of those unseen historical taxa is an invitation for observers, photographers, and naturalists to help track the local biodiversity with reference to the baseline record.

"
    )
  )
}

cat("============================================================
")
cat("MAJOR CONTEMPORARY CONTRIBUTORS
")
cat("============================================================

")

if (nrow(top_confirmers) > 0) {
  cat(
    paste0(
      "Leading contributors to confirmation of historical records are ",
      paste0(
        paste0(top_confirmers$Observer_std, " (", top_confirmers$n_confirmed_taxa, " confirmed)"),
        collapse = ", "
      ),
      ".

"
    )
  )
}

if (nrow(top_new_contributors) > 0) {
  cat(
    paste0(
      "Leading contributors of taxa added as new records are ",
      paste0(
        paste0(top_new_contributors$Observer_std, " (", top_new_contributors$n_new_taxa, " new)"),
        collapse = ", "
      ),
      ".

"
    )
  )
}

if (nrow(top_total_contributors) > 0) {
  cat(
    paste0(
      "Considering confirmed records and new additions together, the most active contributors are ",
      paste0(
        paste0(top_total_contributors$Observer_std, " (", top_total_contributors$n_total_taxa, " total)"),
        collapse = ", "
      ),
      ". "
    )
  )
}

if (nrow(top_historical_sources) > 0) {
  cat(
    paste0(
      "The most important historical sources, in terms of the number of taxa they contributed to the baseline record, are ",
      paste0(
        paste0(top_historical_sources$Collector_std, " (", top_historical_sources$n_historical_taxa, ")"),
        collapse = ", "
      ),
      ".

"
    )
  )
}

cat("Phylum-level summary table:
")
print(phylum_summary)

cat("
Contemporary contributor summary table:
")
print(contemporary_contributor_summary)

cat("
Historical source summary table:
")
print(historical_source_summary)

cat("
Saved file: ", file.path(OUTPUT_DIR, "Marine_animal_diversity_summary_by_phylum.csv"), "
", sep = "")
cat("Saved file: ", file.path(OUTPUT_DIR, "Marine_animal_contemporary_contributors_summary.csv"), "
", sep = "")
cat("Saved file: ", file.path(OUTPUT_DIR, "Marine_animal_historical_sources_summary.csv"), "
", sep = "")

## ------------------------------------------------------------
## EXPORT FULL NARRATIVE (BIG PICTURE + PER-PHYLUM STORIES)
## ------------------------------------------------------------

# Capture big-picture summary (reuse same logic as printed text)
big_picture_text <- paste0(
  "MARINE ANIMAL DIVERSITY: GALIANO ISLAND\n",
  "============================================================\n\n",
  
  "There are ", n_fmt(overall$total_known_now),
  " species of marine animals known to occur in waters around Galiano Island. Of these, ",
  n_fmt(overall$historical_baseline),
  " reports are based on the historical baseline established by earlier collectors and reporters, while ",
  n_fmt(overall$new_since_project),
  " have been added new to the island life list since the project began.\n\n",
  
  "Of the ", n_fmt(overall$historical_baseline),
  " taxa known from historical sources, ",
  n_fmt(overall$historical_rediscovered), " (",
  pct_fmt(overall$rediscovery_share),
  ") have now been observed by community members, while ",
  n_fmt(overall$historical_still_missing), " (",
  pct_fmt(overall$still_missing_share),
  ") remain unobserved since this project began in 2015. Over the last ",
  RECENT_YEARS_BACK + 1,
  " years (", RECENT_YEAR_MIN, "–", RECENT_YEAR_MAX,
  "), our community has confirmed ",
  n_fmt(overall$recent_rediscoveries),
  " historical ", ifelse(overall$recent_rediscoveries == 1, "report", "reports"),
  " and added ",
  n_fmt(overall$recent_new_additions),
  " new species to the list.\n\n"
)

# Contributor summary text
contributors_text <- paste0(
  "MAJOR CONTEMPORARY CONTRIBUTORS\n",
  "============================================================\n\n",
  
  if (nrow(top_confirmers) > 0)
    paste0(
      "Leading contributors to confirmation of historical records are ",
      paste0(top_confirmers$Observer_std, " (", top_confirmers$n_confirmed_taxa, " confirmed)", collapse = ", "),
      ".\n\n"
    ) else "",
  
  if (nrow(top_new_contributors) > 0)
    paste0(
      "Leading contributors of taxa added as new records are ",
      paste0(top_new_contributors$Observer_std, " (", top_new_contributors$n_new_taxa, " new)", collapse = ", "),
      ".\n\n"
    ) else "",
  
  if (nrow(top_total_contributors) > 0)
    paste0(
      "Considering confirmed records and new additions together, the most active contributors are ",
      paste0(top_total_contributors$Observer_std, " (", top_total_contributors$n_total_taxa, " total)", collapse = ", "),
      ".\n\n"
    ) else ""
)

# Historical sources text
historical_sources_text <- paste0(
  "IMPORTANT HISTORICAL SOURCES\n",
  "============================================================\n\n",
  
  if (nrow(top_historical_sources) > 0)
    paste0(
      "The most important historical sources are ",
      paste0(top_historical_sources$Collector_std, " (", top_historical_sources$n_historical_taxa, ")", collapse = ", "),
      ".\n\n"
    ) else ""
)

# Per-phylum story section
phylum_story_text <- marine_animal_story_examples %>%
  arrange(Phylum, Knowledge_category, Taxon) %>%
  group_by(Phylum) %>%
  summarise(
    block = paste0(
      "------------------------------------------------------------\n",
      unique(Phylum), "\n",
      "------------------------------------------------------------\n",
      paste(story_text, collapse = "\n"),
      "\n\n"
    ),
    .groups = "drop"
  ) %>%
  pull(block) %>%
  paste(collapse = "\n")

# Combine everything
full_story_text <- paste0(
  big_picture_text,
  "\n",
  contributors_text,
  "\n",
  historical_sources_text,
  "\n",
  "PER-PHYLUM STORIES\n",
  "============================================================\n\n",
  phylum_story_text
)

# Write to file
full_story_path <- file.path(OUTPUT_DIR, "Marine_animal_full_narrative.txt")
writeLines(full_story_text, full_story_path)

cat("\nSaved file: ", full_story_path, "\n", sep = "")
