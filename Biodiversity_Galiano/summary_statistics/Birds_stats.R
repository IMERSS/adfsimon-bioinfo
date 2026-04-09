#### R Script for visualizing vascular plant reporting
#### Historical reporters/collectors vs contemporary observers

# TO DO: Upstream summaries are using different fields than the rest of the data
# Likely because of different curation workflows for marine animals
# These are adjusted here for this script but should eventually be normalized upstream

# Set relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Load R packages

library(dplyr)
library(ggplot2)
library(gridExtra)
library(igraph)
library(lubridate)
library(readr)
library(stringr)
library(tidyverse)
library(viridis)
library(scales)
library(rstudioapi)

# Custom R function
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

## ------------------------------------------------------------
## USER CONTROLS
## ------------------------------------------------------------

# Controls how expanded the plotted network is.
# Larger values = more spread out
# Smaller values = more compact
EXPANSION_MULTIPLIER <- 1.2

# Plot window is based on a fixed reference expansion so that
# nodes remain visible while EXPANSION_MULTIPLIER still changes
# the apparent compactness/spread of the network.
REFERENCE_EXPANSION_FOR_WINDOW <- 1.00

# Extra whitespace around the plotting window
PLOT_WINDOW_PADDING <- 0.20

# Maximum number of contributors to keep in each human category
# Use Inf to keep all
MAX_HISTORICAL_CONTRIBUTORS <- 5
MAX_CONTEMPORARY_CONTRIBUTORS <- 48

# Maximum number of taxa to keep in the plotted network
# Taxa are ranked by total number of incident edges among retained people
# Use Inf to keep all
MAX_TAXA <- 192

# Fruchterman-Reingold layout iterations
LAYOUT_NITER <- 2000

# Print a quick summary of allowable ranges before plotting
PRINT_RANGE_CHECK <- TRUE

## ------------------------------------------------------------
## NEW RECORD YEAR FILTER CONTROLS
## ------------------------------------------------------------

# Toggle whether to filter which "new YYYY" records are shown
FILTER_NEW_RECORD_YEARS <- TRUE

# Inclusive year range used only when FILTER_NEW_RECORD_YEARS = TRUE
NEW_RECORD_YEAR_MIN <- 2015
NEW_RECORD_YEAR_MAX <- 2026

## ------------------------------------------------------------
## STYLE CONTROLS
## ------------------------------------------------------------

# Use a clean modern-looking default font family
BASE_FONT_FAMILY <- "sans"

# Title + subtitle styling
PLOT_TITLE <- "Biodiversity Galiano 2026: birds"
PLOT_SUBTITLE <- "Relational network of historical reporting and contemporary biodiversity observations"
TITLE_CEX <- 1.60
SUBTITLE_CEX <- 1
TITLE_FONT <- 2

# Background and text colors
BG_COLOR <- "black"
FG_COLOR <- "white"
SUBTLE_LINE_COLOR <- "gray70"

# Vertex label styling
PERSON_LABEL_CEX <- 0.72
LABEL_COLOR <- "white"

# Label jitter controls for person labels
USE_LABEL_JITTER <- TRUE
LABEL_JITTER_X <- 0.03
LABEL_JITTER_Y <- 0.03
LABEL_ONLY_PEOPLE <- TRUE

# Legend styling
LEGEND_TITLE_CEX <- 1.00
LEGEND_TEXT_CEX <- 0.88

## ------------------------------------------------------------
## Read in data
## ------------------------------------------------------------

BIRDS <- read.csv(
  "../review/Animalia/birds/summaries/Birds_summary_2026-04-05.csv",
  stringsAsFactors = FALSE
)

# Align summary field names

updated_names <- c(
  "Taxon","Taxon.Author","Subtaxon.Author","Common.Name","Kingdom","Phylum",
  "Subphylum","Superclass","Class","Subclass","Superorder","Order","Suborder",
  "Superfamily","Family","Subfamily","Tribe","Genus","Species","Hybrid",
  "Subspecies","Variety","Origin","Provincial.Status","National.Status",
  "Reporting.Status","Observation","Collected.Reported..y.m.d.",
  "Collector.Source","Collection.List","Accession.Number","GBIF.ID",
  "First.Observed","Observer","iNaturalist.Link","Notes","ID","Stats.Code"
)

names(BIRDS) <- updated_names

## ------------------------------------------------------------
## Clean key fields
## ------------------------------------------------------------

df <- BIRDS %>%
  mutate(
    Taxon = str_trim(as.character(Taxon)),
    Reporting.Status = str_to_lower(str_trim(as.character(Reporting.Status))),
    Collector.Source = na_if(str_trim(as.character(Collector.Source)), ""),
    Observer = na_if(str_trim(as.character(Observer)), ""),
    Collected.Reported..y.m.d. = na_if(str_trim(as.character(Collected.Reported..y.m.d.)), ""),
    First.Observed = na_if(str_trim(as.character(First.Observed)), "")
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

## ------------------------------------------------------------
## Optional filter for "new" records by year range
## ------------------------------------------------------------

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
## Assign dominant status per taxon
## Priority: confirmed > new > reported
## For taxa whose dominant status is "new", also store a year
## ------------------------------------------------------------

taxon_status <- df %>%
  group_by(Taxon) %>%
  summarise(
    has_confirmed = any(Reporting.Status.simple == "confirmed", na.rm = TRUE),
    has_new = any(Reporting.Status.simple == "new", na.rm = TRUE),
    has_reported = any(Reporting.Status.simple == "reported", na.rm = TRUE),
    new_year_min = suppressWarnings(min(new_year, na.rm = TRUE)),
    new_year_max = suppressWarnings(max(new_year, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    new_year_min = ifelse(is.infinite(new_year_min), NA, new_year_min),
    new_year_max = ifelse(is.infinite(new_year_max), NA, new_year_max),
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
  select(Taxon, taxon_status, taxon_year)

## ------------------------------------------------------------
## Build edges according to data structure
##
## reported  -> historical person to taxon only
## new       -> contemporary person to taxon only
## confirmed -> historical person to taxon AND contemporary person to taxon
## ------------------------------------------------------------

historical_edges <- df %>%
  filter(
    Reporting.Status.simple %in% c("reported", "confirmed"),
    !is.na(Collector.Source)
  ) %>%
  transmute(
    from = Collector.Source,
    to = Taxon,
    person_name = Collector.Source,
    person_class = "historical",
    edge_class = Reporting.Status.simple,
    weight = 1
  ) %>%
  group_by(from, to, person_name, person_class, edge_class) %>%
  summarise(weight = sum(weight), .groups = "drop")

contemporary_edges <- df %>%
  filter(
    Reporting.Status.simple %in% c("new", "confirmed"),
    !is.na(Observer)
  ) %>%
  transmute(
    from = Observer,
    to = Taxon,
    person_name = Observer,
    person_class = "contemporary",
    edge_class = Reporting.Status.simple,
    weight = 1
  ) %>%
  group_by(from, to, person_name, person_class, edge_class) %>%
  summarise(weight = sum(weight), .groups = "drop")

edges_all <- bind_rows(historical_edges, contemporary_edges)

## ------------------------------------------------------------
## Rank contributors within each human category
## ------------------------------------------------------------

historical_rank <- historical_edges %>%
  group_by(person_name) %>%
  summarise(
    total_links = sum(weight),
    n_taxa = n_distinct(to),
    .groups = "drop"
  ) %>%
  arrange(desc(total_links), desc(n_taxa), person_name)

contemporary_rank <- contemporary_edges %>%
  group_by(person_name) %>%
  summarise(
    total_links = sum(weight),
    n_taxa = n_distinct(to),
    .groups = "drop"
  ) %>%
  arrange(desc(total_links), desc(n_taxa), person_name)

## ------------------------------------------------------------
## Apply contributor filters
## ------------------------------------------------------------

top_historical <- if (is.infinite(MAX_HISTORICAL_CONTRIBUTORS)) {
  historical_rank$person_name
} else {
  historical_rank %>%
    slice_head(n = min(MAX_HISTORICAL_CONTRIBUTORS, nrow(historical_rank))) %>%
    pull(person_name)
}

top_contemporary <- if (is.infinite(MAX_CONTEMPORARY_CONTRIBUTORS)) {
  contemporary_rank$person_name
} else {
  contemporary_rank %>%
    slice_head(n = min(MAX_CONTEMPORARY_CONTRIBUTORS, nrow(contemporary_rank))) %>%
    pull(person_name)
}

edges_people_filtered <- edges_all %>%
  filter(
    (person_class == "historical" & person_name %in% top_historical) |
      (person_class == "contemporary" & person_name %in% top_contemporary)
  )

## ------------------------------------------------------------
## Rank taxa after contributor filtering
## ------------------------------------------------------------

taxon_rank <- edges_people_filtered %>%
  group_by(to) %>%
  summarise(
    total_links = sum(weight),
    n_people = n_distinct(from),
    .groups = "drop"
  ) %>%
  arrange(desc(total_links), desc(n_people), to)

top_taxa <- if (is.infinite(MAX_TAXA)) {
  taxon_rank$to
} else {
  taxon_rank %>%
    slice_head(n = min(MAX_TAXA, nrow(taxon_rank))) %>%
    pull(to)
}

edges_top <- edges_people_filtered %>%
  filter(to %in% top_taxa)

## ------------------------------------------------------------
## Secondary plot summary after filtering
## ------------------------------------------------------------

cat("Plotting with:\n")
cat("  Historical contributors retained:   ", length(unique(edges_top$from[edges_top$person_class == "historical"])), "\n")
cat("  Contemporary contributors retained: ", length(unique(edges_top$from[edges_top$person_class == "contemporary"])), "\n")
cat("  Taxa retained:                      ", length(unique(edges_top$to)), "\n")
cat("  Total edges retained:               ", nrow(edges_top), "\n\n")

## ------------------------------------------------------------
## Build node table
## ------------------------------------------------------------

historical_nodes <- data.frame(
  name = unique(edges_top$from[edges_top$person_class == "historical"]),
  node_class = "historical",
  stringsAsFactors = FALSE
)

contemporary_nodes <- data.frame(
  name = unique(edges_top$from[edges_top$person_class == "contemporary"]),
  node_class = "contemporary",
  stringsAsFactors = FALSE
)

taxon_nodes <- data.frame(
  name = unique(edges_top$to),
  node_class = "taxon",
  stringsAsFactors = FALSE
)

nodes_top <- bind_rows(historical_nodes, contemporary_nodes, taxon_nodes) %>%
  distinct(name, .keep_all = TRUE) %>%
  left_join(taxon_status, by = c("name" = "Taxon"))

## ------------------------------------------------------------
## Build graph
## ------------------------------------------------------------

g_top <- graph_from_data_frame(
  d = edges_top %>% select(from, to, weight, edge_class, person_class),
  vertices = nodes_top,
  directed = FALSE
)

## ------------------------------------------------------------
## Styling
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
    as.character(V(g_top)$taxon_year) %in% names(new_palette) ~
    new_palette[as.character(V(g_top)$taxon_year)],
  V(g_top)$node_class == "taxon" & V(g_top)$taxon_status == "new" ~ "#D55E00",
  TRUE ~ "gray80"
)

deg_top <- degree(g_top)

V(g_top)$size <- case_when(
  V(g_top)$node_class == "historical" ~ rescale(deg_top, to = c(2.0, 3.0)),
  V(g_top)$node_class == "contemporary" ~ rescale(deg_top, to = c(2.0, 3.0)),
  V(g_top)$node_class == "taxon" ~ rescale(deg_top, to = c(1.0, 1.8))
)

V(g_top)$label <- ifelse(
  V(g_top)$node_class %in% c("historical", "contemporary"),
  V(g_top)$name,
  NA
)

edge_colors <- c(
  "reported" = "gray60",
  "confirmed" = "#009E73",
  "new" = "#D55E00"
)

E(g_top)$color <- edge_colors[E(g_top)$edge_class]
E(g_top)$color[is.na(E(g_top)$color)] <- "gray80"
E(g_top)$width <- rescale(E(g_top)$weight, to = c(0.5, 2.2))

## ------------------------------------------------------------
## Layout with working expansion control
## ------------------------------------------------------------

set.seed(123)

lay_base <- layout_with_fr(
  g_top,
  niter = LAYOUT_NITER
)

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

## ------------------------------------------------------------
## Jitter label positions
## ------------------------------------------------------------

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

## ------------------------------------------------------------
## Plot with separate legend panel
## ------------------------------------------------------------

old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)

par(
  family = BASE_FONT_FAMILY,
  bg = BG_COLOR,
  fg = FG_COLOR,
  col = FG_COLOR
)

layout(matrix(c(1, 2), nrow = 1), widths = c(5.2, 1.9))

# Main network panel
par(
  mar = c(1.2, 1.2, 3.2, 0.8),
  bg = BG_COLOR,
  fg = FG_COLOR,
  col = FG_COLOR
)

# Draw edges only
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

# Draw nodes manually so circles always appear
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

# Taxa first (circles)
tax_idx <- which(node_df$node_class == "taxon")
points(
  x = node_df$x[tax_idx],
  y = node_df$y[tax_idx],
  pch = 16,
  col = node_df$color[tax_idx],
  cex = node_df$size[tax_idx]
)

# Historical / contemporary people on top (squares)
hist_idx <- which(node_df$node_class == "historical")
cont_idx <- which(node_df$node_class == "contemporary")

points(
  x = node_df$x[hist_idx],
  y = node_df$y[hist_idx],
  pch = 15,
  col = node_df$color[hist_idx],
  cex = node_df$size[hist_idx]
)

points(
  x = node_df$x[cont_idx],
  y = node_df$y[cont_idx],
  pch = 15,
  col = node_df$color[cont_idx],
  cex = node_df$size[cont_idx]
)

title(
  main = PLOT_TITLE,
  cex.main = TITLE_CEX,
  font.main = TITLE_FONT,
  family = BASE_FONT_FAMILY,
  col.main = FG_COLOR,
  line = 1
)

mtext(
  text = PLOT_SUBTITLE,
  side = 3,
  line = -0.2,
  cex = SUBTITLE_CEX,
  col = FG_COLOR,
  family = BASE_FONT_FAMILY,
  font = 1
)

label_idx <- which(!is.na(vertex_labels) & vertex_labels != "")

text(
  x = label_coords[label_idx, 1],
  y = label_coords[label_idx, 2],
  labels = vertex_labels[label_idx],
  cex = PERSON_LABEL_CEX,
  col = LABEL_COLOR,
  family = BASE_FONT_FAMILY,
  xpd = TRUE
)

# Legend panel
par(
  mar = c(1.2, 0.2, 3.2, 1.2),
  bg = BG_COLOR,
  fg = FG_COLOR,
  col = FG_COLOR
)

plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(
  x = 0.08, y = 0.96,
  labels = "Legend",
  adj = c(0, 1),
  cex = LEGEND_TITLE_CEX,
  font = 2,
  family = BASE_FONT_FAMILY,
  col = FG_COLOR
)

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
  col = c(
    "#CC79A7",
    "#E69F00",
    "gray60",
    "#009E73",
    "#D55E00",
    "gray60",
    "#009E73",
    "#D55E00"
  ),
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
  
  text(
    x = x_left, y = y0 - 0.07,
    labels = min(new_years_present),
    adj = c(0, 1),
    cex = 0.78,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
  text(
    x = x_right, y = y0 - 0.07,
    labels = max(new_years_present),
    adj = c(1, 1),
    cex = 0.78,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
  text(
    x = x_left, y = y0 + 0.07,
    labels = "New taxa year gradient",
    adj = c(0, 0),
    cex = 0.82,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
} else if (length(new_years_present) == 1) {
  text(
    x = 0.10, y = 0.16,
    labels = paste0("New taxa year: ", new_years_present),
    adj = c(0, 0),
    cex = 0.82,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
}

## ------------------------------------------------------------
## Optional diagnostic summaries
## ------------------------------------------------------------

cat("\nTop historical contributors:\n")
print(head(historical_rank, 20))

cat("\nTop contemporary contributors:\n")
print(head(contemporary_rank, 20))

cat("\nTop taxa after contributor filtering:\n")
print(head(taxon_rank, 20))

cat("\nEdge counts by class in plotted network:\n")
print(table(edges_top$edge_class))

cat("\nNode counts by class in plotted network:\n")
print(table(V(g_top)$node_class))

cat("\nTaxon dominant status counts in plotted network:\n")
print(table(V(g_top)$taxon_status, useNA = "ifany"))

cat("\nYears represented among new taxa in plotted network:\n")
print(sort(unique(na.omit(V(g_top)$taxon_year))))

## ------------------------------------------------------------
## Quick range check
## ------------------------------------------------------------

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
    cat(
      "  Available 'new' record years: ",
      paste(range(df$new_year[df$Reporting.Status.simple == "new"], na.rm = TRUE), collapse = " to "),
      "\n",
      sep = ""
    )
  }
  
  if (FILTER_NEW_RECORD_YEARS) {
    cat(
      "  Active new-year filter: ",
      NEW_RECORD_YEAR_MIN, " to ", NEW_RECORD_YEAR_MAX, "\n",
      sep = ""
    )
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
## Top 10 contemporary contributors (quick view)
## ------------------------------------------------------------

top11_contemporary <- contemporary_rank %>%
  slice_head(n = min(11, nrow(contemporary_rank)))

cat("\nTop 11 contemporary contributors:\n")
print(top11_contemporary)

## ------------------------------------------------------------
## BIRD FAMILY SUMMARY FOR CUMULATIVE KNOWLEDGE BARPLOT
## ------------------------------------------------------------

if (!"Family" %in% names(df)) {
  stop("The data do not contain a column named 'Family'.", call. = FALSE)
}

# Clean order field
df <- df %>%
  mutate(
    Family = str_trim(as.character(Family)),
    Family = na_if(Family, "")
  )

# Print available order names
family_names_present <- df %>%
  filter(!is.na(Family)) %>%
  distinct(Family) %>%
  arrange(Family) %>%
  pull(Family)

cat("\nFamilies present in the dataset:\n")
print(family_names_present)

## ============================================================
## BIRD DIVERSITY: CUMULATIVE KNOWLEDGE BY FAMILY
## ============================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(viridis)

## ------------------------------------------------------------
## USER SETTINGS
## ------------------------------------------------------------

OUTPUT_DIR <- "outputs"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

BASE_FONT_FAMILY <- "sans"
BG_COLOR <- "black"
FG_COLOR <- "white"

RECENT_YEARS_BACK <- 1   # gives a 2-year window: max year and previous year
TOP_N_GAINERS <- 5
TOP_N_ATTENTION <- 5

set.seed(123)

## ------------------------------------------------------------
## CHECK REQUIRED COLUMNS
## ------------------------------------------------------------

required_core <- c("Family", "Taxon")
missing_core <- setdiff(required_core, names(df))
if (length(missing_core) > 0) {
  stop(
    paste0(
      "The dataset is missing required column(s): ",
      paste(missing_core, collapse = ", ")
    ),
    call. = FALSE
  )
}

## ------------------------------------------------------------
## HELPER FUNCTIONS
## ------------------------------------------------------------

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

## ------------------------------------------------------------
## BIRD FAMILY FRIENDLY LABELS
## ------------------------------------------------------------

family_label_map <- c(
  "Accipitridae"      = "Hawks, eagles & allies (Accipitridae)",
  "Aegithalidae"      = "Long-tailed tits (Aegithalidae)",
  "Alaudidae"         = "Larks (Alaudidae)",
  "Alcedinidae"       = "Kingfishers (Alcedinidae)",
  "Alcidae"           = "Auks, murres & puffins (Alcidae)",
  "Anatidae"          = "Ducks, geese & swans (Anatidae)",
  "Apodidae"          = "Swifts (Apodidae)",
  "Ardeidae"          = "Herons, egrets & bitterns (Ardeidae)",
  "Bombycillidae"     = "Waxwings (Bombycillidae)",
  "Caprimulgidae"     = "Nightjars (Caprimulgidae)",
  "Cardinalidae"      = "Cardinals, grosbeaks & buntings (Cardinalidae)",
  "Cathartidae"       = "New World vultures (Cathartidae)",
  "Certhiidae"        = "Treecreepers (Certhiidae)",
  "Charadriidae"      = "Plovers (Charadriidae)",
  "Cinclidae"         = "Dippers (Cinclidae)",
  "Columbidae"        = "Pigeons & doves (Columbidae)",
  "Corvidae"          = "Crows, ravens & jays (Corvidae)",
  "Falconidae"        = "Falcons (Falconidae)",
  "Fringillidae"      = "Finches (Fringillidae)",
  "Gaviidae"          = "Loons (Gaviidae)",
  "Gruidae"           = "Cranes (Gruidae)",
  "Haematopodidae"    = "Oystercatchers (Haematopodidae)",
  "Hirundinidae"      = "Swallows & martins (Hirundinidae)",
  "Icteridae"         = "Blackbirds, orioles & meadowlarks (Icteridae)",
  "Laniidae"          = "Shrikes (Laniidae)",
  "Laridae"           = "Gulls, terns & skimmers (Laridae)",
  "Motacillidae"      = "Wagtails & pipits (Motacillidae)",
  "Pandionidae"       = "Osprey (Pandionidae)",
  "Paridae"           = "Chickadees & tits (Paridae)",
  "Parulidae"         = "Wood-warblers (Parulidae)",
  "Passerellidae"     = "New World sparrows (Passerellidae)",
  "Passeridae"        = "Old World sparrows (Passeridae)",
  "Pelecanidae"       = "Pelicans (Pelecanidae)",
  "Phalacrocoracidae" = "Cormorants (Phalacrocoracidae)",
  "Phasianidae"       = "Pheasants, grouse & allies (Phasianidae)",
  "Picidae"           = "Woodpeckers (Picidae)",
  "Podicipedidae"     = "Grebes (Podicipedidae)",
  "Rallidae"          = "Rails, gallinules & coots (Rallidae)",
  "Regulidae"         = "Kinglets (Regulidae)",
  "Scolopacidae"      = "Sandpipers & allies (Scolopacidae)",
  "Sittidae"          = "Nuthatches (Sittidae)",
  "Strigidae"         = "Typical owls (Strigidae)",
  "Sturnidae"         = "Starlings (Sturnidae)",
  "Trochilidae"       = "Hummingbirds (Trochilidae)",
  "Troglodytidae"     = "Wrens (Troglodytidae)",
  "Turdidae"          = "Thrushes (Turdidae)",
  "Tyrannidae"        = "Tyrant flycatchers (Tyrannidae)",
  "Tytonidae"         = "Barn owls (Tytonidae)",
  "Vireonidae"        = "Vireos (Vireonidae)"
)

get_family_label <- function(x) {
  dplyr::recode(x, !!!family_label_map, .default = x)
}

choose_group_phrase <- function(family_label, family_latin = NA_character_) {
  if (!is.na(family_label) && nzchar(family_label)) {
    out <- stringr::str_remove(family_label, "\\s*\\([^\\)]+\\)")
    out <- stringr::str_trim(out)
    return(tolower(out))
  }
  if (!is.na(family_latin) && nzchar(family_latin)) {
    return(family_latin)
  }
  "birds"
}

## ------------------------------------------------------------
## DETECT OPTIONAL COLUMNS
## ------------------------------------------------------------

col_family         <- first_existing_col(df, c("Family"))
col_taxon          <- first_existing_col(df, c("Taxon"))
col_status_raw     <- first_existing_col(df, c("Reporting.Status"))
col_status_simple  <- first_existing_col(df, c("Reporting.Status.simple"))
col_collector      <- first_existing_col(df, c("Collector.Source"))
col_observer       <- first_existing_col(df, c("Observer"))
col_hist_date      <- first_existing_col(df, c("Collected.Reported..y.m.d."))
col_first_observed <- first_existing_col(df, c("First.Observed"))
col_inat_link      <- first_existing_col(df, c("iNaturalist.Link"))
col_obs_id         <- first_existing_col(df, c("ID"))
col_new_year       <- first_existing_col(df, c("new_year"))
col_collection     <- first_existing_col(df, c("Collection.List"))
col_accession      <- first_existing_col(df, c("Accession.Number"))

required_cols <- c(col_family, col_taxon, col_status_raw)
if (any(is.na(required_cols))) {
  stop(
    "Missing one or more required columns: Family, Taxon, and/or Reporting.Status.",
    call. = FALSE
  )
}

## ------------------------------------------------------------
## NORMALIZE INPUT
## ------------------------------------------------------------

dat <- df %>%
  mutate(
    Family_std           = pull_or_na(., col_family),
    Taxon_std            = pull_or_na(., col_taxon),
    Reporting_raw_std    = pull_or_na(., col_status_raw),
    Reporting_simple_std = pull_or_na(., col_status_simple),
    Collector_std        = pull_or_na(., col_collector),
    Observer_std         = pull_or_na(., col_observer),
    Historical_date_std  = pull_or_na(., col_hist_date),
    First_observed_std   = pull_or_na(., col_first_observed),
    iNat_URL_raw_std     = pull_or_na(., col_inat_link),
    iNat_ID_raw_std      = pull_or_na(., col_obs_id),
    Collection_std       = pull_or_na(., col_collection),
    Accession_std        = pull_or_na(., col_accession),
    new_year_std         = suppressWarnings(as.integer(pull_or_na(., col_new_year)))
  ) %>%
  mutate(
    Family_std           = na_if(str_trim(Family_std), ""),
    Taxon_std            = na_if(str_trim(Taxon_std), ""),
    Reporting_raw_std    = str_to_lower(na_if(str_trim(Reporting_raw_std), "")),
    Reporting_simple_std = str_to_lower(na_if(str_trim(Reporting_simple_std), "")),
    Collector_std        = na_if(str_trim(Collector_std), ""),
    Observer_std         = na_if(str_trim(Observer_std), ""),
    Historical_date_std  = na_if(str_trim(Historical_date_std), ""),
    First_observed_std   = na_if(str_trim(First_observed_std), ""),
    iNat_URL_raw_std     = na_if(str_trim(iNat_URL_raw_std), ""),
    iNat_ID_raw_std      = na_if(str_trim(iNat_ID_raw_std), ""),
    Collection_std       = na_if(str_trim(Collection_std), ""),
    Accession_std        = na_if(str_trim(Accession_std), "")
  ) %>%
  mutate(
    status_std = case_when(
      !is.na(Reporting_simple_std) ~ Reporting_simple_std,
      Reporting_raw_std == "reported" ~ "reported",
      Reporting_raw_std == "confirmed" ~ "confirmed",
      str_detect(Reporting_raw_std, "^new\\s+[0-9]{4}$") ~ "new",
      TRUE ~ NA_character_
    ),
    historical_year = extract_year(Historical_date_std),
    observed_year = extract_year(First_observed_std),
    new_year_std = case_when(
      !is.na(new_year_std) ~ new_year_std,
      str_detect(Reporting_raw_std, "^new\\s+[0-9]{4}$") ~
        suppressWarnings(as.integer(str_extract(Reporting_raw_std, "[0-9]{4}"))),
      TRUE ~ NA_integer_
    ),
    Family_label = get_family_label(Family_std)
  ) %>%
  filter(
    !is.na(Family_std),
    !is.na(Taxon_std),
    !is.na(status_std)
  )

if (nrow(dat) == 0) {
  stop("No usable records found after normalization.", call. = FALSE)
}

## ------------------------------------------------------------
## DEFINE RECENT WINDOW
## ------------------------------------------------------------

all_recent_years <- c(dat$new_year_std, dat$observed_year)
all_recent_years <- all_recent_years[!is.na(all_recent_years)]

if (length(all_recent_years) > 0) {
  RECENT_YEAR_MAX <- max(all_recent_years, na.rm = TRUE)
} else {
  RECENT_YEAR_MAX <- as.integer(format(Sys.Date(), "%Y"))
}
RECENT_YEAR_MIN <- RECENT_YEAR_MAX - RECENT_YEARS_BACK

## ------------------------------------------------------------
## COLLAPSE TO ONE STATUS PER TAXON
## Priority: confirmed > new > reported
## ------------------------------------------------------------

taxon_family_status <- dat %>%
  group_by(Family_std, Taxon_std) %>%
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
  filter(!is.na(knowledge_category)) %>%
  mutate(
    Family_label = get_family_label(Family_std)
  )

## ------------------------------------------------------------
## CUMULATIVE KNOWLEDGE SUMMARY BY FAMILY
## ------------------------------------------------------------

family_diversity_summary <- taxon_family_status %>%
  count(Family_std, Family_label, knowledge_category, name = "n_taxa") %>%
  group_by(Family_std, Family_label) %>%
  mutate(total_taxa = sum(n_taxa)) %>%
  ungroup()

family_order <- family_diversity_summary %>%
  distinct(Family_std, Family_label, total_taxa) %>%
  arrange(desc(total_taxa), Family_std) %>%
  pull(Family_std)

family_diversity_summary$Family_std <- factor(
  family_diversity_summary$Family_std,
  levels = family_order
)

family_diversity_summary$knowledge_category <- factor(
  family_diversity_summary$knowledge_category,
  levels = c(
    "Unobserved historical records",
    "Observed historical reports",
    "New records"
  )
)

family_label_levels <- family_diversity_summary %>%
  distinct(Family_std, Family_label) %>%
  mutate(Family_std = factor(Family_std, levels = family_order)) %>%
  arrange(Family_std) %>%
  pull(Family_label)

family_diversity_summary$Family_label <- factor(
  family_diversity_summary$Family_label,
  levels = family_label_levels
)

cat("\nCumulative knowledge summary by bird family:\n")
print(family_diversity_summary, n = nrow(family_diversity_summary))

## ------------------------------------------------------------
## BARPLOT
## ------------------------------------------------------------

vir_cols <- viridis::viridis(3, option = "D", direction = 1)
names(vir_cols) <- c(
  "Unobserved historical records",
  "Observed historical reports",
  "New records"
)

p_cumulative_knowledge <- ggplot(
  family_diversity_summary,
  aes(x = Family_label, y = n_taxa, fill = knowledge_category)
) +
  geom_col(width = 0.82, color = NA) +
  scale_fill_manual(values = vir_cols) +
  scale_x_discrete(expand = expansion(mult = c(0.10, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Biodiversity Galiano 2026: cumulative bird knowledge by family",
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
    axis.text.x = element_text(
      angle = 40,
      hjust = 1,
      vjust = 1,
      color = FG_COLOR,
      size = 10
    ),
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
## DETERMINE RECENT NEW-RECORD WINDOW
## ------------------------------------------------------------

if ("new_year" %in% names(df) && any(!is.na(df$new_year))) {
  RECENT_NEW_YEAR_MAX <- max(df$new_year, na.rm = TRUE)
} else {
  tmp_new_years <- suppressWarnings(as.integer(stringr::str_extract(
    as.character(df[[col_status_raw]]),
    "(?<=new\\s)\\d{4}"
  )))
  if (any(!is.na(tmp_new_years))) {
    RECENT_NEW_YEAR_MAX <- max(tmp_new_years, na.rm = TRUE)
  } else {
    RECENT_NEW_YEAR_MAX <- RECENT_YEAR_MAX
  }
}
RECENT_NEW_YEAR_MIN <- RECENT_NEW_YEAR_MAX - 1

## ------------------------------------------------------------
## RANDOM EXAMPLE OBSERVATIONS TABLE BY FAMILY × CATEGORY
## ------------------------------------------------------------

example_pool <- dat %>%
  mutate(
    knowledge_category = case_when(
      status_std == "reported" ~ "Remaining historical report",
      status_std == "confirmed" ~ "Confirmed record",
      status_std == "new" ~ "New record",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(knowledge_category)) %>%
  mutate(
    iNat_link = case_when(
      str_detect(iNat_URL_raw_std, "^https?://") ~ normalize_inat_link(iNat_URL_raw_std),
      str_detect(iNat_URL_raw_std, "^iNat:[0-9]+$") ~ normalize_inat_link(iNat_URL_raw_std),
      !is.na(iNat_ID_raw_std) & str_detect(iNat_ID_raw_std, "^[0-9]+$") ~
        paste0("https://www.inaturalist.org/observations/", iNat_ID_raw_std),
      TRUE ~ NA_character_
    ),
    iNat_ID = case_when(
      !is.na(iNat_link) ~ extract_inat_id(iNat_link),
      !is.na(iNat_ID_raw_std) & str_detect(iNat_ID_raw_std, "^[0-9]+$") ~ iNat_ID_raw_std,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    knowledge_category != "New record" |
      (!is.na(new_year_std) &
         new_year_std >= RECENT_NEW_YEAR_MIN &
         new_year_std <= RECENT_NEW_YEAR_MAX)
  )

example_table <- example_pool %>%
  group_by(Family_std, Family_label, knowledge_category) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(
    category_sort = case_when(
      knowledge_category == "Remaining historical report" ~ 1L,
      knowledge_category == "Confirmed record" ~ 2L,
      knowledge_category == "New record" ~ 3L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(Family_label, category_sort, Taxon_std) %>%
  select(
    Family = Family_label,
    Family_Latin = Family_std,
    Knowledge_category = knowledge_category,
    Taxon = Taxon_std,
    Collector_Source = Collector_std,
    Observer = Observer_std,
    Historical_date = Historical_date_std,
    First_observed = First_observed_std,
    New_record_year = new_year_std,
    iNat_ID,
    iNat_link
  )

cat("\nRandom example observations by family × knowledge category:\n")
cat(
  "New record examples restricted to years ",
  RECENT_NEW_YEAR_MIN, "-", RECENT_NEW_YEAR_MAX, "\n",
  sep = ""
)
print(example_table, n = nrow(example_table))

write.csv(
  example_table,
  file.path(OUTPUT_DIR, "Birds_random_example_observations_by_family_recent_new_records.csv"),
  row.names = FALSE
)

cat(
  "\nSaved file: ",
  file.path(OUTPUT_DIR, "Birds_random_example_observations_by_family_recent_new_records.csv"),
  "\n",
  sep = ""
)

## ------------------------------------------------------------
## RANDOM STORY EXAMPLES BY FAMILY + STORY TEXT
## ------------------------------------------------------------

example_pool_story <- dat %>%
  mutate(
    Knowledge_category = case_when(
      status_std == "reported" ~ "Remaining historical report",
      status_std == "confirmed" ~ "Confirmed record",
      status_std == "new" ~ "New record",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Knowledge_category)) %>%
  filter(
    Knowledge_category != "New record" |
      (!is.na(new_year_std) &
         new_year_std >= RECENT_NEW_YEAR_MIN &
         new_year_std <= RECENT_NEW_YEAR_MAX)
  ) %>%
  mutate(
    iNat_link = normalize_inat_link(iNat_URL_raw_std),
    iNat_ID = extract_inat_id(iNat_link),
    iNat_link = dplyr::if_else(
      Knowledge_category == "Remaining historical report",
      NA_character_,
      iNat_link
    ),
    iNat_ID = dplyr::if_else(
      Knowledge_category == "Remaining historical report",
      NA_character_,
      iNat_ID
    ),
    Historical_metadata = format_historical_metadata(Collection_std, Accession_std)
  )

bird_story_examples <- example_pool_story %>%
  group_by(Family_std, Family_label, Knowledge_category) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(
    category_sort = case_when(
      Knowledge_category == "Remaining historical report" ~ 1L,
      Knowledge_category == "Confirmed record" ~ 2L,
      Knowledge_category == "New record" ~ 3L,
      TRUE ~ 99L
    ),
    group_phrase = mapply(
      choose_group_phrase,
      Family_label,
      Family_std,
      USE.NAMES = FALSE
    ),
    story_text = case_when(
      
      Knowledge_category == "Remaining historical report" ~
        case_when(
          !is.na(historical_year) & !is.na(Collector_std) ~
            paste0(
              "In ", historical_year, ", ",
              Collector_std, Historical_metadata,
              " reported this taxon of ",
              group_phrase,
              ", which has not been seen since."
            ),
          !is.na(historical_year) ~
            paste0(
              "In ", historical_year,
              ", this taxon of ",
              group_phrase,
              " was historically reported", Historical_metadata,
              ", but it has not been seen since."
            ),
          !is.na(Collector_std) ~
            paste0(
              Collector_std, Historical_metadata,
              " reported this taxon of ",
              group_phrase,
              ", which has not been seen since."
            ),
          TRUE ~
            paste0(
              "This taxon of ",
              group_phrase,
              " was historically reported", Historical_metadata,
              ", but has not been seen since."
            )
        ),
      
      Knowledge_category == "Confirmed record" ~
        case_when(
          !is.na(historical_year) & !is.na(Collector_std) &
            !is.na(observed_year) & !is.na(Observer_std) ~
            paste0(
              "In ", historical_year, ", ",
              Collector_std, Historical_metadata,
              " reported this taxon of ",
              group_phrase,
              ", which was later confirmed by ",
              Observer_std,
              " in ", observed_year, "."
            ),
          !is.na(historical_year) & !is.na(Collector_std) & !is.na(Observer_std) ~
            paste0(
              "In ", historical_year, ", ",
              Collector_std, Historical_metadata,
              " reported this taxon of ",
              group_phrase,
              ", which was later confirmed by ",
              Observer_std, "."
            ),
          !is.na(historical_year) & !is.na(observed_year) ~
            paste0(
              "In ", historical_year,
              ", this taxon of ",
              group_phrase,
              " was historically reported", Historical_metadata,
              " and later confirmed in ",
              observed_year, "."
            ),
          !is.na(Observer_std) & !is.na(observed_year) ~
            paste0(
              "This taxon of ",
              group_phrase,
              " was later confirmed by ",
              Observer_std,
              " in ", observed_year, "."
            ),
          TRUE ~
            paste0(
              "This taxon of ",
              group_phrase,
              " was historically reported", Historical_metadata,
              " and later confirmed."
            )
        ),
      
      Knowledge_category == "New record" ~
        case_when(
          !is.na(new_year_std) & !is.na(Observer_std) ~
            paste0(
              "In ", new_year_std, ", ",
              Observer_std,
              " reported this taxon of ",
              group_phrase,
              " as new for Galiano Island."
            ),
          !is.na(new_year_std) ~
            paste0(
              "In ", new_year_std,
              ", this taxon of ",
              group_phrase,
              " was reported as new for Galiano Island."
            ),
          !is.na(Observer_std) ~
            paste0(
              Observer_std,
              " reported this taxon of ",
              group_phrase,
              " as new for Galiano Island."
            ),
          TRUE ~
            paste0(
              "This taxon of ",
              group_phrase,
              " was reported as new for Galiano Island."
            )
        ),
      
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(Family_label, category_sort, Taxon_std) %>%
  select(
    Family = Family_label,
    Family_Latin = Family_std,
    Knowledge_category,
    Taxon = Taxon_std,
    Collector_Source = Collector_std,
    Observer = Observer_std,
    Collection_List = Collection_std,
    Accession_Number = Accession_std,
    Historical_metadata,
    Historical_date = Historical_date_std,
    First_observed = First_observed_std,
    New_record_year = new_year_std,
    iNat_ID,
    iNat_link,
    story_text
  )

print(bird_story_examples, n = nrow(bird_story_examples))

write.csv(
  bird_story_examples,
  file.path(OUTPUT_DIR, "Birds_reports_by_family.csv"),
  row.names = FALSE
)

cat(
  "\nSaved file: ",
  file.path(OUTPUT_DIR, "Birds_reports_by_family.csv"),
  "\n",
  sep = ""
)

## ------------------------------------------------------------
## BROAD-BRUSH DIVERSITY SUMMARY BY FAMILY
## ------------------------------------------------------------

status_rank <- c("reported" = 1L, "new" = 2L, "confirmed" = 3L)

taxon_level <- dat %>%
  mutate(
    status_rank = unname(status_rank[status_std]),
    contribution_year = case_when(
      status_std == "new" ~ new_year_std,
      status_std == "confirmed" ~ observed_year,
      TRUE ~ NA_integer_
    )
  ) %>%
  arrange(Family_label, Taxon_std, desc(status_rank), desc(contribution_year)) %>%
  group_by(Family_std, Family_label, Taxon_std) %>%
  slice(1) %>%
  ungroup()

family_summary <- taxon_level %>%
  group_by(Family_std, Family_label) %>%
  summarise(
    total_taxa = n_distinct(Taxon_std),
    remaining_historical = sum(status_std == "reported", na.rm = TRUE),
    confirmed_taxa = sum(status_std == "confirmed", na.rm = TRUE),
    new_taxa = sum(status_std == "new", na.rm = TRUE),
    resolved_taxa = confirmed_taxa + new_taxa,
    recent_confirmed = sum(
      status_std == "confirmed" &
        !is.na(contribution_year) &
        contribution_year >= RECENT_YEAR_MIN &
        contribution_year <= RECENT_YEAR_MAX,
      na.rm = TRUE
    ),
    recent_new = sum(
      status_std == "new" &
        !is.na(contribution_year) &
        contribution_year >= RECENT_YEAR_MIN &
        contribution_year <= RECENT_YEAR_MAX,
      na.rm = TRUE
    ),
    recent_total = recent_confirmed + recent_new,
    unresolved_share = ifelse(total_taxa > 0, remaining_historical / total_taxa, NA_real_),
    resolved_share = ifelse(total_taxa > 0, resolved_taxa / total_taxa, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(desc(total_taxa), Family_label)

overall <- taxon_level %>%
  summarise(
    total_taxa = n_distinct(Taxon_std),
    remaining_historical = sum(status_std == "reported", na.rm = TRUE),
    confirmed_taxa = sum(status_std == "confirmed", na.rm = TRUE),
    new_taxa = sum(status_std == "new", na.rm = TRUE),
    resolved_taxa = confirmed_taxa + new_taxa,
    recent_confirmed = sum(
      status_std == "confirmed" &
        !is.na(contribution_year) &
        contribution_year >= RECENT_YEAR_MIN &
        contribution_year <= RECENT_YEAR_MAX,
      na.rm = TRUE
    ),
    recent_new = sum(
      status_std == "new" &
        !is.na(contribution_year) &
        contribution_year >= RECENT_YEAR_MIN &
        contribution_year <= RECENT_YEAR_MAX,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    recent_total = recent_confirmed + recent_new
  )

top_confirmed <- family_summary %>%
  filter(confirmed_taxa > 0) %>%
  arrange(desc(confirmed_taxa), desc(recent_confirmed), Family_label) %>%
  slice_head(n = TOP_N_GAINERS)

top_new <- family_summary %>%
  filter(new_taxa > 0) %>%
  arrange(desc(new_taxa), desc(recent_new), Family_label) %>%
  slice_head(n = TOP_N_GAINERS)

top_recent <- family_summary %>%
  filter(recent_total > 0) %>%
  arrange(desc(recent_total), desc(recent_new), desc(recent_confirmed), Family_label) %>%
  slice_head(n = TOP_N_GAINERS)

top_attention <- family_summary %>%
  filter(remaining_historical > 0) %>%
  arrange(desc(remaining_historical), desc(unresolved_share), desc(total_taxa), Family_label) %>%
  slice_head(n = TOP_N_ATTENTION)

family_summary_out <- family_summary %>%
  mutate(
    resolved_share_pct = round(100 * resolved_share, 1),
    unresolved_share_pct = round(100 * unresolved_share, 1)
  ) %>%
  select(
    Family = Family_label,
    Family_Latin = Family_std,
    total_taxa,
    remaining_historical,
    confirmed_taxa,
    new_taxa,
    resolved_taxa,
    recent_confirmed,
    recent_new,
    recent_total,
    resolved_share_pct,
    unresolved_share_pct
  )

write_csv(
  family_summary_out,
  file.path(OUTPUT_DIR, "Bird_diversity_summary_by_family.csv")
)

## ------------------------------------------------------------
## NARRATIVE OUTPUT
## ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("BIRD DIVERSITY: BROAD-BRUSH SUMMARY BY FAMILY\n")
cat("============================================================\n\n")

cat(
  paste0(
    "This synthesis is based on ", n_fmt(overall$total_taxa),
    " taxa currently represented in the dataset, spanning historical reports, ",
    "subsequently confirmed records, and taxa reported as new for Galiano Island.\n\n"
  )
)

cat(
  paste0(
    "At present, ", n_fmt(overall$resolved_taxa), " taxa (",
    pct_fmt(overall$resolved_taxa / overall$total_taxa),
    ") have moved beyond the status of historical report: ",
    n_fmt(overall$confirmed_taxa), " have been confirmed from historical reporting, and ",
    n_fmt(overall$new_taxa), " represent additions reported as new for the island. ",
    "A further ", n_fmt(overall$remaining_historical), " taxa (",
    pct_fmt(overall$remaining_historical / overall$total_taxa),
    ") remain known only from historical reporting and still await confirmation.\n\n"
  )
)

if (overall$recent_total > 0) {
  cat(
    paste0(
      "Recent work has made a substantial contribution. In the most recent ",
      RECENT_YEARS_BACK + 1, "-year window represented here (",
      RECENT_YEAR_MIN, "–", RECENT_YEAR_MAX, "), ",
      n_fmt(overall$recent_total), " taxa were advanced through contemporary reporting: ",
      n_fmt(overall$recent_confirmed), " through confirmation of historical records and ",
      n_fmt(overall$recent_new), " through newly reported additions to the island bird list.\n\n"
    )
  )
} else {
  cat(
    "No contemporary confirmations or new reports fell within the recent time window used here.\n\n"
  )
}

if (nrow(top_confirmed) > 0) {
  cat("The strongest gains in confirmation of historical records have been made in ")
  cat(
    paste(
      paste0(top_confirmed$Family_label, " (", top_confirmed$confirmed_taxa, ")"),
      collapse = ", "
    )
  )
  cat(".\n")
  if (any(top_confirmed$recent_confirmed > 0)) {
    cat(
      paste0(
        "Within the recent window, the leading families for historical confirmation were ",
        collapse_and(
          paste0(
            top_confirmed$Family_label[top_confirmed$recent_confirmed > 0],
            " (", top_confirmed$recent_confirmed[top_confirmed$recent_confirmed > 0], ")"
          )
        ),
        ".\n"
      )
    )
  }
  cat("\n")
}

if (nrow(top_new) > 0) {
  cat("The largest gains through newly reported taxa have been made in ")
  cat(
    paste(
      paste0(top_new$Family_label, " (", top_new$new_taxa, ")"),
      collapse = ", "
    )
  )
  cat(".\n")
  if (any(top_new$recent_new > 0)) {
    cat(
      paste0(
        "Recent additions have been especially notable in ",
        collapse_and(
          paste0(
            top_new$Family_label[top_new$recent_new > 0],
            " (", top_new$recent_new[top_new$recent_new > 0], ")"
          )
        ),
        ".\n"
      )
    )
  }
  cat("\n")
}

if (nrow(top_recent) > 0) {
  cat(
    paste0(
      "Looking specifically at recent activity, the families showing the most momentum in ",
      RECENT_YEAR_MIN, "–", RECENT_YEAR_MAX, " were ",
      collapse_and(paste0(top_recent$Family_label, " (", top_recent$recent_total, ")")),
      ". These groups account for much of the recent growth in contemporary knowledge of the island bird fauna.\n\n"
    )
  )
}

if (nrow(top_attention) > 0) {
  cat(
    paste0(
      "The families still requiring the most attention for confirmation of historical reports are ",
      collapse_and(
        paste0(
          top_attention$Family_label, " (",
          top_attention$remaining_historical, " historical-only taxa)"
        )
      ),
      ".\n"
    )
  )
  cat(
    paste0(
      "These families stand out either because they contain the largest absolute backlog of historical reports, ",
      "or because a large fraction of their reported diversity remains unconfirmed.\n\n"
    )
  )
}

cat(
  paste0(
    "Taken together, the pattern suggests that the inventory is advancing along two fronts at once: ",
    "first, by recovering and confirming historically reported taxa; and second, by continuing to add taxa not previously documented for the island. ",
    "The recent pulse of activity in ", RECENT_YEAR_MIN, "–", RECENT_YEAR_MAX,
    " indicates that contemporary community observations are continuing to make measurable gains, while also helping reveal which families still contain the largest historical knowledge gaps.\n\n"
  )
)

cat("------------------------------------------------------------\n")
cat("FAMILY-LEVEL SNAPSHOT\n")
cat("------------------------------------------------------------\n\n")

print(
  family_summary %>%
    mutate(
      resolved_share = round(100 * resolved_share, 1),
      unresolved_share = round(100 * unresolved_share, 1)
    ) %>%
    select(
      Family = Family_label,
      total_taxa,
      confirmed_taxa,
      new_taxa,
      remaining_historical,
      recent_confirmed,
      recent_new,
      recent_total,
      resolved_share,
      unresolved_share
    ) %>%
    arrange(desc(total_taxa), Family),
  n = nrow(family_summary)
)

cat(
  "\nSaved file: ",
  file.path(OUTPUT_DIR, "Bird_diversity_summary_by_family.csv"),
  "\n",
  sep = ""
)