#### R Script for visualizing terrestrial arthropod contribution networks
#### Historical reporters/collectors vs contemporary observers

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
EXPANSION_MULTIPLIER <- 1.4

# Plot window is based on a fixed reference expansion so that
# nodes remain visible while EXPANSION_MULTIPLIER still changes
# the apparent compactness/spread of the network.
REFERENCE_EXPANSION_FOR_WINDOW <- 1.00

# Extra whitespace around the plotting window
PLOT_WINDOW_PADDING <- 0.20

# Maximum number of contributors to keep in each human category
# Use Inf to keep all
MAX_HISTORICAL_CONTRIBUTORS <- 20
MAX_CONTEMPORARY_CONTRIBUTORS <- 100

# Maximum number of taxa to keep in the plotted network
# Taxa are ranked by total number of incident edges among retained people
# Use Inf to keep all
MAX_TAXA <- 800

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
PLOT_TITLE <- "Biodiversity Galiano: terrestrial arthropods"
PLOT_SUBTITLE <- "historical reports vs contemporary observersations"
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

TERRESTRIAL_ARTHROPODS <- read.csv(
  "../review/Animalia/terrestrial_arthropods/summaries/Terrestrial_arthropods_summary_2026-04-05.csv",
  stringsAsFactors = FALSE
)

## ------------------------------------------------------------
## Clean key fields
## ------------------------------------------------------------

df <- TERRESTRIAL_ARTHROPODS %>%
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
    "Taxon (reported)",
    "Taxon (confirmed)",
    "Taxon (new; year gradient)",
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