#### Animate / time-slice bird reporting network
#### Uses exported network objects from the bird reporting script
#### Nodes and edges grow through time while the full-network layout remains fixed

# Set relative paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## ------------------------------------------------------------
## LOAD PACKAGES
## ------------------------------------------------------------

library(dplyr)
library(igraph)
library(readr)
library(stringr)
library(scales)
library(rstudioapi)

## ------------------------------------------------------------
## USER CONTROLS
## ------------------------------------------------------------

# Input/export paths
# Correct this one line if the exported network files live somewhere else.
INPUT_DIR <- "outputs/birds/network_animation"
FRAME_DIR <- file.path(INPUT_DIR, "frames")

EDGE_FILE <- file.path(INPUT_DIR, "birds_network_edges.csv")
NODE_FILE <- file.path(INPUT_DIR, "birds_network_nodes.csv")
LAYOUT_FILE <- file.path(INPUT_DIR, "birds_network_layout.csv")
WINDOW_FILE <- file.path(INPUT_DIR, "birds_network_plot_window.csv")
FRAME_LOG_FILE <- file.path(INPUT_DIR, "birds_network_frame_log.csv")

FRAME_PREFIX <- "birds_network"
GIF_FILE <- file.path(INPUT_DIR, "birds_network_growth.gif")

if (!dir.exists(FRAME_DIR)) {
  dir.create(FRAME_DIR, recursive = TRUE)
}

TIME_STEPS <- c(
  seq(1800, 2010, by = 10),
  seq(2011, 2026, by = 1)
)

# Frame export
FRAME_WIDTH <- 13
FRAME_HEIGHT <- 7.5
FRAME_RES <- 300

# Style
BASE_FONT_FAMILY <- "sans"
BG_COLOR <- "black"
FG_COLOR <- "white"
SUBTLE_LINE_COLOR <- "gray70"
PLOT_TITLE <- "Biodiversity Galiano: Birds"
TITLE_CEX <- 1.60
TITLE_FONT <- 2
SUBTITLE_CEX <- 1
PERSON_LABEL_CEX <- 0.72
LABEL_COLOR <- "white"
LEGEND_TITLE_CEX <- 1.00
LEGEND_TEXT_CEX <- 0.88
LABEL_ONLY_PEOPLE <- TRUE

# If TRUE, years with no new visible edges are still exported.
# This produces visual pauses in the animation.
EXPORT_ALL_TIME_STEPS <- TRUE

## ------------------------------------------------------------
## READ EXPORTED NETWORK TABLES
## ------------------------------------------------------------

edges <- read_csv(EDGE_FILE, show_col_types = FALSE)
nodes <- read_csv(NODE_FILE, show_col_types = FALSE)
layout_df <- read_csv(LAYOUT_FILE, show_col_types = FALSE)
plot_window <- read_csv(WINDOW_FILE, show_col_types = FALSE)

## ------------------------------------------------------------
## CHECK INPUTS
## ------------------------------------------------------------

required_edge_cols <- c("from", "to", "person_name", "person_class", "edge_class", "weight", "entry_year")
required_node_cols <- c("name", "node_class", "taxon_status", "taxon_year", "color", "size", "label")
required_layout_cols <- c("name", "x", "y", "label_x", "label_y")
required_window_cols <- c("x_min", "x_max", "y_min", "y_max")

missing_edge_cols <- setdiff(required_edge_cols, names(edges))
missing_node_cols <- setdiff(required_node_cols, names(nodes))
missing_layout_cols <- setdiff(required_layout_cols, names(layout_df))
missing_window_cols <- setdiff(required_window_cols, names(plot_window))

if (length(missing_edge_cols) > 0) {
  stop("Missing required columns in edges file: ", paste(missing_edge_cols, collapse = ", "), call. = FALSE)
}

if (length(missing_node_cols) > 0) {
  stop("Missing required columns in nodes file: ", paste(missing_node_cols, collapse = ", "), call. = FALSE)
}

if (length(missing_layout_cols) > 0) {
  stop("Missing required columns in layout file: ", paste(missing_layout_cols, collapse = ", "), call. = FALSE)
}

if (length(missing_window_cols) > 0) {
  stop("Missing required columns in plot-window file: ", paste(missing_window_cols, collapse = ", "), call. = FALSE)
}

edges <- edges %>%
  mutate(
    entry_year = as.integer(entry_year),
    weight = as.numeric(weight)
  ) %>%
  filter(!is.na(entry_year))

nodes <- nodes %>%
  mutate(
    label = ifelse(is.na(label), NA_character_, as.character(label)),
    color = ifelse(is.na(color), "gray80", as.character(color)),
    size = as.numeric(size)
  )

layout_df <- layout_df %>%
  mutate(
    x = as.numeric(x),
    y = as.numeric(y),
    label_x = as.numeric(label_x),
    label_y = as.numeric(label_y)
  )

xlim_use <- c(plot_window$x_min[1], plot_window$x_max[1])
ylim_use <- c(plot_window$y_min[1], plot_window$y_max[1])

if (!EXPORT_ALL_TIME_STEPS) {
  active_years <- sort(unique(edges$entry_year))
  TIME_STEPS <- TIME_STEPS[TIME_STEPS %in% active_years]
}

## ------------------------------------------------------------
## PLOT FUNCTION
## ------------------------------------------------------------

plot_network_frame <- function(year_current) {
  edges_t <- edges %>%
    filter(entry_year <= year_current)
  
  visible_nodes <- unique(c(edges_t$from, edges_t$to))
  
  nodes_t <- nodes %>%
    filter(name %in% visible_nodes)
  
  layout_t <- layout_df %>%
    filter(name %in% nodes_t$name)
  
  nodes_t <- nodes_t %>%
    filter(name %in% layout_t$name)
  
  edges_t <- edges_t %>%
    filter(from %in% nodes_t$name, to %in% nodes_t$name)
  
  nodes_t <- nodes_t %>%
    arrange(match(name, layout_t$name))
  
  layout_t <- layout_t %>%
    arrange(match(name, nodes_t$name))
  
  if (nrow(edges_t) == 0 || nrow(nodes_t) == 0) {
    plot.new()
    par(bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
    plot.window(xlim = xlim_use, ylim = ylim_use)
    title(
      main = PLOT_TITLE,
      cex.main = TITLE_CEX,
      font.main = TITLE_FONT,
      family = BASE_FONT_FAMILY,
      col.main = FG_COLOR,
      line = 1
    )
    text(0, 0, labels = "No visible records for this time step", col = FG_COLOR, family = BASE_FONT_FAMILY)
    return(invisible(NULL))
  }
  
  g_t <- graph_from_data_frame(
    d = edges_t %>% select(from, to, weight, edge_class, person_class),
    vertices = nodes_t,
    directed = FALSE
  )
  
  coords <- as.matrix(layout_t[, c("x", "y")])
  label_coords <- as.matrix(layout_t[, c("label_x", "label_y")])
  
  node_df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    label_x = label_coords[, 1],
    label_y = label_coords[, 2],
    name = V(g_t)$name,
    node_class = V(g_t)$node_class,
    color = V(g_t)$color,
    size = V(g_t)$size,
    label = V(g_t)$label,
    stringsAsFactors = FALSE
  )
  
  edge_colors <- c("reported" = "gray60", "confirmed" = "#009E73", "new" = "#D55E00")
  E(g_t)$color <- edge_colors[E(g_t)$edge_class]
  E(g_t)$color[is.na(E(g_t)$color)] <- "gray80"
  E(g_t)$width <- scales::rescale(E(g_t)$weight, to = c(0.5, 2.2))
  
  vertex_labels <- node_df$label
  if (LABEL_ONLY_PEOPLE) {
    vertex_labels[!(node_df$node_class %in% c("historical", "contemporary"))] <- NA
  }
  
  layout(matrix(c(1, 2), nrow = 1), widths = c(5.2, 1.9))
  
  par(mar = c(1.2, 1.2, 3.2, 0.8), bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
  plot(
    g_t,
    layout = coords,
    rescale = FALSE,
    xlim = xlim_use,
    ylim = ylim_use,
    vertex.shape = "none",
    vertex.label = NA,
    edge.color = E(g_t)$color,
    edge.width = E(g_t)$width,
    main = NA,
    asp = 0,
    axes = FALSE
  )
  
  tax_idx <- which(node_df$node_class == "taxon")
  hist_idx <- which(node_df$node_class == "historical")
  cont_idx <- which(node_df$node_class == "contemporary")
  
  points(node_df$x[tax_idx], node_df$y[tax_idx], pch = 16, col = node_df$color[tax_idx], cex = node_df$size[tax_idx])
  points(node_df$x[hist_idx], node_df$y[hist_idx], pch = 15, col = node_df$color[hist_idx], cex = node_df$size[hist_idx])
  points(node_df$x[cont_idx], node_df$y[cont_idx], pch = 15, col = node_df$color[cont_idx], cex = node_df$size[cont_idx])
  
  title(
    main = PLOT_TITLE,
    cex.main = TITLE_CEX,
    font.main = TITLE_FONT,
    family = BASE_FONT_FAMILY,
    col.main = FG_COLOR,
    line = 1
  )
  
  label_idx <- which(!is.na(vertex_labels) & vertex_labels != "")
  text(
    node_df$label_x[label_idx],
    node_df$label_y[label_idx],
    labels = vertex_labels[label_idx],
    cex = PERSON_LABEL_CEX,
    col = LABEL_COLOR,
    family = BASE_FONT_FAMILY,
    xpd = TRUE
  )
  
  par(mar = c(1.2, 0.2, 3.2, 1.2), bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  
  text(
    x = 0.08,
    y = 0.96,
    labels = "Legend",
    adj = c(0, 1),
    cex = LEGEND_TITLE_CEX,
    font = 2,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
  
  legend(
    x = 0.08,
    y = 0.90,
    legend = c(
      "Historical reporter / collector",
      "Contemporary observer",
      "Reported species",
      "Confirmed species",
      "New species",
      "Reported edge",
      "Confirmed edge",
      "New edge"
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
  
  text(
    x = 0.08,
    y = 0.05,
    labels = paste0(year_current),
    adj = c(0, 0),
    cex = 2,
    font = 2,
    family = BASE_FONT_FAMILY,
    col = FG_COLOR
  )
  
  invisible(NULL)
}

## ------------------------------------------------------------
## EXPORT TIME-SLICE FRAMES
## ------------------------------------------------------------

old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)

frame_log <- data.frame(
  year = integer(),
  n_edges = integer(),
  n_nodes = integer(),
  frame_file = character(),
  stringsAsFactors = FALSE
)

for (yr in TIME_STEPS) {
  edges_t <- edges %>% filter(entry_year <= yr)
  nodes_t <- unique(c(edges_t$from, edges_t$to))
  
  frame_file <- file.path(
    FRAME_DIR,
    paste0(FRAME_PREFIX, "_", yr, ".png")
  )
  
  png(
    filename = frame_file,
    width = FRAME_WIDTH,
    height = FRAME_HEIGHT,
    units = "in",
    res = FRAME_RES,
    bg = BG_COLOR
  )
  
  par(family = BASE_FONT_FAMILY, bg = BG_COLOR, fg = FG_COLOR, col = FG_COLOR)
  plot_network_frame(yr)
  dev.off()
  
  frame_log <- bind_rows(
    frame_log,
    data.frame(
      year = yr,
      n_edges = nrow(edges_t),
      n_nodes = length(nodes_t),
      frame_file = frame_file,
      stringsAsFactors = FALSE
    )
  )
  
  cat("Saved frame: ", frame_file, "\n", sep = "")
}

write_csv(frame_log, FRAME_LOG_FILE)

cat("\nSaved frame log: ", FRAME_LOG_FILE, "\n", sep = "")
cat("Saved frames in: ", FRAME_DIR, "\n", sep = "")

## ------------------------------------------------------------
## OPTIONAL GIF EXPORT
## ------------------------------------------------------------

# Uncomment this section if the magick package is installed.
# This uses the exported PNG frames and does not affect the frame outputs.

# if (requireNamespace("magick", quietly = TRUE)) {
#   frame_files <- frame_log$frame_file
#   gif <- magick::image_read(frame_files) %>%
#     magick::image_animate(fps = 2)
#   magick::image_write(gif, GIF_FILE)
#   cat("Saved GIF: ", GIF_FILE, "\n", sep = "")
# }
