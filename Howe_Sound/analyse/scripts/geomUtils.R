library(sf)

# From https://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude
hortis.WGS84a = 6378137;
hortis.WGS84b = 6356752.3142;
hortis.WGS84e2 = (hortis.WGS84a * hortis.WGS84a - hortis.WGS84b * hortis.WGS84b) / (hortis.WGS84a * hortis.WGS84a);

# Length in metres for a degree of longitude at given latitude
hortis.longitudeLength <- function (latitude) {
    latrad <- pi * latitude / 180;
    sinrad <- sin(latrad);
    return (pi * hortis.WGS84a * cos(latrad) / (180 * sqrt(1 - hortis.WGS84e2 * sinrad * sinrad)))
}

#Length in metres for a degree of latitude at given latitude
hortis.latitudeLength = function (latitude) {
    latrad <- pi * latitude / 180;
    sinrad <- sin(latrad);
    return (pi * hortis.WGS84a * (1 - hortis.WGS84e2) / (180 * (1 - hortis.WGS84e2 * sinrad * sinrad) ^ 1.5));
}

hortis.longToLat <- function (lng, lat) {
    longLength <- hortis.longitudeLength(lat)
    latLength <- hortis.latitudeLength(lat)
    return (lng * longLength / latLength);
}

make_grid_frame <- function (points, cellsize) {
  bbox_b <- st_bbox(points) # order xmin, ymin, xmax, ymax
  # Undo insufferable wrapping as "named numbers"
  bbox <- list(xmin = as.numeric(bbox_b$xmin), ymin = as.numeric(bbox_b$ymin), 
               xmax = as.numeric(bbox_b$xmax), ymax = as.numeric(bbox_b$ymax))
  midlat <- (bbox$ymax + bbox$ymin) / 2
  longdeg <- hortis.longitudeLength(midlat)
  longsize <- round(cellsize / longdeg, 6)
  latsize <- round(hortis.longToLat(longsize, midlat), 6)
  
  longcount <- as.integer(ceiling((bbox$xmax - bbox$xmin) / longsize))
  latcount <-  as.integer(ceiling((bbox$ymax - bbox$ymin) / latsize))
  
  list(bbox = bbox, bbox_b = bbox_b, longsize = longsize, latsize = latsize, longcount = longcount, latcount = latcount)
}

point_to_cell <- function (gridframe, long, lat) {
  # Check if the point is within the bbox
  if (long < gridframe$bbox$xmin || long > gridframe$bbox$xmax ||
      lat < gridframe$bbox$ymin || lat > gridframe$bbox$ymax) {
    warning("Point (", long, ", ", lat, ") lies outside the bounding box ", format(gridframe$bbox_b))
    return (NA)
  }
  
  # Calculate the column index (0-based)
  col_index <- as.integer(floor((long - gridframe$bbox$xmin) / gridframe$longsize))
  
  # Calculate the row index (0-based)
  row_index <- as.integer(floor((gridframe$bbox$ymax - lat) / gridframe$latsize))
  
  # Calculate the cell_id as a 0-based index
  cell_id <- row_index * gridframe$longcount + col_index
}

# Given an sf frame with POINT geometries, derive an extra column cell_id containing the gridded cell id
assign_cell_id <- function (points, gridframe) {
  assign.start <- Sys.time()
  coords <- sf::st_coordinates(points)
  longs <- coords[, "X"]
  lats <- coords[, "Y"]
  # Apply the calculate_cell_id function to all points in the sf object
  points$cell_id <- mapply(point_to_cell, longs, lats,
                           MoreArgs = list(gridframe = gridframe))
  assign.end <- Sys.time()
  cat ("Assigned ", nrow(points), " points in ", (assign.end - assign.start), "s")
  return (points)
}

cell_id_to_polygon <- function (gridframe, cell_id) {
  # Calculate row and column indices
  row_index <- floor(cell_id / gridframe$longcount)
  col_index <- cell_id %% gridframe$longcount
  
  # Calculate the coordinates for the corners of the polygon
  xmin <- gridframe$bbox$xmin + col_index * gridframe$longsize
  xmax <- xmin + gridframe$longsize
  ymax <- gridframe$bbox$ymax - row_index * gridframe$latsize
  ymin <- ymax - gridframe$latsize
  
  # Create the POLYGON geometry
  polygon <- st_polygon(list(rbind(c(xmin, ymin), c(xmax, ymin), 
                                   c(xmax, ymax), c(xmin, ymax), 
                                   c(xmin, ymin))))
  return (polygon)
}

cell_id_to_centroid <- function (gridframe, cell_id) {
  # Calculate row and column indices
  row_index <- floor(cell_id / gridframe$longcount)
  col_index <- cell_id %% gridframe$longcount
  
  # Calculate the coordinates for the corners of the polygon
  xmin <- gridframe$bbox$xmin + col_index * gridframe$longsize
  ymax <- gridframe$bbox$ymax - row_index * gridframe$latsize

  return (c(xmin + gridframe$longsize / 2, ymax - gridframe$latsize / 2))
}

# Accepts a dataframe with column cell_id and assigns a polygon geometry to it for the cell
assign_cell_geometry_sf <- function (with_cell_id, gridframe) {
  polygons <- mapply(cell_id_to_polygon, cell_id = with_cell_id$cell_id, 
                     MoreArgs = list(gridframe = gridframe), SIMPLIFY = FALSE)
  
  # Create an sf dataframe with POLYGON geometry
  sf_df <- st_sf(with_cell_id, geometry = st_sfc(polygons))
  st_crs(sf_df) <- "WGS84"
  
  return (sf_df)
}

# Supply extra columns longitude, latitude to an incoming dataframe with an existing column cell_id
assign_cell_centroids <- function(with_cell_id, gridframe) {
  # Apply calculate_centroid to each cell_id in the dataframe
  centroids <- mapply(cell_id_to_centroid, cell_id = with_cell_id$cell_id, 
                      MoreArgs = list(gridframe = gridframe))
  
  # Transpose the centroids matrix to get separate longitude and latitude vectors
  with_cell_id$longitude <- round(centroids[1, ], 6)
  with_cell_id$latitude <- round(centroids[2, ], 6)
  
  return (with_cell_id)
}

# Assigns point sf geometry holding centroids to incoming dataframe with an existing column cell_id
assign_cell_centroids_sf <- function(with_cell_id, gridframe) {
  with_coords <- assign_cell_centroids(with_cell_id, gridframe) %>% st_as_sf(coords=c("longitude", "latitude"))
  st_crs(with_coords) <- "WGS84"
  with_coords
}

# For a set of sf features which have cell_id defined, determine the cell_ids of them which intersect the supplied polygons
cells_for_polygons <- function (points, polygons) {
  intersections <- st_intersects(points, polygons, sparse = FALSE)
  # Get the indices of points that intersect with any polygon
  intersecting_points_indices <- apply(intersections, 1, any)
  intersecting_cell_ids <- points$cell_id[intersecting_points_indices]
}

# Expands a supplied region defined by a list of cell_ids by one square orthogonally in each direction
expandCells <- function (cell_ids, gridframe) {
    combined <- c(cell_ids, cell_ids + 1, cell_ids - 1, cell_ids + gridframe$longcount, cell_ids - gridframe$longcount)
    sort(unique(combined))
}