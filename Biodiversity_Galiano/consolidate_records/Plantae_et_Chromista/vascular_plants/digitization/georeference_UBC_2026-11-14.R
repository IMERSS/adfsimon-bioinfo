# Georeference UBC Herbarium vascular plant specimens from Galiano Island that lack coordinates
#
# Output: ../../../records/digitized/UBC_Galiano_Island_vascular_plants_georeferences_2026-11-14.csv, one row per
# catalogNumber with Darwin Core georeference fields, joined to the UBC records by the consolidation script.
#
# Method: point-radius (Chapman & Wieczorek 2020, Georeferencing Best Practices) from the feature named in the
#      locality: OpenStreetMap roads, bays, capes and parks (records/raw/OpenStreetMap_Galiano_2026-11-14) and
#      CPCAD park polygons. Radius = extent of the feature from its point, plus 50 m for an unstated offset from a road
# Localities naming only Galiano Island are left without coordinates.
# Run from this script's directory.

suppressMessages({ library(sf); library(dplyr); library(jsonlite) })

osm.dir <- "../../../records/raw/OpenStreetMap_Galiano_2026-11-14"
ubc <- read.csv("../../../records/digitized/DarwinCore/UBC_Galiano_Island_vascular_plants_2022-10-31_DwC.csv", colClasses = "character")
gazetteer <- read.csv("../../../records/digitized/Galiano_vascular_plant_site_gazetteer_2026-11-14.csv", colClasses = "character")

todo <- ubc %>% filter(is.na(decimalLatitude) | decimalLatitude == "" | decimalLatitude == "NA")

# ---- Point-radius from named features ----

roads <- fromJSON(file.path(osm.dir, "Galiano_named_roads_2026-11-14.json"), simplifyVector = FALSE)$elements
road_geom <- function (name) {
  ways <- Filter(function (w) identical(w$tags$name, name), roads)
  lines <- lapply(ways, function (w) st_linestring(do.call(rbind, lapply(w$geometry, function (g) c(g$lon, g$lat)))))
  st_transform(st_sfc(st_multilinestring(lines), crs = 4326), 3005)
}
pois <- fromJSON(file.path(osm.dir, "Galiano_named_places_2026-11-14.json"), simplifyVector = FALSE)$elements
poi <- function (name) {
  p <- Filter(function (x) identical(x$tags$name, name), pois)[[1]]
  c <- if (!is.null(p$center)) p$center else p
  st_transform(st_sfc(st_point(c(c$lon, c$lat)), crs = 4326), 3005)
}
pa <- st_transform(st_make_valid(st_read(Sys.getenv("GALIANO_PROTECTED_AREAS",
  "../../../../../../galiano-island-flora/spatial_data/vectors/Protected_Areas/Protected_Areas.geojson"), quiet = TRUE)), 3005)

out <- function (pt, radius, source, remark) {
  ll <- st_coordinates(st_transform(pt, 4326))
  data.frame(decimalLatitude = sprintf("%.5f", ll[2]), decimalLongitude = sprintf("%.5f", ll[1]), geodeticDatum = "WGS84",
             coordinateUncertaintyInMeters = as.character(ceiling(radius / 10) * 10),
             georeferenceProtocol = "point-radius method (Chapman & Wieczorek 2020, Georeferencing Best Practices)",
             georeferenceSources = source, georeferenceRemarks = remark)
}
along_road <- function (name, remark) {
  g <- road_geom(name)
  pt <- st_line_sample(st_cast(st_line_merge(g), "LINESTRING")[which.max(st_length(st_cast(st_line_merge(g), "LINESTRING")))], sample = 0.5)
  r <- max(st_distance(st_cast(pt, "POINT"), st_cast(st_cast(g, "MULTIPOINT"), "POINT")))
  out(st_cast(pt, "POINT"), as.numeric(r) + 50, paste0("OpenStreetMap: ", name), remark)
}
around_point <- function (pt, radius, source, remark) out(pt, radius, source, remark)
# A bay's OpenStreetMap point lies on the water; a specimen locality naming the bay is placed at the nearest point of
# its shore (Land.geojson of galiano-island-flora), keeping the radius
land <- st_transform(st_union(st_make_valid(st_read(Sys.getenv("GALIANO_LAND",
  "../../../../../../galiano-island-flora/spatial_data/vectors/Land.geojson"), quiet = TRUE))), 3005)
on_shore <- function (pt) if (st_intersects(pt, land, sparse = FALSE)[1]) pt else st_cast(st_nearest_points(pt, land), "POINT")[2]
around_bay <- function (name, radius, remark) out(on_shore(poi(name)), radius,
  paste0("OpenStreetMap: ", name, " (bay), placed at nearest shore (galiano-island-flora Land.geojson)"), remark)
polygon_site <- function (name, source, remark) {
  u <- st_union(pa$geometry[pa$name == name]); pt <- st_point_on_surface(u)
  out(pt, as.numeric(max(st_distance(pt, st_cast(st_cast(u, "MULTIPOINT"), "POINT")))), source, remark)
}

junction <- function (a, b) st_centroid(st_nearest_points(road_geom(a), road_geom(b)))

montague_1km <- function () {
  g <- st_line_merge(road_geom("Montague Road")); park <- st_union(pa$geometry[pa$name == "Montague Harbour Marine Park"])
  ring <- st_cast(st_boundary(st_buffer(park, 1000)), "MULTILINESTRING")
  x <- st_cast(st_intersection(g, ring), "POINT")
  out(x[1], 500, "OpenStreetMap: Montague Road; CPCAD polygon: Montague Harbour Marine Park",
      "'ca. 1 km from Montague Harbor Park, along Montague Road': point on Montague Road 1 km from the park boundary")
}

rules <- list(
  "Galiano Island, Bluff Park road" = function () along_road("Bluff Road", "'Bluff Park road' taken as Bluff Road"),
  "Galiano Island, Bluff Road" = function () along_road("Bluff Road", "somewhere along Bluff Road"),
  "Gulf Islands, Galiano Island, southeast end, Bluff Park" = function () polygon_site("Bluffs Park", "CPCAD polygon: Bluffs Park", "Bluffs Park; 'southeast end' not resolved further"),
  "Gulf Islands, Galiano Island, above Active Pass, Bluff Park" = function () polygon_site("Bluffs Park", "CPCAD polygon: Bluffs Park", "Bluffs Park, above Active Pass"),
  "Galiano Island, Bluffs Park" = function () polygon_site("Bluffs Park", "CPCAD polygon: Bluffs Park", "Bluffs Park"),
  "Gulf of Georgia, Galiano Island, Sturdies Bay" = function () around_bay("Sturdies Bay", 800, "Sturdies Bay and its shore"),
  "Gulf of Georgia, Galiano Island, Montague Harbour" = function () around_bay("Montague Harbour", 1500, "Montague Harbour and its shore"),
  "Gulf Islands, southern Galiano Island, along Burrell Road" = function () along_road("Burrill Road", "'Burrell Road' taken as Burrill Road"),
  "Gulf of Georgia, southern Galiano Island, along Burrill Road" = function () along_road("Burrill Road", "somewhere along Burrill Road"),
  "southern Galiano Island, along Burrill Road" = function () along_road("Burrill Road", "somewhere along Burrill Road"),
  "Gulf Islands, Galiano Island, southern Galiano Island, Burrell Road" = function () along_road("Burrill Road", "'Burrell Road' taken as Burrill Road"),
  "Gulf Islands, southern Galiano Island, along Burrell Road, near Sturdies Bay road" = function () around_point(junction("Burrill Road", "Sturdies Bay Road"), 300, "OpenStreetMap: Burrill Road and Sturdies Bay Road", "'Burrell Road' taken as Burrill Road; near its junction with Sturdies Bay Road"),
  "Gulf of Georgia, Galiano Island, ca. 1 km from Montague Harbor Park, along Montague Road" = montague_1km,
  "Galiano Island, DL-57, near western shore, Trincomali Channel" = function () {
    g <- gazetteer[gazetteer$site == "Millard Learning Centre (DL57)", ]
    data.frame(decimalLatitude = g$decimalLatitude, decimalLongitude = g$decimalLongitude, geodeticDatum = "WGS84",
               coordinateUncertaintyInMeters = g$coordinateUncertaintyInMeters,
               georeferenceProtocol = "point-radius method (Chapman & Wieczorek 2020, Georeferencing Best Practices)",
               georeferenceSources = g$georeferenceSources, georeferenceRemarks = "DL57; 'near western shore' not resolved further") },
  "southern Galiano Island, Bellhouse Park Road" = function () along_road("Bellhouse Road", "'Bellhouse Park Road' taken as Bellhouse Road"),
  "Gulf Islands, Galiano Island, Bellhouse Park road, southern Galiano Island" = function () along_road("Bellhouse Road", "'Bellhouse Park road' taken as Bellhouse Road"),
  "Gulf Islands, Galiano Island, on road to Sturdies Bay, Retreat Cove 6.5 miles southeast of Virago Point" = function () around_bay("Retreat Cove", 1500, "Retreat Cove (about 6.5 mi southeast of Virago Point, consistent with locality); 'on road to Sturdies Bay' not resolved"),
  "Gulf of Georgia, north end of Galiano Island, along Retreat Cove Road" = function () along_road("Retreat Cove Road", "somewhere along Retreat Cove Road"),
  "Galiano Island, Matthews Point, southwest side" = function () around_point(poi("Matthews Point"), 250, "OpenStreetMap: Matthews Point (cape)", "southwest side of Matthews Point")
)

rest <- todo
feature.match <- bind_rows(lapply(seq_len(nrow(rest)), function (i) {
  f <- rules[[rest$locality[i]]]
  if (is.null(f)) return(NULL)
  cbind(catalogNumber = rest$catalogNumber[i], f())
}))

geo <- feature.match %>%
  mutate(georeferencedBy = "automated georeference prepared for Andrew Simon", georeferencedDate = "2026-11-14",
         georeferenceVerificationStatus = "requires verification")
write.csv(geo, "../../../records/digitized/UBC_Galiano_Island_vascular_plants_georeferences_2026-11-14.csv", row.names = FALSE)

cat(nrow(todo), "UBC records without coordinates;", nrow(feature.match), "point-radius;",
    nrow(todo) - nrow(geo), "left (island-wide localities)\n")
print(feature.match %>% select(catalogNumber, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, georeferenceSources), row.names = FALSE)
