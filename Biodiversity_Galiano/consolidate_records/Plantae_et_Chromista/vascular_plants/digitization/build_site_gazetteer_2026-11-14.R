# Point-radius georeferences (Chapman & Wieczorek 2020, Georeferencing Best Practices) for the named sites of the
# GCA-compiled vascular plant lists, written to ../../../records/digitized/Galiano_vascular_plant_site_gazetteer_2026-11-14.csv
#
# Point: point-on-surface of the site's polygon(s); radius: the greatest distance from that point to the polygon(s),
# plus any GPS'd plots of the same survey. Polygons: CPCAD protected and conserved areas, as held by the
# galiano-island-flora project (spatial_data/vectors/Protected_Areas/Protected_Areas.geojson).
# Each polygon match was checked against A. Simon's 2018 GPS'd transects named for the same site.
# Run from this script's directory.

suppressMessages({ library(sf); library(dplyr) })

pa_file <- Sys.getenv("GALIANO_PROTECTED_AREAS", "../../../../../../galiano-island-flora/spatial_data/vectors/Protected_Areas/Protected_Areas.geojson")
pa <- st_transform(st_make_valid(st_read(pa_file, quiet = TRUE)), 3005)

point_radius <- function (geoms) {
  u <- st_union(geoms)
  centre <- st_point_on_surface(st_union(st_convex_hull(u)))
  if (!st_intersects(centre, u, sparse = FALSE)[1]) centre <- st_point_on_surface(u)
  r <- max(st_distance(centre, st_cast(st_cast(u, "MULTIPOINT"), "POINT")))
  ll <- st_coordinates(st_transform(centre, 4326))
  c(lat = round(ll[2], 5), lon = round(ll[1], 5), radius = ceiling(as.numeric(r) / 10) * 10)
}

poly <- function (...) pa$geometry[pa$name %in% c(...)]

# Erickson 2016 GPS'd plots (NAD83 UTM 10N), to extend the Mount Sutil radius over DL21 & DL22
plots <- read.csv("../../../records/raw/Erickson_2016_Mount_Sutil_plots/Erickson_2016_Mount_Sutil_baseline_plot_metadata.csv")
plots <- plots[plots$Easting > 0, ]
plots <- st_transform(st_as_sf(plots, coords = c("Easting", "Northing"), crs = 26910), 3005)$geometry

# Simon 2018 Crystal Mountain transects (no CPCAD polygon for Crystal Mountain)
simon <- read.csv("../../../records/digitized/DarwinCore/Simon_2018_transect_coordinates_DwC.csv")
veg <- read.csv("../../../records/digitized/DarwinCore/Simon_2018_vegetation_data_DwC.csv")
cm <- simon[simon$Name %in% unique(veg$TID[veg$locality == "Crystal Mountain"]), ]
cm <- st_buffer(st_transform(st_as_sf(cm, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326), 3005), 500)$geometry

sites <- list(
  "Trincomali Nature Sanctuary" = list(poly("Trincomali Nature Sanctuary"), "CPCAD polygon: Trincomali Nature Sanctuary"),
  "Quadra Hill" = list(poly("Quadra Hill Nature Reserve"), "CPCAD polygon: Quadra Hill Nature Reserve"),
  "Pebble Beach" = list(poly("Pebble Beach Nature Reserve (DL 63)"), "CPCAD polygon: Pebble Beach Nature Reserve (DL 63)"),
  "Cable Bay (DL64)" = list(poly("Galiano Island - Qw'xwulwis - Cable Bay"), "CPCAD polygon: Galiano Island - Qw'xwulwis - Cable Bay"),
  "Millard Learning Centre (DL57)" = list(poly("Galiano Ecological Learning Centre"), "CPCAD polygon: Galiano Ecological Learning Centre (contains Simon 2018 'Millard Learning Centre' transects)"),
  "Mount Sutil (DL21, DL22, REM Lot 23)" = list(c(poly("Mt. Sutil", "Mt. Sutil, Galiano Island"), plots), "CPCAD polygons: Mt. Sutil (2), extended to Erickson 2016 GPS'd plots on DL21 & DL22"),
  "Bluffs Park; Matthews Point; Bellhouse Park" = list(poly("Bluffs Park", "Matthews Point Regional Park", "Bellhouse Park"), "CPCAD polygons: Bluffs Park, Matthews Point Regional Park, Bellhouse Park (list covers all three)"),
  "Crystal Mountain (DL89, DL90)" = list(cm, "Simon 2018 Crystal Mountain transects buffered by 500 m (no polygon available)")
)

gaz <- do.call(rbind, lapply(names(sites), function (s) {
  pr <- point_radius(sites[[s]][[1]])
  data.frame(site = s, decimalLatitude = pr["lat"], decimalLongitude = pr["lon"], coordinateUncertaintyInMeters = pr["radius"],
             geodeticDatum = "WGS84", georeferenceSources = sites[[s]][[2]])
}))
write.csv(gaz, "../../../records/digitized/Galiano_vascular_plant_site_gazetteer_2026-11-14.csv", row.names = FALSE)
print(gaz, row.names = FALSE)
