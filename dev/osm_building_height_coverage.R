# How many OpenStreetMap buildings near 417 E Fayette St, Baltimore have a
# height or building:levels tag? Reads buildings from a local Geofabrik Maryland
# GeoPackage (see /Users/elipousson/Projects/02_analysis/baltimore-assets/dev/
# build_osm_extract_gpkg.R for how the extract is built). Development script;
# not part of the package.
#
# Result on 2026-11-05 (3 km radius): 49,997 buildings, 3.1% with height or
# levels (7.3% in the 1 km core); skyline towers are well tagged, City Hall and
# 100 Light Street are not. Reads ~59k buildings in ~1 s with a spatial filter.
# Heights live in the other_tags hstore column.
suppressMessages({
  library(sf)
  library(dplyr)
})
sf_use_s2(FALSE)
g <- path.expand("~/Library/Caches/osmextract/geofabrik_maryland-latest.gpkg")
obs <- st_sfc(st_point(c(-76.60943, 39.29031)), crs = 4326)
circle <- st_transform(st_buffer(st_transform(obs, 26985), 3000), 4326)
t0 <- Sys.time()
b <- read_sf(
  g,
  layer = "multipolygons",
  query = "SELECT osm_id, osm_way_id, name, building, other_tags, geometry FROM multipolygons WHERE building IS NOT NULL OR other_tags LIKE '%building:part%'",
  wkt_filter = st_as_text(st_as_sfc(st_bbox(circle)))
)
cat("read:", nrow(b), "in", format(Sys.time() - t0), "\n")
b <- b[st_intersects(b, circle, sparse = FALSE)[, 1], ]
tag <- function(x, k) {
  m <- regmatches(x, regexec(sprintf('"%s"=>"([^"]*)"', k), x))
  vapply(m, \(y) if (length(y) > 1) y[2] else NA_character_, "")
}
b <- b |>
  mutate(
    height = tag(other_tags, "height"),
    levels = tag(other_tags, "building:levels"),
    min_height = tag(other_tags, "min_height"),
    part = grepl("building:part", other_tags),
    wikidata = tag(other_tags, "wikidata"),
    roof_h = tag(other_tags, "roof:height"),
    roof_shape = tag(other_tags, "roof:shape")
  )
cat("buildings in 3 km:", nrow(b), " (building:part:", sum(b$part), ")\n")
cat(
  "height:",
  sum(!is.na(b$height)),
  sprintf("(%.1f%%)", 100 * mean(!is.na(b$height))),
  " levels:",
  sum(!is.na(b$levels)),
  sprintf("(%.1f%%)", 100 * mean(!is.na(b$levels))),
  " either:",
  sum(!is.na(b$height) | !is.na(b$levels)),
  sprintf("(%.1f%%)", 100 * mean(!is.na(b$height) | !is.na(b$levels))),
  " wikidata:",
  sum(!is.na(b$wikidata)),
  " named:",
  sum(!is.na(b$name)),
  "\n"
)
# core downtown: 1 km
core <- st_transform(st_buffer(st_transform(obs, 26985), 1000), 4326)
bc <- b[st_intersects(b, core, sparse = FALSE)[, 1], ]
cat(
  "\n1 km core:",
  nrow(bc),
  " height:",
  sum(!is.na(bc$height)),
  " levels:",
  sum(!is.na(bc$levels)),
  " either:",
  sum(!is.na(bc$height) | !is.na(bc$levels)),
  sprintf("(%.1f%%)", 100 * mean(!is.na(bc$height) | !is.na(bc$levels))),
  "\n"
)
cat("\nunit suffix check on height values:\n")
print(head(
  sort(table(grep("[^0-9.]", b$height, value = TRUE)), decreasing = TRUE),
  8
))
cat("\ntallest by height tag:\n")
b$h_num <- suppressWarnings(as.numeric(b$height))
print(
  b |>
    st_drop_geometry() |>
    filter(!is.na(h_num)) |>
    arrange(desc(h_num)) |>
    select(osm_id, osm_way_id, name, building, h_num, levels) |>
    head(15),
  width = 160
)
cat("\nlevels top:\n")
b$l_num <- suppressWarnings(as.numeric(b$levels))
print(
  b |>
    st_drop_geometry() |>
    filter(!is.na(l_num)) |>
    arrange(desc(l_num)) |>
    select(name, building, l_num, height) |>
    head(12),
  width = 160
)
cat("\nkey landmarks present?\n")
for (nm in c(
  "City Hall",
  "War Memorial",
  "Zion",
  "Shot Tower",
  "Bromo",
  "World Trade",
  "100 Light",
  "Battle Monument",
  "Courthouse"
)) {
  hit <- b[grepl(nm, b$name, ignore.case = TRUE), ]
  cat(sprintf(
    "%-16s n=%d  heights: %s  levels: %s\n",
    nm,
    nrow(hit),
    paste(na.omit(hit$height), collapse = ","),
    paste(na.omit(hit$levels), collapse = ",")
  ))
}
