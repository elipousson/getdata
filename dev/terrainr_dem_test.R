# Test: get a 1 m bare-earth DEM from the USGS National Map with terrainr
# (keyless). Development script; not part of the package.
#
# Result on 2026-11-05: 500 x 500 cells at 1 m (service 3DEPElevation) for a
# 500 m window around City Hall in ~4 s; returned in EPSG:4326 (reproject to a
# planar CRS such as EPSG:26985 before using terra::viewshed()). DEM at City
# Hall: 11 m, consistent with lidar ground of ~10.8 m. Elevation range in the
# window: -0.1 to 26.3 m.
suppressMessages({
  library(terrainr)
  library(sf)
  library(terra)
})

out_dir <- tempdir()

pt <- st_as_sf(
  data.frame(x = -76.6122, y = 39.2904),
  coords = c("x", "y"),
  crs = 4326
)
bb <- set_bbox_side_length(pt, 500)

tiles <- get_tiles(
  bb,
  output_prefix = file.path(out_dir, "dem"),
  services = "elevation",
  resolution = 1,
  georeference = TRUE
)

dem <- rast(tiles[["elevation"]][[1]])
print(dem)
print(terra::extract(dem, vect(st_transform(pt, crs(dem)))))
