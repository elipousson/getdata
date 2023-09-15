nc <- sfext::read_sf_path(system.file("shape/nc.shp", package = "sf"))

civic_buildings <- get_osm_data(
  location = nc[37,],
  features = c("building" = "civic"),
  geometry = "polygons"
)

civic_buildings
