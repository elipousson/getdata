nc <- overedge::read_sf_path(system.file("shape/nc.shp", package = "sf"))

# get_location works with a type sf object and name and id values
get_location(type = nc, name = "Warren", name_col = "NAME")
get_location(type = nc, id = 37185, id_col = "FIPSNO")

# type can also be a file path
get_location(
  type = system.file("shape/nc.shp", package = "sf"),
  name = "Hertford",
  name_col = "NAME"
)

# type can also be an index name (if a named list of data sets, url values, or
# path values is passed to index)
get_location(
  type = "smaller",
  name = "Hertford",
  name_col = "NAME",
  index = list(
    "smaller" = dplyr::filter(nc, AREA <= 0.10),
    "larger" = dplyr::filter(nc, AREA > 0.15)
  )
)
