test_that("get_osm_data works", {
  location <- get_location(
    system.file("shape/nc.shp", package = "sf"),
    name = "Hyde",
    name_col = "NAME"
  )
  # Use sf object for location
  expect_s3_class(
    get_osm_data(
      location = location,
      key = "leisure",
      value = "park",
      geometry = "polygons"
    ),
    "sf"
  )
  # Use character object for location
  expect_s3_class(
    get_osm_data(
      location = paste(location$NAME, "County, North Carolina"),
      key = "leisure",
      value = "park",
      geometry = "polygons"
    ),
    "sf"
  )
  expect_s3_class(
    get_osm_id(
      id = "way/79235072"
    ),
    "sf"
  )
  expect_s3_class(
    get_osm_data(
      id = c("way/79235072", "way/70517965")
    ),
    "sf"
  )
  expect_s3_class(
    get_osm_data(
      id = c("way" = 79235072)
    ),
    "sf"
  )
  expect_s3_class(
    get_osm_boundaries(
      location = location,
      level = c(2, 4)
    ),
    "sf"
  )
})
