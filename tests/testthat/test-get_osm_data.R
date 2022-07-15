test_that("get_osm_data works", {
  location <-
    get_location(system.file("shape/nc.shp", package = "sf"), name = "Hyde", name_col = "NAME")

  skip_if_offline()
  expect_s3_class(
    get_osm_data(
      location = location,
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
    get_osm_boundaries(
      location = location,
      level = 1
    ),
    "sf"
  )
})
