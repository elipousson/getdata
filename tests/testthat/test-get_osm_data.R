test_that("get_osm_data works", {
  location <-
    get_location(type = system.file("shape/nc.shp", package = "sf"), name = "Hyde", name_col = "NAME")

  expect_s3_class(
    get_osm_data(
      location = location,
      key = "leisure",
      value = "park",
      geometry = "polygons"
    ),
    "sf"
  )

})
