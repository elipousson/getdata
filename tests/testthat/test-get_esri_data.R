test_that("get_esri_data works", {
  location <-
    get_location(type = system.file("shape/nc.shp", package = "sf"), name = "Hyde", name_col = "NAME")

  expect_s3_class(
    get_esri_data(
      location = location,
      url = "https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer/29"
    ),
    "sf"
  )
})
