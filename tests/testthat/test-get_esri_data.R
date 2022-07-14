test_that("get_esri_data works", {
  location <-
    get_location(type = system.file("shape/nc.shp", package = "sf"), name = "Hyde", name_col = "NAME")

  skip_if_offline()
  expect_s3_class(
    get_esri_data(
      location = location,
      url = "https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer/29"
    ),
    "sf"
  )

  # TODO: Add these three tests
  # Add where query
  # Add dataframe with coords and bbox query
  # Add name_col query
})
