test_that("get_esri_data works", {
  location <-
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Hyde", name_col = "NAME"
    )

  test_url <-
    "https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer/29"

  expect_s3_class(
    get_esri_data(
      location = location,
      url = test_url
    ),
    "sf"
  )
  expect_s3_class(
    get_esri_data(
      location = location,
      url = test_url
    ),
    "sf"
  )
  expect_s3_class(
    get_esri_data(
      url = test_url,
      name = "Cape Lookout National Seashore",
      name_col = "NAME"
    ),
    "sf"
  )
  expect_s3_class(
    get_esri_data(
      url = test_url,
      where = "NAME like 'Cape Hatteras National Seashore'"
    ),
    "sf"
  )
  expect_s3_class(
    get_esri_metadata(
      url = test_url,
      meta = "fields"
    ),
    "data.frame"
  )
})

test_that("get_esri_layers works", {
  location <-
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Hyde", name_col = "NAME"
    )

  test_url <-
    "https://services.nconemap.gov/secure/rest/services/NC1Map_Boundaries/FeatureServer"

  test_layers <-
    get_esri_layers(
      location = location,
      url = test_url
    )
  expect_type(
    test_layers,
    "list"
  )
  expect_s3_class(
    test_layers[[1]],
    "sf"
  )
})
