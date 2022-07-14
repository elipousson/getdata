test_that("get_location works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, 3857)

  # Check if type as sf object with name/id lookup works
  expect_s3_class(
    get_location(type = nc, name = "Warren", name_col = "NAME"),
    "sf"
  )

  expect_s3_class(
    get_location(type = nc, id = 37185, id_col = "FIPSNO"),
    "sf"
  )

  expect_s3_class(
    get_location(type = nc, id = "37185", id_col = "FIPSNO"),
    "sf"
  )

  # Check if type as path works
  expect_s3_class(
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Hertford",
      name_col = "NAME"
    ),
    "sf"
  )

  expect_s3_class(
    get_location(
      type = nc,
      name = c("Ashe", "Alleghany", "Surry"),
      name_col = "NAME",
      union = TRUE,
      label = "Three NC counties"
    ),
    "sf"
  )

  expect_s3_class(
    get_location(
      type = nc,
      name = c("Ashe", "Alleghany", "Surry"),
      name_col = "NAME",
      union = TRUE,
      label = "Three NC counties"
    ),
    "sf"
  )


  # Check if index list works

  expect_s3_class(
    get_location(
      type = "smaller",
      name = "Hertford",
      name_col = "NAME",
      index = list(
        "smaller" = dplyr::filter(nc, AREA <= 0.10),
        "larger" = dplyr::filter(nc, AREA > 0.15)
      )
    ),
    "sf"
  )

  # Check passing an sf object to location
  expect_s3_class(
    get_location(
      type = nc,
      location = sf::st_buffer(nc[1, ], units::as_units(250, "m"))
    ),
    "sf"
  )

  expect_error(
    get_location(
      type = nc,
      name = "XYZ",
      name_col = "ABC"
    )
  )

  expect_error(
    get_location(
      type = nc,
      name = "Ashe",
      index = "This index is not a list."
    ),
    "is.list(index) || is.null(index) is not TRUE"
  )

  # Check if type as url works with passing extra parameters to get_location_data()
  # In this case, no location information is passed to get_location() so it warns before returning all types
  skip_if_offline()
  expect_s3_class(
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1797.geojson",
      name = "1st Ward"
    ),
    "sf"
  )

  skip_if_offline()
  expect_s3_class(
    get_location(
      type = "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Parks/FeatureServer/0",
      name_col = "NAME",
      name = "Chesapeake and Ohio Canal National Historic Park"
    ),
    "sf"
  )

  # skip_on_ci()
  skip_if_offline()
  expect_s3_class(
    get_location(
      type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1802.geojson",
      location = "100 Holliday St, Baltimore, MD 21202"
    ),
    "sf"
  )
})
