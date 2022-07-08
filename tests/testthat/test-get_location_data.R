test_that("get_location_data works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(
    get_location_data(
      location = nc[1, ],
      dist = 50,
      units = "mi",
      asp = 1,
      data = nc
    ),
    "sf"
  )

  withr::with_package(package = "mapbaltimore", {
    expect_s3_class(
      get_location_data(
        location = get_location(
          type = "neighborhoods",
          package = "mapbaltimore",
          location = "100 Holliday St, Baltimore, MD 21202"
        ),
        dist = 0.25,
        units = "mi",
        asp = 1,
        data = "neighborhoods",
        package = "mapbaltimore"
      ),
      "sf"
    )


    expect_s3_class(
      get_location_data(
        data = "vegetated_area",
        location = get_location(
          type = "council_districts",
          package = "mapbaltimore",
          id = 12
        ),
        package = "mapbaltimore",
        filetype = "gpkg"
      ),
      "sf"
    )

    expect_s3_class(
      get_location_data(
        data = "council_districts",
        location = get_location(
          type = "neighborhoods",
          package = "mapbaltimore",
          id = "Harwood"
        ),
        package = "mapbaltimore",
        trim = TRUE
      ),
      "sf"
    )
  })

  expect_s3_class(
    get_location_data(
      location = get_location(
        type = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/baltimore-city-wards-1802.geojson",
        name = "1st Ward"
      ),
      data = "https://raw.githubusercontent.com/baltimoreheritage/geojson/master/1814-baltimore-defenders.geojson",
    ),
    "sf"
  )
})
