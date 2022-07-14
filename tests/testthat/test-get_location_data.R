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

  skip_if_offline()
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
