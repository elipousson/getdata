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

  expect_equal(
    nrow(
      get_location_data(
        location = nc[1, ],
        crop = FALSE,
        trim = TRUE,
        data = nc
      )
    ),
    4
  )

  expect_equal(
    nrow(
      get_location_data(
        location = nc[1, ],
        dist = 10,
        unit = "mi",
        crop = FALSE,
        data = nc
      )
    ),
    5
  )

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

test_that("map_location_data works with multiple locations", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  data_path <- system.file("shape/nc.shp", package = "sf")
  locs <- list(nc[1, ], nc[2, ])

  result_list <- map_location_data(
    location = locs,
    data = data_path,
    class = "list",
    crop = FALSE
  )

  expect_type(result_list, "list")
  expect_length(result_list, 2)
  expect_true(all(vapply(result_list, inherits, logical(1), "sf")))

  result_sf <- map_location_data(
    location = locs,
    data = data_path,
    class = "sf",
    crop = FALSE
  )

  expect_s3_class(result_sf, "sf")
  expect_equal(nrow(result_sf), sum(vapply(result_list, nrow, integer(1))))
})

test_that("map_location_data works with multiple data sources", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  data_path <- system.file("shape/nc.shp", package = "sf")

  result <- map_location_data(
    location = nc[1, ],
    data = c(data_path, data_path),
    class = "list",
    crop = FALSE,
    label = "nc"
  )

  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(vapply(result, inherits, logical(1), "sf")))
})
