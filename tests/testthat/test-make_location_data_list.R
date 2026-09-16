test_that("make_location_data_list works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  result <- make_location_data_list(
    data = list(nc[1, ], nc[2, ]),
    location = list(nc[1, ], nc[2, ])
  )

  expect_named(result, c("location", "data"))
  expect_length(result$location, 2)
  expect_length(result$data, 2)

  result_custom_key <- make_location_data_list(
    data = nc[1, ],
    location = nc[2, ],
    key = c("loc", "dat")
  )

  expect_named(result_custom_key, c("loc", "dat"))
})

test_that("make_location_data_list recycles length 1 inputs", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  result <- make_location_data_list(
    data = nc[1, ],
    location = list(nc[1, ], nc[2, ])
  )

  expect_length(result$location, 2)
  expect_length(result$data, 2)
})

test_that("make_location_data_list requires data and location", {
  expect_error(
    make_location_data_list(data = NULL, location = NULL)
  )
})
