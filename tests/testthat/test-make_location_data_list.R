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

  expect_no_warning(
    result <- make_location_data_list(
      data = nc[1, ],
      location = list(nc[1, ], nc[2, ])
    )
  )

  expect_length(result$location, 2)
  expect_length(result$data, 2)

  expect_no_warning(
    result_flipped <- make_location_data_list(
      data = list(nc[1, ], nc[2, ]),
      location = nc[1, ]
    )
  )

  expect_length(result_flipped$location, 2)
  expect_length(result_flipped$data, 2)
})

test_that("make_location_data_list warns on unrecyclable length mismatches", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_warning(
    result <- make_location_data_list(
      data = list(nc[1, ], nc[2, ]),
      location = list(nc[1, ], nc[2, ], nc[3, ])
    ),
    "location.*length 3.*data.*length 2"
  )

  expect_length(result$location, 3)
  expect_length(result$data, 2)
})

test_that("make_location_data_list does not trigger case_when deprecation warning", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_no_warning(
    make_location_data_list(
      data = nc[1, ],
      location = list(nc[1, ], nc[2, ])
    )
  )
})

test_that("make_location_data_list requires data and location", {
  expect_error(
    make_location_data_list(data = NULL, location = NULL)
  )
})
