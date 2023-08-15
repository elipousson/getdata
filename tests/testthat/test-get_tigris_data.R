test_that("get_tigris_data works", {
  expect_s3_class(
    get_tigris_data(type = "counties", state = "RI"),
    "sf"
  )
  expect_error(
    get_tigris_data(type = "XYZ", state = "RI")
  )
  expect_s3_class(
    get_tigris_data(type = "counties", state = "RI", name = "Providence"),
    "sf"
  )

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  nc_counties <- get_tigris_data(type = "counties", location = nc[1, ])

  expect_s3_class(
    nc_counties,
    "sf"
  )
  expect_identical(
    nrow(nc_counties),
    6L
  )
})
