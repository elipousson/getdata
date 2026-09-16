test_that("format_sf_data works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  result <- format_sf_data(nc, crs = 4326)

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 4326)

  result_sf_col <- format_sf_data(nc, sf_col = "geom")

  expect_equal(attr(result_sf_col, "sf_column"), "geom")

  result_erased <- format_sf_data(nc[2:3, ], erase_data = nc[1, ])

  expect_s3_class(result_erased, "sf")
  expect_equal(nrow(result_erased), 2)

  result_simplified <- format_sf_data(nc, dTolerance = 1000)

  expect_s3_class(result_simplified, "sf")

  expect_error(
    format_sf_data(data.frame(x = 1), sf_req = TRUE)
  )
})
