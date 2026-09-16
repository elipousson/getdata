test_that("cache_location_data caches sf data as gpkg", {
  skip_if_not_installed("filenamr")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  tmp <- withr::local_tempdir()

  path <- cache_location_data(
    data = nc[1, ],
    name = "test_nc",
    path = tmp,
    cache = FALSE
  )

  expect_true(file.exists(path))
  expect_match(path, "test_nc\\.gpkg$")
  expect_s3_class(sf::st_read(path, quiet = TRUE), "sf")
})

test_that("cache_location_data caches non-sf data as rds", {
  skip_if_not_installed("filenamr")

  df <- data.frame(x = 1:3)
  tmp <- withr::local_tempdir()

  path <- cache_location_data(
    data = df,
    name = "test_df",
    path = tmp,
    cache = FALSE,
    fileext = "rds"
  )

  expect_true(file.exists(path))
  expect_match(path, "test_df\\.rds$")
  expect_equal(readRDS(path), df)
})

test_that("cache_location_data filters data by location before caching", {
  skip_if_not_installed("filenamr")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  tmp <- withr::local_tempdir()

  path <- cache_location_data(
    data = nc,
    location = nc[1, ],
    name = "loc_nc",
    path = tmp,
    cache = FALSE
  )

  result <- sf::st_read(path, quiet = TRUE)

  expect_lt(nrow(result), nrow(nc))
})
