test_that("get_elev_profile works", {
  skip_on_ci()
  skip_if_not_installed("elevatr")

  line <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-76.61, 39.29, -76.60, 39.30), ncol = 2, byrow = TRUE)
    ),
    crs = 4326
  )

  result <- get_elev_profile(line)

  expect_s3_class(result, "sf")
  expect_true(all(rlang::has_name(result, c("elevation", "elev_units"))))
  expect_equal(nrow(result), 2)

  result_units <- get_elev_profile(line, units = "ft", drop_units = TRUE)

  expect_type(result_units$elevation, "double")
  expect_equal(unique(result_units$elev_units), "ft")

  point <- sf::st_sfc(sf::st_point(c(-76.61, 39.29)), crs = 4326)
  result_point <- get_elev_profile(point)

  expect_s3_class(result_point, "sf")
  expect_equal(nrow(result_point), 1)

  expect_error(
    get_elev_profile(sf::st_sfc(sf::st_polygon(), crs = 4326))
  )
})

test_that("get_elev_profile works with dist = TRUE", {
  skip_on_ci()
  skip_if_not_installed("elevatr")

  line <- sf::st_sfc(
    sf::st_linestring(
      matrix(c(-76.61, 39.29, -76.60, 39.30), ncol = 2, byrow = TRUE)
    ),
    crs = 4326
  )

  result <- get_elev_profile(line, dist = TRUE)

  expect_true(rlang::has_name(result, "distance"))
  expect_s3_class(result$distance, "units")
  expect_equal(units(result$distance)$numerator, "m")
  expect_equal(as.numeric(result$distance[[1]]), 0)
  expect_gt(as.numeric(result$distance[[2]]), 0)

  result_cumulative <- get_elev_profile(line, dist = TRUE, cumulative = TRUE)

  expect_equal(
    as.numeric(result_cumulative$distance),
    cumsum(as.numeric(result$distance))
  )

  result_units <- get_elev_profile(
    line,
    dist = TRUE,
    units = "ft",
    drop_units = TRUE
  )

  expect_type(result_units$distance, "double")
  expect_gt(result_units$distance[[2]], as.numeric(result$distance[[2]]))
})
