test_that("make_location_grid works", {
  expect_s3_class(2 * 2, 4)
  location <-
    get_location(type = system.file("shape/nc.shp", package = "sf"),
                 name = "Hyde", name_col = "NAME")

  expect_s3_class(
    make_location_grid(
      location = location,
      nrow = 2,
      ncol = 2
    ),
    "sf"
  )

})
