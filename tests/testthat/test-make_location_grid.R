test_that("make_location_grid works", {
  location <-
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Hyde", name_col = "NAME"
    )

  expect_s3_class(
    make_location_grid(
      location = location,
      nrow = 2,
      ncol = 2
    ),
    "sf"
  )
})
