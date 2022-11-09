test_that("rename_with_xwalk works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_names <- names(nc)
  xwalk <- list("County" = "CNTY_")
  nc_comparison_names <- nc_names
  nc_comparison_names[[3]] <- "County"

  # expect dataframe with County in colnames
  expect_equal(
    names(rename_with_xwalk(nc, xwalk, .strict = TRUE)),
    nc_comparison_names
  )

  expect_error(
    rename_with_xwalk(nc, c(xwalk, list("Name" = "NME"))),
    "`xwalk` values must all be column names in `x`."
  )

  # expect two column tibble with colnames County and geometry
  expect_equal(
    names(rename_with_xwalk(nc, xwalk, keep_all = FALSE)),
    c("County", "geometry")
  )
})
