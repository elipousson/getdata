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
    rename_with_xwalk(nc, c(xwalk, list("Name" = "NME"))) # ,
    # "`xwalk` values must all be column names in `x`."
  )

  # expect two column tibble with colnames County and geometry
  expect_equal(
    names(rename_with_xwalk(nc, xwalk, keep_all = FALSE)),
    c("County", "geometry")
  )
})

test_that("assorted format functions work", {
  skip_on_ci()
  # FIXME: This is not working on GitHub - need to sort out why
  expect_equal(
    fix_epoch_date(
      data.frame(
        "date" = c(1000000000, 900000000),
        "name" = c("A", "B"),
        "num" = c(1, 2)
      ),
      tz = "EST"
    )$date,
    as.POSIXct(c("1970-01-12 08:46:40", "1970-01-11 05:00:00"),
      origin = "1970-01-01",
      tz = "EST"
    )
  )
})
