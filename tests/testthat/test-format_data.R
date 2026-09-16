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

test_that("make_xwalk_list works", {
  expect_equal(
    make_xwalk_list(list("County Name" = "cnty", "State" = "st")),
    list("County Name" = "cnty", "State" = "st")
  )

  xwalk_df <- data.frame(label = c("County Name", "State"), name = c("cnty", "st"))

  expect_equal(
    make_xwalk_list(xwalk_df),
    list("County Name" = "cnty", "State" = "st")
  )

  xwalk_df_unnamed <- data.frame(new = "County Name", old = "cnty")

  expect_equal(
    make_xwalk_list(xwalk_df_unnamed),
    list("County Name" = "cnty")
  )

  expect_error(
    make_xwalk_list(xwalk_df, cols = c(1, 2, 3))
  )

  expect_error(
    make_xwalk_list("not a data frame or list")
  )
})

test_that("label_with_xwalk works", {
  skip_if_not_installed("labelled")

  df <- data.frame(cnty = "Wake", st = "NC")
  xwalk <- list(cnty = "County name", st = "State abbreviation")

  labeled <- label_with_xwalk(df, xwalk = xwalk, label = "var")

  expect_equal(
    labelled::var_label(labeled),
    list(cnty = "County name", st = "State abbreviation")
  )

  expect_error(
    label_with_xwalk(df, xwalk = xwalk, label = "invalid")
  )
})

test_that("make_variable_dictionary works", {
  skip_if_not_installed("labelled")

  df <- data.frame(cnty = "Wake", st = "NC")

  dict <- make_variable_dictionary(df)

  expect_s3_class(dict, "data.frame")
  expect_true(all(rlang::has_name(dict, c("variable", "label"))))

  labeled_dict <- make_variable_dictionary(
    df,
    .labels = c("County name", "State abbreviation")
  )

  expect_equal(
    labeled_dict$label,
    c("County name", "State abbreviation")
  )

  dict_with_defs <- make_variable_dictionary(
    df,
    .definitions = c("County name definition", "State abbreviation definition")
  )

  expect_equal(
    dict_with_defs$definitions,
    c("County name definition", "State abbreviation definition")
  )
})

test_that("str_trim_squish_across works", {
  df <- data.frame(
    a = c("  x   y ", NA, "z"),
    b = c(1, 2, 3)
  )

  result <- str_trim_squish_across(df)

  expect_equal(result$a, c("x y", NA, "z"))
  expect_equal(result$b, c(1, 2, 3))
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
    as.POSIXct(
      c("1970-01-12 08:46:40", "1970-01-11 05:00:00"),
      origin = "1970-01-01",
      tz = "EST"
    )
  )
})
