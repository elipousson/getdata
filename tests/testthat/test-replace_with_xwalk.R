test_that("replace_with_xwalk works", {
  address_df <-
    data.frame(
      "bldg_num" = c("100", "1415", "600", "10"),
      "street_dir_prefix" = c(NA, NA, "N", NA),
      "street_name" = c("Holiday", "Key", "Charles", "Art Museum"),
      "street_type" = c("St", "Hwy", "St", "Dr")
    )

  expect_equal(
    replace_street_suffixes(
      c("Street", "Highway", "Avenue", "Drive")
    ),
    c("ST", "HWY", "AVE", "DR")
  )

  expect_equal(
    replace_street_suffixes(
      address_df,
      abb = FALSE,
      case = "sentence"
    )$street_type,
    c("Street", "Highway", "Street", "Drive")
  )

  expect_equal(
    replace_street_dir_prefixes(
      c("North", "East", "West")
    ),
    c("N", "E", "W")
  )

  expect_equal(
    replace_street_dir_prefixes(
      c("S", "W", "N"),
      abb = FALSE,
      case = "sentence"
    ),
    c("South", "West", "North")
  )
})
