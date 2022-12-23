test_that("multiplication works", {
  address_df <-
    data.frame(
      "bldg_num" = c("100", "1415", "600"),
      "street_dir_prefix" = c(NA, NA, "N"),
      "street_name" = c("Holiday", "Key", "Charles"),
      "street_type" = c("Street", "Highway", "St")
    )

  address_df <-
    bind_block_col(
      x = address_df,
      street_col = "street_address"
    )

  expect_true(
    all(
      rlang::has_name(
      address_df,
      c("block_num", "block_even_odd", "block_segment",
        "block_face", "street_address")
      )
    )
  )

  address_df <-
    bind_address_col(
    address_df,
    city = "Baltimore",
    state = "MD"
  )

  expect_true(
    all(
      rlang::has_name(
        address_df,
        c("city", "state", "address")
      )
    )
  )
})
