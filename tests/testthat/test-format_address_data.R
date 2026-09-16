test_that("format_address_data works", {
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
        c(
          "block_num",
          "block_even_odd",
          "block_segment",
          "block_face",
          "street_address"
        )
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

test_that("bind_location_text_col works", {
  text_df <- data.frame(
    text = c(
      "100 block Holliday St",
      "N Charles St between E Read St and E Chase St",
      "both sides of the 200 block"
    )
  )

  result <- bind_location_text_col(text_df)

  expect_true(
    all(
      rlang::has_name(
        result,
        c("is_address", "is_block_face", "is_street_corridor", "block_side")
      )
    )
  )

  expect_equal(result$is_address, c(TRUE, TRUE, TRUE))
  expect_equal(result$is_block_face, c(TRUE, FALSE, TRUE))
  expect_equal(result$is_street_corridor, c(FALSE, TRUE, FALSE))
  expect_equal(result$block_side, c(NA_character_, NA_character_, "multiple"))

  expect_error(
    bind_location_text_col(
      data.frame(text = "100 block Holliday St", is_address = TRUE)
    )
  )
})
