test_that("get_tigris_data works", {
  skip_if_offline()
  expect_s3_class(
    get_tigris_data(type = "counties", state = "RI"),
    "sf"
  )
  expect_error(
    get_tigris_data(type = "XYZ", state = "RI")
  )
  expect_s3_class(
    get_tigris_data(type = "counties", state = "RI", name = "Providence"),
    "sf"
  )
})
