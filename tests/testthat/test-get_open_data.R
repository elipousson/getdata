test_that("get_open_data works", {
  withr::with_envvar(
    new = c("TEST_MARYLAND_OPEN_DATA_API_KEY" = Sys.getenv("MARYLAND_OPEN_DATA_API_KEY")),
    {
      skip_if_no_token("TEST_MARYLAND_OPEN_DATA_API_KEY")
      source_url <- "https://opendata.maryland.gov"

      expect_s3_class(
        get_open_data(
          data = "6jva-hr4v",
          source_url = source_url,
          type = "TEST_MARYLAND_OPEN_DATA_API_KEY"
        ),
        "data.frame"
      )

      expect_s3_class(
        get_socrata_data(
          data = "6jva-hr4v",
          source_url = source_url,
          type = "TEST_MARYLAND_OPEN_DATA_API_KEY"
        ),
        "data.frame"
      )
    }
  )

  expect_s3_class(
    get_socrata_data(
      data = "list",
      source_url = source_url
    ),
    "data.frame"
  )

  expect_s3_class(
    list_socrata_data(
      source_url
    ),
    "tbl_df"
  )
})
