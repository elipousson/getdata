test_that("get_airtable_data works", {
  skip_on_ci()
  withr::with_envvar(
    new = c("TEST_AIRTABLE_TOKEN" = Sys.getenv("AIRTABLE_TOKEN")),
    {
      expect_s3_class(
        get_airtable_data(
          base = "appBo0csgMlypViJt",
          table = "tblbKA4iIfyHc9lps",
          type = "TEST_AIRTABLE_TOKEN",
          max_records = 10
        ),
        "tbl_df"
      )

      expect_s3_class(
        get_airtable_metadata(
          base = "appBo0csgMlypViJt",
          type = "TEST_AIRTABLE_TOKEN"
        ),
        "tbl_df"
      )
    }
  )
})
