test_that("get_airtable_data works", {
  withr::with_envvar(
    new = c("TEST_AIRTABLE_TOKEN" = Sys.getenv("AIRTABLE_TOKEN")),
    {
      skip_if_no_token("TEST_AIRTABLE_TOKEN")

      expect_s3_class(
        get_airtable_data(
          base = "app1lcJCwi0mpQGqZ",
          table = "tbl81zsVzjBxVZePB",
          type = "TEST_AIRTABLE_TOKEN",
          max_records = 10
        ),
        "tbl_df"
      )

      expect_s3_class(
        get_airtable_metadata(
          base = "app1lcJCwi0mpQGqZ",
          type = "TEST_AIRTABLE_TOKEN"
        ),
        "tbl_df"
      )
    }
  )
})
