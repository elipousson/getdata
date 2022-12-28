test_that("get_gsheet_data works", {
  ss <- "1DTXkaTYVLJt9qUJBTm2f2oUl0T5nx5QjwY3ncxMBAxU"
  withr::with_package("googlesheets4", {
    skip_if_not(googlesheets4::gs4_has_token())
    expect_s3_class(
      get_gsheet_data(
        ss = ss
      ),
      "tbl_df"
    )
    expect_s3_class(
      get_gsheet_data(
        ss = ss,
        geometry = TRUE
      ),
      "sf"
    )
  })
})
