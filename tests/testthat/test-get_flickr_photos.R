test_that("get_flickr_photos works", {
  location <- get_tigris_data("counties", "MD")

  withr::with_envvar(
    new = c("TEST_FLICKR_API_KEY" = Sys.getenv("FLICKR_API_KEY")),
    {
      expect_s3_class(
        get_flickr_photos(
          location = location[24,],
          user_id = "baltimoreheritage",
          key = Sys.getenv("TEST_FLICKR_API_KEY"),
          per_page = 20
        ),
        "tbl_df"
      )

      expect_s3_class(
        get_flickr_photos(
          location = location[24,],
          user_id = "baltimoreheritage",
          key = Sys.getenv("TEST_FLICKR_API_KEY"),
          per_page = 20,
          page = 2
        ),
        "tbl_df"
      )

      expect_s3_class(
        get_flickr_photos(
          location = location[24,],
          user_id = "baltimoreheritage",
          key = Sys.getenv("TEST_FLICKR_API_KEY"),
          orientation = "landscape",
          per_page = 20
        ),
        "tbl_df"
      )

      expect_message(
        get_flickr_photos(
          location = location[1,],
          user_id = "baltimoreheritage",
          key = Sys.getenv("TEST_FLICKR_API_KEY")
        ),
        "No photos can be found with the provided parameters."
      )
    }
  )
})
