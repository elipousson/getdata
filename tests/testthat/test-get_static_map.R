test_that("get_static_map works", {
  location <-
    get_tigris_data(
      type = "counties",
      state = "MD",
      name = "Baltimore city"
    )

  withr::with_envvar(
    new = c("TEST_BING_MAPS_API_KEY" = Sys.getenv("BING_MAPS_API_KEY")),
    {
      skip_if_no_token("TEST_BING_MAPS_API_KEY")
      expect_s3_class(
        get_static_bingmap(
          location = location,
          token = Sys.getenv("TEST_BING_MAPS_API_KEY")
        ),
        "magick-image"
      )
    }
  )

  withr::with_envvar(
    new = c("TEST_MAPBOX_PUBLIC_TOKEN" = Sys.getenv("MAPBOX_PUBLIC_TOKEN")),
    {
      skip_if_no_token("TEST_MAPBOX_PUBLIC_TOKEN")
      expect_s3_class(
        get_static_mapbox(
          location = location,
          token = Sys.getenv("TEST_MAPBOX_PUBLIC_TOKEN")
        ),
        "magick-image"
      )
      expect_s3_class(
        get_osm_static_mapbox(
          id = "relation/133345",
          overlay_location = FALSE,
          token = Sys.getenv("TEST_MAPBOX_PUBLIC_TOKEN")
        ),
        "magick-image"
      )
      skip()
      expect_s3_class(
        get_location_static_mapbox(
          type = location,
          token = Sys.getenv("TEST_MAPBOX_PUBLIC_TOKEN")
        ),
        "magick-image"
      )
    }
  )
})
