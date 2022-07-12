test_that("get_admin_data works", {
  expect_s3_class(
    get_states(
      location = "Maryland"
    ),
    "data.frame"
  )

  expect_s3_class(
    get_counties(
      name = "Baltimore city, Maryland"
    ),
    "data.frame"
  )

  # Using short names with abbreviated state names for look-up may result in inexact matches
  expect_s3_class(
    get_counties(
      county = "Baltimore, MD"
    ),
    "data.frame"
  )

  # Two-digit integer GeoIDs are supported
  # bbox and wkt columns are dropped when returning class "sf"
  expect_s3_class(
    get_states(
      geoid = 24,
      class = "sf"
    ),
    "sf"
  )

  # sf locations are used as a spatial filter
  # expect_s3_class(
  #  get_counties(
  #    location = get_states("MD", class = "sf"),
  #    class = "sf"
  #  ),
  #  "sf"
  # )
})
