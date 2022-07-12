test_that("get_wiki_data works", {
  nc_county <-
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Ashe",
      name_col = "NAME"
    )

  nc_county <- suppressWarnings(sf::st_centroid(nc_county))

  expect_s3_class(
    get_wiki_data(
      location = nc_county,
      dist = 4,
      unit = "mi"
    ),
    "sf"
  )

  expect_s3_class(
    get_wiki_data(
      location = nc_county,
      radius = 4,
      unit = "mi"
    ),
    "sf"
  )
})
