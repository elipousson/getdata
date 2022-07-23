test_that("get_wiki_data works", {
  location <-
    get_location(
      type = system.file("shape/nc.shp", package = "sf"),
      name = "Ashe",
      name_col = "NAME",
      fn = ~ suppressWarnings(sf::st_centroid(.x))
    )

  expect_s3_class(
    get_wiki_data(
      location = location,
      dist = 4,
      unit = "mi"
    ),
    "sf"
  )
  expect_s3_class(
    get_wiki_data(
      location = location,
      radius = 4,
      unit = "mi"
    ),
    "sf"
  )
})
