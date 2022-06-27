test_that("get_wiki_data works", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  expect_s3_class(
    get_wiki_data(
      location = mapbaltimore::council_districts[1, ],
      dist = 0.25, unit = "mile"
    ),
    "sf"
  )
})
