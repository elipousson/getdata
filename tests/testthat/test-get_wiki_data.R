wiki_query_params <- function(req) {
  httr2::url_parse(req$url)$query
}

test_that("get_wiki_data works", {
  skip_if_offline()
  skip_on_ci()

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

test_that("get_wiki_data returns details, data frames, and responses", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  data <- get_wiki_data(
    location,
    radius = TRUE,
    dist = 500,
    details = c("type", "name"),
    limit = 5
  )

  expect_s3_class(data, "sf")
  expect_lte(nrow(data), 5)
  expect_contains(names(data), c("title", "type", "name"))

  expect_false(inherits(
    get_wiki_data(
      location,
      radius = TRUE,
      dist = 500,
      limit = 5,
      geometry = FALSE
    ),
    "sf"
  ))

  resp <- get_wiki_data(
    location,
    radius = TRUE,
    dist = 500,
    limit = 5,
    list = "resp"
  )
  expect_named(resp, c("batchcomplete", "query"), ignore.order = TRUE)

  expect_s3_class(
    get_wiki_data("Baltimore City Hall", limit = 5),
    "sf"
  )
})

test_that("get_wiki_data checks inputs", {
  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  expect_snapshot(get_wiki_data(location, list = "search"), error = TRUE)
  expect_snapshot(get_wiki_data(), error = TRUE)
  expect_snapshot(get_wiki_data(1), error = TRUE)
  expect_snapshot(
    suppressMessages(get_wiki_data(location, radius = TRUE, dist = 0)),
    error = TRUE
  )
})

test_that("req_wiki_query builds query parameters", {
  req <- req_wiki_query(
    lang = "en",
    gscoord = "39.2904|-76.6122",
    gsradius = 500L
  )

  expect_equal(httr2::url_parse(req$url)$hostname, "en.wikipedia.org")
  expect_equal(
    wiki_query_params(req),
    list(
      action = "query",
      list = "geosearch",
      gscoord = "39.2904|-76.6122",
      gsradius = "500",
      gsprimary = "primary",
      gslimit = "50",
      format = "json"
    )
  )

  expect_equal(
    httr2::url_parse(
      req_wiki_query(lang = "fr", gspage = "Paris")$url
    )$hostname,
    "fr.wikipedia.org"
  )

  expect_equal(
    wiki_query_params(
      req_wiki_query(lang = "en", gspage = "Paris", list = "resp")
    )$list,
    "geosearch"
  )
})

test_that("req_wiki_query collapses multiple details and primary values", {
  req <- req_wiki_query(
    lang = "en",
    gspage = "Baltimore",
    details = c("type", "name", "country")
  )
  expect_equal(wiki_query_params(req)$gsprop, "type|name|country")

  expect_equal(
    wiki_query_params(
      req_wiki_query(lang = "en", gspage = "Baltimore", details = "type")
    )$gsprop,
    "type"
  )

  expect_null(
    wiki_query_params(req_wiki_query(lang = "en", gspage = "Baltimore"))$gsprop
  )

  expect_equal(
    wiki_query_params(
      req_wiki_query(lang = "en", gspage = "Baltimore", primary = "all")
    )$gsprimary,
    "all"
  )
})

test_that("req_wiki_query validates arguments", {
  expect_snapshot(
    req_wiki_query(lang = "en", gspage = "Baltimore", details = "height"),
    error = TRUE
  )
  expect_snapshot(
    req_wiki_query(lang = "en", gspage = "Baltimore", primary = "none"),
    error = TRUE
  )
  expect_snapshot(req_wiki_query(lang = "en"), error = TRUE)
  expect_snapshot(req_wiki_query(lang = 1, gspage = "Baltimore"), error = TRUE)
})

test_that("req_wiki_query resets limit to the max", {
  expect_snapshot(
    req <- req_wiki_query(lang = "en", gspage = "Baltimore", limit = 1000)
  )
  expect_equal(wiki_query_params(req)$gslimit, "500")
})

test_that("st_gscoord and st_gsbbox format geosearch parameters", {
  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  expect_equal(st_gscoord(location), glue::glue("39.2904|-76.6122"))

  bbox <- strsplit(st_gsbbox(location, dist = 100), "|", fixed = TRUE)[[1]]
  bbox <- as.numeric(bbox)

  # top|left|bottom|right
  expect_length(bbox, 4)
  expect_gt(bbox[[1]], bbox[[3]])
  expect_lt(bbox[[2]], bbox[[4]])
})

test_that("get_wiki_data sets radius from diag_ratio", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = -76.62, ymin = 39.28, xmax = -76.60, ymax = 39.30),
      crs = 4326
    )
  )

  data <- get_wiki_data(
    location,
    radius = TRUE,
    diag_ratio = 0.5,
    limit = 5,
    geometry = FALSE
  )
  expect_s3_class(data, "data.frame")
})

test_that("get_wiki_data uses the getdata.crs option", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  withr::local_options(getdata.crs = 26985)
  data <- get_wiki_data(location, radius = TRUE, dist = 500, limit = 5)
  expect_equal(sf::st_crs(data), sf::st_crs(26985))
})

test_that("st_diagdist_meter returns the geodesic bbox diagonal", {
  bbox <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = -76.62, ymin = 39.28, xmax = -76.60, ymax = 39.30),
      crs = 4326
    )
  )

  expect_equal(st_diagdist_meter(bbox), 2820, tolerance = 0.02)
  expect_equal(
    st_diagdist_meter(sf::st_transform(bbox, 26985)),
    st_diagdist_meter(bbox),
    tolerance = 0.02
  )
  expect_equal(
    st_diagdist_meter(bbox, unit = "km"),
    st_diagdist_meter(bbox) / 1000
  )
})
