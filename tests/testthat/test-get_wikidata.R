wikidata_binding <- function(
  qid,
  label,
  coord,
  dist,
  type = NULL,
  height = NULL,
  article = NULL
) {
  val <- \(x) if (is.null(x)) NULL else list(value = as.character(x))

  Filter(
    Negate(is.null),
    list(
      item = val(paste0("http://www.wikidata.org/entity/", qid)),
      itemLabel = val(label),
      coord = val(coord),
      dist = val(dist),
      typeLabel = val(type),
      height_m = val(height),
      article = val(article)
    )
  )
}

test_that("get_wikidata works", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  data <- get_wikidata(location, dist = 500, limit = 10)

  expect_s3_class(data, "sf")
  expect_lte(nrow(data), 10)
  expect_named(
    data,
    c("qid", "label", "dist", "height", "instance_of", "wikipedia", "geometry")
  )
  expect_false(is.unsorted(data$dist))
  expect_true(all(data$dist <= 500))
  expect_equal(anyDuplicated(data$qid), 0)
  expect_equal(sf::st_crs(data), sf::st_crs(3857))
})

test_that("get_wikidata supports class, geometry, crs, and polygon locations", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)

  data <- get_wikidata(
    location,
    dist = 1,
    unit = "km",
    class = "Q41176",
    limit = 5,
    geometry = FALSE
  )
  expect_s3_class(data, "data.frame")
  expect_false(inherits(data, "sf"))
  expect_contains(names(data), c("lon", "lat"))

  expect_equal(
    sf::st_crs(get_wikidata(location, dist = 200, limit = 3, crs = 26985)),
    sf::st_crs(26985)
  )

  area <- sf::st_buffer(sf::st_transform(location, 26985), 300)
  expect_s3_class(get_wikidata(area, limit = 3), "sf")
})

test_that("get_wikidata warns when no items are found", {
  skip_if_offline()
  skip_on_ci()

  location <- sf::st_sfc(sf::st_point(c(-30, -30)), crs = 4326)

  expect_snapshot(data <- suppressMessages(get_wikidata(location, dist = 100)))
  expect_equal(nrow(data), 0)
})

test_that("get_wikidata checks inputs", {
  expect_snapshot(get_wikidata(), error = TRUE)
  expect_snapshot(
    suppressMessages(
      get_wikidata(sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326))
    ),
    error = TRUE
  )
  expect_snapshot(
    suppressMessages(
      get_wikidata(
        sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326),
        dist = 100,
        class = "building"
      )
    ),
    error = TRUE
  )
})

test_that("wikidata_radius returns meters", {
  point <- sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)), crs = 4326)
  area <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = -76.62, ymin = 39.28, xmax = -76.60, ymax = 39.30),
      crs = 4326
    )
  )

  expect_equal(wikidata_radius(point, dist = 500), 500)
  expect_equal(wikidata_radius(point, dist = 1, unit = "km"), 1000)
  expect_equal(
    wikidata_radius(area, diag_ratio = 0.5),
    st_diagdist_meter(area) * 0.5
  )
  expect_equal(wikidata_radius(area), st_diagdist_meter(area) / 2)
  expect_equal(wikidata_radius(area, dist = 250), 250)
})

test_that("wikidata_query_around builds a SPARQL query", {
  query <- wikidata_query_around(lon = -76.6122, lat = 39.2904, radius = 500)

  expect_match(query, 'Point(-76.6122 39.2904)"^^geo:wktLiteral', fixed = TRUE)
  expect_match(query, 'wikibase:radius "0.5"', fixed = TRUE)
  expect_match(query, "<https://en.wikipedia.org/>", fixed = TRUE)
  expect_match(query, 'wikibase:language "en"', fixed = TRUE)
  expect_match(query, "GROUP BY ?item ORDER BY ?dist", fixed = TRUE)
  expect_no_match(query, "LIMIT")
  expect_no_match(query, "VALUES")
})

test_that("wikidata_query_around supports class, limit, and lang", {
  query <- wikidata_query_around(
    lon = -76.6122,
    lat = 39.2904,
    radius = 1500,
    class = c("Q41176", "Q16560"),
    limit = 25,
    lang = "es"
  )

  expect_match(query, "VALUES ?class { wd:Q41176 wd:Q16560 }", fixed = TRUE)
  expect_match(query, "?item wdt:P31/wdt:P279* ?class", fixed = TRUE)
  expect_match(query, "LIMIT 25", fixed = TRUE)
  expect_match(query, "<https://es.wikipedia.org/>", fixed = TRUE)
  expect_match(query, 'wikibase:language "es,en"', fixed = TRUE)
  expect_match(query, 'wikibase:radius "1.5"', fixed = TRUE)

  expect_no_match(
    wikidata_query_around(
      lon = -76.6122,
      lat = 39.2904,
      radius = 500,
      class = "Q41176",
      subclasses = FALSE
    ),
    "P279",
    fixed = TRUE
  )
})

test_that("wikidata_query_around validates arguments", {
  expect_snapshot(
    wikidata_query_around(-76.6, 39.3, 500, class = "building"),
    error = TRUE
  )
  expect_snapshot(
    wikidata_query_around(-76.6, 39.3, 500, class = 41176),
    error = TRUE
  )
  expect_snapshot(
    wikidata_query_around(-76.6, 39.3, 500, limit = 0),
    error = TRUE
  )
  expect_snapshot(wikidata_query_around(-76.6, 39.3, -1), error = TRUE)
  expect_snapshot(
    wikidata_query_around(-76.6, 39.3, 500, lang = 1),
    error = TRUE
  )
})

test_that("req_wikidata_query builds a request", {
  req <- req_wikidata_query("SELECT * WHERE {}")
  url <- httr2::url_parse(req$url)

  expect_equal(url$hostname, "query.wikidata.org")
  expect_equal(url$path, "/sparql")
  expect_equal(url$query, list(query = "SELECT * WHERE {}", format = "json"))

  withr::local_options(getdata.wikidata_endpoint = "https://example.org/sparql")
  expect_equal(
    httr2::url_parse(req_wikidata_query("SELECT * WHERE {}")$url)$hostname,
    "example.org"
  )
})

test_that("wikidata_bindings_to_df returns one row per item", {
  bindings <- list(
    wikidata_binding(
      "Q2",
      "Second",
      "Point(-76.61 39.29)",
      0.2,
      type = "building",
      height = 30,
      article = "https://en.wikipedia.org/wiki/Second"
    ),
    wikidata_binding(
      "Q1",
      "First",
      "Point(-76.6 39.3)",
      0.1,
      type = "tower"
    ),
    wikidata_binding(
      "Q2",
      "Second",
      "Point(-76.61 39.29)",
      0.2,
      type = "skyscraper",
      height = 45,
      article = "https://en.wikipedia.org/wiki/Second"
    ),
    wikidata_binding("Q3", "Third", "Point(-76.62 39.28)", 0.3)
  )

  data <- wikidata_bindings_to_df(bindings)

  expect_equal(data$qid, c("Q1", "Q2", "Q3"))
  expect_equal(data$label, c("First", "Second", "Third"))
  expect_equal(data$dist, c(100, 200, 300))
  expect_equal(data$height, c(NA, 45, NA))
  expect_equal(data$instance_of, c("tower", "building; skyscraper", NA))
  expect_equal(
    data$wikipedia,
    c(NA, "https://en.wikipedia.org/wiki/Second", NA)
  )
  expect_equal(data$lon, c(-76.6, -76.61, -76.62))
  expect_equal(data$lat, c(39.3, 39.29, 39.28))
})

test_that("wikidata_bindings_to_df handles empty results", {
  data <- wikidata_bindings_to_df(list())

  expect_equal(nrow(data), 0)
  expect_named(
    data,
    c(
      "qid",
      "label",
      "dist",
      "height",
      "instance_of",
      "wikipedia",
      "lon",
      "lat"
    )
  )
})
