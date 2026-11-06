#' Get Wikidata items for a location
#'
#' Use the Wikidata Query Service to get items with coordinates (property P625)
#' within a radius of a location. The search uses the `wikibase:around` SPARQL
#' service. See <https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual>
#' and <https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries> for
#' more information.
#'
#' Compared to [get_wiki_data()], which searches Wikipedia articles, this
#' function can filter by what an item is (`class`) and returns the item
#' identifier, the "instance of" labels, the Wikipedia article URL (if any), and
#' the height (property P2048), if available. Height values are reported by
#' Wikidata contributors, are missing for most items, and may not describe the
#' highest point of a structure, so check them before using them in an analysis.
#' This function does not return other properties or qualifiers. For those, use
#' a package like [tidywikidatar](https://edjnet.github.io/tidywikidatar/) with
#' the returned `qid` values.
#'
#' The search area is always a circle. If `dist` is `NULL` and `diag_ratio` is
#' `NULL`, the circle is the smallest circle around the center of the bounding
#' box for `location` that includes the corners of the bounding box. If
#' `location` is a point, `dist` or `diag_ratio` must be provided.
#'
#' @inheritParams get_wiki_data
#' @param dist,diag_ratio,unit Radius of the search area around the center of
#'   `location` and the unit for the distance. If `diag_ratio` is provided,
#'   `dist` is set to the diagonal distance of the location bounding box
#'   multiplied by the ratio.
#' @param class A character vector of Wikidata class identifiers (e.g.
#'   "Q41176" for building) used to filter items by "instance of" (property
#'   P31). Defaults to `NULL` for no filtering.
#' @param subclasses If `TRUE` (default), include items that are an instance of
#'   a subclass (property P279) of `class`. Searching subclasses can make the
#'   query slower.
#' @param limit Maximum number of items to return. Items are ordered by
#'   distance from the center of the location. Defaults to `NULL` for no limit.
#' @param lang Language for item labels and the Wikipedia article URL; defaults
#'   to "en". Items without a label in `lang` use the English label.
#' @param geometry If `TRUE` (default), return sf object. If `FALSE`, return a
#'   data frame with `lon` and `lat` columns.
#' @param crs Coordinate reference system for the returned data. Only used if
#'   `geometry = TRUE`.
#' @returns A data frame or `sf` object with one row per item and columns for
#'   `qid`, `label`, `dist` (in meters), `height` (in meters), `instance_of`,
#'   and `wikipedia`. If no items are found, an empty data frame is returned
#'   with a warning.
#' @example examples/get_wikidata.R
#' @export
#' @importFrom sfext get_coords convert_dist_units
get_wikidata <- function(
  location,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  class = NULL,
  subclasses = TRUE,
  limit = NULL,
  lang = getOption("getdata.lang", default = "en"),
  geometry = TRUE,
  crs = getOption("getdata.crs", 3857)
) {
  check_required(location)
  check_string(lang)

  center <- sfext::get_coords(location, crs = 4326)
  radius <- wikidata_radius(
    location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit
  )

  query <- wikidata_query_around(
    lon = center$lon,
    lat = center$lat,
    radius = radius,
    class = class,
    subclasses = subclasses,
    limit = limit,
    lang = lang
  )

  bindings <- resp_wikidata_query(req_wikidata_query(query))
  data <- wikidata_bindings_to_df(bindings)

  if (nrow(data) == 0) {
    cli_warn("No Wikidata items found for {.arg location}.")
    return(data)
  }

  if (!geometry) {
    return(data)
  }

  sf::st_transform(
    sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326),
    crs = crs
  )
}

#' Get search radius in meters
#'
#' @noRd
wikidata_radius <- function(
  location,
  dist = NULL,
  diag_ratio = NULL,
  unit = "meter",
  call = caller_env()
) {
  if (is.null(dist) && !is.null(diag_ratio)) {
    dist <- st_diagdist_meter(location, unit = unit) * diag_ratio
  }

  if (is.null(dist)) {
    diagdist <- st_diagdist_meter(location, unit = "meter")

    cli_abort_ifnot(
      c(
        "{.arg dist} or {.arg diag_ratio} must be provided for a point {.arg location}.",
        "i" = "Use a polygon {.arg location} to set the search area automatically."
      ),
      condition = (diagdist > 1),
      call = call
    )

    return(diagdist / 2)
  }

  as.numeric(sfext::convert_dist_units(dist, from = unit, to = "meter"))
}

#' Make a SPARQL query for items around a point
#'
#' @param lon,lat Longitude and latitude of the center point.
#' @param radius Radius in meters.
#' @noRd
wikidata_query_around <- function(
  lon,
  lat,
  radius,
  class = NULL,
  subclasses = TRUE,
  limit = NULL,
  lang = "en",
  call = caller_env()
) {
  check_string(lang, call = call)
  check_number_decimal(radius, min = 0, call = call)

  class_pattern <- ""
  if (!is.null(class)) {
    check_character(class, call = call)
    cli_abort_ifnot(
      "{.arg class} must be Wikidata identifiers like {.val Q41176}.",
      condition = all(grepl("^Q[0-9]+$", class)),
      call = call
    )

    class_pattern <- sprintf(
      "VALUES ?class { %s } ?item wdt:P31%s ?class .",
      paste0("wd:", class, collapse = " "),
      if (subclasses) "/wdt:P279*" else ""
    )
  }

  limit_pattern <- ""
  if (!is.null(limit)) {
    check_number_whole(limit, min = 1, call = call)
    limit_pattern <- sprintf("LIMIT %d", as.integer(limit))
  }

  # Fall back to English for items without a label in lang
  label_lang <- paste(unique(c(lang, "en")), collapse = ",")

  # wikibase:radius is in kilometers
  sprintf(
    paste(
      "SELECT ?item ?itemLabel ?coord ?dist ?height_m ?typeLabel ?article WHERE {",
      "  {",
      "    SELECT ?item (SAMPLE(?c) AS ?coord) (MIN(?d) AS ?dist) WHERE {",
      "      SERVICE wikibase:around {",
      "        ?item wdt:P625 ?c .",
      "        bd:serviceParam wikibase:center \"Point(%s %s)\"^^geo:wktLiteral .",
      "        bd:serviceParam wikibase:radius \"%s\" .",
      "        bd:serviceParam wikibase:distance ?d .",
      "      }",
      "      %s",
      "    } GROUP BY ?item ORDER BY ?dist %s",
      "  }",
      "  OPTIONAL { ?item p:P2048/psn:P2048/wikibase:quantityAmount ?height_m . }",
      "  OPTIONAL { ?item wdt:P31 ?type . }",
      "  OPTIONAL {",
      "    ?article schema:about ?item ;",
      "      schema:isPartOf <https://%s.wikipedia.org/> .",
      "  }",
      "  SERVICE wikibase:label { bd:serviceParam wikibase:language \"%s\" . }",
      "}",
      sep = "\n"
    ),
    format(lon, digits = 10),
    format(lat, digits = 10),
    format(radius / 1000, digits = 10),
    class_pattern,
    limit_pattern,
    lang,
    label_lang
  )
}

#' Create Wikidata Query Service request
#'
#' @noRd
#' @importFrom httr2 request req_url_query
req_wikidata_query <- function(
  query,
  endpoint = getOption(
    "getdata.wikidata_endpoint",
    default = "https://query.wikidata.org/sparql"
  )
) {
  httr2::req_url_query(
    httr2::request(endpoint),
    query = query,
    format = "json"
  )
}

#' Perform query and get SPARQL result bindings
#'
#' @noRd
#' @importFrom httr2 resp_body_json
resp_wikidata_query <- function(req) {
  resp <- httr2::resp_body_json(
    req_getdata(req),
    simplifyVector = FALSE
  )

  resp[["results"]][["bindings"]]
}

#' Convert SPARQL result bindings to a data frame with one row per item
#'
#' @noRd
wikidata_bindings_to_df <- function(bindings) {
  value <- function(name) {
    vapply(
      bindings,
      \(x) x[[name]][["value"]] %||% NA_character_,
      character(1)
    )
  }

  coord <- value("coord")

  data <- dplyr::tibble(
    qid = basename(value("item")),
    label = value("itemLabel"),
    dist = as.numeric(value("dist")) * 1000,
    height = as.numeric(value("height_m")),
    instance_of = value("typeLabel"),
    wikipedia = value("article"),
    lon = as.numeric(sub("^Point\\((\\S+) (\\S+)\\)$", "\\1", coord)),
    lat = as.numeric(sub("^Point\\((\\S+) (\\S+)\\)$", "\\2", coord))
  )

  data <- dplyr::group_by(data, qid)
  data <- dplyr::summarise(
    data,
    label = dplyr::first(label),
    dist = dplyr::first(dist),
    height = if (all(is.na(height))) NA_real_ else max(height, na.rm = TRUE),
    instance_of = if (all(is.na(instance_of))) {
      NA_character_
    } else {
      paste(unique(instance_of[!is.na(instance_of)]), collapse = "; ")
    },
    wikipedia = dplyr::first(wikipedia[!is.na(wikipedia)]),
    lon = dplyr::first(lon),
    lat = dplyr::first(lat),
    .groups = "drop"
  )

  dplyr::arrange(data, dist)
}
