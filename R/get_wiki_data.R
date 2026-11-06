#' Get Wikipedia articles for a location
#'
#' Use the Wikipedia geosearch API to get Wikipedia articles near a location.
#' Only articles with coordinates are returned. See the GeoData extension
#' <https://www.mediawiki.org/wiki/Extension:GeoData> and the API documentation
#' for the geosearch module
#' <https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bgeosearch>
#' for more information.
#'
#' This function only covers the geosearch module. It returns the page ID,
#' title, coordinates, and distance from the search point for each article (plus
#' optional coordinate details) but not article text, extracts, images,
#' categories, or Wikidata properties such as height or construction date. It
#' also only searches one language edition of Wikipedia at a time and does not
#' search other Wikimedia projects (e.g. Wikidata or Wikimedia Commons).
#'
#' For this function, `location` can be either an `sf`, `sfc`, or `bbox` object
#' or the title of a Wikipedia article with a related location. If `location` is
#' an `sf` object, the search is made using the bounding box of the location
#' (adjusted by `dist`, `diag_ratio`, and `asp`) or, if `radius = TRUE`, a
#' circle around the center of the location. The API limits the search area: a
#' radius must be between 10 and 10,000 meters and a bounding box can't be much
#' larger than about 25 kilometers across.
#'
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams get_location_data
#' @param radius If `TRUE`, use dist as a buffer around the center of the
#'   location; defaults to `FALSE`. If numeric, the value is used as `dist` and
#'   `radius` is set to `TRUE`.
#' @param primary If `NULL`, search for primary coordinates. Set primary to "all"
#'   or "secondary" to search other coordinate types.
#' @param details Additional details to return with results. Options include
#'   "name", "type", "dim", "scale", "region", "country", and "globe"; defaults
#'   to `NULL`.
#' @param limit Number of pages to return (max 500); defaults to 50
#' @param list method to use for query; "geosearch" returns data, "resp" returns response
#' @param lang Language to search on Wikipedia; defaults to "en".
#' @param geometry If `TRUE` (default), return sf object. If `FALSE`, return data frame.
#' @rdname get_wiki_data
#' @inheritParams format_data
#' @inheritParams sfext::df_to_sf
#' @seealso `GNfindNearbyWikipedia()` and `GNwikipediaBoundingBox()` functions
#'   in the [geonames](https://docs.ropensci.org/geonames/) package
#' @example examples/get_wiki_data.R
#' @export
#' @importFrom sfext is_sf sf_bbox_diagdist as_bbox convert_dist_units df_to_sf
get_wiki_data <- function(
  location,
  radius = FALSE,
  primary = NULL,
  details = NULL,
  limit = 50,
  list = "geosearch",
  lang = getOption("getdata.lang", default = "en"),
  geometry = TRUE,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  asp = getOption("getdata.asp"),
  crs = getOption("getdata.crs", 3857),
  remove_coords = TRUE,
  clean_names = TRUE
) {
  cli_abort_ifnot(
    c(
      "{.arg list} must be {.val geosearch} or {.val resp}.",
      "i" = "Support for additional options may be added in the future."
    ),
    condition = (list %in% c("geosearch", "resp"))
  )

  check_required(location)

  gsbbox <- NULL
  gscoord <- NULL
  gsradius <- NULL
  gspage <- NULL

  if (sfext::is_sf(location, ext = TRUE)) {
    if (is.numeric(radius)) {
      dist <- radius
      cli::cli_bullets(
        c(
          "i" = "Using {.arg radius} value of {.val {dist}} for {.arg dist}.",
          "v" = "Setting {.arg radius} to TRUE."
        )
      )
      radius <- TRUE
    }

    if (radius) {
      gscoord <- st_gscoord(location)

      if (is.null(dist) && !is.null(diag_ratio)) {
        dist <- st_diagdist_meter(location, unit = unit) * diag_ratio
      }

      gsradius <- as.integer(round(sfext::convert_dist_units(
        dist,
        from = unit,
        to = "meter"
      )))

      cli_abort_ifnot(
        "radius {.arg dist} must be greater than 1.",
        condition = (gsradius >= 1)
      )
    } else {
      gsbbox <- st_gsbbox(
        location,
        dist = dist,
        diag_ratio = diag_ratio,
        asp = asp,
        unit = unit
      )
    }
  } else {
    check_character(location)
    gspage <- location
  }

  req <- req_wiki_query(
    lang = lang,
    gsbbox = gsbbox,
    gscoord = gscoord,
    gsradius = gsradius,
    gspage = gspage,
    primary = primary,
    list = list,
    details = details,
    limit = limit
  )

  data <- resp_wiki_query(
    req,
    list = list
  )

  if (list == "resp") {
    return(data)
  }

  if (geometry) {
    data <- sfext::df_to_sf(data, crs = crs, remove_coords = remove_coords)
  }

  format_data(data, clean_names = clean_names)
}


#' Create Wikipedia query request
#'
#' @noRd
#' @importFrom httr2 request req_url_query
req_wiki_query <- function(
  lang = NULL,
  gsbbox = NULL,
  gscoord = NULL,
  gsradius = NULL,
  gspage = NULL,
  primary = NULL,
  list = "geosearch",
  details = NULL,
  limit = 50,
  format = "json",
  call = caller_env()
) {
  # <https://www.mediawiki.org/wiki/Extension:GeoData>
  check_string(lang, call = call)

  req <- httr2::request(glue("https://{lang}.wikipedia.org/w/api.php"))

  cli_abort_ifnot(
    "One of {.arg gsbbox}, {.arg gscoord}, or {.arg gspage} must be supplied.",
    condition = !is.null(c(gsbbox, gscoord, gspage)),
    call = call
  )

  # The API list module is always "geosearch"; list = "resp" only changes what
  # resp_wiki_query() returns.
  req <- httr2::req_url_query(
    req,
    action = "query",
    list = "geosearch",
    gsbbox = gsbbox,
    gscoord = gscoord,
    gsradius = gsradius,
    gspage = gspage
  )

  primary <- primary %||% "primary"
  primary <- arg_match(
    primary,
    c("primary", "all", "secondary"),
    error_call = call
  )

  req <- httr2::req_url_query(
    req,
    gsprimary = paste0(primary, collapse = "|")
  )

  if (!is.null(details)) {
    details <- arg_match(
      details,
      c("name", "type", "dim", "scale", "region", "country", "globe"),
      multiple = TRUE,
      error_call = call
    )

    req <- httr2::req_url_query(
      req,
      gsprop = details,
      .multi = "pipe"
    )
  }

  if (limit > 500) {
    cli_warn(
      c(
        "{.arg limit} can't be greater than {.val 500}.",
        "v" = "Resetting provided {.val {limit}} limit value to max."
      )
    )

    limit <- 500
  }

  httr2::req_url_query(
    req,
    gslimit = limit,
    format = format
  )
}

#' Perform query and get response
#'
#' @noRd
#' @importFrom httr2 resp_body_json
resp_wiki_query <- function(
  req,
  list = "geosearch",
  simplifyVector = TRUE,
  call = caller_env()
) {
  resp <- httr2::resp_body_json(
    resp = req_getdata(req),
    simplifyVector = simplifyVector
  )

  if (has_name(resp, "error")) {
    cli_abort(
      'Wikipedia {.arg {list}} query can\'t be completed due to an error: {resp[["error"]][["info"]]}.',
      call = call
    )
  }

  list <- arg_match(list, c("resp", "geosearch"))

  switch(list, "resp" = resp, "geosearch" = resp[["query"]][[list]])
}

#' Make geospatial coordinate query
#'
#' @noRd
#' @importFrom sfext get_coords
st_gscoord <- function(location, crs = 4326) {
  center <- sfext::get_coords(location, crs = crs)
  glue("{center$lat}|{center$lon}")
}

#' Make geospatial bbox query
#'
#' @noRd
#' @importFrom sfext st_bbox_ext
st_gsbbox <- function(
  location,
  dist = NULL,
  diag_ratio = NULL,
  asp = NULL,
  unit = "meter",
  crs = 4326
) {
  bbox <- sfext::st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    asp = asp,
    unit = unit,
    crs = crs
  )

  # top|left|bottom|right order
  paste0(
    c(
      bbox["ymax"],
      bbox["xmin"],
      bbox["ymin"],
      bbox["xmax"]
    ),
    collapse = "|"
  )
}

#' Get the diagonal distance of a location bounding box
#'
#' Uses geodesic distance between bounding box corners so the result is in
#' `unit` even for lon/lat or projected input.
#'
#' @noRd
st_diagdist_meter <- function(location, unit = "meter") {
  bbox <- sf::st_bbox(
    sf::st_transform(sf::st_as_sfc(sfext::as_bbox(location)), 4326)
  )

  corners <- sf::st_sfc(
    sf::st_point(c(bbox[["xmin"]], bbox[["ymin"]])),
    sf::st_point(c(bbox[["xmax"]], bbox[["ymax"]])),
    crs = 4326
  )

  as.numeric(
    sfext::convert_dist_units(
      as.numeric(sf::st_distance(corners[1], corners[2])),
      from = "meter",
      to = unit
    )
  )
}
