#' Get Wikipedia articles for a location
#'
#' Use the Wikipedia API geosearch API to get Wikipedia articles for a location.
#' See <https://www.mediawiki.org/wiki/Extension:GeoData> for more information.
#' Only returns Wikipedia articles with coordinates.
#'
#' @inheritParams overedge::st_bbox_ext
#' @inheritParams get_location_data
#' @param radius If `TRUE`, use dist as a buffer around the center of the
#'   location; defaults to `FALSE`
#' @param primary If `NULL`, search for primary coordinates. Set primary to "all"
#'   or "secondary" to search other coordinate types.
#' @param details Additional detailed to return with results. Options include
#'   "type", "name", "country", "region"; defaults to `NULL`.
#' @param limit Number of pages to return (max 500); defaults to 50
#' @param lang Language to search on Wikipedia; defaults to "en".
#' @param geometry If `TRUE`, return sf object. If `FALSE`, return data frame. Defaults to `FALSE`.
#' @rdname get_wiki_data
#' @export
#' @importFrom httr2 request req_url_query req_perform resp_body_json
get_wiki_data <- function(location,
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
                          crs = getOption("getdata.unit", 3857),
                          coords = getOption("getdata.coords", c("lon", "lat")),
                          remove_coords = TRUE,
                          clean_names = TRUE) {
  cli_abort_ifnot(
    c("{.arg list} must be {.val geosearch} or {.val resp}.",
      "i" = "Support for additional options may be added in the future."
    ),
    condition = (list %in% c("geosearch", "resp"))
  )

  check_required(location)

  if (radius) {
    gscoord <- st_gscoord(location)
    gsradius <- dist # FIXME: gsradius should be based on dist and/or diag_ratio and must take unit into account
    gsbbox <- NULL
  } else {
    gsbbox <- st_gsbbox(location, dist = dist, diag_ratio = diag_ratio, asp = asp, unit = unit)
    gscoord <- NULL
    gsradius <- NULL
  }

  req <-
    req_wiki_query(
      lang = lang,
      gsbbox = gsbbox,
      gscoord = gscoord,
      gsradius = gsradius,
      primary = primary,
      list = list,
      details = details,
      limit = limit
    )

  data <-
    resp_wiki_query(
      req,
      list = list
    )

  if (geometry) {
    # FIXME: Add clean_names support
    data <-
      overedge::df_to_sf(data, crs = crs, remove_coords = remove_coords)
  }

  format_data(data, clean_names = clean_names)
}


#' Create Wikipedia query request
#'
#' @noRd
req_wiki_query <- function(lang = NULL,
                           gsbbox = NULL,
                           gscoord = NULL,
                           gsradius = NULL,
                           primary = NULL,
                           list = "geosearch",
                           details = NULL,
                           limit = 50,
                           format = "json") {

  # <https://www.mediawiki.org/wiki/Extension:GeoData>
  check_null(lang)

  req <-
    httr2::request(glue("https://{lang}.wikipedia.org/w/api.php"))

  cli_abort_ifnot(
    condition = (check_null(gsbbox, null.ok = TRUE) | check_null(gscoord, null.ok = TRUE))
  )

  req <-
    httr2::req_url_query(
      req,
      action = "query",
      list = list,
      gsbbox = gsbbox,
      gscoord = gscoord,
      gsradius = gsradius
    )

  primary <- match.arg(primary, c("primary", "all", "secondary"))

    req <-
      httr2::req_url_query(
        req,
        gsprimary = paste0(primary, collapse = "|")
      )

  if (!is.null(details)) {
    details <-
      match.arg(details, c("type", "name", "country", "region"), several.ok = TRUE)

    req <-
      httr2::req_url_query(
        req,
        details = details
      )
  }

  if (limit > 500) {
    cli_warn(
      c("{.arg limit} can't be greater than {.val 500}.",
      "v" = "Resetting provided {.val {limit}} limit value to max.")
    )

    limit <- 500
  }

  req <-
    httr2::req_url_query(
      req,
      gslimit = limit,
      format = format
    )

  req_getdata_user(req)
}

#' Perform query and get response
#'
#' @noRd
resp_wiki_query <- function(req,
                            list = "geosearch",
                            simplifyVector = TRUE) {
  resp <- httr2::req_perform(req = req)

  resp <-
    httr2::resp_body_json(
      resp = resp,
      simplifyVector = TRUE
    )

  list <- match.arg(list, c("resp", "geosearch"))

  switch(list,
    "resp" = resp,
    "geosearch" = resp[["query"]][[list]]
  )
}

#' Make geospatial coordinate query
#'
#' @noRd
st_gscoord <- function(location, crs = 4326) {
  center <- overedge::get_coords(location, crs = crs)
  glue("{center$lat}|{center$lon}")
}

#' Make geospatial bbox query
#'
#' @noRd
st_gsbbox <- function(location, dist = NULL, diag_ratio = NULL, asp = NULL, unit = "meter", crs = 4326) {
  bbox <-
    overedge::st_bbox_ext(
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
