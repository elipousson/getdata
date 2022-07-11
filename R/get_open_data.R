#' Get data from an open data portal (Socrata) for a location
#'
#' get_socrata_data is get_open_data with source_type set to "socrata" (the only
#' currently supported option). get_open_data can return a selected dataset
#' using Socrata Query Language (SoQL) parameters as a tibble or sf object.
#' Details on SoQL queries are found in the Socrata API documentation
#' <https://dev.socrata.com/docs/queries/>.
#'
#' @param source_url A data source url. For Socrata, this should the base url
#'   for the open data portal.
#' @param source_type Data source type; defaults to "socrata" which is currently
#'   the only supported option.
#' @param data A data set identifier (known as a resource for Socrata). If data
#'   is set to "list" and a valid source_url is provided, the function returns a
#'   list of all available resources.
#' @inheritParams sfext::st_bbox_ext
#' @param name_col name of column in Socrata data with
#'   location names (e.g. County)
#' @param name location name to return
#' @param select SODA $select parameter. Set of columns to be returned, similar
#'   to a SELECT in SQL. <https://dev.socrata.com/docs/queries/select.html>
#' @param where SODA $where parameter. Filters the rows to be returned, similar
#'   to WHERE. <https://dev.socrata.com/docs/queries/where.html>
#' @param query SODA $query parameter. A full SoQL query string, all as one
#'   parameter. <https://dev.socrata.com/docs/queries/query.html>
#' @param geometry If `TRUE` and latitude/longitude columns available, return a
#'   [sf()] object. Default `FALSE`.
#' @param coords Name(s) of column with coordinate data, Default: c("longitude",
#'   "latitude")
#' @param token Access token or API Key; required to access data from Socrata.
#' @param location sf or bbox obkect
#' @param from_crs Coordinate reference system used to match the location CRS to
#'   the source data.
#' @param crs Coordinate reference system to return.
#' @inheritParams format_data
#' @example examples/get_open_data.R
#' @export
#' @importFrom cli cli_abort
#' @importFrom janitor clean_names
get_open_data <- function(data = NULL,
                          source_url = NULL,
                          source_type = "socrata",
                          select = NULL,
                          where = NULL,
                          query = NULL,
                          location = NULL,
                          dist = NULL,
                          diag_ratio = NULL,
                          unit = NULL,
                          asp = NULL,
                          name_col = NULL,
                          name = NULL,
                          coords = c("longitude", "latitude"),
                          geometry = FALSE,
                          token = NULL,
                          from_crs = 4326,
                          crs = NULL,
                          clean_names = TRUE) {
  cli_abort_ifnot(
    c("{.arg source_type} must be {.val socarata}.",
      "i" = "Socrata is currently the only supported open data source for this function.",
      " " = "Other open data access options (e.g. CKAN, Flat Data) may be added in the future.",
    ),
    condition = (source_type == "socrata")
  )

  is_pkg_installed("RSocrata")

  if (data == "list") {
    url <- source_url
    data_list <- RSocrata::ls.socrata(url = url)
    return(data_list)
  }

  # Check for an API key
  if (is.null(token) | (token == "")) {
    cli_abort("An API key or access token is required.")
  }

  # FIXME: Check on how to access the point or polygon data types via SODA
  # See <https://dev.socrata.com/docs/datatypes/point.html> for more information
  # Get adjusted bounding box if any adjustment variables provided
  bbox <- sfext::st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = from_crs
  )

  cli_abort_ifnot(
    c(
      "{.arg source_url} must be a URL.",
      "i" = "The provided {.arg source_url} is not valid: {.url {source_url}}"
    ),
    condition = is_url(source_url)
  )

  url <-
    make_socrata_url(
      data = data,
      source_url = source_url,
      select = select,
      where = where,
      query = query,
      bbox = bbox,
      name_col = name_col,
      name = name,
      coords = coords
    )

  # Download data from Socrata Open Data portal
  data <-
    dplyr::as_tibble(RSocrata::read.socrata(url = url, app_token = token))

  if (clean_names) {
    data <-
      janitor::clean_names(data)
  }

  if (geometry) {
    data <- sfext::df_to_sf(x = data, coords = coords, crs = crs)
  }

  data
}

#' @noRd
#' @importFrom glue glue
make_socrata_url <- function(data = NULL,
                             source_url = NULL,
                             select = NULL,
                             where = NULL,
                             query = NULL,
                             bbox = NULL,
                             name_col = NULL,
                             name = NULL,
                             coords = c("longitude", "latitude")) {

  # Make parameter calls
  if (!is.null(select)) {
    select <- paste0("$select=", select)
  }

  if (!is.null(where)) {
    if (!is.null(bbox)) {
      where <- paste0("$where=", paste0(c(where, sfext::sf_bbox_to_lonlat_query(bbox = bbox, coords = coords)), collapse = " AND "))
    } else if (!is.null(name_col) && !is.null(name)) {
      where <- paste0("$where=", paste0(c(where, glue::glue("{name_col} like '{name}'")), collapse = " AND "))
    } else {
      where <- paste0("$where=", where)
    }
  }

  if (!is.null(query)) {
    query <- paste0("$query=", query)
  }

  if (grepl("/dataset/", source_url) && is.null(data)) {
    url <- source_url
  } else if (!grepl("/dataset/", source_url)) {
    # Assemble url from data identifier, and select, where, and query parameters
    source_url <- gsub("/$", "", source_url)
    url <- paste0(source_url, "/resource/", data, ".json")

    if (!any(sapply(c(select, where, query), is.null))) {
      url <- paste0(url, "?", paste0(c(select, where, query), collapse = "&"))
    }
  }

  url
}

#' @rdname get_open_data
#' @name get_socrata_data
#' @export
get_socrata_data <- function(data = NULL,
                             source_url = NULL,
                             select = NULL,
                             where = NULL,
                             query = NULL,
                             location = NULL,
                             dist = NULL,
                             diag_ratio = NULL,
                             unit = NULL,
                             asp = NULL,
                             name_col = NULL,
                             name = NULL,
                             coords = c("longitude", "latitude"),
                             geometry = FALSE,
                             token = NULL,
                             from_crs = 4326,
                             crs = NULL,
                             clean_names = TRUE) {
  is_pkg_installed("RSocrata")

  get_open_data(
    data = data,
    source_url = source_url,
    source_type = "socrata",
    select = select,
    where = where,
    query = query,
    location = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    name_col = name_col,
    name = name,
    coords = coords,
    geometry = geometry,
    token = token,
    from_crs = from_crs,
    crs = crs,
    clean_names = clean_names
  )
}
