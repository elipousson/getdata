#' Get data from an open data portal (Socrata) for a location
#'
#' get_socrata_data is get_open_data with source_type set to "socrata" (the only
#' currently supported option). get_open_data can return a selected dataset
#' using Socrata Query Language (SoQL) parameters as a tibble or sf object.
#' Details on SoQL queries are found in the Socrata API documentation
#' <https://dev.socrata.com/docs/queries/>.
#'
#' @param data A data set identifier (known as a resource for Socrata) or a url
#'   for an individual dataset. If data is set to "list" and a valid source_url
#'   is provided, the function returns a list of all available resources. If
#'   data is a url, source_url must be NULL. [get_socrata_metadata] requires the
#'   data parameter.
#' @param source_url A data source url. For Socrata, this should the base url
#'   for the open data portal.
#' @param source_type Data source type; defaults to "socrata" which is currently
#'   the only supported option.
#' @inheritParams sfext::st_bbox_ext
#' @param name,name_col Name of column in Socrata data resource with
#'   location names (e.g. County) and name of location to return.
#' @param select Names of of columns to return or transformed, equivalent to a
#'   SELECT in SQL. Passed to SODA $select parameter, see
#'   <https://dev.socrata.com/docs/queries/select.html> for more information.
#' @param where Condition to filters the rows to return, equivalent to WHERE in
#'   SQL. Passed to the SODA $where parameter, see
#'   <https://dev.socrata.com/docs/queries/where.html> for more information.
#' @param query A full SoQL query string, all as one
#'   parameter. Passed to the SODA $query parameter, see
#'   <https://dev.socrata.com/docs/queries/query.html> for more information.
#' @param location_col Name of a "location" or "point" type column in a Socrata dataset.
#' @param geometry If `TRUE` and coords are provided, return a
#'   `sf` object. Default `FALSE`.
#' @param token,type Access token or API Key and token type (name used to store
#'   token in .Renvironment). A token may be required to access data from
#'   Socrata and other open data portals but can be stored as an environment
#'   variable with [set_access_token].
#' @param quiet If `TRUE`, suppress messages when downloading data. Defaults to
#'   `FALSE`.
#' @inheritParams get_location_data
#' @inheritParams sfext::df_to_sf
#' @inheritParams format_data
#' @example examples/get_open_data.R
#' @export
#' @importFrom janitor make_clean_names
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom sfext st_bbox_ext df_to_sf
#' @importFrom cli cli_alert_info cli_dl
#' @importFrom tibble as_tibble
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
                          location_col = NULL,
                          coords = c("longitude", "latitude"),
                          geometry = FALSE,
                          token = NULL,
                          type = NULL,
                          from_crs = 4326,
                          crs = NULL,
                          clean_names = TRUE,
                          quiet = FALSE,
                          .name_repair = janitor::make_clean_names) {
  cliExtras::cli_abort_ifnot(
    c(
      "{.arg source_url} must be a valid URL or, if {.arg data} is a url, {.arg source_url} must be NULL."
    ),
    condition = is_url(source_url) || (is_url(data) && is.null(source_url))
  )

  cli_quiet(quiet)

  source_type <- tolower(source_type)

  cliExtras::cli_abort_ifnot(
    c("{.arg source_type} must be {.val socrata}.",
      "i" = "Socrata is currently the only supported open data source for this function.
      Other open data access options (e.g. CKAN, Flat Data) may be added in the future."
    ),
    condition = (source_type == "socrata")
  )

  if ((source_type == "socrata") && (data == "list")) {
    return(list_socrata_data(source_url))
  }

  # Get an API key if type is provided
  if (!is.null(type)) {
    token <- get_access_token(token = token, type = type)
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

  if (source_type == "socrata") {
    if (!is.null(data)) {
      meta <- get_socrata_metadata(
        source_url = source_url,
        data = data
      )

      cli::cli_alert_info(
        "Downloading {.val {meta$name}} from {.url {meta$dataUri}}"
      )

      cli::cli_dl(
        items = c(
          `Attribution` = "{.val {meta$attribution}} / {.url {meta$attributionLink}}",
          `Data created` = "{.val {meta$createdAt}}",
          `Data updated` = "{.val {meta$dataUpdatedAt}}",
          `Category` = "{.val {meta$category}}",
          `License` = "{.val {meta$license}}"
        )
      )
    }

    url <- make_socrata_url(
      data = data,
      source_url = source_url,
      select = select,
      where = where,
      query = query,
      bbox = bbox,
      name_col = name_col,
      name = name,
      location_col = location_col,
      coords = coords
    )

    check_dev_installed("RSocrata", repo = "Chicago/RSocrata")
    # Download data from Socrata Open Data portal
    data <- tibble::as_tibble(
      RSocrata::read.socrata(url = url, app_token = token)
    )
  }

  if (clean_names) {
    .name_repair <- janitor::make_clean_names
  }

  if (!is.null(.name_repair)) {
    data <- use_name_repair(data, .name_repair)
  }

  if (!geometry) {
    return(data)
  }

  sfext::df_to_sf(x = data, coords = coords, from_crs = from_crs, crs = crs)
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
                             location_col = NULL,
                             coords = c("longitude", "latitude")) {
  # FIXME: Rewrite this to work with httr2
  # Make parameter calls
  if (!is.null(select)) {
    select <- paste0("$select=", paste0(select, collapse = ","))
  }

  if (!is.null(where)) {
    where <- glue("({where})")
  }

  where_name <- NULL

  if (!is.null(name_col) && !is.null(name)) {
    # FIXME: This probably fails with multiple names
    where_name <- glue("({name_col} like '{name}')")
  }

  where_bbox <- NULL

  if (!is.null(bbox)) {
    if (is.null(location_col)) {
      where_bbox <- glue("({sfext::sf_bbox_to_lonlat_query(bbox = bbox, coords = coords)})")
    } else {
      where_bbox <- glue("within_box({location_col},
             {bbox$ymax}, {bbox$xmax}, {bbox$ymin}, {bbox$xmin})")
    }
  }

  if (!all(sapply(c(bbox, name_col, where), is.null))) {
    where <- glue("$where=({paste0(c(where, where_bbox, where_name),
                  collapse = ' AND ')})")
  }

  if (!is.null(query)) {
    query <- paste0("$query=", query)
  }

  url <- make_dataset_url(data = data, source_url = source_url)

  # Append select, where, and query parameters to the url
  if (!any(sapply(c(select, where, query), is.null))) {
    url <- glue("{url}?{paste0(c(select, where, query), collapse = '&')}")
  }

  url
}

#' @noRd
make_dataset_url <- function(data = NULL,
                             source_url = NULL) {
  if (grepl("/dataset/", source_url) && is.null(data)) {
    # If data is null but source_url is for a dataset
    url <- source_url
  } else if (!grepl("/dataset/", source_url) && !is_url(data)) {
    # If data is an identifier and source_url is not a dataset
    source_url <- gsub("/$", "", source_url)
    url <- paste0(source_url, "/resource/", data, ".json")
  } else if (is_url(data) && !grepl(".json$", data)) {
    # If data is a url but does not a direct url for the json endpoint
    data <- gsub("/$", "", data)
    url <- paste0(data, ".json")
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
                             location_col = NULL,
                             coords = c("longitude", "latitude"),
                             geometry = FALSE,
                             token = NULL,
                             type = NULL,
                             from_crs = 4326,
                             crs = NULL,
                             clean_names = TRUE) {
  check_dev_installed("RSocrata", repo = "Chicago/RSocrata")

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
    location_col = location_col,
    coords = coords,
    geometry = geometry,
    token = token,
    type = type,
    from_crs = from_crs,
    crs = crs,
    clean_names = clean_names
  )
}

#' @rdname get_open_data
#' @name get_socrata_data
#' @export
#' @importFrom httr2 request req_url_path_append req_perform resp_body_json
get_socrata_metadata <- function(source_url = NULL,
                                 data = NULL) {
  req <- httr2::request(source_url)
  req <- httr2::req_url_path_append(req, "api/views/metadata/v1", data)
  httr2::resp_body_json(req_getdata(req))
}

#' @rdname get_open_data
#' @name list_socrata_data
#' @export
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom tibble as_tibble
list_socrata_data <- function(source_url) {
  req <- httr2::request(paste0(source_url, "/data.json"))
  resp <- httr2::resp_body_json(req_getdata(req), simplifyVector = TRUE)

  datasets <- tibble::as_tibble(resp[["dataset"]])
  datasets[["issued"]] <- as.POSIXct(datasets[["issued"]])
  datasets[["modified"]] <- as.POSIXct(datasets[["modified"]])

  datasets
}
