#' Get data from an Airtable base and optionally convert to a sf object
#'
#' `r lifecycle::badge("experimental")`
#' Get data from an Airtable base using the Airtable API, a development version
#' of the [rairtable package](https://github.com/elipousson/rairtable/tree/dev),
#' and the httr2 package. If the base includes coordinate fields/columns,
#' optionally convert the data to a simple feature object using
#' [sfext::df_to_sf()] if `geometry = TRUE`.
#'
#' This function an Airtable personal access token which you can create at
#' <https://airtable.com/create/tokens> and save to your local environment with
#' `set_access_token(token = <YOUR_PERSONAL_ACCESS_TOKEN>, type =
#' "AIRTABLE_TOKEN")`. The function previously required an Airtable API key
#' which you can set using `set_access_token(token = <YOUR_API_KEY>, type =
#' "AIRTABLE_API_KEY")`. However, Airtable is in the process of deprecating user
#' API keys.
#'
#' [get_airtable_data()] requires a scope that includes `data.records:read` and
#' [get_airtable_metadata()] a scope including `schema.bases:read`.
#'
#' As of May 2023, this function depends on the dev branch of my fork of the
#' rairtable package. I expect this dependency to switch back to the rairtable
#' package when the fork is merged.
#'
#' Learn more about the Airtable API
#' <https://airtable.com/developers/web/api/introduction>
#'
#'
#' @param base Airtable base id starting with with "app". Optional if url or
#'   airtable are supplied. If base is an Airtable url, the table and view are
#'   replaced based on the values parsed from the url. Required.
#' @param table Airtable table id or name. If table is a table ID it is a string
#'   starting with "viw". Optional only if base is a url.
#' @inheritParams rairtable::list_records
#' @param per_page Passed to page_size parameter of
#'   [rairtable::list_records()]
#' @param filter Placeholder for filterByFormula API parameter allowing use of
#'   SQL style queries to filter data. Not yet implemented.
#' @param record Airtable record identifier, Default: `NULL` Superseded by
#'   [rairtable::list_records()] function.
#' @param offset Offset parameter, Default: `NULL`
#' @param token,type API token and type, token defaults to `NULL` and type to
#'   `"AIRTABLE_TOKEN"` (same as `get_access_token(type =
#'   "AIRTABLE_TOKEN")`).
#' @param desc Deprecated. Sort results in descending order. Replaced by
#'   direction parameter.
#' @param geometry If `TRUE`, convert data into a simple feature object.
#'   Defaults to `FALSE`.
#' @param resp_type Response type to return, Reprecated. Previously, set resp_type
#'   to "resp" to return the API response without any additional formatting or
#'   conversion.
#' @inheritParams sfext::df_to_sf
#' @inheritParams get_location_data
#' @inheritParams format_data
#' @inheritDotParams rairtable::list_records
#' @rdname get_airtable_data
#' @export
#' @importFrom sfext df_to_sf
get_airtable_data <- function(base,
                              table = NULL,
                              view = NULL,
                              record = deprecated(),
                              fields = NULL,
                              # filterByFormula = NULL, # SQL style query
                              filter = NULL,
                              sort = NULL,
                              direction = "asc",
                              desc = deprecated(),
                              max_records = 100,
                              per_page = NULL,
                              cell_format = "json",
                              tz = NULL,
                              locale = NULL,
                              fields_by_id = FALSE,
                              offset = NULL,
                              geometry = FALSE,
                              location = NULL,
                              dist = getOption("getdata.dist"),
                              diag_ratio = getOption("getdata.diag_ratio"),
                              unit = getOption("getdata.unit", "meter"),
                              asp = getOption("getdata.asp"),
                              crs = getOption("getdata.crs", 3857),
                              coords = getOption("getdata.coords", c("lon", "lat")),
                              from_crs = getOption("getdata.from_crs", 4326),
                              remove_coords = TRUE,
                              address = getOption("getdata.address", "address"),
                              geo = FALSE,
                              name_repair = janitor::make_clean_names,
                              # label = FALSE,
                              token = NULL,
                              type = "AIRTABLE_TOKEN",
                              resp_type = deprecated(),
                              ...) {
  check_installed("rairtable")

  if (is_url(base)) {
    ids <- rairtable::parse_airtable_url(base)
    base <- ids$base
    table <- ids$table
    view <- ids$view
  }

  data <- rairtable::list_records(
    base = base,
    table = table,
    fields = fields,
    sort = sort,
    direction = direction,
    view = view,
    max_records = max_records,
    page_size = per_page,
    cell_format = cell_format,
    tz = tz,
    locale = locale,
    fields_by_id = fields_by_id,
    offset = offset,
    token = get_access_token(
      token = token,
      type = type
    ),
    type = type,
    .name_repair = name_repair,
    ...
  )



  # if (label) {
  #   labels <- names(data)
  # } else {
  #   labels <- NULL
  # }

  data <- format_data(
    data,
    fix_date = FALSE,
    .name_repair = NULL # ,
    # label = label # ,
    # labels = labels
  )

  if (!geometry) {
    return(data)
  }

  data <- sfext::df_to_sf(
    data,
    coords = coords,
    remove_coords = remove_coords,
    from_crs = from_crs,
    address = address,
    geo = geo,
    crs = crs
  )

  get_location_data(
    location = location,
    data = data,
    dist = dist,
    diag_ratio = diag_ratio,
    asp = asp,
    unit = unit
  )
}



#' @name get_airtable_metadata
#' @rdname get_airtable_data
#' @param fields For [get_airtable_metadata()], if `TRUE`, return the fields
#'   column from the data.frame with the Airtable response. If only one table is
#'   provided, fields are returned as a data frame. Ignored if table is `NULL`
#' @export
#' @importFrom rlang is_false
get_airtable_metadata <- function(base,
                                  table = NULL,
                                  token = NULL,
                                  type = "AIRTABLE_TOKEN",
                                  resp_type = "tables",
                                  fields = FALSE) {
  base_url <- "https://api.airtable.com/v0/meta/bases/"
  req <- httr2::request(base_url)

  check_required(base)
  check_starts_with(base, "app")

  req <- httr2::req_url_path_append(
    req, base, resp_type
  )

  req <- req_auth_airtable(req, type = type)

  resp <- resp_airtable(req, resp_type = resp_type)

  if (is_null(table)) {
    return(resp)
  }

  table <- arg_match(table, resp[["id"]], multiple = TRUE)

  table_resp <- resp[resp[["id"]] %in% table, ]

  if (is_false(fields)) {
    return(table_resp)
  }

  if (nrow(table_resp) == 1) {
    return(table_resp[["fields"]][[1]])
  }

  table_resp[["fields"]]
}

#' Set rate limit, set user agent, and authenticate Airtable request with
#' key/token
#'
#' @noRd
req_auth_airtable <- function(req,
                              token = NULL,
                              type = "AIRTABLE_API_KEY",
                              rate = 5 / 1,
                              realm = NULL) {
  req <- httr2::req_auth_bearer_token(
    req = req,
    token = get_access_token(token = token, type = type)
  )

  httr2::req_throttle(
    req = req,
    rate = rate,
    realm = realm
  )
}

#' Perform request and get data based on response type
#'
#' @noRd
#' @importFrom httr2 resp_body_json req_url_query
#' @importFrom rlang is_empty has_name
#' @importFrom tibble enframe as_tibble
#' @importFrom dplyr bind_rows
resp_airtable <- function(req,
                          simplifyVector = TRUE,
                          resp_type = "records",
                          max_records = 100) {
  resp_type <- match.arg(resp_type, c("resp", "fields", "records", "tables"))

  resp <- httr2::resp_body_json(
    resp = req_getdata(req),
    simplifyVector = simplifyVector
  )

  if (resp_type == "record") {
    rlang::check_installed("tidyr")
  }

  if (resp_type != "resp") {
    resp <- resp[[resp_type]]

    cli_abort_ifnot(
      c("{resp_type} can't be found for this request:",
        "i" = "{.url {req$url}}"
      ),
      condition = !rlang::is_empty(resp)
    )
  }

  # Add offset checks
  data <- switch(resp_type,
    "resp" = resp,
    "record" = tidyr::pivot_wider(tibble::enframe(resp)),
    "records" = tibble::as_tibble(resp[["fields"]]),
    "tables" = tibble::as_tibble(resp)
  )

  skip_offset <- any(
    c(
      is.null(max_records),
      max_records <= 100,
      !rlang::has_name(resp, "offset")
    )
  )

  if (skip_offset) {
    return(data)
  }

  offset_req <- httr2::req_url_query(
    req,
    offset = resp$offset
  )

  dplyr::bind_rows(
    data,
    resp_airtable(
      offset_req,
      simplifyVector,
      resp_type,
      max_records = max_records - 100
    )
  )
}
