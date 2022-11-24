#' Get data from an Airtable base and optionally convert to a sf object
#'
#' Get data from an Airtable base using the Airtable API and the httr2 package.
#' If the base includes coordinate fields/columns, optionally convert the data
#' to a simple feature object using [sfext::df_to_sf()] if `geometry = TRUE`.
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
#' Learn more about the Airtable API
#' <https://airtable.com/developers/web/api/introduction>
#'
#' @param base Airtable base identifier. Required.
#' @param table Airtable table name or identifier. Required.
#' @param view Airtable view identifier, Default: `NULL`
#' @param record Airtable record identifier, Default: `NULL`
#' @param fields Fields to return from Airtable base, Default: `NULL`
#' @param filter Filter to apply to records, Note: This parameter is a
#'   placeholder and is not currently implemented. Default: `NULL`
#' @param sort Field to sort by, Default: `NULL`
#' @param desc If `TRUE`, sort in descending order, Default: `FALSE`
#' @param max_records Maximum number of records to return, Default: `NULL`. If
#'   max_records is larger than 100, the offset parameter is used to return
#'   multiple pages of a results in a single dataframe.
#' @param per_page Max records to return per page, Default: `NULL`
#' @param cell_format Cell format for "Link to another record" fields (either
#'   "json" (unique ID) or "string" (displayed character string)), Default:
#'   'json'
#' @param tz,locale Time zone and locale, Defaults: `NULL`
#' @param fields_by_id If `TRUE`, return fields by id, Default: `FALSE`
#' @param offset Offset parameter, Default: `NULL`
#' @param token,type API token and type, token defaults to `NULL` and type to
#'   `"AIRTABLE_TOKEN"` (same as `get_access_token(type =
#'   "AIRTABLE_TOKEN")`).
#' @param geometry If `TRUE`, convert data into a simple feature object.
#'   Defaults to `FALSE`.
#' @param resp_type Response type to return, Default: "records". Set resp_type
#'   to "resp" to return the API response without any additional formatting or
#'   conversion.
#' @inheritParams sfext::df_to_sf
#' @inheritParams get_location_data
#' @inheritParams format_data
#' @rdname get_airtable_data
#' @export
#' @importFrom sfext df_to_sf
get_airtable_data <- function(base,
                              table,
                              view = NULL,
                              record = NULL,
                              fields = NULL,
                              # filterByFormula = NULL, # SQL style query
                              filter = NULL,
                              sort = NULL,
                              desc = FALSE,
                              max_records = NULL, # integer
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
                              resp_type = "records") {
  req <-
    req_airtable(
      base = base,
      table = table,
      record = record,
      fields = fields,
      filter = filter,
      sort = sort,
      desc = desc,
      view = view,
      max_records = max_records,
      per_page = per_page,
      cell_format = cell_format,
      tz = tz,
      locale = locale,
      fields_by_id = fields_by_id,
      offset = offset,
      token = token,
      type = type
    )

  data <-
    resp_airtable(
      req,
      resp_type = resp_type,
      max_records = max_records
    )

  if (resp_type == "resp") {
    return(data)
  }

  # if (label) {
  #   labels <- names(data)
  # } else {
  #   labels <- NULL
  # }

  data <-
    format_data(
      data,
      fix_date = FALSE,
      .name_repair = name_repair # ,
      # label = label # ,
      # labels = labels
    )

  if (!geometry) {
    return(data)
  }

  data <-
    sfext::df_to_sf(
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
#' @export
get_airtable_metadata <- function(base = NULL,
                                  token = NULL,
                                  type = "AIRTABLE_TOKEN",
                                  resp_type = "tables") {
  base_url <- "https://api.airtable.com/v0/meta/bases/"
  req <- httr2::request(base_url)

  check_required(base)
  check_starts_with(base, "app")

  req <-
    httr2::req_url_path_append(
      req, base, resp_type
    )

  req <- req_auth_airtable(req, type = type)

  resp_airtable(req, resp_type = resp_type)
}

#' List or retrieve records from an Airtable base
#'
#' @noRd
#' @importFrom httr2 request req_url_path_append req_url_query
req_airtable <- function(base,
                         table,
                         record = NULL,
                         view = NULL,
                         sort = NULL,
                         max_records = 100,
                         per_page = NULL,
                         tz = NULL,
                         locale = NULL,
                         token = NULL,
                         type = "AIRTABLE_API_KEY",
                         fields_by_id = FALSE,
                         fields = NULL,
                         filter = NULL,
                         desc = FALSE,
                         cell_format = NULL,
                         offset = NULL,
                         base_url = "https://api.airtable.com/v0/") {
  req <- httr2::request(base_url)

  check_required(base)
  check_starts_with(base, "app")
  check_required(table)
  # check_starts_with(table, "tbl")

  req <-
    httr2::req_url_path_append(
      req, base, table
    )

  if (!is.null(record)) {
    check_starts_with(record, "rec")

    req <-
      httr2::req_url_path_append(
        req, record
      )

    return(req_auth_airtable(req, token, type))
  }

  if (!is.null(sort)) {
    sort <- glue('field: "{sort}"')

    if (desc) {
      sort <- glue('{sort}, direction: "desc"')
    }

    sort <- glue("[{{sort}}]")
  }

  cellFormat <- match.arg(cell_format, c("json", "string"))

  if (cellFormat == "string") {
    tz <- Sys.timezone()
    locale <- Sys.getlocale("LC_TIME")
  }

  if (!fields_by_id) {
    fields_by_id <- NULL
  }

  req <-
    httr2::req_url_query(
      req,
      view = view,
      sort = sort,
      cellFormat = cellFormat,
      timeZone = tz,
      userLocale = locale,
      maxRecords = max_records,
      pageSize = per_page,
      returnFieldsByFieldId = fields_by_id
    )

  if (!is.null(fields)) {
    for (field in fields) {
      req <-
        httr2::req_url_query(
          req,
          field = glue("[{field}]")
        )
    }
  }

  req_auth_airtable(req, token, type)
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
  req <-
    httr2::req_auth_bearer_token(
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

  resp <-
    httr2::resp_body_json(
      resp = req_getdata(req),
      simplifyVector = simplifyVector
    )

  if (resp_type == "record") {
    is_pkg_installed("tidyr")
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
  data <-
    switch(name,
      "resp" = resp,
      "record" = tidyr::pivot_wider(tibble::enframe(resp)),
      "records" = tibble::as_tibble(resp[["fields"]]),
      "tables" = tibble::as_tibble(resp)
    )

  skip_offset <-
    any(
      c(
        is.null(max_records),
        max_records <= 100,
        !rlang::has_name(resp, "offset")
      )
    )

  if (skip_offset) {
    return(data)
  }

  offset_req <-
    httr2::req_url_query(
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
