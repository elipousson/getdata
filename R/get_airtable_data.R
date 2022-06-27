
get_airtable_data <- function(base,
                              table,
                              record = NULL,
                              fields = NULL,
                              # filterByFormula = NULL, # SQL style query
                              filter = NULL,
                              sort = NULL,
                              desc = FALSE,
                              view = NULL,
                              max_records = NULL, # integer
                              per_page = NULL,
                              cell_format = "json",
                              tz = NULL,
                              locale = NULL,
                              fields_by_id = FALSE,
                              offset = NULL,
                              token = NULL,
                              list = "records",
                              geometry = TRUE,
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
                              clean_names = TRUE,
                              label = TRUE) {


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
      token = token
      )

  data <-
    resp_airtable(req, list = list)

  if (list == "resp") {
    return(data)
  }


  if (label) {
    labels <- names(data)
  } else {
    labels <- NULL
  }

  data <-
    format_data(
      dplyr::as_tibble(data),
      clean_names = clean_names,
      label = label,
      labels = labels
    )

  if (!geometry) {
    return(data)
  }

  get_location_data(
    location = location,
    data = data,
    dist = dist,
    diag_ratio = diag_ratio,
    asp = asp,
    unit = unit,
    coords = coords,
    remove_coords = remove_coords,
    from_crs = from_crs,
    address = address,
    geo = geo,
    crs = crs
  )

}


#' List or retrieve records from an Airtable base
#'
#' @noRd
req_airtable <- function(base,
                         table,
                         record = NULL,
                         view = NULL,
                         sort = NULL, max_records = NULL, per_page = NULL, tz = NULL, locale = NULL, token = NULL, fields_by_id = FALSE) {
  req <-
    httr2::request(
      "https://api.airtable.com/v0/"
    )

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

    return(req_auth_airtable(req, token))
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
          field =  glue("[{field}]")
        )
    }
  }

  req_auth_airtable(req, token)
}

#' Set rate limit, set user agent, and authenticate Airtable request with key/token
#'
#' @noRd
req_auth_airtable <- function(req, token = NULL, type = "AIRTABLE_API_KEY", realm = NULL) {

  req <-
    httr2::req_throttle(
      req = req,
      rate = 5 / 1,
      realm = realm
    )

  httr2::req_auth_bearer_token(
    req = req_getdata_user(req),
    token = get_access_token(token = token, type = type)
  )
}

#' Perform request and get data based on list
#' @noRd
resp_airtable <- function(req, simplifyVector = TRUE, list = "records") {

  list <- match.arg(list, c("resp", "fields", "records"))

  resp <-
    httr2::resp_body_json(
      resp = httr2::req_perform(req),
      simplifyVector = simplifyVector
      )

  if (list == "record") {
    is_pkg_installed("tidyr")
  }

  # Add offset checks
  switch (list,
          "resp" = resp,
          "record" = tidyr::pivot_wider(tibble::enframe(resp[[list]])),
          "records" = tibble::as_tibble(resp[[list]][["fields"]])
  )
}
