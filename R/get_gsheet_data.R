#' Use googlesheets4 to get a data frame or simple feature data from a Google Sheet
#'
#' @param url A Google Sheets url
#' @name get_gsheet_data
#' @inheritParams googlesheets4::read_sheet
#' @inheritParams sfext::df_to_sf
#' @inheritParams get_location_data
#' @param ask If `TRUE`, ask for the name of the Google Sheet to read if ss is
#'   not provided to [sfext::read_sf_gsheet].
#' @export
#' @importFrom cliExtras cli_ask
get_gsheet_data <- function(url,
                            sheet = NULL,
                            ss = NULL,
                            ask = FALSE,
                            geometry = FALSE,
                            location = NULL,
                            dist = getOption("getdata.dist"),
                            diag_ratio = getOption("getdata.diag_ratio"),
                            unit = getOption("getdata.unit", "meter"),
                            asp = getOption("getdata.asp"),
                            coords = getOption("getdata.coords", c("lon", "lat")),
                            remove_coords = TRUE,
                            address = getOption("getdata.address", "address"),
                            geo = FALSE,
                            from_crs = 4326,
                            clean_names = TRUE,
                            ...) {
  rlang::check_installed("googlesheets4")

  if (is.null(ss) && !is_missing(url)) {
    ss <- url
  }

  if (is.null(ss) && ask) {
    ss <- googlesheets4::gs4_find(
      cliExtras::cli_ask("What is the name of the Google Sheet to return?")
    )
  }

  data <- googlesheets4::read_sheet(ss = ss, sheet = sheet, ...)

  if (!geometry) {
    return(format_data(data, clean_names = clean_names))
  }

  get_location_data(
    location = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    data = data,
    coords = coords,
    remove_coords = remove_coords,
    from_crs = from_crs,
    address = address,
    geo = geo,
    crs = crs,
    clean_names = clean_names
  )
}
