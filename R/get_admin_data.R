#' Get U.S. State and County boundary data (1:5 mi scale) for a location
#'
#' Get U.S. states and counties from packaged [us_counties] or [us_states] data.
#' See [get_tigris_data()] for more options.
#'
#' @param location A `sf`, `sfc`, or `bbox` object or a character string that
#'   matches a geoid, name, abb, or statefp for [us_states] or [us_counties].
#' @param class Class of data to return, "df" (default), "sf", "bbox", or "sfc"
#' @inheritParams get_location_data
#' @inheritParams rlang::abort
#' @param ... Additional parameters including geoid, name, state, or county.
#'   These additional identifier parameters are only used if location is `NULL`.
#' @name get_admin_data
#' @example examples/get_admin_data.R
NULL

#' @rdname get_admin_data
#' @name get_states
#' @export
#' @importFrom dplyr case_when
get_states <- function(location = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL,
                       class = "df",
                       call = caller_env(),
                       ...) {
  location <- location %||% us_admin_dots_to_location(...)

  if (!is_sf(location, ext = TRUE)) {
    lookup <-
      c(
        grep(x = us_states$geoid, pattern = location),
        grep(x = us_states$name, pattern = location),
        grep(x = us_states$abb, pattern = location)
      )

    condition <- !is_empty(lookup)
  } else {
    lookup <-
      location_intersects_us_admin(
        us_states,
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp
      )

    condition <- any(lookup)
  }

  cli_abort_ifnot(
    "No U.S. states can be found using the provided {.arg location}, {.arg geoid}, or {.arg name}: {.val {location}}",
    condition = condition
  )

  convert_us_admin_class(us_states[lookup, ], class = class)
}

#' @rdname get_admin_data
#' @name get_counties
#' @export
get_counties <- function(location = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL,
                         class = "df",
                         ...) {
  if (is.null(location)) {
    location <- us_admin_dots_to_location(...)
  }

  if (!is_sf(location, ext = TRUE)) {
    lookup <-
      c(
        grep(x = us_counties$geoid, pattern = location),
        grep(x = us_counties$name, pattern = location),
        grep(x = paste0(us_counties$name_short, ", ", us_counties$abb_state), pattern = location)
      )

    condition <- !is_empty(lookup)
  } else {
    lookup <-
      location_intersects_us_admin(
        us_counties,
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp
      )

    condition <- any(lookup)
  }

  cli_abort_ifnot(
    "No U.S. counties can be found using the provided {.arg location}, {.arg geoid}, or {.arg name}: {.val {location}}",
    condition = condition
  )

  convert_us_admin_class(us_counties[lookup, ], class = class)
}

#' @noRd
us_admin_dots_to_location <- function(...) {
  params <- list2(...)

  location <-
    dplyr::case_when(
      !is.null(params$geoid) ~ "geoid",
      !is.null(params$name) ~ "name",
      !is.null(params$state) ~ "state",
      !is.null(params$county) ~ "county",
      TRUE ~ "none"
    )

  cli_abort_ifnot(
    "A {.arg location}, {.arg geoid}, {.arg name}, {.arg state}, or {.arg county} parameter must be provided.",
    condition = (location != "none")
  )

  switch(location,
    "geoid" = params$geoid,
    "name" = params$name,
    "state" = params$state,
    "county" = params$county
  )
}

#' @noRd
location_intersects_us_admin <- function(data,
                                         location = NULL,
                                         dist = NULL,
                                         diag_ratio = NULL,
                                         unit = NULL,
                                         asp = NULL) {
  if (!is.null(asp)) {
    location <-
      sfext::st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp
      )
  } else {
    location <-
      sfext::st_buffer_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit
      )
  }

  sf::st_intersects(
    wkt_col_as_sfc(data),
    location$geometry,
    sparse = FALSE
  )
}

#' @noRd
wkt_col_as_sfc <- function(x, wkt_col = "wkt", crs = 3857) {
  sf::st_as_sfc(x[[wkt_col]], crs = crs)
}

#' @noRd
#' @importFrom sf st_as_sf st_as_sfc
#' @importFrom dplyr bind_cols
convert_us_admin_class <- function(data,
                                   class = "df",
                                   sf_col = "geometry",
                                   call = caller_env()) {
  check_df(data, call = call)

  class <-
    arg_match(class, c("df", "bbox", "sf", "sfc"), call = call)

  if (class == "sf") {
    wkt <- data
    data$bbox <- NULL
    data$wkt <- NULL
  }

  switch(class,
    "df" = data,
    "sfc" = wkt_col_as_sfc(data),
    "bbox" = data[["bbox"]],
    "sf" = sf::st_as_sf(
      dplyr::bind_cols(
        data,
        "{sf_col}" := wkt_col_as_sfc(wkt)
      ),
      sf_column_name = sf_col
    )
  )
}

#' @noRd
is_state_name <- function(x, null.ok = TRUE) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  x %in% c(us_states$name, us_states$abb)
}

#' @noRd
is_us_state <- function(x, null.ok = TRUE, type = NULL) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  type <- match.arg(type, c("unspecified", "geoid", "name"))

  switch(type,
    "unspecified" = any(is_us_state_geoid(x), is_us_state_name(x)),
    "geoid" = any(is_us_state_geoid(x)),
    "name" = any(is_us_state_name(x))
  )
}

is_us_state_geoid <- function(x) {
  as.character(x) %in% us_states$geoid
}

is_us_state_name <- function(x) {
  x %in% us_states$name | x %in% us_states$abb
}

#' @noRd
is_us_county <- function(x, null.ok = TRUE, type = NULL) {
  if (is.null(x) && null.ok) {
    return(FALSE)
  }

  type <- match.arg(type, c("unspecified", "geoid", "name"))

  switch(type,
    "unspecified" = any(is_us_county_geoid(x), is_us_county_name(x)),
    "geoid" = is_us_county_geoid(x),
    "name" = is_us_county_name(x)
  )
}

#' @noRd
is_us_county_geoid <- function(x) {
  as.character(x) %in% us_counties$geoid
}

#' @noRd
is_us_county_name <- function(x) {
  x %in% us_counties$name
}
