#' Use tigris to get state-level data from the U.S. Census Bureau
#'
#' Use the [{tigris}](https://github.com/walkerke/tigris) package to download
#' state-level data from the U.S. Census Bureau API and optionally filter by
#' name or GeoID.
#'
#' @details Supported data types:
#'
#'  Different type values corresponded to different tigris functions for
#'  downloading from the U.S. Census Bureau API include. Supported options
#'  include: "counties", "census places", "congressional districts",
#'  "legislative districts", "senate district", "tracts", "block groups",
#'  "blocks", "pumas", "voting districts", "zctas", "roads", "primary secondary
#'  roads", "area water",  "linear water", and "landmarks".
#'
#'   tigris functions that do not use a "state" parameter (e.g.
#'   [tigris::coastline] or [tigris::rails])  are not supported by this
#'   function. Note that the default value of the cb parameter for
#'   [get_tigris_data] is `TRUE` and the default value of for the original
#'   {tigris} package is `FALSE`.
#'
#' @param state State name, abbreviation, or GeoID. Required. Defaults to
#'   getOption("getdata.state").
#' @param name,name_col Name and columns to filter by name. name defaults to
#'   `NULL`, and name_col defaults to c("namelsad", "namelsad", "geoid")
#'   columns.
#' @param type Type of data to return, Default: `NULL`; See details for
#'   supported options.
#' @param cb If `TRUE`, download a generalized (1:500k) file. If `FALSE`,
#'   download the most detailed TIGER/Line file. Defaults to `TRUE` (reverse of
#'   the default for tigris functions). This parameter is *not* used when type
#'   is set to blocks, roads, primary secondary roads, area water, linear water,
#'   landmarks, or zctas.
#' @param cache If `TRUE`, set `options(tigris_use_cache = TRUE)` to cache
#'   downloaded tigris data. Ignored if `getOption("tigris_use_cache")` is not
#'   `NULL`.
#' @param location A sf, sfc, or bbox object passed to [sfext::st_bbox_ext()]
#'   and used to create the geometry passed to the `filter_by` parameter passed
#'   to the tigris functions.
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams format_sf_data
#' @inheritParams format_data
#' @param ... Additional parameters passed on to tigris functions.
#' @return A simple feature object matching the type provided.
#' @rdname get_state_tigris
#' @export
get_tigris_data <- function(type = NULL,
                            state = getOption("getdata.state"),
                            location = NULL,
                            dist = getOption("getdata.dist"),
                            diag_ratio = getOption("getdata.diag_ratio"),
                            unit = getOption("getdata.unit", "meter"),
                            asp = getOption("getdata.asp"),
                            crs = getOption("getdata.crs", default = 3857),
                            name = NULL,
                            name_col = c("name", "namelsad", "geoid"),
                            cb = TRUE,
                            clean_names = TRUE,
                            cache = TRUE,
                            ...) {
  if (!is.null(type)) {
    type <- tolower(type)
  }

  if (cache && is.null(getOption("tigris_use_cache"))) {
    options(tigris_use_cache = TRUE)
  }

  rlang::check_installed("tigris")

  type <- match.arg(
    type,
    c(
      "counties", "census places", "congressional districts",
      "legislative districts", "senate district", "voting districts",
      "tracts", "block groups", "blocks", "pumas", "zctas",
      "roads", "primary secondary roads", "area water", "linear water", "landmarks"
    )
  )

  filter_by <- sfext::st_bbox_ext(
    location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp
  )

  data <- switch(type,
    "counties" = tigris::counties(state = state, cb = cb, filter_by = filter_by, ...),
    "census places" = tigris::places(state = state, cb = cb, filter_by = filter_by, ...),
    "congressional districts" = tigris::congressional_districts(state = state, cb = cb, filter_by = filter_by, ...),
    "legislative districts" = tigris::state_legislative_districts(state = state, house = "lower", cb = cb, filter_by = filter_by, ...),
    "senate districts" = tigris::state_legislative_districts(state = state, house = "upper", cb = cb, filter_by = filter_by, ...),
    "county subdivisions" = tigris::county_subdivisions(state = state, cb = cb, filter_by = filter_by, ...),
    "block groups" = tigris::block_groups(state = state, cb = cb, filter_by = filter_by, ...),
    "blocks" = tigris::blocks(state = state, filter_by = filter_by, ...),
    "pumas" = tigris::pumas(state = state, cb = cb, filter_by = filter_by, ...),
    "voting districts" = tigris::voting_districts(state = state, cb = cb, filter_by = filter_by, ...),
    "roads" = tigris::roads(state = state, filter_by = filter_by, ...),
    "primary secondary roads" = tigris::primary_secondary_roads(state = state, filter_by = filter_by, ...),
    "area water" = tigris::area_water(state = state, filter_by = filter_by, ...),
    "linear water" = tigris::linear_water(state = state, filter_by = filter_by, ...),
    "landmarks" = tigris::landmarks(state = state, filter_by = filter_by, ...),
    "zctas" = tigris::zctas(state = state, filter_by = filter_by, ...)
  )

  data <- format_sf_data(
    data,
    crs = crs,
    clean_names = clean_names
  )

  if (is.null(name)) {
    return(data)
  }

  data[lookup_tigris_name(name, name_col, data), ]
}

#' Helper to filter rows with name values in name_cols
#'
#' @noRd
lookup_tigris_name <- function(name, name_col = c("name", "namelsad", "geoid"), data = NULL) {
  lookup <- FALSE
  name <- tolower(name)
  name_col <- name_col[name_col %in% names(data)]

  lookup_col <- function(x) {
    tolower(data[[x]]) %in% name
  }

  for (x in name_col) {
    lookup <- lookup | lookup_col(x)
  }

  lookup
}
