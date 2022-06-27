#' Use package data or the {tigris} package to get state-level data from the U.S. Census Bureau
#'
#' Use the tigris package to download state-level data from the U.S. Census
#' Bureau API and filter by name or GeoID.
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
#'   function.
#'
#' @param state State name, abbreviation, or GeoID. Required. Defaults to
#'   getOption("getdata.state").
#' @param name Name matching a value in the namelsad, namelsad, or geoid
#'   columns. Default: `NULL`
#' @param type Type of data to return, Default: `NULL`; See details for
#'   supported options.
#' @inheritParams format_data
#' @param ... Additional parameters passed on to {tigris} functions.
#' @return A simple feature object matching the type provided.
#' @rdname get_state_tigris
#' @export
get_tigris_data <- function(type = NULL,
                            state = getOption("getdata.state"),
                            name = NULL,
                            crs = getOption("getdata.crs", default = 3857),
                            clean_names = TRUE,
                            ...) {
  if (!is.null(type)) {
    type <- tolower(type)
  }

  is_pkg_installed("tigris")

  type <-
    match.arg(
      type,
      c(
        "counties", "census places", "congressional districts",
        "legislative districts", "senate district", "voting districts",
        "tracts", "block groups", "blocks", "pumas", "zctas",
        "roads", "primary secondary roads", "area water", "linear water", "landmarks"
      )
    )

  data <-
    switch(type,
      "counties" = tigris::counties(state = state, ...),
      "census places" = tigris::places(state = state, ...),
      "congressional districts" = tigris::congressional_districts(state = state, ...),
      "legislative districts" = tigris::state_legislative_districts(state = state, house = "lower", ...),
      "senate districts" = tigris::state_legislative_districts(state = state, house = "upper", ...),
      "county subdivisions" = tigris::county_subdivisions(state = state, ...),
      "block groups" = tigris::block_groups(state = state, ...),
      "blocks" = tigris::blocks(state = state, ...),
      "pumas" = tigris::pumas(state = state, ...),
      "voting districts" = tigris::voting_districts(state = state, ...),
      "roads" = tigris::roads(state = state, ...),
      "primary secondary roads" = tigris::primary_secondary_roads(state = state, ...),
      "area water" = tigris::area_water(state = state, ...),
      "linear water" = tigris::linear_water(state = state, ...),
      "landmarks" = tigris::landmarks(state = state, ...),
      "zctas" = tigris::zctas(state = state, ...)
    )

  data <-
    format_data(
      data,
      # FIXME: incorporate functionality from mapmaryland::format_sf_data
      # crs = crs,
      clean_names = clean_names
    )

  if (is.null(name)) {
    return(data)
  }

  data[lookup_tigris_name(name, data), ]
}

#' @noRd
lookup_tigris_name <- function(name, data = NULL) {
  name <- tolower(name)

  lookup <-
    ((tolower(data[["name"]]) %in% name) | (tolower(data[["namelsad"]]) %in% name) | (data[["geoid"]] %in% name))

  return(lookup)
}
