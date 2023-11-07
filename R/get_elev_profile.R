#' Use `elevatr::get_elev_point` to get the elevation along a profile
#'
#' `r lifecycle::badge("experimental")`
#' [get_elev_profile()] is a wrapper for [elevatr::get_elev_point()] that takes
#' a LINESTRING or POINT input for profile and returns a data frame of
#' elevation. Optionally, create a series of points along a line with
#' [sf::st_line_sample()] and/or include a column with the distance between each
#' successive POINT in the data frame.
#'
#' This function is proposed for addition to `{elevatr}`:
#' <https://github.com/elipousson/getdata/issues/4>
#'
#' @param profile A `sfc` or `sf` geometry with a LINESTRING or POINT geometry
#'   type.
#' @param units If `NULL`, output elevation and distance is in meters. If a
#'   valid distance unit is supplied, elevation and distance are converted to
#'   match the supplied unit.
#' @param dist If `TRUE`
#' @param drop_units If `TRUE`, return a plain numeric column for the elevation
#'   and distance columns. If `FALSE` (default) return a units class column.
#' @param cumulative If `TRUE`, and dist is `TRUE` return distance as a
#'   cumulative sum.
#' @inheritParams sf::st_line_sample
#' @export
get_elev_profile <- function(profile,
                             units = NULL,
                             dist = FALSE,
                             n = NULL,
                             density = NULL,
                             type = "regular",
                             sample = NULL,
                             ...,
                             drop_units = FALSE,
                             cumulative = FALSE) {
  check_installed("elevatr")

  if (!inherits(profile, "sf")) {
    profile <- sf::st_as_sf(profile)
  }

  if (!sf::st_is(profile, "LINESTRING") && !sf::st_is(profile, "POINT")) {
    geom_type <- sf::st_geometry_type(profile, by_geometry = FALSE)

    cli::cli_abort(
      "{.arg profile} must use {.val LINESTRING} or {.val POINT} geometry,
      not {.val {geom_type}}."
    )
  }

  if (is.numeric(sample) || is.numeric(n) || is.numeric(density)) {
    profile <- sf::st_line_sample(
      profile,
      n = n, density = density,
      sample = sample, type = type
    )

    locations <- sf::st_cast(profile, to = "POINT")
  } else if (sf::st_is(profile, "LINESTRING")) {
    locations <- suppressWarnings(sf::st_cast(profile, to = "POINT"))
  } else {
    locations <- profile
  }

  location_coords <- as.data.frame(sf::st_coordinates(locations))
  location_coords <- setNames(location_coords, tolower(names(location_coords)))
  locations_crs <- sf::st_crs(locations)

  elev_point <- suppressMessages(
    elevatr::get_elev_point(location_coords, prj = locations_crs)
  )

  if (nrow(elev_point) < 2) {
    dist <- FALSE
  }

  if (dist) {
    dist_point <- 0
    elev_point_geom <- vctrs::vec_chop(sf::st_geometry(elev_point))

    for (i in (seq(length(elev_point_geom) - 1))) {
      dist_add <- sf::st_distance(
        elev_point_geom[[i]],
        elev_point_geom[[i + 1]]
      )

      dist_point <- c(dist_point, dist_add)
    }

    if (cumulative) {
      dist_point <- cumsum(dist_point)
    }

    elev_point[["distance"]] <- units::set_units(
      dist_point, locations_crs$units_gdal,
      mode = "standard"
    )
  }

  elev_units <- unique(elev_point[["elev_units"]])

  if (!is.null(units)) {
    mode <- "standard"
    elev_units_label <- units

    if (!is.character(units)) {
      elev_units_label <- unique(as.character(base::units(x)[["numerator"]]))
      mode <- units::units_options("set_units_mode")
    }

    elev_point[["elevation"]] <- units::as_units(
      elev_point[["elevation"]],
      elev_units
    )

    elev_point[["elevation"]] <- units::set_units(
      elev_point[["elevation"]],
      value = units, mode = mode
    )

    elev_point[["elev_units"]] <- elev_units_label

    if (dist) {
      elev_point[["distance"]] <- units::set_units(
        elev_point[["distance"]],
        value = units, mode = mode
      )
    }
  } else if (dist) {
    elev_point[["distance"]] <- units::set_units(
      elev_point[["distance"]], elev_units
    )
  }

  if (drop_units) {
    elev_point[["elevation"]] <- units::drop_units(elev_point[["elevation"]])

    if (dist) {
      elev_point[["distance"]] <- units::drop_units(elev_point[["distance"]])
    }
  }

  elev_point
}
