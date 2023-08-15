#' Make a grid over the bounding box of a location
#'
#' If location is a single feature sf object, the original columns of the object
#' are included in the output grid. If location has mutiple features, the values
#' of name_col are combined with [sfext::st_union_ext] and other columns are
#' dropped. The input sf object should not have columns named id, rows, or cols.
#'
#' @param location A sf, sfc, or bbox object passed to [sfext::st_make_grid_ext]
#' @inheritParams sfext::st_make_grid_ext
#' @inheritParams sfext::st_union_ext
#' @inheritDotParams sfext::st_make_grid_ext
#' @export
make_location_grid <- function(location,
                               name_col = "name",
                               unit = NULL,
                               ...) {
  grid <- sfext::st_make_grid_ext(x = location, unit = unit, ...)

  # FIXME: Would be nice if sfext::st_union_ext also allowed retaining columns
  # where all the values were the same
  location <- sfext::st_union_ext(location, name_col = name_col)

  grid <- dplyr::bind_cols(
    sf::st_drop_geometry(location),
    grid
  )

  sfext::df_to_sf(grid)
}
