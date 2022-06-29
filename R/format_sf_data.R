#' Format simple feature data
#'
#' The main [format_sf_data] function is a wrapper for the following common
#' steps in transforming an `sf` object and preparing for mapping or analysis:
#'
#'  - Convert data to an `sf` object with [overedge::as_sf] if `sf_req` is `FALSE`
#'  - Make data valid with [sf::st_make_valid] if needed
#'  - Format data with [format_data] using the ... parameters
#'  - Erase any data overlapping with `erase_data` (suggested for use with water or open space)
#'  - Simplify geometry with [sf::st_simplify] if `dTolerance` is provided
#'  - Smooth geometry with [smoothr::smooth] if `smooth` is `TRUE`
#'  - Rename the sf column to match `sf_col` (defaults to "geometry")
#'
#'  The helper functions for format_sf_data and additional formatting functions
#'  for `sf` data are described in the details.
#'
#' @param data A `sf` object or, if `sf_req` is `FALSE`, any object that can be
#'   converted to an `sf` object with [overedge::as_sf].
#' @param crs Coordinate reference system for returned data, Default: getOption("getdata.crs", default =
#'   3857)
#' @param erase If TRUE, pass `data` and `erase_data` to [overedge::st_erase],
#'   Default: `FALSE`
#' @param erase_data A `sf`, `sfc`, or `bbox` object with the geometry to erase
#'   from data, Default: `NULL`
#' @inheritParams sf::st_simplify
#' @param smooth If `TRUE`, smooth data with [smoothr::smooth] using default
#'   method and parameters, Default: `FALSE`.
#' @param sf_col Name to use for output `sf` column, Default: 'geometry'.
#' @param sf_req If `TRUE`, data must be a `sf` object. If `FALSE`, data is
#'   passed to [overedge::as_sf] to convert data to an `sf` object.
#' @param ... Additional parameters passed to `format_data`
#' @return A `sf` object with columns and geometry modified based parameters.
#'
#' @details Helper functions for [format_sf_data]:
#'
#' - [rename_sf_col]: Rename `sf` column.
#' - [relocate_sf_col]: Relocate `sf` column after selected columns (defaults to [everything()]).
#'
#' @details Additional formatting functions for `sf` objects:
#'
#'  - [bind_boundary_col] uses [sf::st_join] to assign simple feature data to an
#'  enclosing polygon. This function with other join functions to add columns
#'  based on other spatial relationships, e.g. bind a column for the nearest
#'  point feature.
#'
#' @example examples/format_sf_data.R
#' @rdname format_sf_data
#' @export
#' @importFrom overedge is_sf is_same_crs st_erase rename_sf_col
#' @importFrom sf st_is_valid st_make_valid st_transform st_simplify
format_sf_data <- function(x,
                           crs = getOption("getdata.crs", default = 3857),
                           erase_data = NULL,
                           dTolerance = NULL,
                           smooth = FALSE,
                           sf_col = NULL,
                           sf_req = TRUE,
                           ...) {
  if (!sf_req) {
    x <- as_sf(x)
  }

  check_sf(x, ext = TRUE)

  if (!all(sf::st_is_valid(x))) {
    x <- sf::st_make_valid(x)
  }

  x <- overedge::st_transform_ext(x, crs = crs)

  if (is.data.frame(x)) {
    x <- format_data(x, ...)
  }

  if (!is.null(erase_data) && is_sf(erase_data)) {
    x <- overedge::st_erase(x, erase_data)
  }

  if (!is.null(dTolerance)) {
    x <- sf::st_simplify(x, dTolerance = dTolerance)
  }

  if (smooth) {
    x <- smoothr::smooth(x)
  }

  if (!is.null(sf_col)) {
    x <- overedge::rename_sf_col(x, sf_col = sf_col)
  }

  x
}


#' @name relocate_sf_col
#' @rdname format_sf_data
#' @param .after The location to place sf column after; defaults to
#'   [dplyr::everything].
#' @export
#' @importFrom dplyr everything relocate all_of
relocate_sf_col <- function(x, .after = dplyr::everything(), ...) {
  dplyr::relocate(
    x,
    dplyr::all_of(attributes(x)$sf_column),
    .after = .after
  )
}


#' @name rename_sf_col
#' @rdname format_sf_data
#' @param sf_col Name to use for the sf column after renaming; defaults to "geometry".
#' @export
#' @importFrom dplyr everything relocate all_of
rename_sf_col <- function(x, sf_col = "geometry") {
  check_null(sf_col)

  names(x)[names(x) == attr(x, "sf_column")] <- sf_col
  attr(x, "sf_column") <- sf_col

  x
}

#' @name bind_boundary_col
#' @rdname format_sf_data
#' @param boundary An sf object with a column named "name" or a list of sf
#'   objects where all items in the list have a "name" column.
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON objects
#'   or [sf::st_nearest_feature] if key_list contains other types.
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr rename select
#' @importFrom sf st_join
bind_boundary_col <- function(x, boundary = NULL, join = NULL, col = "name", ...) {
  if (!is_sf_list(boundary)) {
    boundary <- overedge::as_sf_list(boundary, crs = x, col = col)
  } else {
    boundary <- overedge::st_transform_ext(boundary, crs = x)
  }

  cli_abort_ifnot(
    "{.arg boundary} must be a list of `sf` objects where each object has a column named {.arg {col}}.",
    condition = all(sapply(boundary, rlang::has_name, col))
  )

  join <- set_join_by_geom_type(boundary, join = join)

  for (nm in names(boundary)) {
    y <-
      dplyr::rename(
        dplyr::select(boundary[[nm]], name),
        "{nm}" := "name"
      )

    x <- has_same_name_col(x, col = nm, drop = FALSE)
    x <- sf::st_join(x, y, join = join, ...)
  }

  relocate_sf_col(x)
}

#' @name bind_units_col
#' @rdname format_sf_data
#' @param y Vector of numeric or units values to bind to x.
#' @param units Units to use for y (if numeric) or convert to (if y is units
#'   class); defaults to `NULL`.
#' @param drop If `TRUE`, apply the [units::drop_units] function to the column
#'   with units class values and return numeric values instead; defaults to
#'   `FALSE`.
#' @param keep_all If `FALSE`, keep all columns. If `FALSE`, return only the
#'   named .id column.
#' @param .id Name to use for vector of units provided to "y" parameter, when
#'   "y" is bound to the "x" data frame or tibble as a new column.
#' @param ... passed to [relocate_sf_col]
#' @export
#' @importFrom dplyr bind_cols
bind_units_col <- function(x, y, units = NULL, drop = FALSE, keep_all = TRUE, .id = NULL, ...) {
  is_pkg_installed("units")

  if (!is.null(units)) {
    y <-
      overedge::convert_dist_units(
        dist = y,
        from = get_dist_units(y),
        to = units
      )
  } else {
    units <- get_dist_units(y)
  }

  if (drop) {
    y <- units::drop_units(y)
  }

  if (!keep_all) {
    return(y)
  }

  if (is.null(.id)) {
    .id <- janitor::make_clean_names(units)
  }

  x <- has_same_name_col(x, col = .id)

  x <-
    dplyr::bind_cols(
      x,
      "{.id}" := y
    )

  if (is_sf(x)) {
    x <- relocate_sf_col(x, ...)
  }

  x
}
