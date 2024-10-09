#' Format simple feature data
#'
#' The main [format_sf_data] function is a wrapper for the following common
#' steps in transforming an `sf` object and preparing for mapping or analysis:
#'
#'  - Convert data to an `sf` object with [sfext::as_sf] if `sf_req` is `FALSE`
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
#' @param x A `sf` object or, if `sf_req` is `FALSE`, any object that can be
#'   converted to an `sf` object with [sfext::as_sf].
#' @param crs Coordinate reference system for returned data, Default: getOption("getdata.crs", default =
#'   3857)
#' @param erase_data A `sf`, `sfc`, or `bbox` object with geometry that should be erased
#'   from the data, Default: `NULL`
#' @inheritParams sf::st_simplify
#' @param smooth If `TRUE`, smooth data with [smoothr::smooth] using default
#'   method and parameters, Default: `FALSE`.
#' @param sf_col Name to use for output `sf` column, Default: 'geometry'.
#' @param sf_req If `TRUE`, data must be a `sf` object. If `FALSE`, data is
#'   passed to [sfext::as_sf] to convert data to an `sf` object.
#' @param ... Additional parameters passed to `format_data`
#' @return A `sf` object with columns and geometry modified based parameters.
#'
#' @details Helper functions for [format_sf_data]:
#'
#' - [erase_data]: erase intersection of x and erase_data (validity of
#' erase_data checked before [sfext::st_erase] and for x after completing the
#' operation.
#' - [rename_sf_col]: Rename `sf` column.
#' - [relocate_sf_col]: Relocate `sf` column after selected columns (defaults to
#' [dplyr::everything()]).
#'
#' @example examples/format_sf_data.R
#' @rdname format_sf_data
#' @export
#' @importFrom sfext as_sf check_sf st_transform_ext
#' @importFrom sf st_is_valid st_make_valid st_simplify
format_sf_data <- function(x,
                           crs = getOption("getdata.crs", default = 3857),
                           erase_data = NULL,
                           dTolerance = NULL,
                           smooth = FALSE,
                           sf_col = NULL,
                           sf_req = TRUE,
                           ...) {
  if (!sf_req) {
    x <- sfext::as_sf(x)
  }

  sfext::check_sf(x, ext = TRUE)

  if (!all(sf::st_is_valid(x))) {
    x <- sf::st_make_valid(x)
  }

  x <- sfext::st_transform_ext(x, crs = crs)

  if (is.data.frame(x)) {
    x <- format_data(x, ...)
  }

  x <- erase_data(x, erase_data)

  if (!is.null(dTolerance)) {
    x <- sf::st_simplify(x, dTolerance = dTolerance)
  }

  if (smooth) {
    x <- smoothr::smooth(x)
  }

  if (!is.null(sf_col)) {
    x <- rename_sf_col(x, sf_col = sf_col)
  }

  x
}

#' @name erase_data
#' @rdname format_sf_data
#' @importFrom sfext check_sf st_erase
#' @importFrom sf st_is_valid st_make_valid
erase_data <- function(x, erase_data = NULL) {
  if (is.null(erase_data) ||
    # FIXME: This check should probably be incorporated into sfext::st_erase
    (is_sf(erase_data) && nrow(erase_data) == 0) ||
    (is_sfc(erase_data) && length(erase_data) == 0)) {
    return(x)
  }

  sfext::check_sf(erase_data, ext = TRUE)

  if (!all(sf::st_is_valid(erase_data))) {
    erase_data <- sf::st_make_valid(erase_data)
  }

  x <- sfext::st_erase(x, erase_data)

  if (!all(sf::st_is_valid(x))) {
    x <- sf::st_make_valid(x)
  }

  x
}

#' Set join function based on geometry type
#'
#' @name set_join_by_geom_type
#' @inheritParams is_geom_type
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature] if key_list contains other types.
#' @importFrom sf st_intersects st_nearest_feature
#' @noRd
set_join_by_geom_type <- function(x, join = NULL) {
  if (!is.null(join)) {
    return(join)
  }

  if (all(sapply(x, sfext::is_polygon) || sapply(x, sfext::is_multipolygon))) {
    return(sf::st_intersects)
  }

  sf::st_nearest_feature
}
