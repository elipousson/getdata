#' Get location of a specified type based on name, id, or location
#'
#' Filter by name or id or use a spatial filter based on an sf object or
#' geocoded street address. Optionally you can use an index list to match the
#' type to a named list of URLs or sf objects.
#'
#' @param type Type of location to return. Type can be an sf object, e.g. a data
#'   frame with multiple neighborhoods or a character string that can be passed
#'   to [get_location_data()]. If index is provided, character can also be a
#'   character string to match the name of a list.
#' @param name Location name to return.
#' @param id Location id to return. id is coerced to character or numeric to
#'   match the class of the id_col for type.
#' @param location An address, bounding box (`bbox`), or simple feature (`sf`)
#'   object passed to [sf::st_filter()]. Any valid address or addresses are
#'   geocoded with [tidygeocoder::geo()], converted to a simple feature object,
#'   and then used as a spatial filter. `bbox` objects are converted using
#'   [sfext::sf_bbox_to_sf()]. Multiple addresses are supported.
#' @param label Label optionally added to "label" column; must be a length 1 or
#'   match the number of rows returned based on the other parameters. If `union
#'   = TRUE`, using label is recommended. Default: `NULL`
#' @param name_col Column name in type with name values, Default: 'name'
#'   Required if name provided.
#' @param id_col Column name in type with id values, Default: 'id'. Required if
#'   id is provided.
#' @param index Optional list used to match type to data, Default: `NULL`
#' @param union If `TRUE`, the location geometry is unioned with
#'   [sf::st_union()] and the names are combined into a single value. Default:
#'   `FALSE`.
#' @param crs Coordinate reference system to return; defaults to `NULL` which
#'   returns data using the same coordinate reference system as the provided
#'   type of location.
#' @param class Class of object to return; defaults to "sf".
#' @param ... Additional parameters passed to [get_location_data()] if type is
#'   character and index is `NULL`.
#' @return A simple feature object from data provided to type.
#' @example examples/get_location.R
#' @rdname get_location
#' @aliases get_location_type
#' @export
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom sfext is_sf st_union_ext as_sf_class
#' @importFrom dplyr case_when mutate all_of
get_location <- function(type,
                         name = NULL,
                         name_col = "name",
                         id = NULL,
                         id_col = "id",
                         location = NULL,
                         index = NULL,
                         union = FALSE,
                         crs = getOption("getdata.crs", 3857),
                         label = NULL,
                         class = "sf",
                         ...) {
  if (is.null(index)) {
    rlang::check_required(type)
    cliExtras::cli_abort_ifnot(
      "{.arg type} must be a {.cls sf} object or character vector,
      not {.obj_type_friendly {type}}.",
      condition = sfext::is_sf(type) || is.character(type)
    )
  }

  check_list(index, allow_null = TRUE)

  stopifnot(
    is.character(location) ||
      sfext::is_sf(location, ext = TRUE, allow_null = TRUE) ||
      is.numeric(location),
    is_logical(union)
  )

  if (is.list(index) && !sfext::is_sf(type)) {
    type <- get_index_param(index = index, type = type)
  }

  if (is.character(type)) {
    # If type is a string
    # Return data if type is a file path, url, or package data
    type <- get_location_data(
      data = type,
      # FIXME: Using name_col in both places may be an issue
      name_col = name_col,
      name = name,
      ...
    )
  }

  params <- dplyr::case_when(
    is.null(location) && !is.null(name) ~ "name",
    is.null(location) && !is.null(id) ~ "id",
    .default = "location"
  )

  location <- switch(params,
    "name" = filter_name(type, name = name, name_col = name_col),
    "id" = filter_name(type, name = id, name_col = id_col),
    "location" = filter_location(type, location = location)
  )

  col <- NULL
  # FIXME: There should be an option for setting the col value
  if (!is.null(name)) {
    col <- name_col
  } else if (!is.null(id)) {
    col <- id_col
  }

  if (union) {
    location <- sfext::st_union_ext(location, name_col = name_col)
  }

  if (!is.null(label)) {
    location <- dplyr::mutate(
      location,
      "label" = label,
      .after = dplyr::all_of(name_col)
    )
  }

  if (!is_null(crs)) {
    location <- sfext::st_transform_ext(location, crs = crs)
  }

  sfext::as_sf_class(location, class = class, col = col)
}

#' Filter by location
#'
#' Converts data frame or address values to sf
#'
#' @noRd
#' @importFrom sfext is_sf as_sf st_filter_ext
filter_location <- function(data = NULL,
                            location = NULL,
                            allow_null = TRUE,
                            ...) {
  if (allow_null && is_null(location)) {
    return(data)
  }

  if (!sfext::is_sf(location, ext = TRUE)) {
    location <- sfext::as_sf(location)
  }

  sfext::st_filter_ext(x = data, y = location, ...)
}

#' @noRd
filter_name <- function(x = NULL,
                        name = NULL,
                        name_col = "name",
                        arg = rlang::caller_arg(name_col),
                        call = rlang::caller_env()) {
  if (is_named(name)) {
    name_col <- names(name)
  }

  if (is.null(name) || is.null(name_col)) {
    return(x)
  }

  name_col <- rlang::arg_match(
    name_col,
    names(x),
    error_arg = arg,
    error_call = call
  )

  x[x[[name_col]] %in% name, ]
}
