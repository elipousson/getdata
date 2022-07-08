#' Get location of a specified type based on name, id, or location
#'
#' Filter by name or id or use a spatial filter based on an sf object or
#' geocoded street address. Optionally you can use an index list to match the
#' type to a named list of URLs or sf objects.
#'
#' @param type Type of location to return. Type can be an sf object, e.g. a data
#'   frame with multiple neighborhoods or a character string that can be passed
#'   to [get_location_data]. If index is provided, character can also be a
#'   character string to match the name of a list.
#' @param name Location name to return.
#' @param id Location id to return. id is coerced to character or numeric to
#'   match the class of the id_col for type.
#' @param location An address, bounding box (`bbox`), or simple feature (`sf`)
#'   object passed to [sf::st_filter]. Any valid address or
#'   addresses are geocoded with [tidygeocoder::geo], converted to
#'   a simple feature object, and then used as a spatial filter. `bbox` objects
#'   are converted using [sfext::sf_bbox_to_sf()]. Multiple addresses are supported.
#' @param label Label optionally added to "label" column; must be a length 1 or
#'   match the number of rows returned based on the other parameters. If `union = TRUE`,
#'   using label is recommended. Default: `NULL`
#' @param name_col Column name in type with name values, Default: 'name'
#'   Required if name provided.
#' @param id_col Column name in type with id values, Default: 'id'. Required if
#'   id is provided.
#' @param index Optional list used to match type to data, Default: `NULL`
#' @param union If `TRUE`, the location geometry is unioned with
#'   [sf::st_union] and the names are combined into a single value.
#'   Default: `FALSE`.
#' @param crs Coordinate reference system to return; defaults to NULL which
#'   returns data using the same coordinate reference system as the provided
#'   type of location.
#' @param class Class of object to return; defaults to "sf".
#' @param ... Additional parameters passed to [get_location_data] if type
#'   is character and index is `NULL`.
#' @return A simple feature object from data provided to type.
#' @example examples/get_location.R
#' @rdname get_location
#' @export
#' @importFrom sf st_crs st_filter st_as_sf st_union
#' @importFrom sfext is_sf st_union_ext as_sf_class
#' @importFrom dplyr bind_cols
get_location <- function(type,
                         name = NULL,
                         name_col = "name",
                         id = NULL,
                         id_col = "id",
                         location = NULL,
                         index = NULL,
                         union = FALSE,
                         crs = getOption("getdata.crs"),
                         label = NULL,
                         class = "sf",
                         ...) {
  stopifnot(
    sfext::is_sf(type) || is.character(type) || (is.null(type) && is.list(index)),
    is.character(location) || sfext::is_sf(location, ext = TRUE, null.ok = TRUE) || is.numeric(location),
    is.list(index) || is.null(index),
    is.logical(union)
  )

  if (is.list(index) && !sfext::is_sf(type)) {
    type <- get_index_param(index = index, type = type)
  }

  if (is.character(type)) {
    # If type is a string
    # Return data if type is a file path, url, or package data
    type <-
      get_location_data(
        data = type,
        # FIXME: Using name_col in both places may be an issue
        name_col = name_col,
        name = name,
        ...
      )
  }

  # If location is not provided
  if (is.null(location)) {
    if (!is.null(name)) {
      # Filter type by name
      location <- type[type[[name_col]] %in% name, ]
    } else if (!is.null(id)) {
      if (is.character(type[[id_col]])) {
        # Filter type by ID
        location <- type[type[[id_col]] %in% as.character(id), ]
      } else if (is.numeric(type[[id_col]])) {
        location <- type[type[[id_col]] %in% as.numeric(id), ]
      }
    }

    # FIXME: The prior error message may be more informative: "The name/id did not match any features of the type provided."
    if (!is.null(location)) {
      check_df_rows(
        location,
        rows = 1
      )
    }
  } else {
    location <-
      location_filter(
        data = type,
        location = location,
        trim = FALSE,
        crop = FALSE
      )
  }

  if (!is.null(name)) {
    col <- name_col
  } else if (!is.null(id)) {
    col <- id_col
  } else {
    col <- NULL
  }

  if (is.null(location) && !is.null(type)) {
    location <- type

    if (is.null(name_col) && (nrow(type) > 1)) {
      cli_warn("Returning all locations of this type.")
    }
  }

  cli_abort_ifnot(
    c("Location can't be found based on the provided parameters."),
    condition = nrow(location) > 0
  )

  if (union) {
    location <- sfext::st_union_ext(location, name_col = name_col)
  }

  if (!is.null(label)) {
    location <-
      dplyr::mutate(
        location,
        "label" = label,
        .after = name_col
      )
  }

  sfext::as_sf_class(location, class = class, crs = crs, col = col)
}

#' Filter by location
#'
#' Converts data frame or address values to sf
#'
#' @noRd
#' @importFrom sfext is_sf as_sf st_filter_ext
location_filter <- function(data = NULL,
                            location = NULL,
                            ...) {
  if (!sfext::is_sf(location, ext = TRUE)) {
    location <- sfext::as_sf(location)
  }

  sfext::st_filter_ext(data, location, ...)
}
