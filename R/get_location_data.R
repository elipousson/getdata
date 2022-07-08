#' Get data for a location
#'
#' Returns data for a selected location or a list of locations (for
#' [map_location_data]). If data is a character string, the parameter is passed
#' to [sfext::read_sf_url], [sfext::read_sf_path], or [sfext::read_sf_pkg]. This
#' function uses [sfext::st_filter_ext()] to filter, crop, or trim data to the
#' provided location. location can also be an an address.
#'
#' This function previously supported county geoid, state name, abbreviation, or
#' geoid as a location. Currently, recommend using [get_states] or
#' [get_counties] and passing a `sf` object to location.
#'
#' @details Working with sf lists for data and locations:
#'
#'   [map_location_data] makes it easier to work with `sf` lists. It supports data
#'   as a character vector, data as an `sf` list when location is a single object,
#'   location as a character vector or `sf` list (including lists of `bbox` or `sfc`
#'   objects), or when both data and location are lists (such as a list created
#'   by [make_location_data_list]).
#'
#' @param location sf object. If multiple areas are provided, they are unioned
#'   into a single sf object using [sf::st_union]
#' @inheritParams sfext::st_bbox_ext
#' @param data Character string (e.g. url, file path, or name of data from
#'   package), a `sf`, `sfc`, or `bbox`  object including data in area.
#' @param package Name of the package to search for data.
#' @param filetype File type to use if passing parameters to [sfext::read_sf_download]
#'   or [sfext::read_sf_pkg] (required for extdata and cached data).
#' @param fn Function to apply to data after filtering by location but before
#'   returning from function.
#' @inheritParams sfext::st_filter_ext
#' @param from_crs Coordinate reference system used to match the location CRS to
#'   the source data.
#' @param crs Coordinate reference system to return.
#' @param class Class of object to return.
#' @param index A list of possible location, data, and (optionally) package
#'   values. List must be named and include a value named package and package
#'   must be `NULL`, to set package based on index. If list is not `NULL` and
#'   location and/or data as character or numeric values, the location and data
#'   are assumed to be index values for the index list. The index parameter
#'   supports nested lists created by [make_location_data_list] (using only the
#'   default key names of "location" and "data"). This feature has not be fully
#'   tested and may result in errors or unexpected results.
#' @param label label is optionally used by [map_location_data] to name the data
#'   objects in the list returned by the function.
#' @inheritParams sfext::as_sf_class
#' @inheritParams format_data
#' @param ... additional parameters passed to [sfext::read_sf_path], [sfext::read_sf_url], or
#'   [sfext::read_sf_pkg] and [sfext::st_filter_ext]
#' @rdname get_location_data
#' @export
#' @importFrom sf st_crs st_crop st_transform st_intersection st_filter
get_location_data <- function(location = NULL,
                              dist = getOption("getdata.dist"),
                              diag_ratio = getOption("getdata.diag_ratio"),
                              unit = getOption("getdata.unit", default = "meter"),
                              asp = getOption("getdata.asp"),
                              data = NULL,
                              package = getOption("getdata.data_package"),
                              filetype = getOption("getdata.data_filetype", default = "gpkg"),
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = getOption("getdata.from_crs"),
                              crs = getOption("getdata.crs"),
                              class = "sf",
                              label = NULL,
                              index = NULL,
                              col = NULL,
                              var_names = NULL,
                              clean_names = FALSE,
                              ...) {
  if (!is.null(index) && is.list(index)) {
    # FIXME: This is set to work with 1 or 2 level list indices with naming conventions that match make_location_data_list
    # This should be clearly documented as alternate index naming conventions supported if possible
    if (("package" %in% names(index)) && is.null(package)) {
      package <- unique(index$package) # could use data as an index
      check_len(package, len = 1)
    }

    location <- get_index_param(index, location = location)
    data <- get_index_param(index, data = data)
  }

  if (!is.null(location) && !sfext::is_sf(location)) {
    location <- sfext::as_sf(location)
  }

  # Get adjusted bounding box using any adjustment variables provided
  bbox <-
    sfext::st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = from_crs
    )

  if (!sfext::is_sf(data)) {
    type <-
      dplyr::case_when(
        sfext::is_bbox(data) ~ "bbox",
        is.data.frame(data) ~ "df",
        is_url(data) ~ "url",
        is.character(data) && fs::file_exists(data) ~ "path",
        !is.null(package) ~ "pkg",
        TRUE ~ "other"
      )

    data <-
      switch(type,
        "df" = sfext::df_to_sf(x = data, ...),
        "bbox" = sfext::as_sf(data, ...),
        "url" = sfext::read_sf_url(url = data, bbox = bbox, ...),
        "path" = sfext::read_sf_path(path = data, bbox = bbox, ...),
        "pkg" = sfext::read_sf_pkg(data = data, bbox = bbox, package = package, filetype = filetype, ...)
      )
  }

  if (crop) {
    data <-
      sfext::st_filter_ext(
        data,
        bbox,
        crop = TRUE
      )
  } else if (trim) {
    data <-
      sfext::st_filter_ext(
        data,
        location,
        trim = TRUE
      )
  } else {
    if (!all(sapply(c(dist, diag_ratio, asp), is_null))) {
      location <- bbox
    }

    data <-
      sfext::st_filter_ext(
        data,
        location
      )
  }

  data <- use_fn(data = data, fn = fn)

  data <- format_data(data, var_names = var_names, clean_names = clean_names)

  sfext::as_sf_class(x = data, class = class, crs = crs, col = col)
}

#' @name map_location_data
#' @rdname get_location_data
#' @param load If `TRUE` and class is "list", load data to local environment;
#'   defaults `FALSE`.
#' @example examples/map_location_data.R
#' @export
#' @importFrom janitor make_clean_names
#' @importFrom purrr set_names map_chr map map2 discard
map_location_data <- function(location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              data = NULL,
                              package = NULL,
                              filetype = "gpkg",
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = NULL,
                              crs = NULL,
                              class = "list",
                              label = NULL,
                              load = FALSE,
                              index = NULL,
                              ...) {
  # FIXME: This triggers an alert with lintr but it is used
  params <- list2(...)

  if (!is.list(data) && (length(data) > 1)) {
    data <- as.list(data)

    # FIXME: The addition of a index parameter to get_location_data should allow the use of the index as a secondary source of name data for map_location_data
    if (!is_named(data)) {
      if (!is.null(label)) {
        label <- janitor::make_clean_names(label)
      }

      data <-
        purrr::set_names(
          data,
          nm = purrr::map_chr(
            data,
            ~ paste0(
              c(
                label,
                janitor::make_clean_names(.x)
              ),
              collapse = "_"
            )
          )
        )
    }
  }

  if (!is.list(location) && (length(location) > 1)) {
    location <- as.list(location)
  }

  if (is.list(data)) {
    data <-
      purrr::map(
        data,
        ~ get_location_data(
          location = location,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = .x,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          name_col = params$name_col,
          name = params$name,
          index = index
        )
      )
  } else if (is.list(location)) {
    data <-
      purrr::map(
        location,
        ~ get_location_data(
          location = .x,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = data,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          name_col = params$name_col,
          name = params$name,
          index = index
        )
      )
  } else if (is.list(data) && is.list(location) && (length(data) == length(location))) {
    data <-
      purrr::map2(
        location,
        data,
        ~ get_location_data(
          location = .x,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          data = .y,
          package = package,
          filetype = filetype,
          fn = fn,
          crop = crop,
          trim = trim,
          from_crs = from_crs,
          crs = crs,
          name_col = params$name_col,
          name = params$name,
          index = index
        )
      )
  }

  data <- purrr::discard(data, ~ nrow(.x) == 0)
  data <- sfext::as_sf_class(x = data, class = class, crs = crs) # , ...)

  if (load && sfext::is_sf_list(data, named = TRUE)) {
    list2env(data, envir = .GlobalEnv)
  } else {
    data
  }
}


#' Get value of location of data parameter from list index
#'
#' @noRd
get_index_param <- function(index = NULL,
                            location = NULL,
                            type = NULL,
                            data = NULL) {

  # Return data from index list if provided (may include bbox, sfc, or sf
  # objects)
  if (!is.null(location)) {
    if ((is.character(data) || is.numeric(data))) {
      if ("location" %in% names(index)) {
        location <- index$location[[location]]
      } else {
        location <- index[[location]]
      }
    }
    return(location)
  }

  # Return data from index list if provided (may include character (e.g. url, file path, data name if in package), bbox, sfc, or sf objects)
  if (!is.null(data)) {
    if ((is.character(data) || is.numeric(data))) {
      if ("data" %in% names(index)) {
        data <- index$data[[data]]
      } else {
        data <- index[[data]]
      }
    }
    return(data)
  }

  if (!is.null(index$type)) {
    type <- unique(index$type)
    return(type)
  } else if (!is.null(type) && (is.character(type) || is.numeric(type))) {
    # Return data from index list if provided
    type <- index[[type]]
    return(type)
  }

  NULL
}