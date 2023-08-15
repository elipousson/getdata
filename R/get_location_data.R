#' Get data for a location
#'
#' Returns data for a selected location or a list of locations (for
#' [map_location_data()]). If data is a character string, the parameter is
#' passed to [sfext::read_sf_url()], [sfext::read_sf_path()], or
#' [sfext::read_sf_pkg()]. This function uses [sfext::st_filter_ext()] to
#' filter, crop, or trim data to the provided location. location can also be an
#' an address.
#'
#' This function previously supported county geoid, state name, abbreviation, or
#' geoid as a location. Currently, recommend using [get_tigris_data()] and
#' passing a `sf` object to location.
#'
#' @details Working with sf lists for data and locations:
#'
#'   [map_location_data()] makes it easier to work with `sf` lists. It supports
#'   data as a character vector, data as an `sf` list when location is a single
#'   object, location as a character vector or `sf` list (including lists of
#'   `bbox` or `sfc` objects), or when both data and location are lists (such as
#'   a list created by [make_location_data_list()]).
#'
#' @param location sf object. If multiple areas are provided, they are unioned
#'   into a single sf object using [sf::st_union()]
#' @inheritParams sfext::st_bbox_ext
#' @param data Character string (e.g. url, file path, or name of data from
#'   package) for a spatial data or a `sf`, `sfc`, or `bbox`  object with
#'   geometry overlapping the location. If data is `NULL`, all unnamed
#'   parameters are passed to [sfext::read_sf_ext()] with a bbox based on
#'   location. If data is not `NULL` and not a data.frame, url, file path, or
#'   bbox, conversion to a sf object will still always be attempted with
#'   [sfext::as_sf()].
#' @param package Name of the package to search for data.
#' @param fileext,filetype File extension or type to use if passing parameters
#'   to [sfext::read_sf_download()] or [sfext::read_sf_pkg()] (required for
#'   extdata and cached data).
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
#'   supports nested lists created by [make_location_data_list()] (using only
#'   the default key names of "location" and "data"). This feature has not be
#'   fully tested and may result in errors or unexpected results.
#' @param label label is optionally used by [map_location_data()] to name the
#'   data objects in the list returned by the function.
#' @inheritParams sfext::as_sf
#' @inheritParams sfext::as_sf_list
#' @inheritParams format_data
#' @inheritParams sfext::lonlat_to_sfc
#' @param ... Additional parameters passed to [sfext::read_sf_path()],
#'   [sfext::read_sf_url()], [sfext::read_sf_pkg()], [sfext::as_sf()] (with
#'   bbox), or [sfext::read_sf_ext()] (with no other parameters).
#' @rdname get_location_data
#' @export
#' @importFrom sf st_crs st_crop st_transform st_intersection st_filter
#' @importFrom rlang is_string list2 `!!!`
get_location_data <- function(location = NULL,
                              dist = getOption("getdata.dist"),
                              diag_ratio = getOption("getdata.diag_ratio"),
                              unit = getOption("getdata.unit", default = "meter"),
                              asp = getOption("getdata.asp"),
                              data = NULL,
                              pkg = getOption("getdata.package"),
                              package = getOption("getdata.package"),
                              fileext = getOption("getdata.fileext", default = "gpkg"),
                              filetype = getOption("getdata.filetype", default = "gpkg"),
                              fn = NULL,
                              crop = TRUE,
                              trim = FALSE,
                              from_crs = getOption("getdata.from_crs"),
                              crs = getOption("getdata.crs", 3857),
                              class = "sf",
                              label = NULL,
                              index = NULL,
                              col = NULL,
                              var_names = NULL,
                              clean_names = FALSE,
                              range = NULL,
                              ...) {
  fileext <- fileext %||% filetype
  pkg <- pkg %||% package

  if (!is.null(index) && is.list(index)) {
    # FIXME: This is set to work with 1 or 2 level list indices with naming
    # conventions that match make_location_data_list This should be clearly
    # documented as alternate index naming conventions supported if possible
    if (any(has_name(index, c("pkg", "package"))) && is.null(pkg)) {
      index[["pkg"]] <- index[["pkg"]] %||% index[["package"]]
      pkg <- unique(index[["pkg"]]) # could use data as an index
      check_string(pkg, allow_empty = FALSE)
    }

    location <- get_index_param(index, location = location)
    data <- get_index_param(index, data = data)
  }

  if (!is_null(location) && !sfext::is_sf(location)) {
    if (sfext::is_geo_coords(location)) {
      location <- sfext::lonlat_to_sfc(location, range)
    } else {
      location <- sfext::as_sf(location)
    }
  }

  # Get adjusted bounding box using any adjustment variables provided
  bbox <- sfext::st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = from_crs
  )

  if (!sfext::is_sf(data)) {
    type <- "ext"

    if (!is.null(data)) {
      type <- dplyr::case_when(
        is_url(data) ~ "url",
        rlang::is_string(data) && file.exists(data) ~ "path",
        !is.null(pkg) ~ "pkg",
        is.data.frame(data) ~ "df",
        .default = "sf"
      )
    }

    data <- switch(type,
      "url" = sfext::read_sf_url(url = data, bbox = bbox, ...),
      "path" = sfext::read_sf_path(path = data, bbox = bbox, ...),
      "pkg" = sfext::read_sf_pkg(
        data = data, bbox = bbox, pkg = pkg, fileext = fileext, ...
      ),
      "sf" = sfext::as_sf(data, ...),
      "df" = sfext::df_to_sf(x = data, from_crs = from_crs, ...),
      "ext" = sfext::read_sf_ext(!!!rlang::list2(...), bbox = bbox)
    )
  }

  if (is_empty(data)) {
    cli_abort(
      "{.arg data} can't be found."
    )
  }
  data <- sfext::st_make_valid_ext(data)

  if (crop) {
    data <- sfext::st_filter_ext(
      data,
      bbox,
      crop = TRUE
    )
  } else if (trim) {
    data <- sfext::st_filter_ext(
      data,
      location,
      trim = TRUE
    )
  } else {
    if (!is_all_null(list(dist, diag_ratio, asp))) {
      location <- bbox
    }

    data <- sfext::st_filter_ext(
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
#' @export
#' @importFrom janitor make_clean_names
#' @importFrom rlang set_names is_named
map_location_data <- function(location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              data = NULL,
                              package = NULL,
                              fileext = "gpkg",
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
                              range = NULL,
                              ...) {
  fileext <- fileext %||% filetype
  # FIXME: This triggers an alert with lintr but it is used
  params <- list2(...)

  if (!is.list(data) && (length(data) > 1)) {
    data <- as.list(data)

    # FIXME: The addition of a index parameter to get_location_data should allow
    # the use of the index as a secondary source of name data for
    # map_location_data
    if (!is_named(data)) {
      if (!is.null(label)) {
        label <- janitor::make_clean_names(label)
      }

      data <- rlang::set_names(
        data,
        nm = map_chr(
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

  if (is.list(data) && is.list(location) &&
    (length(data) == length(location))) {
    data <- map2(
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
        fileext = fileext,
        fn = fn,
        crop = crop,
        trim = trim,
        from_crs = from_crs,
        crs = crs,
        name_col = params[["name_col"]],
        name = params[["name"]],
        index = index,
        range = range
      )
    )
  } else if (is.list(data)) {
    data <- map(
      data,
      ~ get_location_data(
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        data = .x,
        package = package,
        fileext = fileext,
        fn = fn,
        crop = crop,
        trim = trim,
        from_crs = from_crs,
        crs = crs,
        name_col = params[["name_col"]],
        name = params[["name"]],
        index = index
      )
    )
  } else if (is.list(location)) {
    data <- map(
      location,
      ~ get_location_data(
        location = .x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        data = data,
        package = package,
        fileext = fileext,
        fn = fn,
        crop = crop,
        trim = trim,
        from_crs = from_crs,
        crs = crs,
        name_col = params[["name_col"]],
        name = params[["name"]],
        index = index
      )
    )
  }

  data <- discard(data, ~ nrow(.x) == 0)
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
      if (has_name(index, "location")) {
        location <- index[["location"]][[location]]
      } else {
        location <- index[[location]]
      }
    }
    return(location)
  }

  # Return data from index list if provided (may include character (e.g. url,
  # file path, data name if in package), bbox, sfc, or sf objects)
  if (!is.null(data)) {
    if ((is.character(data) || is.numeric(data))) {
      if (has_name(index, "data")) {
        data <- index[["data"]][[data]]
      } else {
        data <- index[[data]]
      }
    }
    return(data)
  }

  if (has_name(index, "type")) {
    type <- unique(index$type)
    return(type)
  } else if (!is.null(type) && (is.character(type) || is.numeric(type))) {
    # Return data from index list if provided
    type <- index[[type]]
    return(type)
  }

  NULL
}
