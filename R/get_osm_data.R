#' Use osmdata to get Open Street Map data for a location
#'
#' Use `osmdata` functions to query the overpass API and access OSM data by
#' adjusted bounding box or by enclosing ways/relations around the center of a
#' location. For more information on key and value options, refer to the
#' `osm_common_tags` reference table or the OSM Wiki
#' <https://wiki.openstreetmap.org/wiki/Map_features>. Use the `osmdata` package
#' directly for more detailed control over queries
#' <https://docs.ropensci.org/osmdata/>
#'
#' @param location A `sf`, `sfc`, or `bbox` object converted to bounding box
#'   with [sfext::st_bbox_ext()] or a character object passed directly to the
#'   bbox parameter of [osmdata::opq()].
#' @param key Feature key for overpass API query.
#' @param value Value of the feature key; can be negated with an initial
#'   exclamation mark, `value = "!this"`, and can also be a vector, `value =
#'   c("this", "that")`. If `value = "all"` or if `key = "building"` the values
#'   passed to the osmdata package are from a preset list extracted from
#'   [osmdata::available_tags()].
#' @inheritParams sfext::st_bbox_ext
#' @param geometry Geometry type to output ("polygons", "points", "lines",
#'   "multilines", or "multipolygons"); if multiple geometry types are needed
#'   set osmdata to `TRUE.` Default `NULL`.
#' @param crs Coordinate reference system for output data; if `NULL`, the data
#'   remains in the Open Street Map coordinate reference system 4326. Default:
#'   `NULL`.
#' @param osmdata If `TRUE` return a `osmdata` class object that includes the
#'   overpass API call, metadata including timestamp and version numbers, and
#'   all available geometry types; defaults to `FALSE`.
#' @param enclosing If enclosing is "relation" or "way", this function uses the
#'   [osmdata::opq_enclosing()] to query the OSM data (instead of
#'   [osmdata::add_osm_feature()]. Defaults to `NULL`. When the enclosing
#'   parameter is provided, the dist, diag_ratio, asp, and unit parameters are
#'   ignored and the center of the provided location is used for the query.
#'   geometry is set automatically based enclosing with "relation" using
#'   "multipolygons" and "way" using "polygons" geometry.
#' @inheritParams osmdata::opq
#' @param features A named list with the format `list("<key>" = "<value>")` or a
#'   character vector of key-value pairs with keys and values enclosed in
#'   escape-formatted quotations (see [osmdata::add_osm_features()]) for
#'   examples of the latter option.
#' @inheritParams format_data
#' @inheritParams osmdata::add_osm_feature
#' @return A simple feature object with features using selected geometry type or
#'   an `osmdata` object with features from all geometry types.
#' @export
get_osm_data <- function(location = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL,
                         key,
                         value = NULL,
                         features = NULL,
                         id = NULL,
                         type = NULL,
                         crs = NULL,
                         geometry = NULL,
                         osmdata = FALSE,
                         enclosing = NULL,
                         nodes_only = FALSE,
                         key_exact = TRUE,
                         value_exact = TRUE,
                         match_case = TRUE) {
  osm_data_attribution()

  if (!is.null(id)) {
    return(
      get_osm_id(
        id = id,
        type = type,
        crs = crs,
        geometry = geometry,
        osmdata = osmdata
      )
    )
  }

  if (is.null(features)) {
    value <- get_osm_value(key, value)
  }

  if (!is.null(enclosing)) {
    return(
      get_osm_data_enclosing(
        location = location,
        key = key,
        value = value,
        enclosing = enclosing,
        crs = crs,
        geometry = geometry,
        osmdata = osmdata
      )
    )
  }

  get_osm_data_features(
    location = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    key = key,
    value = value,
    features = features,
    crs = crs,
    geometry = geometry,
    osmdata = osmdata,
    nodes_only = nodes_only,
    key_exact = key_exact,
    value_exact = value_exact,
    match_case = match_case
  )
}


#' @rdname get_osm_data
#' @name get_osm_id
#' @param id OpenStreetMap feature id with or without a type id prefix. If
#'   multiple id values are provided, they must use a single consistent value
#'   for geometry.
#' @param type Type of feature for the id; "node", "way", or "relation".
#'   Optional if id includes a type prefix.
#' @export
#' @importFrom purrr map_dfr
get_osm_id <- function(id,
                       type = NULL,
                       crs = NULL,
                       geometry = NULL,
                       osmdata = FALSE) {
  osm_data_attribution()

  if (length(id) > 1) {
    return(
      purrr::map_dfr(
        id,
        ~ get_osm_id(
          id = .x,
          type = type,
          crs = crs,
          geometry = geometry,
          osmdata = FALSE
        )
      )
    )
  }

  id_type <- get_osm_id_type(id = id, type = type, geometry = geometry)

  data <-
    try_osmdata_sf(
      osmdata::opq_string(
        osmdata::opq_osm_id(type = id_type$type, id = id_type$id)
      )
    )

  get_osm_data_geometry(
    data,
    geometry = id_type$geometry,
    crs = crs,
    osmdata = osmdata
  )
}

#' Try to fetch data using osmdata_sf and abort if it returns an error
#'
#' @noRd
#' @importFrom rlang caller_env try_fetch
try_osmdata_sf <- function(query, call = caller_env()) {
  data <-
    try_fetch(
      suppressMessages(osmdata::osmdata_sf(query)),
      error = function(cnd) {
        cli_abort(
          "{.fn osmdata::osmdata_sf} encountered an error.",
          parent = cnd,
          call = call
        )
      }
    )

  data
}

#' Get a list of the OSM id, type, and geometry from a named list or type
#' prefixed id value
#'
#' @noRd
#' @importFrom rlang caller_env arg_match
get_osm_id_type <- function(id,
                            type = NULL,
                            geometry = NULL,
                            call = caller_env()) {
  if (is.null(type)) {
    if (starts_with_osm_type(id)) {
      split_id <- strsplit(id, split = "/")
      type <- split_id[[1]][1]
      id <- split_id[[1]][2]
    } else if (has_osm_type_name(id)) {
      type <- names(id)
    }
  }

  type <- arg_match(type, c("node", "way", "relation"), error_call = call)

  if (is.null(geometry)) {
    geometry <-
      switch(type,
        "node" = "points",
        "way" = "polygons",
        "relation" = "multipolygons"
      )
  }

  list("type" = type, "id" = as.character(id), "geometry" = geometry)
}

#' Is this an OpenStreetMap element (id with type)?
#'
#' @noRd
is_osm_element <- function(x) {
  starts_with_osm_type(x) | has_osm_type_name(x)
}

#' @noRd
starts_with_osm_type <- function(x) {
  grepl("^node/|^way/|^relation/", x)
}

#' @noRd
has_osm_type_name <- function(x) {
  names(x) %in% c("node", "way", "relation")
}

#' @param level administrative level (admin_level) of boundary to return;
#'   defaults to `NULL`. See
#'   <https://wiki.openstreetmap.org/wiki/Key:admin_level> for more information.
#'   Only used for [get_osm_boundaries()].
#' @param lang Language for boundary names to include in resulting data frame
#'   (e.g. "en" for English or "es" for Spanish). Default language names should
#'   always be included in results. Defaults to "en". See
#'   <https://wiki.openstreetmap.org/wiki/Multilingual_names> for more
#'   information.
#' @rdname get_osm_data
#' @name get_osm_boundaries
#' @export
#' @importFrom dplyr filter between
#' @importFrom janitor clean_names
#' @importFrom sfext transform_sf
get_osm_boundaries <- function(location,
                               level = NULL,
                               lang = "en",
                               crs = NULL,
                               enclosing = "relation",
                               geometry = NULL,
                               osmdata = FALSE) {
  osm_data_attribution()

  boundaries <-
    get_osm_data_enclosing(
      location = location,
      key = "boundary",
      value = "administrative",
      enclosing = enclosing,
      crs = crs,
      geometry = geometry,
      osmdata = osmdata
    )

  if (osmdata) {
    return(boundaries)
  }

  if (!is.null(level)) {
    boundaries <-
      dplyr::filter(
        boundaries,
        dplyr::between(admin_level, min(level), max(level))
      )
  }

  boundaries <- janitor::clean_names(boundaries)

  boundaries_nm <- names(boundaries)

  nm_prefix <-
    c(
      "name",
      "official_name",
      "short_name",
      "alt_name",
      "alt_short_name",
      "old_name",
      "old_short_name",
      "source_name",
      "not_official_name"
    )

  nm_cols <-
    grep(
      pattern = paste(paste0("^", nm_prefix), collapse = "|"),
      x = boundaries_nm,
      value = TRUE
    )

  drop_nm_cols <-
    nm_cols[!(nm_cols %in% c(nm_prefix, paste(nm_prefix, lang, sep = "_")))]

  boundaries <- boundaries[, !(boundaries_nm %in% drop_nm_cols)]

  sfext::transform_sf(x = boundaries, crs = crs)
}


#' Get OSM data using a bounding box for a location
#'
#' @noRd
get_osm_data_features <- function(location,
                                  key,
                                  value = NULL,
                                  dist = NULL,
                                  diag_ratio = NULL,
                                  unit = NULL,
                                  asp = NULL,
                                  crs = NULL,
                                  geometry = NULL,
                                  nodes_only = FALSE,
                                  features = NULL,
                                  osmdata = FALSE,
                                  key_exact = TRUE,
                                  value_exact = TRUE,
                                  match_case = TRUE,
                                  call = .envir,
                                  .envir = parent.frame()) {
  osm_crs <- 4326

  if (is_sf(location, ext = TRUE)) {
    # Get adjusted bounding box if any adjustment variables provided
    bbox_osm <-
      st_bbox_ext(
        x = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = osm_crs
      )
  } else if (is.character(location)) {
    bbox_osm <- location
  }

  if (nodes_only) {
    geometry <- geometry %||% "points"
  }

  query <-
    osmdata::opq(
      bbox = bbox_osm,
      nodes_only = nodes_only,
      timeout = 90
    )

  if (is.null(features)) {
    query <-
      osmdata::add_osm_feature(
        opq = query,
        key = key,
        value = value,
        key_exact = key_exact,
        value_exact = value_exact,
        match_case = match_case
      )
  } else {
    if (is.list(features) && rlang::is_named(features)) {
      features <- paste0('\"', names(features), '\"=\"', features, '\"')
    } else if (!is.character(features)) {
      cli_abort(
        "{.arg features} must be {.code NULL}, a {.cls character} vector,
        or a named {.cls list}.",
        call = call,
        .envir = .envir
      )
    }

    query <-
      osmdata::add_osm_features(
        opq = query,
        features = features
      )
  }

  data <- try_osmdata_sf(query)

  get_osm_data_geometry(
    data,
    geometry = geometry,
    crs = crs,
    osmdata = osmdata
  )
}

#' @noRd
#' @importFrom sfext sf_to_df
#' @importFrom rlang caller_env arg_match
get_osm_data_enclosing <- function(location,
                                   key,
                                   value,
                                   enclosing = NULL,
                                   crs = NULL,
                                   geometry = NULL,
                                   osmdata = FALSE,
                                   call = caller_env()) {
  enclosing <- rlang::arg_match(enclosing, c("relation", "way"), error_call = call)
  coords <- sfext::sf_to_df(location, crs = 4326)

  query <-
    try(
      osmdata::opq_enclosing(
        lon = coords$lon,
        lat = coords$lat,
        key = key,
        value = value,
        enclosing = enclosing
      ),
      silent = TRUE
    )

  query <- osmdata::opq_string(opq = query)

  data <- try_osmdata_sf(query)

  geometry <-
    geometry %||%
    switch(enclosing,
      "relation" = "multipolygons",
      "way" = "polygons"
    )

  get_osm_data_geometry(
    data,
    geometry = geometry,
    crs = crs,
    osmdata = osmdata
  )
}

#' Get geometry from osmdata list
#'
#' @noRd
#' @importFrom sfext st_transform_ext
#' @importFrom rlang caller_env arg_match
get_osm_data_geometry <- function(data,
                                  geometry = NULL,
                                  crs = NULL,
                                  osmdata = FALSE,
                                  call = caller_env()) {
  if (osmdata | is.null(geometry)) {
    if (is.null(geometry) && !osmdata) {
      cli_warn(
        c("{.arg geometry} is {.code NULL}.",
          "i" = "Setting {.arg osmdata} to {.val TRUE} and returning
         a list of unique features with all geometry types."
        )
      )
    }

    return(osmdata::unique_osmdata(data))
  }

  if (!grepl("s$", geometry, perl = TRUE)) {
    geometry <- paste0(geometry, "s")
  }

  geometry <-
    arg_match(
      geometry,
      c(
        "polygons",
        "points",
        "lines",
        "multilines",
        "multipolygons"
      ),
      error_call = call
    )

  geometry <- paste0("osm_", geometry)

  data <- data[[geometry]]

  sfext::st_transform_ext(data, crs)
}

#' Get OSM value from osm_building_tags or osmdata::available_tags
#'
#' @noRd
get_osm_value <- function(key = NULL, value = NULL) {
  check_null(key)

  is_building_key <- key == "building"

  if (!is_building_key) {
    if (is.null(value)) {
      cli_warn("Setting missing {.arg value} to {.val all}")
      value <- "all"
    }

    if (value == "all") {
      return(osmdata::available_tags(key))
    }
  }

  if (is_building_key && (is.null(value) | (value == "all"))) {
    return(osm_building_tags)
  }

  value
}

#' Display OpenStreetMap data attribution reminder
#'
#' @noRd
osm_data_attribution <- function(.frequency = "regularly",
                                 .frequency_id = "get_osm_data_attribution") {
  rlang::check_installed("osmdata")
  cli_inform(
    c(
      "i" = "OpenStreetMap data is licensed under the Open Database License
      (ODbL). Attribution is required if you use this data.",
      "*" = "Learn more about the ODbL and OSM attribution requirements at
      {.url https://www.openstreetmap.org/copyright}"
    ),
    .frequency = .frequency,
    .frequency_id = .frequency_id
  )
}
