#' Use osmdata to get Open Street Map data for a location
#'
#' Use `osmdata` functions to query OSM data by adjusted bounding box or
#' by enclosing ways/relations around the center of a location.
#'
#' @param location A `sf`, `sfc`, or `bbox` object.
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
#' @inheritParams osmdata::add_osm_features
#' @inheritParams sfext::format_data
#' @return A simple feature object with features using selected geometry type or
#'   an `osmdata` object with features from all geometry types.
#' @export
#' @importFrom purrr pluck
#' @importFrom sf st_transform
#' @importFrom cli cli_alert_info
get_osm_data <- function(location = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = NULL,
                         asp = NULL,
                         key,
                         value = NULL,
                         features = NULL,
                         crs = NULL,
                         geometry = NULL,
                         osmdata = FALSE,
                         enclosing = NULL,
                         nodes_only = FALSE) {
  is_pkg_installed("osmdata")

  if (is.null(features)) {
    value <- get_osm_value(key, value)
  } else {
    stopifnot(
      "features must be NULL or a character vector" = is.character(features)
    )
  }

  if (nodes_only) {
    geometry <- geometry %||% "points"
  }

  cli_inform(
    c(
      "i" = "OpenStreetMap data is licensed under the Open Database License (ODbL) which requires attribution.",
      "*" = "Learn more about the ODbL and OSM attribution requirements at {.url https://www.openstreetmap.org/copyright}"
    ),
    .frequency = "once",
    .frequency_id = "get_osm_data_attribution"
  )

  if (is.null(enclosing)) {
    return(
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
        nodes_only = nodes_only
      )
    )
  }

  get_osm_data_enclosing(
    location = location,
    key = key,
    value = value,
    enclosing = enclosing,
    crs = crs,
    geometry = geometry,
    osmdata = osmdata
  )
}


#' @rdname get_osm_data
#' @name get_osm_id
#' @param id OpenStreetMap feature id
#' @param type type of feature with id; "node", "way", or "relation"
#' @export
#' @importFrom purrr map_dfr
get_osm_id <- function(id, type = NULL, crs = NULL, geometry = NULL, osmdata = FALSE) {
  is_pkg_installed("osmdata")

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
try_osmdata_sf <- function(query, call = caller_env()) {
  data <-
    try_fetch(
      suppressMessages(osmdata::osmdata_sf(
        query
      )),
      error = function(cnd) cli_abort("{.fn osmdata::osmdata_sf} encountered an error.", parent = cnd, call = call)
    )

  data
}

#' Get a list of the OSM id, type, and geometry from a named list or type prefixed id value
#'
#' @noRd
get_osm_id_type <- function(id, type = NULL, geometry = NULL, call = caller_env()) {
  if (is.null(type)) {
    if (has_osm_type_prefix(id)) {
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
  (has_osm_type_prefix(x) | has_osm_type_name(x))
}

#' @noRd
has_osm_type_prefix <- function(x) {
  grepl("^node/|^way/|^relation/", x)
}

#' @noRd
has_osm_type_name <- function(x) {
  names(x) %in% c("node", "way", "relation")
}

#' @param level administrative level (admin_level) of boundary to return;
#'   defaults to `NULL`. See <https://wiki.openstreetmap.org/wiki/Key:admin_level>
#'   for more information. Only used for [get_osm_boundaries].
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
get_osm_boundaries <- function(location,
                               level = NULL,
                               lang = "en",
                               crs = NULL,
                               enclosing = "relation",
                               geometry = NULL,
                               osmdata = FALSE) {
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

  drop_nm_cols <- nm_cols[!(nm_cols %in% c(nm_prefix, paste(nm_prefix, lang, sep = "_")))]

  boundaries <- boundaries[, !(boundaries_nm %in% drop_nm_cols)]

  sfext::st_transform_ext(x = boundaries, crs = crs)
}


#' Get OSM data using a bounding box for a location
#'
#' @noRd
get_osm_data_features <- function(location = NULL,
                                  key,
                                  value,
                                  dist = NULL,
                                  diag_ratio = NULL,
                                  unit = NULL,
                                  asp = NULL,
                                  crs = NULL,
                                  geometry = NULL,
                                  nodes_only = FALSE,
                                  features = NULL,
                                  osmdata = FALSE) {
  osm_crs <- 4326

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
        value = value
      )
  } else {
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
get_osm_data_enclosing <- function(location,
                                   key,
                                   value,
                                   enclosing = NULL,
                                   crs = NULL,
                                   geometry = NULL,
                                   osmdata = FALSE,
                                   call = caller_env()) {
  enclosing <- arg_match(enclosing, c("relation", "way"), error_call = call)
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
#' @importFrom purrr pluck
#' @importFrom sf st_transform
get_osm_data_geometry <- function(data,
                                  geometry = NULL,
                                  crs = NULL,
                                  osmdata = FALSE,
                                  call = caller_env()) {
  if (osmdata | is.null(geometry)) {
    if (is.null(geometry) && !osmdata) {
      cli_warn(
        "{.arg geometry} is {.code NULL}. Setting {.arg osmdata} to {.val TRUE}."
      )
    }

    return(osmdata::unique_osmdata(data))
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

  data <-
    purrr::pluck(
      data,
      var = geometry
    )

  sfext::st_transform_ext(data, crs)
}

#' Get OSM value from osm_building_tags or osmdata::available_tags
#'
#' @noRd
get_osm_value <- function(key = NULL, value = NULL) {
  check_null(key)

  if (is.null(value) && (key != "building")) {
    cli_warn("Setting missing {.arg value} to {.val all}")
    value <- "all"
  }

  if (key == "building" && (is.null(value) | (value == "all"))) {
    return(osm_building_tags)
  }

  if (value == "all") {
    return(osmdata::available_tags(key))
  }

  value
}
