#' Use esri2sf to get data from an ArcGIS FeatureServer or MapServer for a
#' location
#'
#' Wraps the [esri2sf::esri2sf] and [esri2sf::esri2df] function to download an
#' ArcGIS FeatureServer or MapServer. Supports spatial filtering with bounding
#' box based on location and filtering by location name (if location name column
#' is provided).
#'
#' @param location `sf`, `sfc`, or `bbox` object (or other object convertible with
#'   [as_bbox()]. Optional.
#' @param url FeatureServer or MapServer url to retrieve data from. Passed to
#'   `url` parameter of [esri2sf::esri2sf] or
#'   [esri2sf::esri2df] functions.
#' @param where string for where condition. Default is 1=1 for all rows.
#' @param where where query string passed to esri2sf, Default: `NULL`
#' @param coords_col coordinate columns, e.g. c("longitude", "latitude")
#' @param name_col name of ArcGIS FeatureServer or MapServer column with
#'   location names for features
#' @param name location name
#' @param ... Additional arguments passed to [esri2sf::esri2df] or
#'   [esri2sf::esri2sf]
#' @inheritParams format_data
#' @inheritParams sfext::sf_bbox_to_lonlat_query
#' @inheritParams sfext::st_bbox_ext
#' @seealso
#'  [esri2sf::esri2sf()]
#' @rdname get_esri_data
#' @export
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_esri_data <- function(location = NULL,
                          url,
                          dist = getOption("getdata.dist"),
                          diag_ratio = getOption("getdata.diag_ratio"),
                          unit = getOption("getdata.unit"),
                          asp = getOption("getdata.asp"),
                          crs = getOption("getdata.crs"),
                          name = NULL,
                          name_col = NULL,
                          where = NULL,
                          coords = NULL,
                          clean_names = TRUE,
                          ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "yonghah/esri2sf")

  meta <- esri2sf::esrimeta(url)

  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )

    if (!is.null(coords)) {
      # Get Table (no geometry) by filtering coordinate columns with bbox

      if (!is.null(where)) {
        where <- paste0("(", where, ")")
      }

      where <- c(
        where,
        sfext::sf_bbox_to_lonlat_query(bbox = bbox, coords = coords_col)
      )

      data <- esri2sf::esri2df(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    } else {
      # Get FeatureServer with geometry
      data <- esri2sf::esri2sf(
        url = url,
        where = where,
        bbox = bbox,
        crs = crs,
        progress = TRUE,
        ...
      )
    }
  } else if (!is.null(name_col)) {
    where <- c(
      where,
      glue::glue("{name_col} = '{name}'")
    )

    if (meta$type == "Table") {
      # Get Table (no geometry) with location name column
      data <- esri2sf::esri2df(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    } else {
      data <- esri2sf::esri2sf(
        url = url,
        where = paste(where[!is.na(where)], collapse = " AND "),
        ...
      )
    }
  } else {
    if (meta$type == "Table") {
      # Get Table (no geometry)
      data <- esri2sf::esri2df(
        url = url,
        progress = TRUE,
        ...
      )
    } else {
      # Get FeatureServer with no bounding box
      data <- esri2sf::esri2sf(
        url = url,
        progress = TRUE,
        ...
      )
    }
  }

  if (!is.null(coords)) {
    # Convert Table to sf object if coordinate columns exist
    data <- sfext::df_to_sf(data, coords = coords)
  }

  if (clean_names) {
    data <- janitor::clean_names(data)
  }

  data
}

#' Get multiple ArcGIS feature server layers
#'
#' @name get_esri_layers
#' @rdname get_esri_data
#' @param layers Either a vector with URLs, a named list of urls, or a numeric
#'   vector.
#' @param service_url Base service URL with layers are located.
#' @param nm Name or vector of names to add to the layers; defaults to `NULL`.
#' @export
#' @importFrom dplyr case_when
#' @importFrom  purrr map_chr
get_esri_layers <- function(location = NULL, layers, service_url = NULL, nm = NULL, ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "yonghah/esri2sf")

  type <-
    dplyr::case_when(
      is.numeric(layers) && !is.null(service_url) && is_esri_url(service_url) ~ "id",
      is.list(layers) && is_named(layers) ~ "nm_list",
      is.list(layers) ~ "list",
      is.character(layers) ~ "url"
    )

  layer_urls <- NULL

  layer_urls <-
    switch(type,
      "id" = paste0(service_url, "/", layers),
      "nm_list" = as.character(layers),
      "list" = as.character(layers),
      "url" = layers
    )

  if (is.null(nm)) {
    if ((type == "nm_list")) {
      nm <- names(layers)
    } else {
      nm <- purrr::map_chr(layer_urls, ~ get_esri_metadata(.x))
    }
  }

  data <- as.list(layer_urls)
  names(data) <- nm

  map_location_data(
    location = location,
    data = data,
    ...
  )
}

#' @name get_esri_metadata
#' @rdname get_esri_data
#' @param meta Name of metadata list value to return from [esri2sf::esrimeta].
#' @param clean If TRUE, use janitor::make_clean_names on the returned metadata
#'   value (typically used for name values).
#' @export
#' @importFrom janitor make_clean_names
get_esri_metadata <- function(url, meta = "name", clean = TRUE) {
  is_pkg_installed(pkg = "esri2sf", repo = "yonghah/esri2sf")

  meta <- esri2sf::esrimeta(url)[[meta]]

  if (clean) {
    janitor::make_clean_names(meta)
  } else {
    nm
  }
}
