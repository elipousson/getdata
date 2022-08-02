#' Use esri2sf to get data from an ArcGIS FeatureServer or MapServer for a
#' location
#'
#' Wraps the [esri2sf::esri2sf] and [esri2sf::esri2df] function to download an
#' ArcGIS FeatureServer or MapServer. Supports spatial filtering with bounding
#' box based on location and filtering by location name (if location name column
#' is provided). As of July 2022, this package suggests the
#' [elipousson/esri2sf](https://github.com/elipousson/esri2sf/) fork using httr2
#' but I expect to see this fork merged back into the main package in the
#' future.
#'
#' @param location `sf`, `sfc`, or `bbox` object (or other object convertible
#'   with [as_bbox()]. Optional.
#' @param url FeatureServer or MapServer url to retrieve data from. Passed to
#'   `url` parameter of [esri2sf::esri2sf] or [esri2sf::esri2df] functions. For
#'   [get_esri_layers], the optional url must be a service url which is the base
#'   url for one or more layer urls.
#' @param where where query string passed to esri2sf, Default: `NULL`
#' @inheritParams sfext::df_to_sf
#' @param name,name_col Name value and name column found in the ArcGIS
#'   FeatureServer or MapServer data.
#' @inheritParams format_data
#' @inheritParams sfext::sf_bbox_to_lonlat_query
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams esri2sf::esri2sf
#' @inheritDotParams esri2sf::esri2sf
#' @seealso
#'  [esri2sf::esri2sf()]
#' @rdname get_esri_data
#' @export
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_esri_data <- function(location = NULL,
                          dist = getOption("getdata.dist"),
                          diag_ratio = getOption("getdata.diag_ratio"),
                          unit = getOption("getdata.unit"),
                          asp = getOption("getdata.asp"),
                          crs = getOption("getdata.crs", 3857),
                          url,
                          where = NULL,
                          name = NULL,
                          name_col = NULL,
                          coords = NULL,
                          from_crs = getOption("getdata.crs", 4326),
                          clean_names = TRUE,
                          progress = TRUE,
                          ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "elipousson/esri2sf")

  meta <- esri2sf::esrimeta(url)
  table <- any(c(is.null(meta$geometryType), (meta$geometryType == "")))

  bbox <- NULL

  if (!is.null(where)) {
    where <- glue("({where})")
  }

  if (!is.null(location)) {
    # Adjust bounding box
    bbox <- st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp
    )

    if (!is.null(coords) && table) {
      where <- c(
        where,
        sfext::sf_bbox_to_lonlat_query(bbox = bbox, coords = coords)
      )
    }
  }

  if (!is.null(name_col)) {
    where <- c(
      where,
      glue("({name_col} = '{name}')")
    )
  }

  if (!is.null(where)) {
    where <- paste(where[!is.na(where)], collapse = " AND ")
  }

  if (table) {
    # Get Table (no geometry) with location name column
    data <- esri2sf::esri2df(
      url = url,
      where = where,
      progress = progress,
      ...
    )
  } else {
    data <- esri2sf::esri2sf(
      url = url,
      where = where,
      bbox = bbox,
      progress = progress,
      crs = crs,
      ...
    )
  }

  if (!is.null(coords) && table) {
    # Convert Table to sf object if coordinate columns exist
    data <-
      sfext::df_to_sf(
      data,
      coords = coords,
      from_crs = from_crs,
      crs = crs
      )
  }

  if (!clean_names) {
    return(data)
  }

  # TODO: Expand support for format_data parameters especially
  # label_with_xwalk using alias from the esri layer metadata
  format_data(data, clean_names = clean_names)
}

#' Get multiple ArcGIS feature server layers
#'
#' @name get_esri_layers
#' @rdname get_esri_data
#' @param layers Either a vector with URLs, a named list of urls, or a numeric
#'   vector.
#' @param nm Name or vector of names to add to the layers; defaults to `NULL`.
#' @export
#' @importFrom dplyr case_when
#' @importFrom  purrr map_chr
get_esri_layers <- function(location = NULL,
                            layers,
                            url = NULL,
                            nm = NULL,
                            ...) {
  is_pkg_installed(pkg = "esri2sf", repo = "elipousson/esri2sf")

  type <-
    dplyr::case_when(
      is.numeric(layers) && !is.null(url) && is_esri_url(url) ~ "id",
      is.list(layers) && is_named(layers) ~ "nm_list",
      is.list(layers) ~ "list",
      is.character(layers) ~ "url"
    )

  layer_urls <- NULL

  layer_urls <-
    switch(type,
      "id" = paste0(url, "/", layers),
      "nm_list" = as.character(layers),
      "list" = as.character(layers),
      "url" = layers
    )

    if (type == "nm_list") {
      nm <- nm  %||% names(layers)
    } else {
      nm <- nm %||% purrr::map_chr(layer_urls, ~ get_esri_metadata(.x))
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
#' @param clean_names If `TRUE`, use janitor::make_clean_names on the returned metadata
#'   value (typically used for name values).
#' @export
#' @importFrom janitor make_clean_names
get_esri_metadata <- function(url, meta = "name", clean_names = TRUE) {
  is_pkg_installed(pkg = "esri2sf", repo = "elipousson/esri2sf")

  metadata <- esri2sf::esrimeta(url)

  if (!is.null(meta)) {
    meta <- metadata[[meta]]
  }

  if (!clean_names) {
    return(meta)
  }

  janitor::make_clean_names(meta)
}
