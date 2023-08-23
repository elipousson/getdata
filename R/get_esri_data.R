#' Use esri2sf to get data from an ArcGIS FeatureServer or MapServer for a
#' location
#'
#' Wraps the [esri2sf::esri2sf()] and [esri2sf::esri2df()] functions to download
#' an ArcGIS FeatureServer or MapServer. Supports spatial filtering with
#' bounding box based on location and filtering by location name (if location
#' name column is provided). As of fall 2022, this package suggests the
#' [elipousson/esri2sf](https://github.com/elipousson/esri2sf/) fork using
#' httr2.
#'
#' @param url FeatureServer or MapServer url to retrieve data from. Passed to
#'   `url` parameter of [esri2sf::esri2sf()] or [esri2sf::esri2df()] functions.
#'   For [get_esri_layers()], the optional url must be a service url which is
#'   the base url for one or more layer urls.
#' @param location `sf`, `sfc`, or `bbox` object (or other object convertible
#'   with [as_bbox()]. Optional.
#' @param where where query string passed to esri2sf, Default: `NULL`
#' @inheritParams sfext::df_to_sf
#' @param name,name_col Name value and name column found in the ArcGIS
#'   FeatureServer or MapServer data.
#' @inheritParams format_data
#' @inheritParams sfext::sf_bbox_to_lonlat_query
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams esri2sf::esri2sf
#' @inheritDotParams esri2sf::esri2sf -bbox -geometry -geomType -spatialRel
#' @rdname get_esri_data
#' @export
#' @importFrom glue glue
#' @importFrom janitor clean_names
get_esri_data <- function(url,
                          location = NULL,
                          dist = getOption("getdata.dist"),
                          diag_ratio = getOption("getdata.diag_ratio"),
                          unit = getOption("getdata.unit"),
                          asp = getOption("getdata.asp"),
                          crs = getOption("getdata.crs", 3857),
                          where = NULL,
                          name = NULL,
                          name_col = NULL,
                          coords = NULL,
                          from_crs = getOption("getdata.crs", 4326),
                          clean_names = TRUE,
                          token = NULL,
                          progress = TRUE,
                          quiet = FALSE,
                          .name_repair = janitor::make_clean_names,
                          ...) {
  meta <- get_esri_metadata(url, token, clean_names = FALSE)

  if (has_name(meta, "error")) {
    error <- meta[["error"]]
    message <- c(error[["code"]], "i" = error[["message"]])

    if (!is_empty(error[["details"]])) {
      message <- c(message, as.character(error[["details"]]))
    }

    cli::cli_abort(message = message)
  }

  # Set table to TRUE for missing geometry type
  table <- any(c(is.null(meta[["geometryType"]]), (meta[["geometryType"]] == "")))
  # Set table to FALSE for Group Layer URLs
  table <- table && meta[["type"]] != "Group Layer"

  bbox <- NULL

  if (!clean_names) {
    .name_repair <- "check_unique"
  }

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
    name_col <- rlang::arg_match(name_col, meta[["fields"]][["name"]])
    where <- c(where, query_sql(name_col, name))
  }

  if (!is.null(where)) {
    where <- glue::glue_collapse(discard(where, is.na), sep = " AND ")
  }

  if (table) {
    # Get Table (no geometry) with location name column
    data <- esri2sf::esri2df(
      url = url,
      token = token,
      where = where,
      progress = progress,
      .name_repair = .name_repair,
      quiet = quiet,
      ...
    )
  } else {
    data <- esri2sf::esri2sf(
      url = url,
      token = token,
      where = where,
      bbox = bbox,
      progress = progress,
      crs = crs,
      .name_repair = .name_repair,
      quiet = quiet,
      ...
    )
  }

  if (!is.null(coords) && table) {
    # Convert Table to sf object if coordinate columns exist
    data <- sfext::df_to_sf(
      data,
      coords = coords,
      from_crs = from_crs,
      crs = crs
    )
  }

  # TODO: Expand support for format_data parameters especially
  # label_with_xwalk using alias from the esri layer metadata
  # format_data(data, clean_names = clean_names)
  data
}

#' Get multiple ArcGIS feature server layers
#'
#' @name get_esri_layers
#' @rdname get_esri_data
#' @param layers Either a vector with URLs, a named list of urls, or a numeric
#'   vector; defaults to `NULL`. Optional if url is a
#' @param nm Name or vector of names to add to the layers; defaults to `NULL`.
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang has_name list2 set_names is_named
#' @importFrom janitor make_clean_names
get_esri_layers <- function(location = NULL,
                            layers = NULL,
                            url = NULL,
                            nm = NULL,
                            token = NULL,
                            clean_names = TRUE,
                            quiet = FALSE,
                            .name_repair = janitor::make_clean_names,
                            ...,
                            call = caller_env()) {
  if (!is.null(url) && is_esri_url(url)) {
    url <- sub("/$", "", url)
  }

  layers <- layers %||% url

  type <- dplyr::case_when(
    is.numeric(layers) && !is.null(url) && is_esri_url(url) ~ "id",
    all(is.list(layers) && is_named(layers)) ~ "nm_list",
    is.list(layers) ~ "list",
    all(is_esri_url(layers)) ~ "url"
  )

  type <- unique(type)

  if (type == "url") {
    meta <- get_esri_metadata(layers, token = token, call = call)

    if (any(rlang::has_name(meta, c("layers", "subLayers")))) {
      layer_list <- get_layer_list(meta)

      layers <- get_esri_layers(
        location = location,
        layers = layer_list[["id"]],
        url = gsub("[0-9]+$|[0-9]+/$", "", layers, perl = TRUE),
        nm = layer_list[["name"]],
        token = token,
        clean_names = clean_names,
        quiet = quiet,
        .name_repair = .name_repair,
        ...
      )

      if (clean_names) {
        layers <- rlang::set_names(
          layers,
          janitor::make_clean_names(names(layers))
        )
      }

      return(layers)
    }
  }

  layer_urls <- NULL

  layer_urls <- switch(type,
    "id" = paste0(url, "/", layers),
    "nm_list" = as.character(layers),
    "list" = as.character(layers),
    "url" = layers
  )

  if (type == "nm_list") {
    nm <- nm %||% names(layers)

    if (clean_names) {
      nm <- janitor::make_clean_names(nm)
    }
  } else {
    nm <- nm %||%
      map_chr(
        layer_urls,
        function(x) {
          get_esri_metadata(
            x,
            token = token,
            meta = "name",
            clean_names = clean_names,
            call = call
          )
        }
      )
  }

  layer_urls <- as.list(layer_urls)

  params <- rlang::list2(...)

  data <- map(
    layer_urls,
    ~ get_esri_data(
      url = .x,
      location = location,
      token = token,
      dist = params[["dist"]],
      diag_ratio = params[["diag_ratio"]],
      asp = params[["asp"]],
      crs = params[["crs"]] %||% getOption("getdata.crs", 3857),
      unit = params[["unit"]],
      where = params[["where"]],
      name = params[["name"]],
      name_col = params[["name_col"]],
      coords = params[["coords"]],
      clean_names = clean_names,
      quiet = quiet,
      .name_repair = .name_repair,
      progress = params[["progress"]] %||% TRUE
    )
  )

  rlang::set_names(data, nm)
}

#' Get layer and sublayer IDs and names from metadata
#'
#' @noRd
get_layer_list <- function(meta) {
  if (rlang::has_name(meta, "layers")) {
    type <- "layers"
  } else if (rlang::has_name(meta, "subLayers")) {
    type <- "subLayers"
  } else {
    return(NULL)
  }

  layer_list <- meta[[type]]

  if (rlang::has_name(layer_list, "subLayerIds")) {
    layer_list <- dplyr::filter(
      layer_list,
      is.na(subLayerIds) | is.null(subLayerIds)
      )
  }

  list(
    "id" = layer_list[["id"]],
    "name" = layer_list[["name"]]
  )
}

#' @noRd
#' @importFrom rlang current_env
query_sql <- function(var, val, op = "IN", ..., .envir = current_env()) {
  ansi_sql(var, " ", op, " ({val*})", ..., .envir = .envir)
}


#' @noRd
#' @importFrom DBI ANSI
#' @importFrom glue glue_sql
ansi_sql <- function(...,
                     .con = DBI::ANSI()) {
  glue_sql(..., .con = .con)
}

#' @name get_esri_metadata
#' @rdname get_esri_data
#' @param meta Name of metadata list value to return from [esri2sf::esrimeta],
#'   e.g. "name" to return layer name. Defaults to `NULL`.
#' @param clean_names If `TRUE`, set .name_repair to
#'   [janitor::make_clean_names()] Ignored when [get_esri_metadata()] is not
#'   returning a data.frame, e.g. `meta = "id"`.
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom janitor make_clean_names
#' @importFrom rlang check_required is_character
get_esri_metadata <- function(url,
                              token = NULL,
                              meta = NULL,
                              clean_names = TRUE,
                              .name_repair = janitor::make_clean_names,
                              call = caller_env()) {
  rlang::check_required(url, call = call)

  metadata <- esri2sf::esrimeta(url, token, call = call)

  if (!is.null(meta)) {
    metadata <- metadata[[meta]]
  }

  if (clean_names) {
    .name_repair <- janitor::make_clean_names
  }

  if (is.null(.name_repair) || !is.data.frame(metadata)) {
    return(metadata)
  }

  use_name_repair(metadata, .name_repair)
}
