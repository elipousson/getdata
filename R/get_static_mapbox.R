#' Use {mapboxapi} to get a static Mapbox map image
#'
#' Wrapper function for [mapboxapi::static_mapbox()], [get_location()], and
#' [get_osm_data()], [get_osm_id()], or [get_osm_boundaries()] functions.
#'
#' For [get_osm_static_mapbox], the ... parameters are passed to the
#' [get_osm_data()], [get_osm_id()], or [get_osm_boundaries()] functions.
#'
#' @name get_static_mapbox
#' @param type For get_osm_static_mapbox, type of feature with id; ("node", "way", or "relation"); for get_location_static_mapbox, type of location (see [get_location()] for details.
#' @param dist Buffer distance passed to buffer_dist parameter of [mapboxapi::static_mapbox()].
#' @param unit Unit of `dist` argument. `dist` is converted from `unit` to meters for [mapboxapi::static_mapbox()].
#' @param overlay_location If `TRUE`, use the location (or OpenStreetMap feature) as the overlay_sf parameter.
#' @param style_url Style URL; defaults to "mapbox://styles/mapbox/light-v10"
#' @param width,height Map width and height; defaults to 600 px width and 400 px height.
#' @example examples/get_static_mapbox.R
#' @export
#' @inheritParams mapboxapi::static_mapbox
get_static_mapbox <- function(location,
                              dist = NULL,
                              unit = "meter",
                              overlay_location = TRUE,
                              style_url = "mapbox://styles/mapbox/light-v10",
                              overlay_sf = NULL,
                              overlay_style = NULL,
                              zoom = NULL,
                              width = 600,
                              height = 400,
                              bearing = NULL,
                              pitch = NULL,
                              ...) {
  if (overlay_location && is.null(overlay_sf)) {
    overlay_sf <- location
  }

  overlay_sf <-
    set_overlay_style(
      overlay_sf = overlay_sf,
      overlay_style = overlay_style
    )

  if (!is.null(dist)) {
    buffer_dist <-
      as.numeric(
        sfext::convert_dist_units(
          dist = dist,
          from = unit,
          to = "meter"
        )
      )
  } else {
    buffer_dist <- 0
  }

  suppressMessages(
    mapboxapi::static_mapbox(
      location = location,
      buffer_dist = buffer_dist,
      style_url = style_url,
      overlay_sf = overlay_sf,
      zoom = zoom,
      pitch = pitch,
      bearing = bearing,
      width = width,
      height = height,
      ...
    )
  )
}


#' @name get_osm_static_mapbox
#' @rdname get_static_mapbox
#' @export
#' @inheritParams get_osm_data
get_osm_static_mapbox <- function(id = NULL,
                                  key = NULL,
                                  level = NULL,
                                  location = NULL,
                                  dist = NULL,
                                  unit = "meter",
                                  overlay_location = TRUE,
                                  style_url = "mapbox://styles/mapbox/light-v10",
                                  overlay_sf = NULL,
                                  overlay_style = NULL,
                                  zoom = NULL,
                                  width = 600,
                                  height = 400,
                                  bearing = NULL,
                                  pitch = NULL,
                                  ...) {
  if (!is.null(id)) {
    location <- get_osm_id(id = id, crs = 3857, ...)
  } else if (!is.null(key)) {
    location <- get_osm_data(location = location, crs = 3857, key = key, ...)
  } else if (!is.null(level) && !is.null(location)) {
    location <- get_osm_boundaries(location = location, crs = 3857, level = level, ...)
  }

  get_static_mapbox(
    location = location,
    overlay_location = overlay_location,
    overlay_sf = overlay_sf,
    overlay_style = overlay_style,
    dist = dist,
    unit = unit,
    style_url = style_url,
    zoom = zoom,
    pitch = pitch,
    bearing = bearing,
    width = width,
    height = height
  )
}

#' @name get_location_static_mapbox
#' @rdname get_static_mapbox
#' @export
#' @inheritParams get_location
get_location_static_mapbox <- function(type,
                                       dist = NULL,
                                       unit = "meter",
                                       name = NULL,
                                       name_col = "name",
                                       id = NULL,
                                       id_col = "id",
                                       location = NULL,
                                       index = NULL,
                                       union = FALSE,
                                       overlay_location = TRUE,
                                       style_url = "mapbox://styles/mapbox/light-v10",
                                       overlay_sf = NULL,
                                       overlay_style = NULL,
                                       zoom = NULL,
                                       width = 600,
                                       height = 400,
                                       bearing = NULL,
                                       pitch = NULL,
                                       ...) {
  location <-
    get_location(
      type = type,
      name = name,
      name_col = name_col,
      id = id,
      id_col = id_col,
      location = location,
      index = index,
      union = union,
      crs = 3857
    )

  get_static_mapbox(
    location = location,
    overlay_location = overlay_location,
    overlay_sf = overlay_sf,
    overlay_style = overlay_style,
    dist = dist,
    unit = unit,
    style_url = style_url,
    zoom = zoom,
    pitch = pitch,
    bearing = bearing,
    width = width,
    height = height,
    ...
  )
}

#' Set overlay style
#' @noRd
#' @importFrom utils URLencode
set_overlay_style <-
  function(overlay_sf,
           overlay_style = NULL,
           stroke = NULL,
           stroke_opacity = NULL,
           stroke_width = NULL,
           fill = NULL,
           fill_opacity = NULL) {
    if (is.null(overlay_style)) {
      overlay_style <- overlay_sf
    }

    style_names <- names(overlay_style)

    if ("stroke" %in% style_names) {
      if (!grepl("^#", overlay_style$stroke)) {
        overlay_style$stroke <- col2hex(overlay_style$stroke)
      }

      overlay_sf$stroke <-
        utils::URLencode(overlay_style$stroke, reserved = TRUE)
    }

    if ("stroke_opacity" %in% style_names) {
      overlay_sf$`stroke-opacity` <-
        utils::URLencode(overlay_style$stroke_opacity, reserved = TRUE)
    }

    if ("stroke_width" %in% style_names) {
      overlay_sf$`stroke-width` <-
        utils::URLencode(as.character(overlay_style$stroke_width), reserved = TRUE)
    }

    if ("fill" %in% style_names) {
      if (!grepl("^#", overlay_style$fill)) {
        overlay_style$fill <- col2hex(overlay_style$fill)
      }

      overlay_sf$fill <-
        utils::URLencode(overlay_style$fill, reserved = TRUE)
    }

    if ("fill_opacity" %in% style_names) {
      overlay_sf$`fill-opacity` <-
        utils::URLencode(as.character(overlay_style$fill_opacity), reserved = TRUE)
    }

    return(overlay_sf)
  }


#' Convert color name to hex value
#'
#' @noRd
#' @importFrom grDevices rgb col2rgb
col2hex <- function(color) {
  grDevices::rgb(t(grDevices::col2rgb(color)), maxColorValue = 255)
}
