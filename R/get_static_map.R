#' Use {mapboxapi} or {bingmapr} to get a static map image
#'
#' Get a static map image using the  [Mapbox Static Maps
#' API](https://www.mapbox.com/static-maps) using [mapboxapi::static_mapbox] or
#' the [Bing Maps Static Map
#' API](https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map)
#' using [bingmapr::get_map_image]. An API key or access token is required for
#' both services. Set the bingmap API token using [bingmapr::bing_maps_api_key]
#' and the Mapbox token with [mapboxapi::mb_access_token] or use
#' `set_access_token` with `type = "BING_MAPS_API_KEY"` or `type =
#' "MAPBOX_PUBLIC_TOKEN"`.
#'
#' Variations on get_static_mapbox include
#'
#' - [get_location_static_mapbox] wrapping [get_location()]
#' - [get_osm_static_mapbox] wrapping [get_osm_data()], [get_osm_id()], and
#' [get_osm_boundaries()]
#'
#' In those cases, the ... parameters are passed on the getdata functions rather
#' than the static map function.
#'
#' For [get_static_bingmap], parameter names are modified from
#' [bingmapr::get_map_image] for consistency, so the bearing parameter passed to
#' orientation and token is passed to key.
#'
#' @name get_static_map
#' @param dist Buffer distance passed to buffer_dist parameter of
#'   [mapboxapi::static_mapbox()] or to [sfext::st_buffer_ext] for
#'   [get_static_bingmap].
#' @param unit Unit of `dist` argument. Defaults to "meters".
#' @param type For [get_osm_static_mapbox], type of feature with id; ("node",
#'   "way", or "relation"); for [get_location_static_mapbox], type of location
#'   (see [get_location] for details.
#' @param overlay_location If `TRUE`, use the location (or OpenStreetMap
#'   feature) as the overlay_sf parameter. Default to `FALSE`. Ignored if
#'   overlay_sf is provided.
#' @param style_url Style URL; defaults to "mapbox://styles/mapbox/light-v10"
#' @param width,height Map width and height; defaults to 600 px width and 400 px
#'   height.
#' @param token Optional token or API key. Recommend setting the Bing Maps API
#'   key using [bingmapr::bing_maps_api_key] and the Mapbox access token with
#'   [mapboxapi::mb_access_token].
#' @example examples/get_static_mapbox.R
NULL

#' @name get_static_mapbox
#' @rdname get_static_map
#' @export
#' @inheritParams mapboxapi::static_mapbox
get_static_mapbox <- function(location,
                              dist = NULL,
                              unit = "meter",
                              style_url = "mapbox://styles/mapbox/light-v10",
                              overlay_location = FALSE,
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

  dist <- dist %||% 0

  if (!is.null(unit) && (unit != "meter")) {
    dist <-
      sfext::convert_dist_units(
        dist,
        from = unit,
        to = "m",
        drop = TRUE
      )
  }

  mapboxapi::static_mapbox(
    location = location,
    buffer_dist = dist,
    style_url = style_url,
    overlay_sf = overlay_sf,
    zoom = zoom,
    pitch = pitch,
    bearing = bearing,
    width = width,
    height = height,
    ...
  )
}


#' @name get_osm_static_mapbox
#' @rdname get_static_map
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
    location <-
      get_osm_id(id = id, crs = 3857, ...)
  } else if (!is.null(key)) {
    location <-
      get_osm_data(location = location, crs = 3857, key = key, ...)
  } else if (!is.null(level) && !is.null(location)) {
    location <-
      get_osm_boundaries(location = location, crs = 3857, level = level, ...)
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
#' @rdname get_static_map
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

#' @name get_static_bingmap
#' @inheritParams bingmapr::get_map_image
#' @rdname get_static_map
#' @export
get_static_bingmap <- function(location = NULL,
                               dist = NULL,
                               unit = "m",
                               imagery = "BirdsEye",
                               zoom = NULL,
                               width = 600,
                               height = 400,
                               bearing = NULL,
                               token = NULL,
                               ...) {
  is_pkg_installed("bingmapr")

  location <-
    # FIXME: If bingmapr::get_map_image only uses the centroid - does the dist
    # parameter make any difference?
    sfext::st_buffer_ext(
      location,
      dist = dist,
      unit = unit
    )

  bingmapr::get_map_image(
    location = location,
    imagery = imagery,
    zoom = zoom,
    width = width,
    height = height,
    orientation = bearing %||% 0,
    key = token,
    ...
  )
}
