#' Use FlickrAPI to get geotagged photos for a location
#'
#' [get_flickr_photos()] uses [FlickrAPI::get_photo_search()] to get a data
#' frame or sf objects with photos from a specified location or matching other
#' photo search parameters. Set API key using [FlickrAPI::set_flickr_api_key()]
#' or pass to the api_key parameter.
#'
#' @param location A `sf` or `bbox` object to use in creating bounding box for
#'   getting photos from Flickr. Optional.
#' @inheritParams FlickrAPI::get_photo_search
#' @inheritParams sfext::st_bbox_ext
#' @param geometry If `TRUE`, convert data frame with information on photos to
#'   an `sf` object, Default: `TRUE`.
#' @param sort Supported options include "date-posted","date-taken",
#'   "interestingness", or "relevance"
#' @param desc If `TRUE` return images in descending sort order, if `FALSE`,
#'   return in ascending sort order. Ignored if sort is set to "relevance".
#' @param extras Defaults to "description", "date_taken", "tags", and "geo".
#' @param img_size Image size; defaults to "s" (small). Options ranging from
#'   smallest to largest size include "sq" (square), "t", "s", "q", "m", "n",
#'   "z", "c", "l", and "o" (original).
#' @param per_page Photos to return per page of search results, Default: 100.
#'   Maximum 250 if a location is provided or 500 otherwise.
#' @param page Page to return. If page is greater than length 1, loop over all
#'   pages. This may cause issues with API access if a large page range is
#'   provided. Default: 1
#' @param orientation If img_size is length 1, photos are filtered to one or
#'   more of the supported orientations ("portrait", "landscape", and "square");
#'   defaults to `NULL`.
#' @param geometry If `TRUE`, include "geo" in extras and convert photos data
#'   frame to `sf` object. Passed to geo parameter of
#'   [FlickrAPI::get_photo_search()]
#' @param crs Coordinate reference system of `sf` object to return if geometry
#'   is `TRUE`.
#' @param key Flickr API key. If api_key is `NULL`, the
#'   [FlickrAPI::getPhotoSearch] uses [FlickrAPI::getFlickrAPIKey()] to use the
#'   environment variable "FLICKR_API_KEY" as the key. Use [set_access_token()]
#'   w/ `type = "FLICKR_API_KEY"` or [FlickrAPI::setFlickrAPIKey()]
#' @inheritParams FlickrAPI::getPhotoSearch
#' @return A data frame with photo information or `sf` object with geometry
#'   based on latitude and longitude of geocoded photos.
#'
#' @details License id options:
#'
#' license_id can be an integer from 0 to 10 or a corresponding license code
#' including:
#'
#' - "c" (All Rights Reserved),
#' - "by-bc-sa" (Attribution-NonCommercial-ShareAlike),
#' - "by-nc" (Attribution-NonCommercial),
#' - "by-nc-nd" (Attribution-NonCommercial-NoDerivs),
#' - "by" (Attribution),
#' - "by-sa" (Attribution-ShareAlike),
#' - "by-nd" (Attribution-NoDerivs),
#' - "nkc" (No known copyright restrictions),
#' - "pd-us" (United States Government Work),
#' - "cc0" (Public Domain Dedication),
#' - or "pd" (Public Domain Mark).
#'
#' @seealso
#'  [FlickrAPI::getPhotoSearch()]
#' @rdname get_flickr_photos
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
get_flickr_photos <- function(location = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              user_id = NULL,
                              tags = NULL,
                              license_id = "cc0",
                              sort = "date-posted",
                              desc = FALSE,
                              img_size = "s",
                              extras = c("description", "date_taken", "tags", "geo"),
                              per_page = 100,
                              page = 1,
                              orientation = NULL,
                              geometry = TRUE,
                              crs = 4326,
                              key = NULL) {
  rlang::check_installed("FlickrAPI")

  if (length(page) > 1) {
    page_list <- map(
      page,
      \(x) {
        get_flickr_photos(
          location = location,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          user_id = user_id,
          tags = tags,
          img_size = img_size,
          license_id = license_id,
          sort = sort,
          extras = extras,
          per_page = per_page,
          page = x,
          orientation = orientation,
          geometry = geometry,
          crs = crs,
          key = key
        )
      }
    )

    if (geometry) {
      photos <- sfext:::sf_list_rbind(page_list)
    } else {
      photos <- vctrs::vec_rbind(!!!page_list)
    }

    return(photos)
  }

  if (!is.null(location) && !is_sf(location, ext = TRUE)) {
    location <- sfext::as_sf(location)
  }

  # Get adjusted bounding box if any adjustment variables provided
  bbox <- sfext::st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = 4326
  )

  photos <- FlickrAPI::get_photo_search(
    api_key = key,
    user_id = user_id,
    tags = tags,
    img_size = img_size,
    geo = geometry,
    license_id = license_id,
    sort = sort,
    bbox = bbox,
    extras = extras,
    per_page = per_page,
    page = page
  )

  photos <- tibble::as_tibble(photos)

  if (nrow(photos) == 0) {
    cli::cli_inform(
      "No photos can be found with the provided parameters."
    )
    return(invisible(NULL))
  }

  if (!is.null(img_size) && has_length(img_size, 1)) {
    photos <- get_flickr_photos_orientation(
      photos = photos,
      orientation = orientation
    )
  }

  if (all(rlang::has_name(photos, c("owner", "id")))) {
    photos <- dplyr::mutate(
      photos,
      url = glue("https://flickr.com/photos/{owner}/{id}"),
      .after = dplyr::all_of("owner")
    )
  }

  if (!geometry) {
    return(photos)
  }

  sfext::df_to_sf(
    x = photos,
    coords = c("longitude", "latitude"),
    crs = crs
  )
}

#' Get orientation of Flickr photos and optionally filter to select orientations
#'
#' @param photos Data frame with photos including "img_width" and "img_height"
#'   columns.
#' @param orientation Character vector with one or more of the orientation
#'   options "landscape", "portrait", and "square".
#' @noRd
#' @importFrom rlang has_name
#' @importFrom dplyr rename mutate case_when
get_flickr_photos_orientation <- function(photos,
                                          orientation = NULL) {
  if (rlang::has_name(photos, "img_asp")) {
    photos <- dplyr::mutate(
      photos,
      img_orientation = dplyr::case_when(
        img_asp > 1 ~ "landscape",
        img_asp < 1 ~ "portrait",
        TRUE ~ "square"
      )
    )
  }

  if (is.null(orientation) || !rlang::has_name(photos, "img_orientation")) {
    return(photos)
  }

  orientation <- match.arg(
    orientation,
    c("landscape", "portrait", "square"),
    several.ok = TRUE
  )

  photos[photos[["img_orientation"]] %in% orientation, ]
}
