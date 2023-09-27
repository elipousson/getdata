#' Cache location data with `sf::write_sf()` or `readr::write_rds()`
#'
#' [cache_location_data()] is a variant on [get_location_data()] that saves the
#' returned data to a cache directory using [sf::write_sf()] (if data is a sf
#' object) or [readr::write_rds()] (if data is another class). Additional
#' parameters are ignored if location is `NULL`.
#'
#' @inheritParams filenamr::make_filename
#' @param pkg Package name passed to appname parameter of
#'   [rappdirs::user_cache_dir()]
#' @inheritParams filenamr::check_file_overwrite
#' @inheritParams get_location_data
#' @inheritParams readr::write_rds
#' @inheritDotParams get_location_data
#' @returns Save data to file and invisibly return file path.
#' @export
#' @importFrom sf st_write
cache_location_data <- function(data = NULL,
                                ...,
                                location = NULL,
                                name = NULL,
                                label = NULL,
                                fileext = "gpkg",
                                filename = NULL,
                                path = NULL,
                                prefix = NULL,
                                postfix = NULL,
                                cache = TRUE,
                                pkg = "getdata",
                                create = TRUE,
                                overwrite = FALSE,
                                compress = c("none", "gz", "bz2", "xz"),
                                version = 3,
                                call = caller_env()) {
  check_dev_installed("filenamr", repo = "elipousson/filenamr", call = call)

  filename <- filenamr::make_filename(
    name = name,
    label = label,
    fileext = fileext,
    filename = filename,
    path = path,
    prefix = prefix,
    postfix = postfix,
    cache = cache,
    pkg = pkg,
    create = create,
    call = call
  )

  if (!is_null(location)) {
    data <- get_location_data(
      data = data,
      location = location,
      ...,
      call = call
    )
  }

  filenamr::check_file_overwrite(
    filename = filename,
    path = path,
    overwrite = overwrite,
    call = call
  )

  message <- "Writing {.file {basename(filename)}} to {.path {dirname(filename)}}"

  if (inherits(data, "sf")) {
    cli_alert_success(message)
    sf::st_write(data, filename, quiet = TRUE)
    return(invisible(filename))
  }

  check_installed("readr", call = call)
  file <- str_add_fileext(str_remove_fileext(filename), "rds")
  message <- "Writing {.file {basename(filename)}} to {.path {dirname(filename)}}"
  cli_alert_success(message)
  readr::write_rds(data, file = file, compress = compress, version = version)
  invisible(filename)
}
