#' Cache location data with `sf::write_sf()` or `readr::write_rds()`
#'
#' [cache_location_data()] is a variant on [get_location_data()] that saves the
#' returned data to a cache directory using [sf::write_sf()] (if data is a sf
#' object) or [readr::write_rds()] (if data is another class).
#'
#' @inheritParams filenamr::make_filename
#' @inheritParams filenamr::check_file_overwrite
#' @inheritParams get_location_data
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
    pad = pad,
    cache = cache,
    pkg = pkg,
    create = create,
    call = call
  )

  data <- get_location_data(
    data = data,
    location = location,
    ...,
    call = call
  )

  filenamr::check_file_overwrite(
    filename = filename,
    path = path,
    overwrite = overwrite,
    call = call
  )

  cli_alert_success("Writing {.file {basename(filename)}} to {.path {dirname(filename)}}")

  if (inherits(data, "sf")) {
    sf::st_write(data, filename, quiet = TRUE)
    return(invisible(filename))
  }

  check_installed("readr", call = call)
  readr::write_rds(data, str_add_fileext(filename, "rds"))
  invisible(filename)
}
