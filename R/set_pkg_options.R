#' Set overedge packge options
#'
#' Can set options for package, diag_ratio, dist, asp, or crs. Equivalent to
#' [overedge::set_overedge_options()]
#'
#'
#' @param dist,diag_ratio,unit,asp,data_package,data_filetype,coords,from_crs,address,crs,state
#'    options to set, e.g. "crs = 2804" with `pkg = "getdata"` to set
#'   "getdata.crs" to 2804.
#' @param overwrite If `TRUE`, overwrite any existing option value.
#' @name set_pkg_options
#' @export
#' @importFrom purrr set_names
#' @importFrom cli cli_vec
set_pkg_options <- function(dist = NULL,
                            diag_ratio = NULL,
                            unit = NULL,
                            asp = NULL,
                            data_package = NULL,
                            data_filetype = NULL,
                            coords = NULL,
                            from_crs = NULL,
                            address = NULL,
                            crs = NULL,
                            state = NULL,
                            overwrite = TRUE,
                            pkg = "getdata") {
  possible_options <-
    paste0(
      pkg,
      ".",
      c(
        "dist",
        "diag_ratio",
        "unit",
        "asp",
        "data_package",
        "data_filetype",
        "coords",
        "from_crs",
        "address",
        "state",
        "crs"
      )
    )


  update_options <-
    purrr::set_names(
      list(
        dist, diag_ratio, unit, asp,
        data_package, data_filetype, coords,
        from_crs, address, state,
        crs
      ),
      nm = possible_options
    )

  update_options <-
    update_options[!sapply(update_options, is.null)]

  existing_options <-
    sapply(
      possible_options,
      getOption
    )

  existing_options <-
    existing_options[!sapply(existing_options, is.null)]


  if (overwrite | all(sapply(existing_options, is.null))) {
    options(
      update_options
    )

    update_options <-
      cli::cli_vec(update_options, style = list(vec_last = " and "))

    cli_inform(
      c("v" = "{pkg} options updated for {.arg {update_options}}.")
    )
  }
}
