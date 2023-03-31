#' Set getdata or other package-specific options
#'
#' This function can set named options for a package using the convention of
#' "pkg.option". For examples `set_pkg_options(crs = 2804, .pkg =
#' "getdata")` sets the option "getdata.crs" to 2804. If "getdata.crs" is
#' already set, overwrite must be `TRUE` to replace the existing value.
#'
#' @section Options for the getdata package:
#'
#' Implemented options (with defaults if used) for the getdata package include:
#'
#' - dist
#' - diag_ratio
#' - unit ("meter")
#' - asp
#' - crs (3857)
#' - from_crs (4326)
#' - address ("address")
#' - package
#' - filetype ("gpkg")
#'
#' A similar convention is used for the maplayer package. The use of options is
#' not implemented across all functions and may be changed in the future.
#'
#' @param ... Named list of options to set, e.g. "crs = 2804" with `.pkg =
#'   "getdata"` to set "getdata.crs" to 2804.
#' @param overwrite If `TRUE`, overwrite any existing option value.
#' @param .pkg Package name to append to option name. Defaults to "getdata".
#' @name set_pkg_options
#' @export
#' @importFrom cliExtras cli_ul_items
set_pkg_options <- function(...,
                            overwrite = FALSE,
                            .pkg = "getdata") {
  opts <- list2(...)
  pkg_nm <- .pkg

  # Make vector of names based on provided options and .pkg
  nm_opts <- paste0(pkg_nm, ".", names(opts))
  names(opts) <- nm_opts

  # Get existing options matching new_opts
  existing_opts <- sapply(nm_opts, getOption)
  existing_opts <- discard(existing_opts, is.null)

  existing_nm <- names(existing_opts)
  conflict_nm <- existing_nm %in% nm_opts

  conflict_opts <- NULL
  update_opts <- opts

  if (any(conflict_nm)) {
    conflict_opts <- sapply(existing_opts[conflict_nm], as.character)
    names(conflict_opts) <- existing_nm[conflict_nm]
  }

  if (!is.null(conflict_opts)) {
    if (!overwrite) {
      cli::cli_inform(
        c("x" = "The provided options conflict with these existing values:")
      )
      cliExtras::cli_ul_items(conflict_opts)
      cli::cli_inform(
        c("i" = "Set {.code overwrite = TRUE} to replace these
          {.pkg {pkg_nm}} options.")
      )

      if (!all(conflict_nm)) {
        update_opts <- opts[[!conflict_nm]]
      } else {
        update_opts <- NULL
      }
    } else {
      cli::cli_inform(c("!" = "Replacing these existing options:"))
      cliExtras::cli_ul_items(conflict_opts)
    }
  }

  if (!is.null(update_opts)) {
    options(update_opts)

    cli::cli_inform(c("v" = "Updated options for {.pkg {pkg_nm}}:"))
    cliExtras::cli_ul_items(update_opts)
  }
}
