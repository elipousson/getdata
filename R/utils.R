.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("osm_building_tags", "street_suffixes", "street_dir_prefixes"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "admin_level", "bldg_num_even_odd", "block_num",
    "crs", "name", "nm", "street", "x", "city", "county",
    "file_name", "owner", "state", "subLayerIds"
  )
)

# @staticimports pkg:stringstatic
# str_replace

# @staticimports pkg:isstatic
# is_url is_esri_url is_gsheet_url is_gist_url is_gmap_url
# is_unit

#' Add default user agent to request and perform request
#'
#' @noRd
#' @importFrom httr2 req_user_agent
req_getdata <-
  function(req,
           string = getOption(
             "getdata.useragent",
             default = "getdata (https://github.com/elipousson/getdata)"
           )) {
    req <-
      httr2::req_user_agent(
        req = req,
        string = string
      )

    httr2::req_perform(req)
  }


#' Eval and parse data
#'
#' @noRd
use_eval_parse <- function(data, package = NULL) {
  data <- paste0(collapse = "::", c(package, data))
  eval(parse(text = data))
}

#' Apply function to data
#'
#' @param data Data to apply function to
#' @param fn defaults to NULL
#' @importFrom rlang as_function
#' @noRd
use_fn <- function(data, fn = NULL) {
  if (is.null(fn)) {
    return(data)
  }

  fn <- as_function(fn)
  fn(data)
}

#' Use .name_repair parameter
#'
#' @noRd
#' @importFrom vctrs vec_as_names
use_name_repair <- function(data = NULL,
                            .name_repair = "check_unique",
                            repair_arg = "name_repair") {
  names(data) <-
    vctrs::vec_as_names(
      names(data),
      repair = .name_repair,
      repair_arg = repair_arg
    )

  data
}

#' Is this package installed?
#'
#' @param pkg Name of a package.
#' @param repo GitHub repository to use for the package.
#' @noRd
#' @importFrom rlang check_installed
check_dev_installed <- function(pkg = NULL, repo = NULL) {
  if (!is.null(pkg) && !rlang::is_installed(pkg = pkg)) {
    rlang::check_installed(pkg = repo %||% pkg)
  }
}


#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom rlang has_name
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom cliExtras cli_yesno
#' @importFrom dplyr rename
has_same_name_col <- function(x,
                              col = NULL,
                              prefix = "orig",
                              ask = FALSE,
                              quiet = FALSE,
                              drop = TRUE) {
  if (!rlang::has_name(x, col)) {
    return(x)
  }

  if (drop) {
    return(dplyr::select(x, -dplyr::all_of(col)))
  }

  new_col <- paste0(prefix, "_", col)

  if (ask && !quiet) {
    if (!cliExtras::cli_yesno(
      c(
        "!" = "The provided data includes an existing column named {.val col}.",
        " " = "Do you want to proceed and rename this column to {.val new_col}?"
      )
    )) {
      cli_abort("Please rename your column to use this function.")
    }
  }

  if (!quiet) {
    cli::cli_inform(
      c("v" = "Renaming the existing column '{col}' to '{new_col}' to avoid
      overwriting existing values.")
    )
  }

  dplyr::rename(x, "{new_col}" := col)
}
