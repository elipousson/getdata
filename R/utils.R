.onLoad <- function(lib, pkg) {
  run_on_load()

  utils::data(
    list = c(
      "osm_building_tags", "us_counties", "us_states"
    ),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c("admin_level", "bldg_num_even_odd", "block_num", "crs", "name", "nm", "street", "x")
)

#' Add default user agent to request
#'
#' @noRd
#' @importFrom httr2 req_user_agent
req_getdata_user <- function(req,
                             string = getOption("getdata.useragent", default = "getdata (https://github.com/elipousson/getdata)")) {
  httr2::req_user_agent(
    req = req,
    string = string
  )
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

  fn <- rlang::as_function(fn)
  fn(data)
}

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom rlang has_name
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom dplyr rename
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE, drop = TRUE) {
  if (rlang::has_name(x, col) && !drop) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cli_yeah("The provided data includes an existing column named '{col}'.
                   Do you want to proceed and rename this column to {new_col}?")) {
        cli::cli_abort("Please rename your column to use this function.")
      }
    }

    if (!quiet) {
      cli::cli_alert_success(
        "The existing column '{col}' to '{new_col}' to avoid overwriting any existing values."
      )
    }

    x <-
      dplyr::rename(
        x,
        "{new_col}" := col
      )
  } else if (rlang::has_name(x, col) && drop) {
    x <-
      dplyr::select(
        x,
        -dplyr::all_of(col)
      )
  }

  x
}



#' Set join function based on geometry type
#'
#' @name set_join_by_geom_type
#' @inheritParams is_geom_type
#' @param join geometry predicate function; defaults to `NULL`, set to
#'   [sf::st_intersects] if key_list contains only POLYGON or MULTIPOLYGON
#'   objects or [sf::st_nearest_feature] if key_list contains other types.
#' @importFrom sf st_intersects st_nearest_feature
#' @noRd
set_join_by_geom_type <- function(x, join = NULL) {
  if (!is.null(join)) {
    return(join)
  }

  if (all(sapply(x, is_polygon) | sapply(x, is_multipolygon))) {
    return(sf::st_intersects)
  }

  sf::st_nearest_feature
}
