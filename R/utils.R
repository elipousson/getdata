.onLoad <- function(lib, pkg) {
  rlang::run_on_load()
  utils::data(
    list = c("osm_building_tags", "street_suffixes", "street_dir_prefixes"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "admin_level", "bldg_num_even_odd", "block_num",
    "crs", "name", "nm", "street", "x"
  )
)

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

#' Does the data frame has a column with the same name?
#'
#' @name has_same_name_col
#' @noRd
#' @importFrom rlang has_name
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom dplyr rename
has_same_name_col <- function(x,
                              col = NULL,
                              prefix = "orig",
                              ask = FALSE,
                              quiet = FALSE,
                              drop = TRUE) {
  if (!has_name(x, col)) {
    return(x)
  }

  if (drop) {
    return(dplyr::select(x, -dplyr::all_of(col)))
  }

  new_col <- paste0(prefix, "_", col)

  if (ask && !quiet) {
    if (!cli_yeah(
      "The provided data includes an existing column named '{col}'.
    Do you want to proceed and rename this column to {new_col}?"
    )) {
      cli_abort("Please rename your column to use this function.")
    }
  }

  if (!quiet) {
    cli::cli_alert_success(
      "The existing column '{col}' to '{new_col}' to avoid
      overwriting any existing values."
    )
  }

  dplyr::rename(x, "{new_col}" := col)
}
