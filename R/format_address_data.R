#' Format data frames and simple features with address data
#'
#' @description
#' getdata has two helpers for working with address data:
#'
#'  - [bind_address_col] bind a provided value for city, county, and state to a
#'  data frame (to supplement address data with consistent values for these
#'  variables)
#' - [bind_block_col] requires a data frame with columns named "bldg_num",
#' "street_dir_prefix", "street_name", and "street_type" and binds derived
#' values for whether a building is on the even or odd side of a block and
#' create a block segment and a block face (including the even/odd identifier).
#'
#' @name format_address_data
#'
NULL

#' @name bind_block_col
#' @rdname format_address_data
#' @param bldg_num,street_dir_prefix,street_name,street_suffix Column names to
#'   use for address information required to generate a block name and number.
#' @param block_col String to use as prefix for block identifier columns.
#'   Defaults to "block".
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr mutate if_else across all_of
bind_block_col <- function(x,
                           bldg_num = "bldg_num",
                           street_dir_prefix = "street_dir_prefix",
                           street_name = "street_name",
                           street_suffix = "street_type",
                           block_col = "block") {
  address_cols <- c(bldg_num, street_dir_prefix, street_name, street_suffix)
  x_missing_cols <- address_cols[!rlang::has_name(x, address_cols)]

  cli_abort_ifnot(
    c("{.arg x} must have columns named {.val {address_cols}}.",
      "i" = "{.arg x} is missing {length(x_missing_cols)} column{?s}
      {.val {x_missing_cols}}"
    ),
    condition = length(x_missing_cols) == 0
  )

  if (is.character(x[[bldg_num]])) {
    x[[bldg_num]] <- as.numeric(x[[bldg_num]])
  }

  block_col_labels <-
    paste0(block_col, "_", c("num", "even_odd", "segment", "face"))

  x <- dplyr::mutate(
    x,
    "{block_col_labels[[1]]}" := floor(.data[[bldg_num]] / 100) * 100,
    "{block_col_labels[[2]]}" := dplyr::if_else(
      (.data[[bldg_num]] %% 2) == 0, "Even", "Odd"),
    "{block_col_labels[[3]]}" := paste(
      .data[[block_col_labels[[1]]]], block_col,
      .data[[street_dir_prefix]], .data[[street_name]], .data[[street_suffix]]),
      "{block_col_labels[[4]]}" := paste(
        .data[[block_col_labels[[3]]]],
        paste0("(", .data[[block_col_labels[[2]]]], ")")
      )
    )

  squish_cols <- c(block_col_labels[[3]], block_col_labels[[4]])

  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::all_of(squish_cols),
      ~ gsub("\\s\\s+", " ", .x, perl = TRUE)
      )
  )
}


#' @name bind_address_col
#' @rdname format_address_data
#' @param city,county,state City, county, and state to bind to data frame or
#'   `sf` object.
#' @export
#' @importFrom dplyr mutate
bind_address_col <- function(x, city = NULL, county = NULL, state = NULL) {
  if (!is.null(city)) {
    x <- has_same_name_col(x, col = "city")
    x <- dplyr::mutate(x, city = city)
  }

  if (!is.null(county)) {
    x <- has_same_name_col(x, col = "county")
    x <- dplyr::mutate(x, county = county)
  }

  if (!is.null(state)) {
    x <- has_same_name_col(x, col = "state")
    x <- dplyr::mutate(x, state = state)
  }

  x
}
