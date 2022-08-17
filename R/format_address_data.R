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
#' @param case Case to use for text in new columns or in modified values.
#'   Options include "lower", "upper", "title", or "sentence". Defaults to `NULL`.
NULL

#' @name bind_block_col
#' @rdname format_address_data
#' @param bldg_num,street_dir_prefix,street_name,street_suffix Column names to
#'   use for address information required to generate a block name and number.
#' @param block_col String to use as prefix for block identifier columns and
#'   separator between block number and street. Set to "block" when `NULL`
#'   (default).
#' @param replace_suffix If `TRUE`, replace values in street_suffix column with
#'   abbreviations from [street_suffixes].
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr mutate across all_of if_else
bind_block_col <- function(x,
                           bldg_num = "bldg_num",
                           street_dir_prefix = "street_dir_prefix",
                           street_name = "street_name",
                           street_suffix = "street_type",
                           replace_suffix = FALSE,
                           block_col = NULL,
                           case = NULL) {
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

  if (replace_suffix) {
    x <- replace_street_suffixes(x, street_suffix = street_suffix)
  }

  x <-
    str_empty_to_blank_across(
      x,
      .cols = dplyr::all_of(c(street_dir_prefix, street_suffix))
    )

  block_col <- block_col %||% "block"

  block_col_labels <-
    paste0(block_col, "_", c("num", "even_odd", "segment", "face"))

  x <- dplyr::mutate(
    x,
    "{block_col_labels[[1]]}" := floor(.data[[bldg_num]] / 100) * 100,
    "{block_col_labels[[2]]}" := dplyr::if_else(
      (.data[[bldg_num]] %% 2) == 0, "Even", "Odd"
    ),
    "{block_col_labels[[3]]}" := paste(
      .data[[block_col_labels[[1]]]], block_col,
      .data[[street_dir_prefix]], .data[[street_name]], .data[[street_suffix]]
    ),
    "{block_col_labels[[4]]}" := paste(
      .data[[block_col_labels[[3]]]],
      paste0("(", .data[[block_col_labels[[2]]]], ")")
    )
  )

  x <- str_to_case_across(x, dplyr::all_of(block_col_labels), case)

  squish_cols <- c(block_col_labels[[3]], block_col_labels[[4]])

  str_squish_across(x, dplyr::all_of(squish_cols))
}

#' @name bind_address_col
#' @rdname format_address_data
#' @param city,county,state City, county, and state to bind to data frame or
#'   `sf` object.
#' @export
#' @importFrom dplyr mutate
bind_address_col <- function(x, city = NULL, county = NULL, state = NULL, case = NULL) {
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

  str_to_case_across(
    x,
    dplyr::any_of(city, county, state),
    case
  )
}

#' Replace values in a character vector or data frame with a crosswalk
#'
#' @inheritParams dplyr::across
#' @param xwalk,dict Named list or data frame with a minimum of two columns
#'   where one column contains the replacement values and the other the values
#'   to replace. If xwalk is `NULL`, dict is used and vice-versa. If both are
#'   provided, the xwalk values take precedence so they can be used to override
#'   a dict or add new values.
#' @param abb If abb is `TRUE` (default), the second column of the dict is
#'   assumed to be abbreviation that should be used as the replace for the
#'   values in x or the replacement column. Otherwise, the first column is
#'   assumed to hold the replacement values and the second column is assumed to
#'   hold the original values. For example, for [replace_street_suffixes()], If
#'   `TRUE`, replace full suffix names with abbreviations. If `FALSE`, replace
#'   abbreviations with full street suffix names.
#' @export
#' @importFrom rlang is_character
#' @importFrom dplyr mutate
replace_with_xwalk <- function(x,
                               .cols = NULL,
                               xwalk = NULL,
                               dict = NULL,
                               abb = TRUE,
                               case = NULL) {
  is_pkg_installed("stringr")

  if (!is.null(dict)) {
    dict_cols <- c(1:2)
    if (abb) {
      # Keep the abbreviation column first if converting abbreviation to full
      # suffix
      dict_cols <- c(2:1)
    }

    dict <- make_xwalk_list(dict[, dict_cols])

    if (is.null(xwalk)) {
      xwalk <- dict
    } else {
      xwalk <- make_xwalk_list(xwalk)
      xwalk <- modifyList(dict, xwalk)
    }
  }

  # stringr::str_replace_all requires a named vector (not a list)
  xwalk <- unlist(xwalk)

  if (is.data.frame(x)) {
    x <-
      dplyr::mutate(
        x,
        dplyr::across(
          .cols,
          ~ stringr::str_replace_all(.x, xwalk)
        )
      )
  } else if (is.character(x)) {
    x <- stringr::str_replace_all(x, xwalk)
  }

  if (is.null(case)) {
    return(x)
  }

  switch_case(x, case)
}

#' @name replace_street_suffixes
#' @rdname replace_with_xwalk
#' @export
replace_street_suffixes <- function(x,
                                    street_suffix = "street_type",
                                    xwalk = NULL,
                                    case = NULL,
                                    abb = TRUE) {
  replace_with_xwalk(
    x,
    col = street_suffix,
    xwalk = xwalk,
    dict = street_suffixes,
    abb = abb,
    case = case
  )
}

#' @name replace_street_dir_prefixes
#' @rdname replace_with_xwalk
replace_street_dir_prefixes <- function(x,
                                        street_dir_prefix = "street_dir_prefix",
                                        xwalk = NULL,
                                        case = NULL,
                                        abb = TRUE) {
  replace_with_xwalk(
    x,
    col = street_dir_prefix,
    xwalk = xwalk,
    dict = street_dir_prefixes,
    abb = abb,
    case = case
  )
}
