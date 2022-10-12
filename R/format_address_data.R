#' Format data frames and simple features with address data
#'
#' @description
#' getdata has two helpers for working with address data:
#'
#'  - [bind_address_col] bind a provided value for city, county, and state to a
#'  data frame (to supplement address data with consistent values for these
#'  variables). This function is useful for converting partial street addresses
#'  with a consistent values for state, county, or city into full addresses
#' - [bind_block_col] requires a data frame with columns named "bldg_num",
#' "street_dir_prefix", "street_name", and "street_type" and binds derived
#' values for whether a building is on the even or odd side of a block and
#' create a block segment and a block face (including the even/odd identifier).
#'
#' @name format_address_data
#' @param case Case to use for text in new columns or in modified values.
#'   Options include "lower", "upper", "title", or "sentence". Defaults to
#'   `NULL` which leaves the case as is.
NULL

#' @name bind_block_col
#' @rdname format_address_data
#' @param bldg_num,street_dir_prefix,street_name,street_suffix Column names to
#'   use for address information required to generate a block name and number.
#' @param block_col String to use as prefix for block identifier columns and
#'   separator between block number and street. Set to "block" when `NULL`
#'   (default). If length 2 (e.g. c("blk", "block")), the second value is used
#'   as the block separator and the first as the column identifier prefix.
#' @param replace_suffix If `TRUE`, replace values in street_suffix column with
#'   abbreviations from [street_suffixes].
#' @param .after passed to [dplyr::mutate()] defaults to street_suffix for
#'   [bind_block_col()] and "address" for [bind_address_col()].
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
                           .after = street_suffix,
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
      .cols = dplyr::all_of(c(street_dir_prefix, street_name, street_suffix))
    )

  block_col <- block_col %||% "block"

  if (length(block_col) > 1) {
    block_sep <- block_col[[2]]
    block_col <- block_col[[1]]
  } else {
    block_sep <- block_col
  }

  block_col_labels <-
    paste0(block_col, "_", c("num", "even_odd", "segment", "face"))

  x <- dplyr::mutate(
    x,
    "{block_col_labels[[1]]}" := floor(.data[[bldg_num]] / 100) * 100,
    "{block_col_labels[[2]]}" := dplyr::if_else(
      (.data[[bldg_num]] %% 2) == 0, "Even", "Odd"
    ),
    "{block_col_labels[[3]]}" := dplyr::if_else(
      !is.na(.data[[block_col_labels[[1]]]]),
      paste(
        .data[[block_col_labels[[1]]]], block_sep,
        .data[[street_dir_prefix]], .data[[street_name]], .data[[street_suffix]]
      ), ""
    ),
    "{block_col_labels[[4]]}" := dplyr::if_else(
      !is.na(.data[[block_col_labels[[1]]]]),
      paste(
        .data[[block_col_labels[[3]]]],
        paste0("(", .data[[block_col_labels[[2]]]], ")")
      ), ""
    ),
    .after = street_suffix
  )

  x <- str_to_case_across(x, dplyr::all_of(block_col_labels), case)

  squish_cols <- c(block_col_labels[[3]], block_col_labels[[4]])

  str_squish_across(x, dplyr::all_of(squish_cols))
}

#' @name bind_address_col
#' @rdname format_address_data
#' @param street Street address column name, (e.g. 100 Holliday St) Default:
#'   'street_address'.
#' @param address Full address column name. If city and state or city and county
#'   are provided, a combined address column is added to the data.frame using
#'   the street address column. If both county and city are provided, county is
#'   ignored.
#' @param address_cols Named list specifying the additional column names to use
#'   for city, county, and state data.
#' @export
#' @importFrom dplyr mutate
bind_address_col <- function(x, ...,
                             case = NULL,
                             street = "street_address",
                             address = "address",
                             address_cols = list(
                               city = "city",
                               county = "county",
                               state = "state"
                             ),
                             .after = NULL) {
  x <- dplyr::mutate(x, ..., .after = .after %||% street)

  params <- rlang::list2(...)

  x <-
    str_to_case_across(
      x,
      dplyr::any_of(city, county, state),
      case
    )

  has_city <- rlang::has_name(params, address_cols$city)
  has_county <- rlang::has_name(params, address_cols$county)

  if (rlang::has_name(params, address_cols$state) && any(c(has_city, has_county))) {
    x <-
      dplyr::mutate(
        x,
        "{address}" := dplyr::case_when(
          has_city ~ glue("{.data[[street]]}, {.data[[address_cols$city]]} {.data[[address_cols$state]]}"),
          has_county ~ glue("{.data[[street]]}, {.data[[address_cols$county]]} {.data[[address_cols$state]]}")
        ),
        .after = dplyr::all_of(address_cols$state)
      )
  }
}

#' Replace values in a character vector or data frame with a crosswalk
#'
#' Use [stringr::str_replace_all()] to replace values in a character vector or
#' (with [dplyr::across()]) in select columns from a data.frame.
#' [replace_street_dir_prefixes()] and [replace_street_suffixes()] pass
#' reference data ([street_dir_prefixes] and [street_suffixes]) to the dict
#' parameter to support formatting addresses with [bind_block_cols()].
#'
#' @param x A data.frame or character vector. If x is a character vector, .cols
#'   is optional. If x is a data.frame, x is required.
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
#' @inheritParams format_address_data
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
      # Move the abbreviation column to the second column if converting
      # abbreviation to full value
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
#' @importFrom dplyr all_of
replace_street_suffixes <- function(x,
                                    street_suffix = "street_type",
                                    xwalk = NULL,
                                    abb = TRUE,
                                    case = NULL) {
  replace_with_xwalk(
    x,
    .cols = dplyr::all_of(street_suffix),
    xwalk = xwalk,
    dict = street_suffixes,
    abb = abb,
    case = case
  )
}

#' @name replace_street_dir_prefixes
#' @rdname replace_with_xwalk
#' @importFrom dplyr all_of
replace_street_dir_prefixes <- function(x,
                                        street_dir_prefix = "street_dir_prefix",
                                        xwalk = NULL,
                                        abb = TRUE,
                                        case = NULL) {
  replace_with_xwalk(
    x,
    .cols = dplyr::all_of(street_dir_prefix),
    xwalk = xwalk,
    dict = street_dir_prefixes,
    abb = abb,
    case = case
  )
}
