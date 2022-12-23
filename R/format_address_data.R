#' Format data frames and simple features with address data
#'
#' @description
#' getdata has two helpers for working with address data:
#'
#'  - [bind_address_col()] bind a provided value for city, county, and state to a
#'  data frame (to supplement address data with consistent values for these
#'  variables). This function is useful for converting partial street addresses
#'  with a consistent values for state, county, or city into full addresses
#' - [bind_block_col()] requires a data frame with columns named "bldg_num",
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
#' @param street_col String to use for street address column added based on
#'   component column values.
#' @param block_col String to use as prefix for block identifier columns and
#'   separator between block number and street. Set to "block" when `NULL`
#'   (default). If length 2 (e.g. c("blk", "block")), the second value is used
#'   as the block separator and the first as the column identifier prefix.
#' @param replace_suffix If `TRUE`, replace values in street_suffix column with
#'   abbreviations from [street_suffixes].
#' @param .after passed to [dplyr::mutate()] defaults to street_suffix for
#'   [bind_block_col()] and "address" for [bind_address_col()].
#' @export
#' @example examples/format_address_data.R
#' @importFrom rlang has_name
#' @importFrom dplyr all_of mutate if_else
bind_block_col <- function(x,
                           bldg_num = "bldg_num",
                           street_dir_prefix = "street_dir_prefix",
                           street_name = "street_name",
                           street_suffix = "street_type",
                           replace_suffix = FALSE,
                           street_col = NULL,
                           block_col = NULL,
                           .after = street_suffix,
                           case = NULL) {
  address_cols <- c(bldg_num, street_dir_prefix, street_name, street_suffix)
  x_missing_cols <- address_cols[!rlang::has_name(x, address_cols)]

  cliExtras::cli_abort_ifnot(
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

  if (!is.null(street_col)) {
    x <-
      dplyr::mutate(
        x,
        "{street_col}" := str_replace(
          glue::glue(
            "{as.character(.data[[address_cols[1]]])} {.data[[address_cols[2]]]} {.data[[address_cols[3]]]} {.data[[address_cols[4]]]}",
            .na = ""
          ),
          "  ", " "
        )
      )
  }

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
        .data[[street_dir_prefix]], .data[[street_name]],
        .data[[street_suffix]]
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
#' @param x A data frame to bind the address columns to.
#' @param .cols Named list specifying the additional column names to use for
#'   city, county, and state data. "street" is used for street address column
#'   name, (e.g. 100 Holliday St) Default: 'street_address'. "address" is used
#'   for full address column name. If "city" and "state" or "city" and "county"
#'   are provided, a combined address column is added to the data.frame using
#'   the "address" column name. If both county and city are provided, county is
#'   ignored.
#' @param ... Additional parameters passed to [dplyr::mutate()] intended for use
#'   in filling missing values, e.g. state = "MD" to add a missing state column.
#' @export
#' @importFrom dplyr mutate any_of case_when all_of
#' @importFrom rlang list2 has_name
bind_address_col <- function(x, ...,
                             case = NULL,
                             .cols = NULL,
                             .after = NULL) {
  .cols <-
    modifyList(
      .cols %||% list(),
      list(
        street = "street_address",
        address = "address",
        city = "city",
        county = "county",
        state = "state"
      )
    )

  x <- dplyr::mutate(x, ..., .after = .after %||% .cols$street)

  x <-
    str_to_case_across(
      x,
      dplyr::any_of(.cols$city, .cols$county, .cols$state),
      case
    )

  if (!all(rlang::has_name(x, c(.cols$street, .cols$state)))) {
    return(x)
  }

  if (rlang::has_name(x, .cols$city)) {
    return(
      dplyr::mutate(
        x,
        "{.cols$address}" := glue("{.data[[.cols$street]]}, {.data[[.cols$city]]} {.data[[.cols$state]]}"),
        .after = dplyr::all_of(.cols$state)
      )
    )
  }

  if (rlang::has_name(x, .cols$county)) {
    dplyr::mutate(
      x,
      "{.cols$address}" := glue("{.data[[.cols$street]]}, {.data[[.cols$county]]} {.data[[.cols$state]]}"),
      .after = dplyr::all_of(.cols$state)
    )
  }
}

#' Replace values in a character vector or data frame with a crosswalk
#'
#' Use [stringr::str_replace_all()] to replace values in a character vector or
#' (with [dplyr::across()]) in select columns from a data.frame.
#' [replace_street_dir_prefixes()] and [replace_street_suffixes()] pass
#' reference data ([street_dir_prefixes] and [street_suffixes]) to the dict
#' parameter to support formatting addresses with [bind_block_col()].
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
#' @param .strict If `TRUE` (default), match whole strings by appending "^" to
#'   the front and "$" to the end of each pattern in the xwalk.
#' @param ignore_case Passed to [stringr::regex()]
#' @inheritParams format_address_data
#' @example examples/replace_with_xwalk.R
#' @export
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom utils modifyList
#' @importFrom rlang set_names
#' @importFrom cli cli_warn
#' @importFrom dplyr mutate across all_of
replace_with_xwalk <- function(x,
                               .cols = NULL,
                               xwalk = NULL,
                               dict = NULL,
                               abb = TRUE,
                               case = NULL,
                               .strict = TRUE,
                               ignore_case = TRUE) {
  is_pkg_installed("stringr")

  dict <- dict %||% xwalk

  cliExtras::cli_abort_ifnot(
    "{.arg dict} or {.arg xwalk} must be provided.",
    condition = !is.null(dict)
  )

  xwalk_cols <- c(1:2)

  if (abb) {
    # Move the abbreviation column to the second column if converting
    # abbreviation to full value
    xwalk_cols <- rev(xwalk_cols)
  }

  dict <- make_xwalk_list(dict, xwalk_cols)

  if (!is.null(xwalk)) {
    xwalk <- utils::modifyList(dict, make_xwalk_list(xwalk))
  }

  xwalk <- xwalk %||% dict

  if (.strict) {
    xwalk <- rlang::set_names(xwalk, paste0("^", names(xwalk), "$"))
  }

  # stringr::str_replace_all requires a named vector (not a list)
  xwalk <- unlist(xwalk)

  if (is.data.frame(x)) {
    x <-
      dplyr::mutate(
        x,
        dplyr::across(
          dplyr::all_of(.cols),
          ~ stringr::str_replace_all(
            .x,
            stringr::regex(xwalk, ignore_case = ignore_case)
          )
        )
      )
  }

  if (is.character(x)) {
    x <- stringr::str_replace_all(
      x,
      stringr::regex(xwalk, ignore_case = ignore_case)
    )
  }

  if (is.null(case)) {
    return(x)
  }

  switch_case(x, case = case, .cols = .cols)
}

#' @name replace_street_suffixes
#' @rdname replace_with_xwalk
#' @param street_suffix Street suffix column to apply replacement function to.
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
#' @param street_dir_prefix Street direction prefix column to apply replacement
#'   function to.
#' @export
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
