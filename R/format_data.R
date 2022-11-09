#' Format data frames and simple features using common approaches
#'
#' @description
#' This function can apply the following common data cleaning tasks:
#'
#' - Applies [stringr::str_squish] and [stringr::str_trim] to all character
#' columns
#' - Optionally replaces all character values of "" with `NA` values
#' - Optionally corrects UNIX formatted dates with 1970-01-01 origins
#' - Optionally renames variables by passing a named list of variables
#'
#' The address functions previously included with [format_data()] are now
#' documented at [format_address_data()].
#'
#' @param x A tibble or data frame object
#' @param var_names A named list following the format, `list("New var name" =
#'   old_var_name)`, or a two column data frame with the first column being the
#'   new variable names and the second column being the old variable names;
#'   defaults to `NULL`.
#' @param clean_names If `TRUE`, pass data frame to [janitor::clean_names];
#'   defaults to `TRUE`.
#' @param replace_na_with A named list to pass to [tidyr::replace_na]; defaults
#'   to `NULL`.
#' @param replace_with_na A named list to pass to [naniar::replace_with_na];
#'   defaults to `NULL`.
#' @param replace_empty_char_with_na If `TRUE`, replace "" with `NA` using
#'   [naniar::replace_with_na_if], Default: `TRUE`
#' @param format_sf If `TRUE`, pass x and additional parameters to
#'   [format_sf_data].
#' @param fix_date If `FALSE`, fix UNIX epoch dates (common issue with dates from
#'   FeatureServer and MapServer sources) using the [fix_epoch_date] function,
#'   Default: `TRUE`
#' @param ... Additional parameters passed to [format_sf_data]
#' @return The input data frame or simple feature object with formatting
#'   functions applied.
#'
#' @rdname format_data
#' @export
#' @importFrom tibble deframe
format_data <- function(x,
                        var_names = NULL,
                        xwalk = NULL,
                        clean_names = TRUE,
                        replace_na_with = NULL,
                        replace_with_na = NULL,
                        replace_empty_char_with_na = FALSE,
                        fix_date = FALSE,
                        label = FALSE,
                        format_sf = FALSE,
                        ...) {
  x <- str_trim_squish_across(x)

  if (!is.null(var_names) | !is.null(xwalk)) {
    xwalk <- xwalk %|% var_names
    x <- rename_with_xwalk(x, xwalk = xwalk, label = label)
  }

  if (clean_names) {
    x <- janitor::clean_names(x, "snake")
  }

  if (!is.null(replace_na_with)) {
    is_pkg_installed("tidyr")
    x <- tidyr::replace_na(x, replace = replace_na_with)
  }

  if (!is.null(replace_with_na) || replace_empty_char_with_na) {
    is_pkg_installed("naniar")

    if (!is.null(replace_with_na)) {
      x <- naniar::replace_with_na(x, replace = replace_with_na)
    }

    if (replace_empty_char_with_na) {
      x <- naniar::replace_with_na_if(x, is.character, ~ .x == "")
    }
  }

  if (fix_date) {
    x <- fix_epoch_date(x)
  }

  if (sfext::is_sf(x) && format_sf) {
    x <- format_sf_data(x, ...)
  }

  x
}

#' @noRd
#' @importFrom tibble deframe
make_xwalk_list <- function(xwalk, nm = c("label", "name")) {
  if (is.data.frame(xwalk)) {
    if (sfext::is_sf(xwalk)) {
      xwalk <- sf::st_drop_geometry(xwalk)
    }

    if (ncol(xwalk) == 2) {
      return(as.list(tibble::deframe(xwalk)))
    } else if (all(nm %in% colnames(xwalk))) {
      return(rlang::set_names(as.list(xwalk[[nm[1]]]), xwalk[[nm[2]]]))
    }
  }

  cli_abort_ifnot(
    message = c("{.arg xwalk} must be a {.cls list}, a two column {.cls data.frame}, or a {.cls data.frame} with column names {.val {nm}}.",
      "i" = "The provided {.arg xwalk} has class {.cls {class(xwalk)}}."
    ),
    condition = is_named(xwalk),
  )

  xwalk
}

#' @name rename_with_xwalk
#' @rdname format_data
#' @param xwalk a data frame with two columns using the first column as name and
#'   the second column as value; or a named list. The existing names of x must
#'   be the values and the new names must be the names.
#' @param .strict If `TRUE` (default), require that all values from the xwalk
#'   are found in the column names of the x data.frame. If `FALSE`, unmatched
#'   values from the xwalk are ignored.
#' @param keep_all If `FALSE`, columns that are not named in the xwalk are
#'   dropped. If `TRUE` (default), all columns are retained. If x is an `sf`
#'   object, the geometry column will not be dropped even it is not renamed.
#' @export
#' @importFrom tibble deframe
#' @importFrom dplyr rename_with
#' @importFrom sfext is_sf
rename_with_xwalk <- function(x,
                              xwalk = NULL,
                              label = FALSE,
                              .strict = TRUE,
                              keep_all = TRUE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  # From https://twitter.com/PipingHotData/status/1497014703473704965
  # https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names/41343022#41343022
  xwalk <- make_xwalk_list(xwalk)
  xwalk_in_x <- rlang::has_name(x, xwalk)

  cli_abort_ifnot(
    c("{.arg xwalk} values must all be column names in {.arg x}.",
      "i" = "{.val {xwalk[!xwalk_in_x]}} can't be found in {.arg x} column names.",
      "*" = "Set {.arg .strict} to {.code FALSE} to ignore missing values."
    ),
    condition = (all(xwalk_in_x) && .strict) | !.strict,
    arg = arg,
    call = call
  )

  if (!keep_all) {
    x <- x[, colnames(x) %in% xwalk]
    xwalk <- xwalk[xwalk %in% names(x)]
  }

  if (sfext::is_sf(x) && (attributes(x)$sf_column %in% xwalk)) {
    sf_col <- as.character(names(xwalk[xwalk == attributes(x)$sf_column]))
    x <- sfext::rename_sf_col(x, sf_col = sf_col)
    xwalk[[sf_col]] <- NULL
  }

  x <-
    dplyr::rename_with(
      x,
      ~ names(xwalk)[which(xwalk == .x)],
      .cols = dplyr::any_of(as.character(xwalk))
    )

  if (!label) {
    return(x)
  }

  label_with_xwalk(x, xwalk = xwalk, label = "var")
}

#' @name label_with_xwalk
#' @param label For [label_with_xwalk()] use `label = "val"` to use
#'   [labelled::set_value_labels()] or "var" (default) to use
#'   [labelled::set_variable_labels()]. For [rename_with_xwalk()], if label is
#'   `TRUE`, xwalk is passed to [label_with_xwalk()] with label = "var" to label
#'   columns using the original names. Defaults to `FALSE`.
#' @rdname format_data
#' @export
label_with_xwalk <- function(x, xwalk = NULL, label = "var", ...) {
  is_pkg_installed("labelled")

  label <- rlang::arg_match(label, c("val", "var"))
  if (label == "var") {
    labelled::set_variable_labels(x, .labels = make_xwalk_list(xwalk), ...)
  } else {
    labelled::set_value_labels(x, .labels = make_xwalk_list(xwalk), ...)
  }
}

#' @name make_variable_dictionary
#' @param .labels Replaces labels column created by
#'   [labelled::generate_dictionary()] if column is all `NA` (no existing labels
#'   assigned); defaults to `NULL`.
#' @param .definitions Character vector of definitions appended to dictionary
#'   data frame. Must be in the same order as the variables in the provided data
#'   frame x.
#' @rdname format_data
#' @export
make_variable_dictionary <- function(x, .labels = NULL, .definitions = NULL) {
  is_pkg_installed("labelled")

  dict <- labelled::generate_dictionary(x)

  if (all(is.na(dict$label)) && (length(.labels) == ncol(x))) {
    dict$label <- .labels
  }

  if (!is.null(.definitions)) {
    dict$definitions <- .definitions
  }

  dict
}

#' @name fix_epoch_date
#' @rdname format_data
#' @param .cols tidyselect for columns to apply epoch date fixing function to.
#' @export
#' @importFrom dplyr mutate across contains
fix_epoch_date <- function(x, .cols = dplyr::contains("date")) {
  dplyr::mutate(
    x,
    dplyr::across(
      .cols,
      ~ as.POSIXct(.x / 1000, origin = "1970-01-01")
    )
  )
}

#' Helper function for getting names of character columns
#'
#' @noRd
chr_colnames <- function(x) {
  names(x)[vapply(x, is.character, TRUE)]
}

#' @name str_trim_squish_across
#' @noRd
#' @importFrom dplyr mutate across if_else
str_trim_squish_across <- function(x) {
  is_pkg_installed("stringr")

  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::any_of(chr_colnames(x)),
      ~ dplyr::if_else(
        is.na(.x) | is.null(.x),
        .x,
        stringr::str_trim(stringr::str_squish(.x))
      )
    )
  )
}

#' Helper function to squish white space across data.frame columns
#'
#' @noRd
#' @importFrom dplyr everything mutate across if_else
str_empty_to_blank_across <- function(x, .cols = dplyr::everything(), blank = "") {
  dplyr::mutate(
    x,
    dplyr::across(
      .cols,
      ~ dplyr::if_else(
        is.na(.x) | is.null(.x),
        blank,
        as.character(.x)
      )
    )
  )
}

#' Helper function to squish white space across dataframe columns
#'
#' @noRd
#' @importFrom dplyr everything mutate across
str_squish_across <- function(x, .cols = dplyr::everything()) {
  dplyr::mutate(
    x,
    dplyr::across(
      .cols,
      ~ gsub("\\s\\s+", " ", .x, perl = TRUE)
    )
  )
}

#' Helper function to change case across dataframe columns
#'
#' @noRd
#' @importFrom dplyr everything mutate across
str_to_case_across <- function(x, .cols = dplyr::everything(), case = NULL) {
  if (is.null(case)) {
    return(x)
  }

  x <-
    dplyr::mutate(
      x,
      dplyr::across(
        .cols,
        ~ switch_case(.x, case)
      )
    )
}

#' @noRd
switch_case <- function(x, case = NULL, ...) {
  if (is.data.frame(x)) {
    return(str_to_case_across(x, case = case, ...))
  }

  case <- tolower(case)
  case <- rlang::arg_match(case, c("lower", "upper", "title", "sentence"))

  if (case == "sentence") {
    is_pkg_installed("stringr")
  }

  switch(case,
    "lower" = tolower(x),
    "upper" = toupper(x),
    "title" = str_capitalize(x),
    "sentence" = stringr::str_to_sentence(x)
  )
}

#' Helper function from examples for toupper and tolower
#'
#' @noRd
str_capitalize <- function(string, strict = FALSE) {
  cap <- function(string) {
    paste(toupper(substring(string, 1, 1)),
      {
        string <- substring(string, 2)
        if (strict) tolower(string) else string
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(string, split = " "), cap, USE.NAMES = !is.null(names(string)))
}
