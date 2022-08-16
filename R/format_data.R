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
                        clean_names = TRUE,
                        replace_na_with = NULL,
                        replace_with_na = NULL,
                        replace_empty_char_with_na = FALSE,
                        fix_date = FALSE,
                        label = FALSE,
                        format_sf = FALSE,
                        xwalk = NULL,
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
make_xwalk_list <- function(xwalk) {
  if (is.data.frame(xwalk) && (ncol(xwalk) == 2)) {
    return(as.list(tibble::deframe(xwalk)))
  }

  cli_abort_ifnot(
    condition = is_named(xwalk),
    message = "{.arg xwalk} must be a named list or two column data frame."
  )

  xwalk
}

#' @name rename_with_xwalk
#' @rdname format_data
#' @param xwalk a data frame with two columns using the first column as name and
#'   the second column as value; or a named list. The existing names of x must
#'   be the values and the new names must be the names.
#' @param label If `TRUE`, pass xwalk to [label_with_xwalk] to label columns
#'   using the original names. Defaults to `FALSE`.
#' @export
#' @importFrom tibble deframe
#' @importFrom dplyr rename_with
#' @importFrom sfext is_sf
rename_with_xwalk <- function(x, xwalk = NULL, label = FALSE) {
  # From https://twitter.com/PipingHotData/status/1497014703473704965
  # https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names/41343022#41343022
  xwalk <- make_xwalk_list(xwalk)

  cli_abort_ifnot(
    condition = all(xwalk %in% colnames(x)),
    message = "{.arg xwalk} must include all column names for the data frame to be renamed."
  )

  if (sfext::is_sf(x) && (attributes(x)$sf_column %in% xwalk)) {
    sf_col <- as.character(names(xwalk[xwalk == attributes(x)$sf_column]))

    x <- rename_sf_col(x, sf_col = sf_col)

    xwalk[[sf_col]] <- NULL
  }

  x <-
    dplyr::rename_with(
      x,
      ~ names(xwalk)[which(xwalk == .x)],
      .cols = as.character(xwalk)
    )

  if (!label) {
    return(x)
  }

  label_with_xwalk(x, xwalk = xwalk)
}

#' @name label_with_xwalk
#' @rdname format_data
label_with_xwalk <- function(x, xwalk = NULL) {
  is_pkg_installed("labelled")

  # TODO: Consider adding an optional for value labelling as well as variable
  # labelling
  labelled::set_variable_labels(x, .labels = make_xwalk_list(xwalk))
}

#' @name fix_epoch_date
#' @rdname format_data
#' @export
#' @importFrom dplyr mutate across contains
fix_epoch_date <- function(x) {
  # FIXME: This needs better documentation and some kind of check to make sure
  # the date is in the correct format Alternatively, considering renaming so the
  # focused scope for this function is clear
  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::contains("date"),
      ~ as.POSIXct(.x / 1000, origin = "1970-01-01")
    )
  )
}

#' @name str_trim_squish_across
#' @noRd
#' @importFrom dplyr mutate across if_else
str_trim_squish_across <- function(x, .cols = where(is.character)) {
  is_pkg_installed("stringr")

  dplyr::mutate(
    x,
    dplyr::across(
      .cols,
      ~ dplyr::if_else(
        !rlang::is_empty(.x),
        stringr::str_trim(stringr::str_squish(.x)),
        .x
      )
    )
  )
}

#' Helper function to squish white space across dataframe columns
#'
#' @noRd
#' @importFrom dplyr everything mutate across if_else
str_empty_to_blank_across <- function(x, .cols = everything(), blank = "") {
  dplyr::mutate(
    x,
    dplyr::across(
      .cols,
      ~ dplyr::if_else(is.na(.x) | is.null(.x), blank, .x)
    )
  )
}

#' Helper function to squish white space across dataframe columns
#'
#' @noRd
#' @importFrom dplyr everything mutate across
str_squish_across <- function(x, .cols = everything()) {
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
str_to_case_across <- function(x, .cols = everything(), case = NULL) {
  if (is.null(case)) {
    return(x)
  }

  case <- match.arg(tolower(case), c("lower", "upper", "title"))

  x <-
    dplyr::mutate(
      x,
      dplyr::across(
        .cols,
        ~ switch(case,
          "lower" = tolower(.x),
          "upper" = toupper(.x),
          "title" = str_capitalize(.x)
        )
      )
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
