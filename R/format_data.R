#' Format data frames and simple features using common approaches
#'
#' This function can apply the following common data cleaning tasks:
#'
#' - Applies [stringr::str_squish] and [stringr::str_trim] to all character
#' columns ([str_trim_squish])
#' - Optionally replaces all character values of "" with `NA` values
#' - Optionally corrects UNIX formatted dates with 1970-01-01 origins
#' - Optionally renames variables by passing a named list of variables
#'
#' @param x A tibble or data frame object
#' @param var_names A named list following the format, `list("New var name" = old_var_name)`, or a two column data frame with the first column being the
#'   new variable names and the second column being the old variable names;
#'   defaults to `NULL`.
#' @param clean_names If `TRUE`, pass data frame to [janitor::clean_names]; defaults to `TRUE`.
#' @param replace_na_with A named list to pass to [tidyr::replace_na]; defaults to
#'   `NULL`.
#' @param replace_with_na A named list to pass to [naniar::replace_with_na];
#'   defaults to `NULL`.
#' @param replace_empty_char_with_na If `TRUE`, replace "" with `NA` using
#'   [naniar::replace_with_na_if], Default: `TRUE`
#' @param fix_date If `TRUE`, fix UNIX dates (common issue with dates from
#'   FeatureServer and MapServer sources) , Default: `TRUE`
#' @param ... Additional parameters passed to [format_sf_data]
#' @return The input data frame or simple feature object with formatting functions applied.
#'
#' @details Bind columns to address data:
#'
#'  - [bind_address_col] bind a provided value for city, county, and state to a
#'  data frame (to supplement address data with consistent values for these
#'  variables)
#' - [bind_block_col] requires a data frame with columns named "bldg_num",
#' "street_dir_prefix", "street_name", and "street_type" and binds derived
#' values for whether a building is on the even or odd side of a block and
#' create a block number (street segment), and block face (street segment side)
#' identifier.
#'
#' @rdname format_data
#' @export
#' @importFrom tibble deframe
format_data <- function(x,
                        var_names = NULL,
                        clean_names = TRUE,
                        replace_na_with = NULL,
                        replace_with_na = NULL,
                        replace_empty_char_with_na = TRUE,
                        fix_date = FALSE,
                        label = FALSE,
                        format_sf = FALSE,
                        ...) {
  x <- sfext::str_trim_squish(x)

  if (!is.null(var_names)) {
    x <- rename_with_xwalk(x, xwalk = var_names)
  }

  if (clean_names) {
    x <- janitor::clean_names(x, "snake")
  }

  if (!is.null(replace_na_with)) {
    is_pkg_installed("tidyr")

    x <-
      tidyr::replace_na(x, replace = replace_na_with)
  }


  if (!is.null(replace_with_na) || replace_empty_char_with_na) {
    is_pkg_installed("naniar")

    if (!is.null(replace_with_na)) {
      x <-
        naniar::replace_with_na(
          x,
          replace = replace_with_na
        )
    }

    if (replace_empty_char_with_na) {
      x <-
        naniar::replace_with_na_if(
          x,
          is.character, ~ .x == ""
        )
    }
  }

  if (fix_date) {
    x <- fix_date(x)
  }

  if (sfext::is_sf(x) && format_sf) {
    x <- format_sf_data(x, ...)
  }

  x
}

#' @noRd
make_xwalk_list <- function(xwalk) {
  if (is.data.frame(xwalk) && (ncol(xwalk) == 2)) {
    return(as.list(tibble::deframe(xwalk)))
  }

  cli_abort_ifnot(
    condition = is_named(xwalk),
    message = "{.arg xwalk} must be a named list or two column data frame."
  )

  x
}

#' @name rename_with_xwalk
#' @rdname format_data
label_with_xwalk <- function(x, xwalk) {
  is_pkg_installed("labelled")

  labelled::set_variable_labels(x, .labels = make_xwalk_list(xwalk))
}

#' @name rename_with_xwalk
#' @rdname format_data
#' @param xwalk a data frame with two columns using the first column as name and
#'   the second column as value; or a named list. The existing names of x must
#'   be the values and the new names must be the names.
#' @export
#' @importFrom tibble deframe
#' @importFrom dplyr rename
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

    x <-
      rename_sf_col(
        x,
        sf_col = sf_col
      )

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

#' @name format_data
#' @rdname format_data
#' @export
#' @importFrom dplyr mutate across contains
fix_date <- function(x) {
  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::contains("date"),
      ~ as.POSIXct(.x / 1000, origin = "1970-01-01")
    )
  )
}


#' @name bind_address_col
#' @rdname format_data
#' @param city,county,state City, county, and state to bind to data frame or
#'   `sf` object.
#' @export
#' @importFrom dplyr mutate
bind_address_col <- function(x, city = NULL, county = NULL, state = NULL) {
  if (!is.null(city)) {
    x <- has_same_name_col(x, col = "city")

    x <-
      dplyr::mutate(
        x,
        city = city
      )
  }

  if (!is.null(county)) {
    x <- has_same_name_col(x, col = "county")

    x <-
      dplyr::mutate(
        x,
        county = county
      )
  }

  if (!is.null(state)) {
    x <- has_same_name_col(x, col = "state")

    x <-
      dplyr::mutate(
        x,
        state = state
      )
  }

  x
}

#' @name bind_block_col
#' @rdname format_data
#' @export
#' @importFrom rlang has_name
#' @importFrom dplyr mutate if_else
bind_block_col <- function(x,
                           bldg_num = "bldg_num",
                           street_dir_prefix = "street_dir_prefix",
                           street_name = "street_name",
                           street_suffix = "street_type") {
  stopifnot(
    has_name(x, c(bldg_num, street_dir_prefix, street, street_suffix))
  )

  dplyr::mutate(
    x,
    block_num = floor({{ bldg_num }} / 100) * 100,
    bldg_num_even_odd = dplyr::if_else(({{ bldg_num }} %% 2) == 0, "Even", "Odd"),
    block_num_st = paste(block_num, {{ street_dir_prefix }}, {{ street }}, {{ street_suffix }}),
    block_face_st = paste(bldg_num_even_odd, {{ street_dir_prefix }}, {{ street }}, {{ street_suffix }})
  )
}
