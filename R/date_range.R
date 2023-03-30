#' Use lubridate to convert an object to a date range
#'
#' Use [lubridate::as_date()] to convert an object to a length 2 list with a
#' minimum and maximum date. By default the dates in the list will be named
#' named "start" and "end". [date_range_query()] is a variation that returns a
#' query string that can be passed to the "where" parameter of
#' [get_esri_data()]. [between_date_range()] works the same way identical but
#' uses the BETWEEN DATE syntax. [check_date_range()] validates whether a date
#' or date range falls within supplied limits.
#'
#' @param x Date range as character vector in format of `c("<start date>", "<end
#'   date>")`. If length 1 and days is not `NULL`, return a range based on
#'   `c(date_range, date_range + lubridate::days(days))`
#' @param year If date_range is NULL and year is provided, date range is set to
#'   `c("<year>-01-01", "<year>-12-31")`. year is ignored if date_range is
#'   provided.
#' @param days Default range duration in days to use if date_range is length 1.
#' @param start_date,end_date Start and end date used if year and date_range are
#'   both `NULL`.
#' @param limits Optional range of allowed dates. If dates supplied to
#'   [as_date_range()] falls outside limits range, abort function.
#' @param nm Names to use for returned date range list. Defaults to `c("start",
#'   "end")`.
#' @inheritDotParams lubridate::as_date
#' @examples
#' as_date_range("2022-01-01", days = 10)
#'
#' as_date_range(c("2022-01-01", "2022-01-31"))
#'
#' as_date_range(year = 2022)
#'
#' date_range_query(c("2022-01-01", "2022-01-31"))
#'
#' check_date_range("2022-09-01", limits = c("2022-07-01", "2022-09-30"))
#'
#' @return A length 2 list with min and max Date values.
#' @export
#' @importFrom glue glue
#' @importFrom rlang current_env set_names %||%
as_date_range <- function(x = NULL,
                          year = NULL,
                          days = 90,
                          ...,
                          start_date = NULL,
                          end_date = NULL,
                          limits = NULL,
                          nm = c("start", "end")) {
  rlang::check_installed("lubridate")

  if (!is.null(year)) {
    x <-
      x %||% vapply(
        c("{year}-01-01", "{year}-12-31"),
        glue, NA_character_,
        .envir = rlang::current_env()
      )
  }

  x <- x %||% c(start_date, end_date)

  x <- lubridate::as_date(x, ...)

  if (length(x) == 1 && !is.null(days)) {
    x <- c(x, x + lubridate::days(days))
  }

  if (!is.null(limits)) {
    check_date_range(x = x, ..., limits = limits)
  }

  rlang::set_names(list(min(x), max(x)), nm)
}

#' @name date_range_query
#' @rdname as_date_range
#' @param .col Name of date column to use for query. Defaults to "date".
#' @export
#' @importFrom glue glue
date_range_query <- function(x = NULL,
                             .col = "date",
                             ...,
                             nm = c("start", "end")) {
  x <- as_date_range(x, ..., nm = nm)
  glue("({.col} >= '{x[[nm[1]]]}') AND ({.col} <= '{x[[nm[2]]]}')")
}

#' @name between_date_range
#' @rdname as_date_range
#' @export
#' @importFrom glue glue
between_date_range <- function(x = NULL,
                               .col = "date",
                               ...,
                               nm = c("start", "end")) {
  x <- as_date_range(x, ..., nm = nm)
  glue("({.col} BETWEEN DATE '{x[[nm[1]]]}' AND DATE '{x[[nm[2]]]}')")
}

#' @name check_date_range
#' @rdname as_date_range
#' @export
#' @importFrom cli cli_vec
check_date_range <- function(x = NULL, ..., limits = NULL) {
  if (is.null(limits)) {
    return(invisible(NULL))
  }

  rlang::check_installed("lubridate")

  x <- lubridate::as_date(x, ...)
  limits <- lubridate::as_date(limits, ...)

  below_limit <- min(x) < min(limits)
  above_limit <- max(x) > max(limits)

  if (any(c(below_limit, above_limit))) {
    aboveinfo <- NULL

    if (below_limit) {
      below_limit <- "Min range value ({.val {min(x)}}) falls below the limit."
    } else {
      below_limit <- NULL
    }

    if (above_limit) {
      above_limit <- "Max range value ({.val {max(x)}}) falls above the limit."
    } else {
      above_limit <- NULL
    }

    limits <- cli::cli_vec(
      c(min(limits), max(limits)),
      list("vec-last" = " to ")
    )

    cli_abort(
      c("Supplied date range must fall within {.arg limits}: {.val {limits}}.",
        "*" = below_limit,
        "*" = above_limit
      )
    )
  }
}
