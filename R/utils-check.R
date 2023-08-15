#' Check if object is a list
#'
#' @noRd
#' @importFrom rlang is_list
check_list <- function(x,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (allow_na && is.na(x)) {
    return(invisible(NULL))
  }

  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  if (!is_list(x)) {
    stop_input_type(
      x,
      what = "a list",
      allow_na = allow_na, allow_null = allow_null,
      arg = arg, call = call
    )
  }
}

#' @noRd
#' @importFrom rlang is_null
check_null <- function(x = NULL, arg = caller_arg(x), allow_null = FALSE, req_null = FALSE, call = caller_env(), ...) {
  if (req_null) {
    allow_null <- req_null
  }

  if (rlang::is_null(x) && !allow_null) {
    cli_abort("{.arg {arg}} must not be NULL.",
      call = call, ...
    )
  }

  if (!rlang::is_null(x) && req_null) {
    cli_abort("{.arg {arg}} must be NULL.",
      call = call, ...
    )
  }

  invisible(return(TRUE))
}

#' @noRd
check_length <- function(x = NULL,
                         n = 1,
                         arg = caller_arg(x),
                         allow_na = FALSE,
                         allow_null = TRUE,
                         call = caller_env(),
                         ...) {
  if (allow_na && is.na(x)) {
    return(invisible(NULL))
  }

  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  if (has_length(n, 1)) {
    if (has_length(x, n)) {
      return(invisible(NULL))
    }

    cli_abort(
      "{.arg {arg}} must be length {n}, not {length(x)}.",
      call = call,
      ...
    )
  }

  if (has_length(n, 2)) {
    if ((length(x) >= min(n)) && (length(x) <= max(n))) {
      return(invisible(NULL))
    }

    cli_abort(
      "{.arg {arg}} must be between length {min(n)} and {max(n)},
      not {length(x)}.",
      call = call,
      ...
    )
  }
}

#' @noRd
#' @importFrom rlang is_null
check_grepl <- function(x = NULL, pattern = NULL, arg = caller_arg(x), allow_null = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_null(x, arg, allow_null, call = call)

  if (grepl(pattern, x, ignore.case = ignore.case, perl = perl)) {
    invisible(return(TRUE))
  }

  if (rlang::is_null(message)) {
    cli_abort(
      "Can't detect pattern {.val {pattern}} in {.arg {arg}}."
    )
  }

  cli_abort(message = message)
}

#' @noRd
#' @importFrom rlang is_null
check_starts_with <- function(x = NULL, string = NULL, arg = caller_arg(x), allow_null = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_character(x, arg, allow_null)

  if (all(grepl(paste0("^", string), x, ignore.case = ignore.case, perl = perl))) {
    invisible(return(TRUE))
  }

  if (rlang::is_null(message)) {
    cli_abort(
      c("{.arg {arg}} must start with {.val {string}}.",
        "i" = "The provided string is {.val {x}}."
      ),
      ...
    )
  }

  cli_abort(message = message, ...)
}

#' @noRd
check_df <- function(x, arg = caller_arg(x), allow_null = FALSE, ...) {
  check_null(x, arg, allow_null)

  if (is.data.frame(x)) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be a data frame.",
      "i" = "You've supplied a {class(x)} object."
    ),
    ...
  )
}

#' @noRd
check_df_rows <- function(x, rows = 1, arg = caller_arg(x), allow_null = FALSE, ...) {
  check_data_frame(x, arg = arg, allow_null = allow_null)

  if (nrow(x) >= rows) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must have {rows} row(s) or more.",
      "i" = "You've supplied a data frame with {nrow(x)} rows."
    ),
    ...
  )
}

#' @noRd
#' @importFrom rlang has_name
check_df_paper <- function(x, cols = c("width", "height", "orientation", "units"), arg = caller_arg(x), allow_null = FALSE, ...) {
  check_null(x, arg, allow_null)
  check_df(x, arg, allow_null)

  if (all(has_name(x, names))) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be a data frame with columns named {cols}.",
      "i" = "You've supplied a data frame that is missing {cols[cols %in% names(x)]}."
    ),
    ...
  )
}
