check_null <- function(x = NULL, arg = caller_arg(x), null.ok = FALSE, null.req = FALSE, call = caller_env(), ...) {
  if (null.req) {
    null.ok <- null.req
  }

  if (is_null(x) && !null.ok) {
    cli_abort("{.arg {arg}} must not be NULL.",
      call = call, ...
    )
  }

  if (!is_null(x) && null.req) {
    cli_abort("{.arg {arg}} must be NULL.",
      call = call, ...
    )
  }

  invisible(return(TRUE))
}

check_character <- function(x = NULL, arg = caller_arg(x), null.ok = FALSE, n = NULL, call = caller_env(), ...) {
  check_null(x, arg, null.ok, FALSE, call)

  if (is_character(x, n = n)) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be a character vector.",
      "i" = "You've supplied a {class(x)} object."
    ),
    call = call,
    ...
  )
}

check_logical <- function(x = NULL, arg = caller_arg(x), null.ok = FALSE, n = NULL, call = caller_env(), ...) {
  check_null(x, arg, null.ok, FALSE, call)

  if (is_logical(x, n = n)) {
    invisible(return(TRUE))
  }

  cli_abort(
    c("{.arg {arg}} must be a character vector.",
      "i" = "You've supplied a {class(x)} object."
    ),
    call = call,
    ...
  )
}

check_len <- function(x = NULL, len = 1, arg = caller_arg(x), null.ok = FALSE, call = caller_env(), ...) {
  check_null(x, arg, null.ok, FALSE, call)

  if ((length(x) >= min(len)) && (length(x) <= max(len))) {
    invisible(return(TRUE))
  }

  if (length(len) > 1) {
    len <- glue("have a length between {min(len)} and {max(len)}")
  } else {
    len <- glue("be length {len}")
  }

  cli_abort(
    c("{.arg {arg}} must {len}.",
      "i" = "You've supplied a length {length(x)} vector."
    ),
    call = call,
    ...
  )
}

check_grepl <- function(x = NULL, pattern = NULL, arg = caller_arg(x), null.ok = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_null(x, arg, null.ok, FALSE, call)

  if (grepl(pattern, x, ignore.case = ignore.case, perl = perl)) {
    invisible(return(TRUE))
  }

  if (is.null(message)) {
    cli_abort(
      "Can't detect pattern {.val {pattern}} in {.arg {arg}}."
    )
  }

  cli_abort(message = message)
}

check_starts_with <- function(x = NULL, string = NULL, arg = caller_arg(x), null.ok = FALSE, ignore.case = FALSE, perl = FALSE, message = NULL, ...) {
  check_character(x, arg, null.ok)

  if (all(grepl(paste0("^", string), x, ignore.case = ignore.case, perl = perl))) {
    invisible(return(TRUE))
  }

  if (is.null(message)) {
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
check_df <- function(x, arg = caller_arg(x), null.ok = FALSE, ...) {
  check_null(x, arg, null.ok)

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
check_df_rows <- function(x, rows = 1, arg = caller_arg(x), null.ok = FALSE, ...) {
  check_null(x, arg, null.ok)
  check_df(x, arg, null.ok)

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
check_df_paper <- function(x, cols = c("width", "height", "orientation", "units"), arg = caller_arg(x), null.ok = FALSE, ...) {
  check_null(x, arg, null.ok)
  check_df(x, arg, null.ok)

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

#' @noRd
check_sf <- function(x, arg = caller_arg(x), null.ok = FALSE, ext = FALSE, ...) {
  check_null(x, arg, null.ok)

  if (overedge::is_sf(x, ext = ext)) {
    invisible(return(TRUE))
  }

  if (ext) {
    sf <- c("sf", "sfc", "bbox")
  } else {
    sf <- "sf"
  }

  cli_abort(
    c("{.arg {arg}} must be a {.code sf} object.",
      "i" = "You've supplied a {class(x)} object."
    ),
    ...
  )
}
