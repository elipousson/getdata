# This file contains code from the usethis R package <https://github.com/r-lib/usethis>
# The license and copyright for this package follows:
#
# MIT License
#
# Copyright (c) 2020 usethis authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Modified version of [usethis::ui_yeah]
#'
#' @noRd
#' @importFrom glue glue_collapse glue
#' @importFrom rlang is_interactive
#' @importFrom cli cli_abort cli_alert
#' @importFrom utils menu
cli_yeah <- function(x,
                     yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                     no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                     n_yes = 1,
                     n_no = 2,
                     shuffle = TRUE,
                     .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)

  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "User input required, but session is not interactive.",
        "Query: {x}"
      )
    )
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))
  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  cli::cli_alert(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

#' @noRd
#' @importFrom cli cli_alert
cli_ask <- function(text, prompt = ">> ", ..., .envir = parent.frame()) {
  cli::cli_alert(text, ..., .envir = .envir)
  readline(prompt = prompt)
}



#' @noRd
cli_abort_ifnot <- function(..., condition = FALSE, .data = NULL, call = caller_env()) {
  if (!is_logical(condition)) {
    condition <- as_function(condition)
    condition <- condition(.data)
    check_logical(condition, call = call)
  }

  if (!condition) {
    cli_abort(...,
      call = call
    )
  }

  invisible(return(NULL))
}



#' @noRd
cli_warn_ifnot <- function(..., condition = FALSE, .data = NULL, call = caller_env()) {
  if (!is_logical(condition)) {
    condition <- as_function(condition)
    condition <- condition(.data)
    check_logical(condition, call = call)
  }


  if (!condition) {
    cli_warn(
      ...,
      call = call
    )
  }

  invisible(return(NULL))
}
