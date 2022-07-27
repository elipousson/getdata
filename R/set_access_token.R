#' Set or get an access token or API key to/from environment variables.
#'
#' Based on the [mapboxapi::mb_access_token] function from the {mapboxapi}
#' package by Kyle Walker.
#'
#' @param token An access token or API key; required for [set_access_token()].
#'   If token is not provided; type is required for [get_access_token()].
#' @param overwrite If `TRUE`, overwrite existing token; Default: `FALSE`
#' @param install If `TRUE`, install token for use in future sessions; Default:
#'   `FALSE`
#' @param type Name of token; defaults to `NULL`.
#' @param call Passed as the call parameter for [cli::cli_abort] to improve
#'   error messages when function is used internally.
#' @rdname set_access_token
#' @aliases set_token_type
#' @export
#'
#' @importFrom utils read.table write.table
set_access_token <- function(token, overwrite = FALSE, install = FALSE, type = NULL, call = caller_env()) {
  check_required(token)
  check_character(token)
  check_character(type)

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (!file.exists(renv)) {
      file.create(renv)
    } else {

      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))

      if (overwrite) {
        oldenv <- read.table(renv, stringsAsFactors = FALSE)
        newenv <- oldenv[-grep(type, oldenv), ]

        write.table(
          newenv, renv,
          quote = FALSE, sep = "\n",
          col.names = FALSE, row.names = FALSE
        )

        cli_inform(
          c("v" = "Your original .Renviron is backed up to your R HOME directory if needed.")
        )
      } else {
        if (any(grepl(type, readLines(renv)))) {
          cli_abort(
            c("{.val {type}} already exists in your .Renviron.",
              "*" = "Set {.arg overwrite = TRUE} to replace this token."
            ),
            call = call
          )
        }
      }
    }

    # Append access token to .Renviron file
    write(glue("{type}=\'{token}\'"), renv, sep = "\n", append = TRUE)

    cli_inform(
      c(
        "i" = "The token {.val {token}} is now stored in your .Renviron.",
        "*" = 'To use it, restart R or run {.code readRenviron("~/.Renviron")}.
        You can then access the token using {.code Sys.getenv("{type}")}.'
      )
    )

    invisible(return(NULL))
  }

  cli_inform(
    c(
      "v" = "Token set with {.fun Sys.setenv}.",
      "*" = "To use the token {.val {token}} in future sessions, run this function with {.arg install = TRUE}."
    )
  )

  Sys.setenv(type = token)
}

#' @name get_access_token
#' @rdname set_access_token
#' @aliases get_token_type
#' @export
get_access_token <- function(token = NULL, type = NULL, call = caller_env()) {
  if (!is.null(token)) {
    return(token)
  }

  check_null(x = type)
  check_character(type, n = 1)

  token <- Sys.getenv(type)

  if (!is.null(token) && token != "") {
    return(token)
  }

  cli_abort("{.val {type}} can't be found in your .Renviron.", call = call)
}
