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
#' @rdname set_access_token
#' @export
#'
#' @importFrom utils read.table write.table
set_access_token <- function(token, overwrite = FALSE, install = FALSE, type = NULL) {
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
              "i" = "Set {.arg overwrite = TRUE} to replace this token."
            )
          )
        }
      }
    }

    # Append access token to .Renviron file
    write(glue("{type}='{token}'"), renv, sep = "\n", append = TRUE)

    cli_inform(
      c(
        "i" = "The token {.val {token}} is now stored in your .Renviron.",
        " " = 'To use it, restart R or run {.code readRenviron("~/.Renviron")}.
        You can then access the token using `Sys.getenv("{type}")`.'
      )
    )

    invisible(return(NULL))
  }

  cli_inform(
    c(
      "v" = "Token set with {.fun Sys.setenv}.",
      "i" = "To use the token {.val {token}} in future sessions, run this function with {.arg install = TRUE}."
    )
  )

  Sys.setenv(type = token)
}

#' @name get_access_token
#' @rdname set_access_token
#' @export
get_access_token <- function(token = NULL, type = NULL) {
  if (!is.null(token)) {
    return(token)
  }

  check_null(x = type)
  check_character(type)
  check_len(type, len = 1)

  token <- Sys.getenv(type)

  if (token != "") {
    return(token)
  }

  cli_abort("Can’t find token {.val {type}} in your .Renviron.")
}