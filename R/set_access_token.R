#' Set or get an access token or API key to/from environment variables.
#'
#' Based on the [mapboxapi::mb_access_token()] function from the {mapboxapi}
#' package by Kyle Walker.
#'
#' @param token An access token or API key; required for [set_access_token()].
#'   If token is not provided; type is required for [get_access_token()]. Length
#'   1 character vector or list. If named, the name of the token is used in
#'   place of type.
#' @param overwrite If `TRUE`, overwrite existing token; Default: `FALSE`
#' @param install If `TRUE`, install token for use in future sessions; Default:
#'   `FALSE`
#' @param type Name of token; defaults to `NULL`. Optional if token is named.
#' @param call Passed as the call parameter for [cli::cli_abort()] to improve
#'   error messages when function is used internally.
#' @rdname set_access_token
#' @aliases set_token_type
#' @export
#' @importFrom rlang is_named
#' @importFrom utils read.table write.table
set_access_token <- function(token,
                             overwrite = FALSE,
                             install = FALSE,
                             type = NULL,
                             call = caller_env(),
                             .frame = parent.frame()) {
  check_required(token)

  if (rlang::is_named(token)) {
    type <- type %||% names(token)
    token <- as.character(token)
  }

  check_character(token, n = 1)
  check_character(type, n = 1)

  if (!install) {
    cli_inform(
      c(
        "v" = "Token {.envvar {type}} set to {.val {token}} with {.fn Sys.setenv}.",
        "*" = "To use the token in future sessions,
        run {.fn set_access_token} using {.arg install = TRUE}."
      )
    )

    Sys.setenv(type = token)
    return(invisible(NULL))
  }

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (file.exists(renv)) {
    has_type <- any(grepl(type, readLines(renv)))

    if (has_type && !overwrite) {
      cli_abort(
        c("{.envvar {type}} already exists in your {.file .Renviron}.",
          "*" = "Set {.arg overwrite = TRUE} to replace this token."
        )
      )
    }

    # Backup original .Renviron before doing anything else here.
    backup <- file.path(home, ".Renviron_backup")
    file.copy(renv, backup)

    cli_inform(
      c("v" = "{.file .Renviron} backed up to {.path {backup}}.")
    )

    if (has_type) {
      oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[!grepl(type, readLines(renv)), ]

      utils::write.table(
        newenv, renv,
        quote = FALSE, sep = "\n",
        col.names = FALSE, row.names = FALSE
      )
    }
  } else {
    file.create(renv)
  }

  # Append access token to .Renviron file
  write(glue("{type}=\'{token}\'"), renv, sep = "\n", append = TRUE)

  cli_inform(
    c(
      "v" = "Token {.val {token}} saved to {.file .Renviron} variable {.envvar {type}}.",
      "*" = 'Restart R or run {.code readRenviron("~/.Renviron")} then use
        {.code Sys.getenv("{type}")} to access the token.'
    )
  )
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

  cli_abort("{.envvar {type}} can't be found in your {.file .Renviron}.")
}
