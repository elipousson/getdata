#' Set or get an access token or API key to/from environment variables.
#'
#' Based on the [mapboxapi::mb_access_token()] function from the \{mapboxapi\}
#' package by Kyle Walker.
#'
#' @param token An access token or API key; required for [set_access_token()].
#'   If token is not provided; type is required for [get_access_token()]. Length
#'   1 character vector or list. If named, the name of the token is used in
#'   place of type.
#' @param type Default name used for environment variable where the token is
#'   saved.
#' @inheritParams set_r_environ_token
#' @rdname set_access_token
#' @aliases set_token_type
#' @export
set_access_token <- function(token,
                             overwrite = FALSE,
                             install = FALSE,
                             type = NULL,
                             quiet = FALSE,
                             call = caller_env()) {
  set_r_environ_token(
    token = token,
    overwrite = overwrite,
    install = install,
    default = type,
    quiet = quiet,
    call = call
  )
}

#' @name get_access_token
#' @rdname set_access_token
#' @aliases get_token_type
#' @export
get_access_token <- function(token = NULL,
                             type = NULL,
                             call = caller_env()) {
  get_r_environ_token(
    token = token,
    default = type,
    call = call
  )
}
