skip_if_no_token <- function(type = NULL) {
  skip_if_not(!is.null(type) && (Sys.getenv(type) != ""))
}
