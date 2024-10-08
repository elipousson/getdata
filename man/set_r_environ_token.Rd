% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-standalone-settoken.R
\name{set_r_environ_token}
\alias{set_r_environ_token}
\alias{get_r_environ_token}
\title{Set or get a token from your  \code{.Renviron} file}
\source{
Adapted from the \code{census_api_key()} function in the
\href{https://walker-data.com/tidycensus/}{tidycensus} package.
}
\usage{
set_r_environ_token(
  token,
  install = FALSE,
  overwrite = FALSE,
  default = "TOKEN",
  quiet = FALSE,
  call = caller_env()
)

get_r_environ_token(
  token = NULL,
  default = "TOKEN",
  message = NULL,
  pattern = NULL,
  perl = TRUE,
  strict = TRUE,
  call = caller_env(),
  ...
)
}
\arguments{
\item{token}{A personal access token, API key, or other environment variable.
Optional for \code{\link[=get_r_environ_token]{get_r_environ_token()}}.}

\item{install}{If \code{TRUE}, this function adds your token to your \code{.Renviron}
for use in future sessions. Defaults to \code{FALSE}.}

\item{overwrite}{If \code{TRUE}, overwrite any existing token in \code{.Renviron} using
the same environment variable name. Defaults to \code{FALSE}.}

\item{default}{Default name used for environment variable where the token
is saved.}

\item{quiet}{If \code{TRUE}, suppress messages when setting token by locally
setting the \code{cli.default_handler} option to \code{\link[=suppressMessages]{suppressMessages()}}. Defaults
to \code{FALSE}.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{message}{Optional error message to use if token can't be found.}

\item{pattern}{Optional pattern passed to \code{\link[=grepl]{grepl()}} and used to validate the
stored token. If pattern is supplied, the returned token must be a string.}

\item{perl}{Should Perl-compatible regexps be used when checking \code{pattern}?
Defaults to \code{TRUE}.}

\item{strict}{If \code{TRUE} (default), error if no environment variable with the
supplied name is found. If \code{FALSE}, warn instead of error.}
}
\value{
\code{\link[=set_r_environ_token]{set_r_environ_token()}} invisibly returns a string supplied to
\code{token}.

\code{\link[=get_r_environ_token]{get_r_environ_token()}} returns a string supplied to \code{token} or
obtained from the environment variable named with \code{default}.
}
\description{
Set or get a token from your  \code{.Renviron} file
}
\author{
Kyle Walker \email{kyle@walker-data.com}

Eli Pousson \email{eli.pousson@gmail.com}
(\href{https://orcid.org/0000-0001-8280-1706}{ORCID})

\code{\link[=set_r_environ_token]{set_r_environ_token()}} can set an API key or personal access token (PAT) as
a local environment variable temporarily for the current session or saved for
future sessions.

\code{\link[=get_r_environ_token]{get_r_environ_token()}} can return an environment variable or error if the
token is missing or if the token does not match a supplied pattern.
}
\keyword{internal}
