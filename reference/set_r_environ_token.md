# Set or get a token from your `.Renviron` file

Set or get a token from your `.Renviron` file

## Usage

``` r
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
```

## Source

Adapted from the `census_api_key()` function in the
[tidycensus](https://walker-data.com/tidycensus/) package.

## Arguments

- token:

  A personal access token, API key, or other environment variable.
  Optional for `get_r_environ_token()`.

- install:

  If `TRUE`, this function adds your token to your `.Renviron` for use
  in future sessions. Defaults to `FALSE`.

- overwrite:

  If `TRUE`, overwrite any existing token in `.Renviron` using the same
  environment variable name. Defaults to `FALSE`.

- default:

  Default name used for environment variable where the token is saved.

- quiet:

  If `TRUE`, suppress messages when setting token by locally setting the
  `cli.default_handler` option to
  [`suppressMessages()`](https://rdrr.io/r/base/message.html). Defaults
  to `FALSE`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- message:

  Optional error message to use if token can't be found.

- pattern:

  Optional pattern passed to
  [`grepl()`](https://rdrr.io/r/base/grep.html) and used to validate the
  stored token. If pattern is supplied, the returned token must be a
  string.

- perl:

  Should Perl-compatible regexps be used when checking `pattern`?
  Defaults to `TRUE`.

- strict:

  If `TRUE` (default), error if no environment variable with the
  supplied name is found. If `FALSE`, warn instead of error.

## Value

`set_r_environ_token()` invisibly returns a string supplied to `token`.

`get_r_environ_token()` returns a string supplied to `token` or obtained
from the environment variable named with `default`.

## Author

Kyle Walker <kyle@walker-data.com>

Eli Pousson <eli.pousson@gmail.com>
([ORCID](https://orcid.org/0000-0001-8280-1706))

`set_r_environ_token()` can set an API key or personal access token
(PAT) as a local environment variable temporarily for the current
session or saved for future sessions.

`get_r_environ_token()` can return an environment variable or error if
the token is missing or if the token does not match a supplied pattern.
