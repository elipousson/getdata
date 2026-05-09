# Set or get an access token or API key to/from environment variables.

Based on the
[`mapboxapi::mb_access_token()`](https://walker-data.com/mapboxapi/reference/mb_access_token.html)
function from the {mapboxapi} package by Kyle Walker.

## Usage

``` r
set_access_token(
  token,
  overwrite = FALSE,
  install = FALSE,
  type = NULL,
  quiet = FALSE,
  call = caller_env()
)

get_access_token(token = NULL, type = NULL, call = caller_env())
```

## Arguments

- token:

  An access token or API key; required for `set_access_token()`. If
  token is not provided; type is required for `get_access_token()`.
  Length 1 character vector or list. If named, the name of the token is
  used in place of type.

- overwrite:

  If `TRUE`, overwrite any existing token in `.Renviron` using the same
  environment variable name. Defaults to `FALSE`.

- install:

  If `TRUE`, this function adds your token to your `.Renviron` for use
  in future sessions. Defaults to `FALSE`.

- type:

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
