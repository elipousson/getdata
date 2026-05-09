# Use lubridate to convert an object to a date range

Use
[`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)
to convert an object to a length 2 list with a minimum and maximum date.
By default the dates in the list will be named named "start" and "end".
`date_range_query()` is a variation that returns a query string that can
be passed to the "where" parameter of
[`get_esri_data()`](https://elipousson.github.io/getdata/reference/get_esri_data.md).
`between_date_range()` works the same way identical but uses the BETWEEN
DATE syntax. `check_date_range()` validates whether a date or date range
falls within supplied limits.

## Usage

``` r
as_date_range(
  x = NULL,
  year = NULL,
  days = 90,
  ...,
  start_date = NULL,
  end_date = NULL,
  limits = NULL,
  nm = c("start", "end"),
  call = caller_env()
)

date_range_query(x = NULL, .col = "date", ..., nm = c("start", "end"))

between_date_range(x = NULL, .col = "date", ..., nm = c("start", "end"))

check_date_range(
  x = NULL,
  ...,
  limits = NULL,
  nm = c("start", "end"),
  call = caller_env()
)
```

## Arguments

- x:

  Date range as character vector in format of
  `c("<start date>", "<end date>")`. If length 1 and days is not `NULL`,
  return a range based on
  `c(date_range, date_range + lubridate::days(days))`. `as_date_range()`
  also allows a Date class object or a list of Date class objects. For
  all other functions, x can also be a named list of Date objects with
  names matching nm.

- year:

  If date_range is `NULL` and year is provided, date range is set to
  `c("<year>-01-01", "<year>-12-31")`. year is ignored if date_range is
  provided.

- days:

  Default range duration in days to use if date_range is length 1.

- ...:

  Arguments passed on to
  [`lubridate::as_date`](https://lubridate.tidyverse.org/reference/as_date.html)

  :   

- start_date, end_date:

  Start and end date used if year and date_range are both `NULL`.

- limits:

  Optional range of allowed dates. If dates supplied to
  `as_date_range()` falls outside limits range, abort function.

- nm:

  Names to use for returned date range list. Defaults to
  `c("start", "end")`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .col:

  Name of date column to use for query. Defaults to "date".

## Value

A length 2 list with min and max Date values.

## Examples

``` r
as_date_range("2022-01-01", days = 10)
#> $start
#> [1] "2022-01-01"
#> 
#> $end
#> [1] "2022-01-11"
#> 

as_date_range(c("2022-01-01", "2022-01-31"))
#> $start
#> [1] "2022-01-01"
#> 
#> $end
#> [1] "2022-01-31"
#> 

as_date_range(year = 2022)
#> $start
#> [1] "2022-01-01"
#> 
#> $end
#> [1] "2022-12-31"
#> 

date_range_query(c("2022-01-01", "2022-01-31"))
#> (date >= '2022-01-01') AND (date <= '2022-01-31')

# check_date_range("2022-09-01", limits = c("2022-07-01", "2022-09-30"))
```
