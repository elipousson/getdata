# Make a crosswalk list for use with [`label_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md) or [`rename_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)

Make a crosswalk list for use with
[`label_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)
or
[`rename_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)

## Usage

``` r
make_xwalk_list(xwalk, cols = c("label", "name"), call = caller_env())
```

## Arguments

- xwalk:

  A data frame with two columns or a named list.

- cols:

  Column names to use for crosswalk.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A named list
