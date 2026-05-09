# Format data frames and simple features using common approaches

This function can apply the following common data cleaning tasks:

- Applies
  [stringr::str_squish](https://stringr.tidyverse.org/reference/str_trim.html)
  and
  [stringr::str_trim](https://stringr.tidyverse.org/reference/str_trim.html)
  to all character columns

- Optionally replaces all character values of "" with `NA` values

- Optionally corrects UNIX formatted dates with 1970-01-01 origins

- Optionally renames variables by passing a named list of variables

The address functions previously included with `format_data()` are now
documented at
[`format_address_data()`](https://elipousson.github.io/getdata/reference/format_address_data.md).

## Usage

``` r
format_data(
  x,
  var_names = NULL,
  xwalk = NULL,
  clean_names = TRUE,
  .name_repair = "check_unique",
  replace_na_with = NULL,
  replace_with_na = NULL,
  replace_empty_char_with_na = FALSE,
  fix_date = FALSE,
  label = FALSE,
  remove_empty = NULL,
  remove_constant = FALSE,
  format_sf = FALSE,
  ...,
  call = caller_env()
)

rename_with_xwalk(
  x,
  xwalk = NULL,
  cols = c("label", "name"),
  label = FALSE,
  .strict = TRUE,
  keep_all = TRUE,
  arg = caller_arg(x),
  call = caller_env()
)

label_with_xwalk(x, xwalk = NULL, label = "var", ...)

make_variable_dictionary(
  x,
  .labels = NULL,
  .definitions = NULL,
  details = c("basic", "none", "full")
)

fix_epoch_date(x, .cols = dplyr::contains("date"), tz = "")
```

## Arguments

- x:

  A tibble or data frame object

- var_names:

  A named list following the format,
  `list("New var name" = old_var_name)`, or a two column data frame with
  the first column being the new variable names and the second column
  being the old variable names; defaults to `NULL`.

- xwalk:

  A data frame with two columns using the first column as name and the
  second column as value; or a named list. The existing names of x must
  be the values and the new names must be the names.

- clean_names:

  If `TRUE`, set .name_repair to
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html);
  defaults to `TRUE`.

- .name_repair:

  Defaults to "check_unique"

- replace_na_with:

  A named list to pass to
  [`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html);
  defaults to `NULL`.

- replace_with_na:

  A named list to pass to
  [`naniar::replace_with_na()`](https://naniar.njtierney.com/reference/replace_with_na.html);
  defaults to `NULL`.

- replace_empty_char_with_na:

  If `TRUE`, replace "" with `NA` using
  [`naniar::replace_with_na_if()`](https://naniar.njtierney.com/reference/replace_with_na_if.html),
  Default: `TRUE`

- fix_date:

  If `FALSE`, fix UNIX epoch dates (common issue with dates from
  FeatureServer and MapServer sources) using the `fix_epoch_date()`
  function, Default: `TRUE`

- label:

  For `label_with_xwalk()` use `label = "val"` to use
  [`labelled::set_value_labels()`](https://larmarange.github.io/labelled/reference/val_labels.html)
  or "var" (default) to use
  [`labelled::set_variable_labels()`](https://larmarange.github.io/labelled/reference/var_label.html).
  For `rename_with_xwalk()`, if label is `TRUE`, xwalk is passed to
  `label_with_xwalk()` with label = "var" to label columns using the
  original names. Defaults to `FALSE`.

- remove_empty:

  If not `NULL`, pass values ("rows", "cols" or c("rows", "cols")
  (default)) to the which parameter of
  [`janitor::remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.html)

- remove_constant:

  If `TRUE`, pass data to janitor::remove_constant() using default
  parameters.

- format_sf:

  If `TRUE`, pass x and additional parameters to
  [`format_sf_data()`](https://elipousson.github.io/getdata/reference/format_sf_data.md).

- ...:

  Additional parameters passed to
  [`format_sf_data()`](https://elipousson.github.io/getdata/reference/format_sf_data.md)

- cols:

  Column names to use for crosswalk.

- .strict:

  If `TRUE` (default), require that all values from the xwalk are found
  in the column names of the x data.frame. If `FALSE`, unmatched values
  from the xwalk are ignored.

- keep_all:

  If `FALSE`, columns that are not named in the xwalk are dropped. If
  `TRUE` (default), all columns are retained. If x is an `sf` object,
  the geometry column will not be dropped even it is not renamed.

- arg, call:

  Additional parameters used internally with
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  to improve error messages.

- .labels:

  Replaces labels column created by
  [`labelled::generate_dictionary()`](https://larmarange.github.io/labelled/reference/look_for.html)
  if column is all `NA` (no existing labels assigned); defaults to
  `NULL`.

- .definitions:

  Character vector of definitions appended to dictionary data frame.
  Must be in the same order as the variables in the provided data frame
  x.

- details:

  add details about each variable (full details could be time consuming
  for big data frames, `FALSE` is equivalent to `"none"` and `TRUE` to
  `"full"`)

- .cols:

  tidyselect for columns to apply epoch date fixing function to.
  Defaults to `dplyr::contains("date")`.

- tz:

  Time zone passed to
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html).

## Value

The input data frame or simple feature object with formatting functions
applied.

## Examples

``` r
nc <- get_location_data(data = system.file("shape/nc.shp", package = "sf"))

format_data(nc)
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9386879 ymin: 4012985 xmax: -8399792 ymax: 4382074
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 100 × 15
#>     area perimeter  cnty cnty_id name  fips  fipsno cress_id bir74 sid74 nwbir74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # ℹ 90 more rows
#> # ℹ 4 more variables: bir79 <dbl>, sid79 <dbl>, nwbir79 <dbl>,
#> #   geometry <MULTIPOLYGON [m]>
```
