# Replace values in a character vector or data frame with a crosswalk

Use
[`stringr::str_replace_all()`](https://stringr.tidyverse.org/reference/str_replace.html)
to replace values in a character vector or (with
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html))
in select columns from a data.frame. `replace_street_dir_prefixes()` and
`replace_street_suffixes()` pass reference data
([street_dir_prefixes](https://elipousson.github.io/getdata/reference/street_dir_prefixes.md)
and
[street_suffixes](https://elipousson.github.io/getdata/reference/street_suffixes.md))
to the dict parameter to support formatting addresses with
[`bind_block_col()`](https://elipousson.github.io/getdata/reference/format_address_data.md).

## Usage

``` r
replace_with_xwalk(
  x,
  .cols = NULL,
  xwalk = NULL,
  dict = NULL,
  abb = TRUE,
  case = NULL,
  .strict = TRUE,
  ignore_case = TRUE
)

replace_street_suffixes(
  x,
  street_suffix = "street_type",
  xwalk = NULL,
  abb = TRUE,
  case = NULL
)

replace_street_dir_prefixes(
  x,
  street_dir_prefix = "street_dir_prefix",
  xwalk = NULL,
  abb = TRUE,
  case = NULL
)
```

## Arguments

- x:

  A data.frame or character vector. If x is a character vector, .cols is
  optional. If x is a data.frame, x is required.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to transform. You can't select grouping columns because they
  are already automatically handled by the verb (i.e.
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  or [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).

- xwalk, dict:

  Named list or data frame with a minimum of two columns where one
  column contains the replacement values and the other the values to
  replace. If xwalk is `NULL`, dict is used and vice-versa. If both are
  provided, the xwalk values take precedence so they can be used to
  override a dict or add new values.

- abb:

  If abb is `TRUE` (default), the second column of the dict is assumed
  to be abbreviation that should be used as the replace for the values
  in x or the replacement column. Otherwise, the first column is assumed
  to hold the replacement values and the second column is assumed to
  hold the original values. For example, for
  `replace_street_suffixes()`, If `TRUE`, replace full suffix names with
  abbreviations. If `FALSE`, replace abbreviations with full street
  suffix names.

- case:

  Case to use for text in new columns or in modified values. Options
  include "lower", "upper", "title", or "sentence". Defaults to `NULL`
  which leaves the case as is.

- .strict:

  If `TRUE` (default), match whole strings by appending "^" to the front
  and "\$" to the end of each pattern in the xwalk.

- ignore_case:

  Passed to
  [`stringr::regex()`](https://stringr.tidyverse.org/reference/modifiers.html)

- street_suffix:

  Street suffix column to apply replacement function to.

- street_dir_prefix:

  Street direction prefix column to apply replacement function to.

## Examples

``` r
address_df <-
  data.frame(
    "bldg_num" = c("100", "1415", "600", "10"),
    "street_dir_prefix" = c(NA, NA, "N", NA),
    "street_name" = c("Holiday", "Key", "Charles", "Art Museum"),
    "street_type" = c("St", "Hwy", "St", "Dr")
  )

replace_street_suffixes(
  c("Street", "Highway", "Avenue", "Drive")
)
#> [1] "ST"  "HWY" "AVE" "DR" 

replace_street_suffixes(
  address_df,
  abb = FALSE,
  case = "sentence"
)
#>   bldg_num street_dir_prefix street_name street_type
#> 1      100              <NA>     Holiday      Street
#> 2     1415              <NA>         Key     Highway
#> 3      600                 N     Charles      Street
#> 4       10              <NA>  Art Museum       Drive

replace_street_dir_prefixes(
  c("North", "East", "West")
)
#> [1] "N" "E" "W"

replace_street_dir_prefixes(
  c("S", "W", "N"),
  abb = FALSE,
  case = "sentence"
)
#> [1] "South" "West"  "North"
```
