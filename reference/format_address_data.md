# Format data frames and simple features with address data

getdata has two helpers for working with address data:

- `bind_address_col()` bind a provided value for city, county, and state
  to a data frame (to supplement address data with consistent values for
  these variables). This function is useful for converting partial
  street addresses with a consistent values for state, county, or city
  into full addresses

- `bind_block_col()` requires a data frame with columns named
  "bldg_num", "street_dir_prefix", "street_name", and "street_type" and
  binds derived values for whether a building is on the even or odd side
  of a block and create a block segment and a block face (including the
  even/odd identifier).

## Usage

``` r
bind_block_col(
  x,
  bldg_num = "bldg_num",
  street_dir_prefix = "street_dir_prefix",
  street_name = "street_name",
  street_suffix = "street_type",
  replace_suffix = FALSE,
  street_col = NULL,
  block_col = NULL,
  .after = street_suffix,
  case = NULL
)

bind_address_col(x, ..., case = NULL, .cols = NULL, .after = NULL)

bind_location_text_col(
  x,
  text_col = "text",
  address_pattern = c("Ave.", "Avenue", "St.", "Street", "Rd.", "Road"),
  block_face_pattern = c("sides\\)", "side\\)", "[:space:]block", "-block", "blocks"),
  street_corridor_pattern = c("between(?=.+and)", "from(?=.+to)"),
  .cols = NULL
)
```

## Arguments

- x:

  A data.frame with a column name matching col and no column names
  matching the list passed to .cols (or the default values listed
  below).

- bldg_num, street_dir_prefix, street_name, street_suffix:

  Column names to use for address information required to generate a
  block name and number.

- replace_suffix:

  If `TRUE`, replace values in street_suffix column with abbreviations
  from
  [street_suffixes](https://elipousson.github.io/getdata/reference/street_suffixes.md).

- street_col:

  String to use for street address column added based on component
  column values.

- block_col:

  String to use as prefix for block identifier columns and separator
  between block number and street. Set to "block" when `NULL` (default).
  If length 2 (e.g. c("blk", "block")), the second value is used as the
  block separator and the first as the column identifier prefix.

- .after:

  passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  defaults to street_suffix for `bind_block_col()` and "address" for
  `bind_address_col()`.

- case:

  Case to use for text in new columns or in modified values. Options
  include "lower", "upper", "title", or "sentence". Defaults to `NULL`
  which leaves the case as is.

- ...:

  Additional parameters passed to
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  intended for use in filling missing values, e.g. state = "MD" to add a
  missing state column.

- .cols:

  Column names to add. Defaults to is_address, is_block_face,
  is_street_corridor, and block_side. x must not have any column names
  matching the names found in .cols.

- text_col:

  Column name containing the information to check for location details,
  Default: 'text'

- address_pattern:

  A character vector of regex patterns to return `TRUE` for is_address.

- block_face_pattern:

  A character vector of regex patterns to return `TRUE` for
  is_block_face.

- street_corridor_pattern:

  A character vector of regex patterns to return `TRUE` for
  is_street_corridor.

## Value

A data.frame with new indicator columns for address and block_face and a
column indicating whether the text references a particular cardinal
direction in describing a block.

## Examples

``` r
address_df <- data.frame(
    "bldg_num" = c("100", "1415", "600"),
    "street_dir_prefix" = c(NA, NA, "N"),
    "street_name" = c("Holiday", "Key", "Charles"),
    "street_type" = c("Street", "Highway", "St")
  )

address_df <- bind_block_col(
    x = address_df,
    street_col = "street_address"
  )

address_df[1,]
#>   bldg_num street_dir_prefix street_name street_type     street_address
#> 1      100                       Holiday      Street 100 Holiday Street
#>   block_num block_even_odd            block_segment
#> 1       100           Even 100 block Holiday Street
#>                        block_face
#> 1 100 block Holiday Street (Even)

address_df <- bind_address_col(
    address_df,
    city = "Baltimore",
    state = "MD"
  )

address_df[2,]
#>   bldg_num street_dir_prefix street_name street_type   street_address block_num
#> 2     1415                           Key     Highway 1415 Key Highway      1400
#>   block_even_odd          block_segment                   block_face      city
#> 2            Odd 1400 block Key Highway 1400 block Key Highway (Odd) Baltimore
#>   state                        address
#> 2    MD 1415 Key Highway, Baltimore MD

location_df <- data.frame(
    "text" = c(
      "100 Holiday St.",
      "1400 block Key Highway (north side)",
      "Charles St. from E. Centre St. to E. Madison St."
    )
)

location_df <- bind_location_text_col(location_df)

location_df
#>                                               text is_address is_block_face
#> 1                                  100 Holiday St.       TRUE         FALSE
#> 2              1400 block Key Highway (north side)       TRUE          TRUE
#> 3 Charles St. from E. Centre St. to E. Madison St.       TRUE         FALSE
#>   is_street_corridor block_side
#> 1              FALSE       <NA>
#> 2              FALSE      north
#> 3               TRUE       <NA>
```
