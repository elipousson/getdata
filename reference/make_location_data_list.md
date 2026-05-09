# Make a list of data and corresponding locations

This function converts data and location into lists of sf objects using
[sfext::as_sf_list](https://elipousson.github.io/sfext/reference/sf_list.html).
If location_col, data_col, or col (which sets both to the same value),
are provided the col is passed to
[sfext::as_sf_list](https://elipousson.github.io/sfext/reference/sf_list.html)
to allow the creation of an sf list from a sf data frame using
[`dplyr::group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.html).

## Usage

``` r
make_location_data_list(data, location, key = c("location", "data"), ...)
```

## Arguments

- data, location:

  A sf object or list of sf objects with data and corresponding
  locations.

- key:

  Names for location and data in the returned list.

- ...:

  Pass location_col and/or data_col to group and nest the data provided
  to location and data. Use col to set both to the same value.

## Details

If location and data are the same length are the same length, they are
combined into a single list. If either one is length 1 when the other is
not, the length 1 object is repeated to match the length of the longer
object. Different length objects where neither are length 1 gives a
warning.
