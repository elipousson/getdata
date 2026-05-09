# Use `elevatr::get_elev_point` to get the elevation along a profile

**\[experimental\]** `get_elev_profile()` is a wrapper for
[`elevatr::get_elev_point()`](https://rdrr.io/pkg/elevatr/man/get_elev_point.html)
that takes a LINESTRING or POINT input for profile and returns a data
frame of elevation. Optionally, create a series of points along a line
with
[`sf::st_line_sample()`](https://r-spatial.github.io/sf/reference/st_line_sample.html)
and/or include a column with the distance between each successive POINT
in the data frame.

## Usage

``` r
get_elev_profile(
  profile,
  units = NULL,
  dist = FALSE,
  n = NULL,
  density = NULL,
  type = "regular",
  sample = NULL,
  ...,
  drop_units = FALSE,
  cumulative = FALSE
)
```

## Arguments

- profile:

  A `sfc` or `sf` geometry with a LINESTRING or POINT geometry type.

- units:

  If `NULL`, output elevation and distance is in meters. If a valid
  distance unit is supplied, elevation and distance are converted to
  match the supplied unit.

- dist:

  If `TRUE`

- n:

  integer; number of points to choose per geometry; if missing, n will
  be computed as `round(density * st_length(geom))`.

- density:

  numeric; density (points per distance unit) of the sampling, possibly
  a vector of length equal to the number of features (otherwise
  recycled); `density` may be of class `units`.

- type:

  character; indicate the sampling type, either "regular" or "random"

- sample:

  numeric; a vector of numbers between 0 and 1 indicating the points to
  sample - if defined sample overrules n, density and type.

- ...:

  Unused at this time.

- drop_units:

  If `TRUE`, return a plain numeric column for the elevation and
  distance columns. If `FALSE` (default) return a units class column.

- cumulative:

  If `TRUE`, and dist is `TRUE` return distance as a cumulative sum.

## Details

This function is proposed for addition to `{elevatr}`:
<https://github.com/elipousson/getdata/issues/4>
