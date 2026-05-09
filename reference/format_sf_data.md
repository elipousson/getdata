# Format simple feature data

The main format_sf_data function is a wrapper for the following common
steps in transforming an `sf` object and preparing for mapping or
analysis:

## Usage

``` r
format_sf_data(
  x,
  crs = getOption("getdata.crs", default = 3857),
  erase_data = NULL,
  dTolerance = NULL,
  smooth = FALSE,
  sf_col = NULL,
  sf_req = TRUE,
  ...
)

erase_data(x, erase_data = NULL)
```

## Arguments

- x:

  A `sf` object or, if `sf_req` is `FALSE`, any object that can be
  converted to an `sf` object with
  [sfext::as_sf](https://elipousson.github.io/sfext/reference/as_sf.html).

- crs:

  Coordinate reference system for returned data, Default:
  getOption("getdata.crs", default = 3857)

- erase_data:

  A `sf`, `sfc`, or `bbox` object with geometry that should be erased
  from the data, Default: `NULL`

- dTolerance:

  numeric; tolerance parameter, specified for all or for each feature
  geometry. If you run `st_simplify`, the input data is specified with
  long-lat coordinates and
  [`sf_use_s2()`](https://r-spatial.github.io/sf/reference/s2.html)
  returns `TRUE`, then the value of `dTolerance` must be specified in
  meters.

- smooth:

  If `TRUE`, smooth data with
  [smoothr::smooth](https://strimas.com/smoothr/reference/smooth.html)
  using default method and parameters, Default: `FALSE`.

- sf_col:

  Name to use for output `sf` column, Default: 'geometry'.

- sf_req:

  If `TRUE`, data must be a `sf` object. If `FALSE`, data is passed to
  [sfext::as_sf](https://elipousson.github.io/sfext/reference/as_sf.html)
  to convert data to an `sf` object.

- ...:

  Additional parameters passed to `format_data`

## Value

A `sf` object with columns and geometry modified based parameters.

## Details

- Convert data to an `sf` object with
  [sfext::as_sf](https://elipousson.github.io/sfext/reference/as_sf.html)
  if `sf_req` is `FALSE`

- Make data valid with
  [sf::st_make_valid](https://r-spatial.github.io/sf/reference/valid.html)
  if needed

- Format data with
  [format_data](https://elipousson.github.io/getdata/reference/format_data.md)
  using the ... parameters

- Erase any data overlapping with `erase_data` (suggested for use with
  water or open space)

- Simplify geometry with
  [sf::st_simplify](https://r-spatial.github.io/sf/reference/geos_unary.html)
  if `dTolerance` is provided

- Smooth geometry with
  [smoothr::smooth](https://strimas.com/smoothr/reference/smooth.html)
  if `smooth` is `TRUE`

- Rename the sf column to match `sf_col` (defaults to "geometry")

The helper functions for format_sf_data and additional formatting
functions for `sf` data are described in the details.

Helper functions for format_sf_data:

- erase_data: erase intersection of x and erase_data (validity of
  erase_data checked before
  [sfext::st_erase](https://elipousson.github.io/sfext/reference/st_erase.html)
  and for x after completing the operation.

- [sfext::rename_sf_col](https://elipousson.github.io/sfext/reference/misc_sf.html):
  Rename `sf` column.

- [sfext::relocate_sf_col](https://elipousson.github.io/sfext/reference/misc_sf.html):
  Relocate `sf` column after selected columns (defaults to
  [`dplyr::everything()`](https://dplyr.tidyverse.org/reference/reexports.html)).

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
nc_county <- nc[2,]

# Transform coordinate reference system
st_crs(nc)$epsg
#> [1] 4267
st_crs(format_sf_data(nc, crs = 3857))$epsg
#> [1] 3857

# Simplify and smooth geometry
plot(nc_county, max.plot = 1)

nc_county_simple <- format_sf_data(nc_county, dTolerance = 5000, smooth = TRUE)
plot(nc_county_simple, max.plot = 1)


# Erase data
nc_co_water <- get_tigris_data(type = "area water", state = "NC", county = nc_county$NAME)
#> Retrieving data for the year 2024
#>   |                                                                              |                                                                      |   0%  |                                                                              |===============================================                       |  67%  |                                                                              |=======================================================               |  79%  |                                                                              |======================================================================| 100%
nc_county_erased <- format_sf_data(nc_county, erase_data = nc_co_water)
plot(nc_county_erased, max.plot = 1)


# If sf_req is set to FALSE, use any object that can be converted with sfext::as_sf
nc_bbox <- st_bbox(nc)
plot(format_sf_data(nc_bbox, erase_data = nc_county_simple, sf_req = FALSE))
```
