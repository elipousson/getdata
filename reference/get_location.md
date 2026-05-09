# Get location of a specified type based on name, id, or location

Filter by name or id or use a spatial filter based on an sf object or
geocoded street address. Optionally you can use an index list to match
the type to a named list of URLs or sf objects.

## Usage

``` r
get_location(
  type,
  name = NULL,
  name_col = "name",
  id = NULL,
  id_col = "id",
  location = NULL,
  index = NULL,
  union = FALSE,
  crs = getOption("getdata.crs", 3857),
  label = NULL,
  class = "sf",
  ...
)
```

## Arguments

- type:

  Type of location to return. Type can be an sf object, e.g. a data
  frame with multiple neighborhoods or a character string that can be
  passed to
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md).
  If index is provided, character can also be a character string to
  match the name of a list.

- name:

  Location name to return.

- name_col:

  Column name in type with name values, Default: 'name' Required if name
  provided.

- id:

  Location id to return. id is coerced to character or numeric to match
  the class of the id_col for type.

- id_col:

  Column name in type with id values, Default: 'id'. Required if id is
  provided.

- location:

  An address, bounding box (`bbox`), or simple feature (`sf`) object
  passed to
  [`sf::st_filter()`](https://r-spatial.github.io/sf/reference/st_join.html).
  Any valid address or addresses are geocoded with
  [`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html),
  converted to a simple feature object, and then used as a spatial
  filter. `bbox` objects are converted using
  [`sfext::sf_bbox_to_sf()`](https://elipousson.github.io/sfext/reference/sf_bbox_misc.html).
  Multiple addresses are supported.

- index:

  Optional list used to match type to data, Default: `NULL`

- union:

  If `TRUE`, the location geometry is unioned with
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  and the names are combined into a single value. Default: `FALSE`.

- crs:

  Coordinate reference system to return; defaults to `NULL` which
  returns data using the same coordinate reference system as the
  provided type of location.

- label:

  Label optionally added to "label" column; must be a length 1 or match
  the number of rows returned based on the other parameters. If
  `union = TRUE`, using label is recommended. Default: `NULL`

- class:

  Class of object to return; defaults to "sf".

- ...:

  Additional parameters passed to
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  if type is character and index is `NULL`.

## Value

A simple feature object from data provided to type.

## Examples

``` r
nc <- sfext::read_sf_path(system.file("shape/nc.shp", package = "sf"))

# get_location works with a type sf object and name and id values
get_location(type = nc, name = "Warren", name_col = "NAME")
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8718651 ymin: 4327628 xmax: -8671629 ymax: 4376985
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 1 × 15
#>    AREA PERIMETER CNTY_ CNTY_ID NAME   FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#> * <dbl>     <dbl> <dbl>   <dbl> <chr>  <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#> 1 0.118      1.42  1836    1836 Warren 37185  37185       93   968     4     748
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [m]>
get_location(type = nc, id = 37185, id_col = "FIPSNO")
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8718651 ymin: 4327628 xmax: -8671629 ymax: 4376985
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 1 × 15
#>    AREA PERIMETER CNTY_ CNTY_ID NAME   FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#> * <dbl>     <dbl> <dbl>   <dbl> <chr>  <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#> 1 0.118      1.42  1836    1836 Warren 37185  37185       93   968     4     748
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [m]>
# if name is named, the name of name is used as name_col
get_location(type = nc, name = c("NAME" = "Warren"))
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8718651 ymin: 4327628 xmax: -8671629 ymax: 4376985
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 1 × 15
#>    AREA PERIMETER CNTY_ CNTY_ID NAME   FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#> * <dbl>     <dbl> <dbl>   <dbl> <chr>  <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#> 1 0.118      1.42  1836    1836 Warren 37185  37185       93   968     4     748
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [m]>

# type can also be a file path
get_location(
  type = system.file("shape/nc.shp", package = "sf"),
  name = "Hertford",
  name_col = "NAME"
)
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8595797 ymin: 4332359 xmax: -8539004 ymax: 4377448
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 1 × 15
#>    AREA PERIMETER CNTY_ CNTY_ID NAME   FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>   <dbl>     <dbl> <dbl>   <dbl> <chr>  <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#> 1 0.097      1.67  1833    1833 Hertf… 37091  37091       46  1452     7     954
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   `_ogr_geometry_` <POLYGON [m]>

# type can also be an index name (if a named list of data sets, url values, or
# path values is passed to index)
get_location(
  type = "smaller",
  name = "Hertford",
  name_col = "NAME",
  index = list(
    "smaller" = dplyr::filter(nc, AREA <= 0.10),
    "larger" = dplyr::filter(nc, AREA > 0.15)
  )
)
#> Simple feature collection with 1 feature and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8595797 ymin: 4332359 xmax: -8539004 ymax: 4377448
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 1 × 15
#>    AREA PERIMETER CNTY_ CNTY_ID NAME   FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#> * <dbl>     <dbl> <dbl>   <dbl> <chr>  <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#> 1 0.097      1.67  1833    1833 Hertf… 37091  37091       46  1452     7     954
#> # ℹ 4 more variables: BIR79 <dbl>, SID79 <dbl>, NWBIR79 <dbl>,
#> #   geometry <MULTIPOLYGON [m]>
```
