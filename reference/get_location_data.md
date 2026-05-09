# Get data for a location

Returns data for a selected location or a list of locations (for
`map_location_data()`). If data is a character string, the parameter is
passed to
[`sfext::read_sf_url()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html),
[`sfext::read_sf_path()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html),
or
[`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html).
This function uses
[`sfext::st_filter_ext()`](https://elipousson.github.io/sfext/reference/st_filter_ext.html)
to filter, crop, or trim data to the provided location. location can
also be an an address.

## Usage

``` r
get_location_data(
  location = NULL,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", default = "meter"),
  asp = getOption("getdata.asp"),
  data = NULL,
  pkg = getOption("getdata.package"),
  package = getOption("getdata.package"),
  fileext = getOption("getdata.fileext", default = "gpkg"),
  filetype = getOption("getdata.filetype", default = "gpkg"),
  fn = NULL,
  crop = TRUE,
  trim = FALSE,
  from_crs = getOption("getdata.from_crs"),
  crs = getOption("getdata.crs", 3857),
  class = "sf",
  label = NULL,
  index = NULL,
  col = NULL,
  var_names = NULL,
  clean_names = FALSE,
  range = NULL,
  .name_repair = "check_unique",
  ...,
  call = caller_env()
)

map_location_data(
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  data = NULL,
  package = NULL,
  fileext = "gpkg",
  filetype = "gpkg",
  fn = NULL,
  crop = TRUE,
  trim = FALSE,
  from_crs = NULL,
  crs = NULL,
  class = "list",
  label = NULL,
  load = FALSE,
  index = NULL,
  range = NULL,
  ...
)
```

## Arguments

- location:

  sf object. If multiple areas are provided, they are unioned into a
  single sf object using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
  returns the same value without modification.

- data:

  Character string (e.g. url, file path, or name of data from package)
  for a spatial data or a `sf`, `sfc`, or `bbox` object with geometry
  overlapping the location. If data is `NULL`, all unnamed parameters
  are passed to
  [`sfext::read_sf_ext()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  with a bbox based on location. If data is not `NULL` and not a
  data.frame, url, file path, or bbox, conversion to a sf object will
  still always be attempted with
  [`sfext::as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.html).

- pkg, package:

  Name of the package to search for data.

- fileext, filetype:

  File extension or type to use if passing parameters to
  [`sfext::read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  or
  [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  (required for extdata and cached data).

- fn:

  Function to apply to data after filtering by location but before
  returning from function.

- crop:

  If `TRUE`, x is cropped to y using
  [`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html).

- trim:

  If `TRUE`, x is trimmed to y with
  [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).

- from_crs:

  Coordinate reference system used to match the location CRS to the
  source data.

- crs:

  Coordinate reference system to return.

- class:

  Class of object to return.

- label:

  label is optionally used by `map_location_data()` to name the data
  objects in the list returned by the function.

- index:

  A list of possible location, data, and (optionally) package values.
  List must be named and include a value named package and package must
  be `NULL`, to set package based on index. If list is not `NULL` and
  location and/or data as character or numeric values, the location and
  data are assumed to be index values for the index list. The index
  parameter supports nested lists created by
  [`make_location_data_list()`](https://elipousson.github.io/getdata/reference/make_location_data_list.md)
  (using only the default key names of "location" and "data"). This
  feature has not be fully tested and may result in errors or unexpected
  results.

- col:

  For
  [as_sf_list](https://elipousson.github.io/sfext/reference/sf_list.html),
  the name of the column used to group data if x is a sf object or used
  to group and nest data before passing to x.

- var_names:

  A named list following the format,
  `list("New var name" = old_var_name)`, or a two column data frame with
  the first column being the new variable names and the second column
  being the old variable names; defaults to `NULL`.

- clean_names:

  If `TRUE`, clean names provided to nm or created based on value of col
  using
  [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
  If `FALSE`, use names as provided.

- range:

  For
  [`lonlat_to_sfc()`](https://elipousson.github.io/sfext/reference/lonlat_to_sfc.html),
  an object that is coercible to a `bbox` object or a length 4 vector
  with names xmin, xmax, ymin, and ymax. If a coordinate pair falls
  outside the latitude/longitude range defined by the vector but inside
  the range if reversed, the coordinates are assumed to be in lat/lon
  order and are switched to lon/lat order before being converted to a
  point. Defaults to
  `c("xmin" = -180, "ymin" = -50, "xmax" = 180, "ymax" = 60)`. Note that
  this default setting will reverse valid coordinates north of
  Anchorage, Alaska or south of New Zealand.

- .name_repair:

  One of "unique", "universal", or "check_unique". See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

- ...:

  Additional parameters passed to
  [`sfext::read_sf_path()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html),
  [`sfext::read_sf_url()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html),
  [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html),
  [`sfext::as_sf()`](https://elipousson.github.io/sfext/reference/as_sf.html)
  (with bbox), or
  [`sfext::read_sf_ext()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  (with no other parameters).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- load:

  If `TRUE` and class is "list", load data to local environment;
  defaults `FALSE`.

## Details

This function previously supported county geoid, state name,
abbreviation, or geoid as a location. Currently, recommend using
[`get_tigris_data()`](https://elipousson.github.io/getdata/reference/get_state_tigris.md)
and passing a `sf` object to location.

Working with sf lists for data and locations:

`map_location_data()` makes it easier to work with `sf` lists. It
supports data as a character vector, data as an `sf` list when location
is a single object, location as a character vector or `sf` list
(including lists of `bbox` or `sfc` objects), or when both data and
location are lists (such as a list created by
[`make_location_data_list()`](https://elipousson.github.io/getdata/reference/make_location_data_list.md)).
