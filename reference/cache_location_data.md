# Cache location data with `sf::write_sf()` or `readr::write_rds()`

`cache_location_data()` is a variant on
[`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
that saves the returned data to a cache directory using
[`sf::write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
(if data is a sf object) or
[`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html)
(if data is another class). Additional parameters are ignored if
location is `NULL`.

## Usage

``` r
cache_location_data(
  data = NULL,
  ...,
  location = NULL,
  name = NULL,
  label = NULL,
  fileext = "gpkg",
  filename = NULL,
  path = NULL,
  prefix = NULL,
  postfix = NULL,
  cache = TRUE,
  pkg = "getdata",
  create = TRUE,
  overwrite = FALSE,
  compress = c("none", "gz", "bz2", "xz"),
  version = 3,
  call = caller_env()
)
```

## Arguments

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

- ...:

  Arguments passed on to
  [`get_location_data`](https://elipousson.github.io/getdata/reference/get_location_data.md)

  `pkg,package`

  :   Name of the package to search for data.

  `fileext,filetype`

  :   File extension or type to use if passing parameters to
      [`sfext::read_sf_download()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      or
      [`sfext::read_sf_pkg()`](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
      (required for extdata and cached data).

  `fn`

  :   Function to apply to data after filtering by location but before
      returning from function.

  `from_crs`

  :   Coordinate reference system used to match the location CRS to the
      source data.

  `crs`

  :   Coordinate reference system to return.

  `class`

  :   Class of object to return.

  `index`

  :   A list of possible location, data, and (optionally) package
      values. List must be named and include a value named package and
      package must be `NULL`, to set package based on index. If list is
      not `NULL` and location and/or data as character or numeric
      values, the location and data are assumed to be index values for
      the index list. The index parameter supports nested lists created
      by
      [`make_location_data_list()`](https://elipousson.github.io/getdata/reference/make_location_data_list.md)
      (using only the default key names of "location" and "data"). This
      feature has not be fully tested and may result in errors or
      unexpected results.

  `dist`

  :   buffer distance in units. Optional.

  `diag_ratio`

  :   ratio of diagonal distance of area's bounding box used as buffer
      distance. e.g. if the diagonal distance is 3000 meters and the
      "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist`
      is provided.

  `asp`

  :   Aspect ratio of width to height as a numeric value (e.g. 0.33) or
      character (e.g. "1:3"). If numeric,
      [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
      returns the same value without modification.

  `unit`

  :   Units for buffer. Supported options include "meter", "foot",
      "kilometer", and "mile", "nautical mile" Common abbreviations
      (e.g. "km" instead of "kilometer") are also supported. Distance in
      units is converted to units matching GDAL units for x; defaults to
      "meter"

  `crop`

  :   If `TRUE`, x is cropped to y using
      [`sf::st_crop()`](https://r-spatial.github.io/sf/reference/st_crop.html).

  `trim`

  :   If `TRUE`, x is trimmed to y with
      [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).

  `col`

  :   For
      [as_sf_list](https://elipousson.github.io/sfext/reference/sf_list.html),
      the name of the column used to group data if x is a sf object or
      used to group and nest data before passing to x.

  `clean_names`

  :   If `TRUE`, clean names provided to nm or created based on value of
      col using
      [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
      If `FALSE`, use names as provided.

  `.name_repair`

  :   One of "unique", "universal", or "check_unique". See
      [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
      for the meaning of these options.

  `var_names`

  :   A named list following the format,
      `list("New var name" = old_var_name)`, or a two column data frame
      with the first column being the new variable names and the second
      column being the old variable names; defaults to `NULL`.

  `range`

  :   For
      [`lonlat_to_sfc()`](https://elipousson.github.io/sfext/reference/lonlat_to_sfc.html),
      an object that is coercible to a `bbox` object or a length 4
      vector with names xmin, xmax, ymin, and ymax. If a coordinate pair
      falls outside the latitude/longitude range defined by the vector
      but inside the range if reversed, the coordinates are assumed to
      be in lat/lon order and are switched to lon/lat order before being
      converted to a point. Defaults to
      `c("xmin" = -180, "ymin" = -50, "xmax" = 180, "ymax" = 60)`. Note
      that this default setting will reverse valid coordinates north of
      Anchorage, Alaska or south of New Zealand.

- location:

  sf object. If multiple areas are provided, they are unioned into a
  single sf object using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- name:

  Name to make file name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html),
  e.g. "Residential zoning map" becomes "residential_zoning_map". If the
  name includes a file extension it is assumed that the filename has
  been provided as the name parameter.

- label:

  Label to combine with name converted to snake case with
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html).
  The label is designed to identify the area or other shared
  characteristics across multiple data files, maps, or plots. label is
  ignored if name is NULL or if name includes a file extension.

- fileext:

  File type or extension. Optional if filename or path include a file
  extension.

- filename:

  File name; if filename is `NULL` and path does not include a file
  extension, name and file extension are both required.

- path:

  Path to file or data directory. Optional. If path includes a file
  extension and filename and fileext are both `NULL`, the filename and
  extension included with path will be used instead. If multiple file
  extensions are provided to filename, path, or fileext,
  `make_filename()` will abort.

- prefix:

  File name prefix. "date" adds a date prefix, "time" adds a date/time
  prefix; defaults to `NULL`.

- postfix:

  File name postfix; defaults to `NULL`.

- cache:

  If `TRUE`, path is set to the package cache directory using
  [`get_data_dir()`](https://elipousson.github.io/filenamr/reference/get_data_dir.html);
  defaults to `FALSE`.

- pkg:

  Package name passed to appname parameter of
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)

- create:

  If `FALSE` and path does not exist, return path with a warning. If
  `TRUE` and
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)
  is `TRUE`, ask user if directory should be created. If the session not
  interactive and create is `TRUE`, a new directory will be created.

- overwrite:

  If `TRUE`, remove a file with the same name and path

- compress:

  Compression method to use: "none", "gz" ,"bz", or "xz".

- version:

  Serialization format version to be used. The default value is 2 as
  it's compatible for R versions prior to 3.5.0. See
  [`base::saveRDS()`](https://rdrr.io/r/base/readRDS.html) for more
  details.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

Save data to file and invisibly return file path.
