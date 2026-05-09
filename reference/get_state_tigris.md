# Use tigris to get state-level data from the U.S. Census Bureau

Use the [{tigris}](https://github.com/walkerke/tigris) package to
download state-level data from the U.S. Census Bureau API and optionally
filter by name or GeoID.

## Usage

``` r
get_tigris_data(
  type = NULL,
  state = getOption("getdata.state"),
  location = NULL,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  asp = getOption("getdata.asp"),
  crs = getOption("getdata.crs", default = 3857),
  name = NULL,
  name_col = c("name", "namelsad", "geoid"),
  cb = TRUE,
  clean_names = TRUE,
  cache = TRUE,
  ...
)
```

## Arguments

- type:

  Type of data to return, Default: `NULL`; See details for supported
  options.

- state:

  State name, abbreviation, or GeoID. Required. Defaults to
  getOption("getdata.state").

- location:

  A sf, sfc, or bbox object passed to
  [`sfext::st_bbox_ext()`](https://elipousson.github.io/sfext/reference/st_bbox_ext.html)
  and used to create the geometry passed to the `filter_by` parameter
  passed to the tigris functions.

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

- crs:

  Coordinate reference system of bounding box to return; defaults to
  `NULL` which maintains the crs of the input object.

- name, name_col:

  Name and columns to filter by name. name defaults to `NULL`, and
  name_col defaults to c("namelsad", "namelsad", "geoid") columns.

- cb:

  If `TRUE`, download a generalized (1:500k) file. If `FALSE`, download
  the most detailed TIGER/Line file. Defaults to `TRUE` (reverse of the
  default for tigris functions). This parameter is *not* used when type
  is set to blocks, roads, primary secondary roads, area water, linear
  water, landmarks, or zctas.

- clean_names:

  If `TRUE`, set .name_repair to
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html);
  defaults to `TRUE`.

- cache:

  If `TRUE`, set `options(tigris_use_cache = TRUE)` to cache downloaded
  tigris data. Ignored if `getOption("tigris_use_cache")` is not `NULL`.

- ...:

  Additional parameters passed on to tigris functions.

## Value

A simple feature object matching the type provided.

## Details

Supported data types:

Different type values corresponded to different tigris functions for
downloading from the U.S. Census Bureau API include. Supported options
include: "counties", "census places", "congressional districts",
"legislative districts", "senate district", "tracts", "block groups",
"blocks", "pumas", "voting districts", "zctas", "roads", "primary
secondary roads", "area water", "linear water", and "landmarks".

tigris functions that do not use a "state" parameter (e.g.
[tigris::coastline](https://rdrr.io/pkg/tigris/man/coastline.html) or
[tigris::rails](https://rdrr.io/pkg/tigris/man/rails.html)) are not
supported by this function. Note that the default value of the cb
parameter for get_tigris_data is `TRUE` and the default value of for the
original {tigris} package is `FALSE`.
