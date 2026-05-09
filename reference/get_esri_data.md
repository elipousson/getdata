# Use esri2sf to get data from an ArcGIS FeatureServer or MapServer for a location

Wraps the
[`esri2sf::esri2sf()`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html) and
[`esri2sf::esri2df()`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html)
functions to download an ArcGIS FeatureServer or MapServer. Supports
spatial filtering with bounding box based on location and filtering by
location name (if location name column is provided). As of fall 2022,
this package suggests the
[elipousson/esri2sf](https://github.com/elipousson/esri2sf/) fork using
httr2.

## Usage

``` r
get_esri_data(
  url,
  location = NULL,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit"),
  asp = getOption("getdata.asp"),
  crs = getOption("getdata.crs", 3857),
  where = NULL,
  name = NULL,
  name_col = NULL,
  coords = NULL,
  from_crs = getOption("getdata.crs", 4326),
  clean_names = TRUE,
  token = NULL,
  progress = TRUE,
  quiet = FALSE,
  .name_repair = janitor::make_clean_names,
  ...,
  call = caller_env()
)

get_esri_layers(
  location = NULL,
  layers = NULL,
  url = NULL,
  nm = NULL,
  token = NULL,
  clean_names = TRUE,
  quiet = FALSE,
  .name_repair = janitor::make_clean_names,
  ...,
  call = caller_env()
)

get_esri_metadata(
  url,
  token = NULL,
  meta = NULL,
  clean_names = TRUE,
  .name_repair = janitor::make_clean_names,
  call = caller_env()
)
```

## Arguments

- url:

  FeatureServer or MapServer url to retrieve data from. Passed to `url`
  parameter of
  [`esri2sf::esri2sf()`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html)
  or
  [`esri2sf::esri2df()`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html)
  functions. For `get_esri_layers()`, the optional url must be a service
  url which is the base url for one or more layer urls.

- location:

  `sf`, `sfc`, or `bbox` object (or other object convertible with
  [`sfext::as_bbox()`](https://elipousson.github.io/sfext/reference/as_sf.html).
  Optional.

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

  Cordinate reference system to return, Default: 4326 for
  [`sf_to_df()`](https://elipousson.github.io/sfext/reference/sf_to_df.html)
  and `NULL` for
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html).

- where:

  where query string passed to esri2sf, Default: `NULL`

- name, name_col:

  Name value and name column found in the ArcGIS FeatureServer or
  MapServer data.

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- from_crs:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html),
  coordinate reference system used by coordinates or well known text in
  data frame.

- clean_names:

  If `TRUE`, set .name_repair to
  [`janitor::make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.html)
  Ignored when `get_esri_metadata()` is not returning a data.frame, e.g.
  `meta = "id"`.

- token:

  string for authentication token. defaults to `NULL`.

- progress:

  Show progress bar from
  [`cli::cli_progress_along()`](https://cli.r-lib.org/reference/cli_progress_along.html)
  if `TRUE`. Default `FALSE`.

- quiet:

  If `TRUE`, use
  [`suppressMessages()`](https://rdrr.io/r/base/message.html) to prevent
  the printing of messages about the requested layer. Defaults to
  `FALSE`.

- .name_repair:

  Passed to
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).

- ...:

  Arguments passed on to
  [`esri2sf::esri2sf`](https://rdrr.io/pkg/esri2sf/man/esri2sf.html)

  `outFields`

  :   vector of fields you want to include. default is `NULL` for all
      fields.

  `replaceDomainInfo`

  :   If `TRUE`, add domain information to the return data frame.
      Default `FALSE`.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- layers:

  Either a vector with URLs, a named list of urls, or a numeric vector;
  defaults to `NULL`. Optional if url is a

- nm:

  Name or vector of names to add to the layers; defaults to `NULL`.

- meta:

  Name of metadata list value to return from
  [esri2sf::esrimeta](https://rdrr.io/pkg/esri2sf/man/esrimeta.html),
  e.g. "name" to return layer name. Defaults to `NULL`.
