# Use osmdata to get Open Street Map data for a location

Use `osmdata` functions to query the overpass API and access OSM data by
adjusted bounding box or by enclosing ways/relations around the center
of a location. For more information on key and value options, refer to
the `osm_common_tags` reference table or the OSM Wiki
<https://wiki.openstreetmap.org/wiki/Map_features>. Use the `osmdata`
package directly for more detailed control over queries
<https://docs.ropensci.org/osmdata/>

## Usage

``` r
get_osm_data(
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  key,
  value = NULL,
  features = NULL,
  id = NULL,
  type = NULL,
  crs = NULL,
  geometry = NULL,
  osmdata = FALSE,
  enclosing = NULL,
  nodes_only = FALSE,
  key_exact = TRUE,
  value_exact = TRUE,
  match_case = TRUE
)

get_osm_id(id, type = NULL, crs = NULL, geometry = NULL, osmdata = FALSE)

get_osm_boundaries(
  location,
  level = NULL,
  lang = "en",
  crs = NULL,
  enclosing = "relation",
  geometry = NULL,
  osmdata = FALSE
)
```

## Arguments

- location:

  A `sf`, `sfc`, or `bbox` object converted to bounding box with
  [`sfext::st_bbox_ext()`](https://elipousson.github.io/sfext/reference/st_bbox_ext.html)
  or a character object passed directly to the bbox parameter of
  [`osmdata::opq()`](https://docs.ropensci.org/osmdata/reference/opq.html).

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

- key:

  Feature key for overpass API query.

- value:

  Value of the feature key; can be negated with an initial exclamation
  mark, `value = "!this"`, and can also be a vector,
  `value = c("this", "that")`. If `value = "all"` or if
  `key = "building"` the values passed to the osmdata package are from a
  preset list extracted from
  [`osmdata::available_tags()`](https://docs.ropensci.org/osmdata/reference/available_tags.html).

- features:

  A named list with the format `list("<key>" = "<value>")` or a
  character vector of key-value pairs with keys and values enclosed in
  escape-formatted quotations (see
  [`osmdata::add_osm_features()`](https://docs.ropensci.org/osmdata/reference/add_osm_features.html))
  for examples of the latter option.

- id:

  OpenStreetMap feature id with or without a type id prefix. If multiple
  id values are provided, they must use a single consistent value for
  geometry.

- type:

  Type of feature for the id; "node", "way", or "relation". Optional if
  id includes a type prefix.

- crs:

  Coordinate reference system for output data; if `NULL`, the data
  remains in the Open Street Map coordinate reference system 4326.
  Default: `NULL`.

- geometry:

  Geometry type to output ("polygons", "points", "lines", "multilines",
  or "multipolygons"); if multiple geometry types are needed set osmdata
  to `TRUE.` Default `NULL`.

- osmdata:

  If `TRUE` return a `osmdata` class object that includes the overpass
  API call, metadata including timestamp and version numbers, and all
  available geometry types; defaults to `FALSE`.

- enclosing:

  If enclosing is "relation" or "way", this function uses the
  [`osmdata::opq_enclosing()`](https://docs.ropensci.org/osmdata/reference/opq_enclosing.html)
  to query the OSM data (instead of
  [`osmdata::add_osm_feature()`](https://docs.ropensci.org/osmdata/reference/add_osm_feature.html).
  Defaults to `NULL`. When the enclosing parameter is provided, the
  dist, diag_ratio, asp, and unit parameters are ignored and the center
  of the provided location is used for the query. geometry is set
  automatically based enclosing with "relation" using "multipolygons"
  and "way" using "polygons" geometry.

- nodes_only:

  WARNING: this parameter is equivalent to `osm_types = "node"` and is
  DEPRECATED. If `TRUE`, query OSM nodes only.

- key_exact:

  If FALSE, `key` is not interpreted exactly; see
  <https://wiki.openstreetmap.org/wiki/Overpass_API>

- value_exact:

  If FALSE, `value` is not interpreted exactly

- match_case:

  If FALSE, matching for both `key` and `value` is not sensitive to case

- level:

  Numeric administrative level (admin_level) of boundary to return;
  defaults to `NULL`. If multiple levels are provided, the any admin
  levels between the min and max values of level is returned. See
  <https://wiki.openstreetmap.org/wiki/Key:admin_level> for more
  information. Only used for `get_osm_boundaries()`.

- lang:

  Language for boundary names to include in resulting data frame (e.g.
  "en" for English or "es" for Spanish). Default language names should
  always be included in results. Defaults to "en". See
  <https://wiki.openstreetmap.org/wiki/Multilingual_names> for more
  information.

## Value

A simple feature object with features using selected geometry type or an
`osmdata` object with features from all geometry types.

## Examples

``` r
nc <- sfext::read_sf_path(system.file("shape/nc.shp", package = "sf"))

civic_buildings <- get_osm_data(
  location = nc[37, ],
  features = c("building" = "civic"),
  geometry = "polygons"
)
#> ℹ OpenStreetMap data is licensed under the Open Database License (ODbL).
#>   Attribution is required if you use this data.
#> • Learn more about the ODbL and OSM attribution requirements at
#>   <https://www.openstreetmap.org/copyright>
#> This message is displayed once every 8 hours.
#> Warning: 'nodes_only = TRUE' is deprecated.
#> Use 'osm_types = "node"' instead.
#> See help("Deprecated") and help("osmdata-deprecated").
#> Error in get_osm_data_features(location = location, dist = dist, diag_ratio = diag_ratio,     unit = unit, asp = asp, key = key, value = value, features = features,     crs = crs, geometry = geometry, osmdata = osmdata, nodes_only = nodes_only,     key_exact = key_exact, value_exact = value_exact, match_case = match_case): `osmdata::osmdata_sf()` encountered an error.
#> Caused by error in `httr2::req_perform()`:
#> ! HTTP 502 Bad Gateway.

civic_buildings
#> Error: object 'civic_buildings' not found
```
