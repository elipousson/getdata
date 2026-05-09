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

  WARNING: this parameter is equivalent to `osm_types = "node"` and will
  be DEPRECATED. If `TRUE`, query OSM nodes only. Some OSM structures
  such as `place = "city"` or `highway = "traffic_signals"` are
  represented by nodes only. Queries are built by default to return all
  nodes, ways, and relation, but this can be very inefficient for
  node-only queries. Setting this value to `TRUE` for such cases makes
  queries more efficient, with data returned in the `osm_points` list
  item.

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
  location = nc[37,],
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

civic_buildings
#> Simple feature collection with 11 features and 36 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -78.89991 ymin: 35.57932 xmax: -78.59919 ymax: 36.02971
#> Geodetic CRS:  WGS 84
#> # A tibble: 11 × 37
#>    osm_id     name  `addr:city` `addr:county` `addr:housenumber` `addr:postcode`
#>    <chr>      <chr> <chr>       <chr>         <chr>              <chr>          
#>  1 28788263   Muse… Durham      NA            433                27704          
#>  2 32145960   Cary… NA          NA            120                NA             
#>  3 47699270   Opti… NA          NA            NA                 NA             
#>  4 47836108   Visi… NA          NA            NA                 NA             
#>  5 378257128  West… Cary        Wake          4000               27519;27519-93…
#>  6 378397242  Morr… NA          NA            NA                 NA             
#>  7 438008281  Fuqu… Fuquay-Var… NA            301                27526          
#>  8 469540465  Town… Fuquay-Var… NA            405                27526          
#>  9 859911045  UNC … NA          NA            6715               27519          
#> 10 913687471  Kiwa… NA          NA            NA                 NA             
#> 11 1223378126 NA    NA          NA            NA                 NA             
#> # ℹ 31 more variables: `addr:state` <chr>, `addr:street` <chr>, alt_name <chr>,
#> #   amenity <chr>, building <chr>, `building:levels` <chr>,
#> #   `building:material` <chr>, `building:prefabricated` <chr>,
#> #   `contact:website` <chr>, description <chr>, healthcare <chr>,
#> #   leisure <chr>, museum <chr>, nat_ref <chr>, note <chr>,
#> #   opening_hours <chr>, operator <chr>, `operator:type` <chr>, phone <chr>,
#> #   `roof:levels` <chr>, `roof:shape` <chr>, smoking <chr>, …
```
