# Get Wikipedia articles for a location

Use the Wikipedia API geosearch API to get Wikipedia articles for a
location. See <https://www.mediawiki.org/wiki/Extension:GeoData> for
more information. Only returns Wikipedia articles with coordinates.

## Usage

``` r
get_wiki_data(
  location,
  radius = FALSE,
  primary = NULL,
  details = NULL,
  limit = 50,
  list = "geosearch",
  lang = getOption("getdata.lang", default = "en"),
  geometry = TRUE,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  asp = getOption("getdata.asp"),
  crs = getOption("getdata.unit", 3857),
  remove_coords = TRUE,
  clean_names = TRUE
)
```

## Arguments

- location:

  sf object. If multiple areas are provided, they are unioned into a
  single sf object using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- radius:

  If `TRUE`, use dist as a buffer around the center of the location;
  defaults to `FALSE`

- primary:

  If `NULL`, search for primary coordinates. Set primary to "all" or
  "secondary" to search other coordinate types.

- details:

  Additional detailed to return with results. Options include "type",
  "name", "country", "region"; defaults to `NULL`.

- limit:

  Number of pages to return (max 500); defaults to 50

- list:

  method to use for query; "geosearch" returns data, "resp" returns
  response

- lang:

  Language to search on Wikipedia; defaults to "en".

- geometry:

  If `TRUE`, return sf object. If `FALSE`, return data frame. Defaults
  to `FALSE`.

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

- remove_coords:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html),
  if `TRUE`, remove the coordinate columns after converting a data frame
  to simple feature object; defaults to `FALSE`.

- clean_names:

  If `TRUE`, clean names provided to nm or created based on value of col
  using
  [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
  If `FALSE`, use names as provided.

## Details

For this function, `location` can be either an `sf`, `sfc`, or `bbox`
object or the title of a Wikipedia article with a related location.

## See also

`GNfindNearbyWikipedia()` and `GNwikipediaBoundingBox()` functions in
the [geonames](https://docs.ropensci.org/geonames/) package
