# Get Wikipedia articles for a location

Use the Wikipedia geosearch API to get Wikipedia articles near a
location. Only articles with coordinates are returned. See the GeoData
extension <https://www.mediawiki.org/wiki/Extension:GeoData> and the API
documentation for the geosearch module
<https://www.mediawiki.org/w/api.php?action=help&modules=query%2Bgeosearch>
for more information.

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
  crs = getOption("getdata.crs", 3857),
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
  defaults to `FALSE`. If numeric, the value is used as `dist` and
  `radius` is set to `TRUE`.

- primary:

  If `NULL`, search for primary coordinates. Set primary to "all" or
  "secondary" to search other coordinate types.

- details:

  Additional details to return with results. Options include "name",
  "type", "dim", "scale", "region", "country", and "globe"; defaults to
  `NULL`.

- limit:

  Number of pages to return (max 500); defaults to 50

- list:

  method to use for query; "geosearch" returns data, "resp" returns
  response

- lang:

  Language to search on Wikipedia; defaults to "en".

- geometry:

  If `TRUE` (default), return sf object. If `FALSE`, return data frame.

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

This function only covers the geosearch module. It returns the page ID,
title, coordinates, and distance from the search point for each article
(plus optional coordinate details) but not article text, extracts,
images, categories, or Wikidata properties such as height or
construction date. It also only searches one language edition of
Wikipedia at a time and does not search other Wikimedia projects (e.g.
Wikidata or Wikimedia Commons).

For this function, `location` can be either an `sf`, `sfc`, or `bbox`
object or the title of a Wikipedia article with a related location. If
`location` is an `sf` object, the search is made using the bounding box
of the location (adjusted by `dist`, `diag_ratio`, and `asp`) or, if
`radius = TRUE`, a circle around the center of the location. The API
limits the search area: a radius must be between 10 and 10,000 meters
and a bounding box can't be much larger than about 25 kilometers across.

## See also

`GNfindNearbyWikipedia()` and `GNwikipediaBoundingBox()` functions in
the [geonames](https://docs.ropensci.org/geonames/) package

## Examples

``` r
# Get articles within a radius of a point
bmore_city_hall <- sf::st_sfc(
  sf::st_point(c(-76.6122, 39.2904)),
  crs = 4326
)

get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 5
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 14963519     0 Battle Monument          37.7 ""      (-8528455 4763397)
#> 2 81206170     0 Equitable Building (Ba…  47.9 ""      (-8528489 4763333)
#> 3 17302400     0 Baltimore City Circuit…  66.5 ""      (-8528371 4763416)
#> 4 21451830     0 One Calvert Plaza        77.8 ""      (-8528431 4763254)
#> 5 21562114     0 United States Post Off…  88   ""      (-8528526 4763417)

# A numeric radius is used as dist (set unit to change the distance unit)
get_wiki_data(
  location = bmore_city_hall,
  radius = 0.25,
  unit = "mi",
  limit = 5
)
#> ℹ Using `radius` value of 0.25 for `dist`.
#> ✔ Setting `radius` to TRUE.
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 14963519     0 Battle Monument          37.7 ""      (-8528455 4763397)
#> 2 81206170     0 Equitable Building (Ba…  47.9 ""      (-8528489 4763333)
#> 3 17302400     0 Baltimore City Circuit…  66.5 ""      (-8528371 4763416)
#> 4 21451830     0 One Calvert Plaza        77.8 ""      (-8528431 4763254)
#> 5 21562114     0 United States Post Off…  88   ""      (-8528526 4763417)

# Search within the bounding box of a polygon or other sf object
bmore_area <- sf::st_buffer(
  sf::st_transform(bmore_city_hall, 26985),
  dist = 300
)

get_wiki_data(
  location = bmore_area,
  limit = 5
)
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 14963519     0 Battle Monument          37.7 ""      (-8528455 4763397)
#> 2 81206170     0 Equitable Building (Ba…  47.9 ""      (-8528489 4763333)
#> 3 17302400     0 Baltimore City Circuit…  66.5 ""      (-8528371 4763416)
#> 4 21451830     0 One Calvert Plaza        77.8 ""      (-8528431 4763254)
#> 5 21562114     0 United States Post Off…  88   ""      (-8528526 4763417)

# Expand the bounding box with dist or diag_ratio
get_wiki_data(
  location = bmore_area,
  dist = 200,
  unit = "meter",
  limit = 5
)
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 14963519     0 Battle Monument          37.7 ""      (-8528455 4763397)
#> 2 81206170     0 Equitable Building (Ba…  47.9 ""      (-8528489 4763333)
#> 3 17302400     0 Baltimore City Circuit…  66.5 ""      (-8528371 4763416)
#> 4 21451830     0 One Calvert Plaza        77.8 ""      (-8528431 4763254)
#> 5 21562114     0 United States Post Off…  88   ""      (-8528526 4763417)

get_wiki_data(
  location = bmore_area,
  radius = TRUE,
  diag_ratio = 0.5,
  limit = 5
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 14963519     0 Battle Monument          37.7 ""      (-8528455 4763397)
#> 2 81206170     0 Equitable Building (Ba…  47.9 ""      (-8528489 4763333)
#> 3 17302400     0 Baltimore City Circuit…  66.5 ""      (-8528371 4763416)
#> 4 21451830     0 One Calvert Plaza        77.8 ""      (-8528431 4763254)
#> 5 21562114     0 United States Post Off…  88   ""      (-8528526 4763417)

# Return additional coordinate details for each article
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  details = c("type", "name"),
  limit = 5
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 5 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528526 ymin: 4763254 xmax: -8528371 ymax: 4763417
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 8
#>     pageid    ns title        dist primary type  name            geometry
#> *    <int> <int> <chr>       <dbl> <chr>   <chr> <lgl>        <POINT [m]>
#> 1 14963519     0 Battle Mon…  37.7 ""      NA    NA    (-8528455 4763397)
#> 2 81206170     0 Equitable …  47.9 ""      NA    NA    (-8528489 4763333)
#> 3 17302400     0 Baltimore …  66.5 ""      NA    NA    (-8528371 4763416)
#> 4 21451830     0 One Calver…  77.8 ""      land… NA    (-8528431 4763254)
#> 5 21562114     0 United Sta…  88   ""      NA    NA    (-8528526 4763417)

# Return a data frame with longitude and latitude columns instead of an sf
# object
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 5,
  geometry = FALSE,
  remove_coords = FALSE
)
#> New names:
#> • `` -> `...1`
#>     pageid ns                                                          title
#> 1 14963519  0                                                Battle Monument
#> 2 81206170  0                                 Equitable Building (Baltimore)
#> 3 17302400  0                             Baltimore City Circuit Courthouses
#> 4 21451830  0                                              One Calvert Plaza
#> 5 21562114  0 United States Post Office and Courthouse (Baltimore, Maryland)
#>        lat       lon dist primary
#> 1 39.29069 -76.61242 37.7        
#> 2 39.29025 -76.61272 47.9        
#> 3 39.29082 -76.61166 66.5        
#> 4 39.28970 -76.61220 77.8        
#> 5 39.29083 -76.61306 88.0        

# Use the title of an article with coordinates as the location
get_wiki_data(
  location = "Baltimore City Hall",
  limit = 5
)
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528371 ymin: 4763297 xmax: -8528146 ymax: 4763533
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>     pageid    ns title                    dist primary           geometry
#> *    <int> <int> <chr>                   <dbl> <chr>          <POINT [m]>
#> 1 16922377     0 Holliday Street Theater  59.9 ""      (-8528186 4763427)
#> 2 17302400     0 Baltimore City Circuit…  82.9 ""      (-8528371 4763416)
#> 3 26400947     0 War Memorial Plaza       90.8 ""      (-8528146 4763427)
#> 4 17932045     0 Business and Governmen…  96.5 ""      (-8528279 4763297)
#> 5 36668721     0 Zion Lutheran Church (… 117.  ""      (-8528161 4763533)

# Search another language edition of Wikipedia
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  lang = "es",
  limit = 5
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 5 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528643 ymin: 4763139 xmax: -8528264 ymax: 4763441
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 5 × 6
#>    pageid    ns title                     dist primary           geometry
#> *   <int> <int> <chr>                    <dbl> <chr>          <POINT [m]>
#> 1 9698159     0 One Calvert Plaza         77.8 ""      (-8528431 4763254)
#> 2 6870681     0 Disturbios de Baltimore… 113.  ""      (-8528300 4763421)
#> 3 6494242     0 Ayuntamiento de Baltimo… 145.  ""      (-8528264 4763441)
#> 4 9999420     0 William Donald Schaefer… 177.  ""      (-8528643 4763268)
#> 5 9990804     0 The Garrett Building     192.  ""      (-8528309 4763139)

# Return the full API response instead of a data frame
resp <- get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 2,
  list = "resp"
)
#> New names:
#> • `` -> `...1`

str(resp, max.level = 2)
#> List of 2
#>  $ batchcomplete: chr ""
#>  $ query        :List of 1
#>   ..$ geosearch:'data.frame':    2 obs. of  7 variables:
```
