# Get Wikidata items for a location

Use the Wikidata Query Service to get items with coordinates (property
P625) within a radius of a location. The search uses the
`wikibase:around` SPARQL service. See
<https://www.mediawiki.org/wiki/Wikidata_Query_Service/User_Manual> and
<https://www.wikidata.org/wiki/Wikidata:SPARQL_query_service/queries>
for more information.

## Usage

``` r
get_wikidata(
  location,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  class = NULL,
  subclasses = TRUE,
  limit = NULL,
  lang = getOption("getdata.lang", default = "en"),
  geometry = TRUE,
  crs = getOption("getdata.crs", 3857)
)
```

## Arguments

- location:

  sf object. If multiple areas are provided, they are unioned into a
  single sf object using
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)

- dist, diag_ratio, unit:

  Radius of the search area around the center of `location` and the unit
  for the distance. If `diag_ratio` is provided, `dist` is set to the
  diagonal distance of the location bounding box multiplied by the
  ratio.

- class:

  A character vector of Wikidata class identifiers (e.g. "Q41176" for
  building) used to filter items by "instance of" (property P31).
  Defaults to `NULL` for no filtering.

- subclasses:

  If `TRUE` (default), include items that are an instance of a subclass
  (property P279) of `class`. Searching subclasses can make the query
  slower.

- limit:

  Maximum number of items to return. Items are ordered by distance from
  the center of the location. Defaults to `NULL` for no limit.

- lang:

  Language for item labels and the Wikipedia article URL; defaults to
  "en". Items without a label in `lang` use the English label.

- geometry:

  If `TRUE` (default), return sf object. If `FALSE`, return a data frame
  with `lon` and `lat` columns.

- crs:

  Coordinate reference system for the returned data. Only used if
  `geometry = TRUE`.

## Value

A data frame or `sf` object with one row per item and columns for `qid`,
`label`, `dist` (in meters), `height` (in meters), `instance_of`, and
`wikipedia`. If no items are found, an empty data frame is returned with
a warning.

## Details

Compared to
[`get_wiki_data()`](https://elipousson.github.io/getdata/reference/get_wiki_data.md),
which searches Wikipedia articles, this function can filter by what an
item is (`class`) and returns the item identifier, the "instance of"
labels, the Wikipedia article URL (if any), and the height (property
P2048), if available. Height values are reported by Wikidata
contributors, are missing for most items, and may not describe the
highest point of a structure, so check them before using them in an
analysis. This function does not return other properties or qualifiers.
For those, use a package like
[tidywikidatar](https://edjnet.github.io/tidywikidatar/) with the
returned `qid` values.

The search area is always a circle. If `dist` is `NULL` and `diag_ratio`
is `NULL`, the circle is the smallest circle around the center of the
bounding box for `location` that includes the corners of the bounding
box. If `location` is a point, `dist` or `diag_ratio` must be provided.

## Examples

``` r
bmore_city_hall <- sf::st_sfc(
  sf::st_point(c(-76.6122, 39.2904)),
  crs = 4326
)

# Get items within 500 meters of a point
get_wikidata(
  location = bmore_city_hall,
  dist = 500,
  limit = 10
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528531 ymin: 4763254 xmax: -8528300 ymax: 4763455
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 10 × 7
#>    qid        label  dist height instance_of wikipedia           geometry
#>  * <chr>      <chr> <dbl>  <dbl> <chr>       <chr>            <POINT [m]>
#>  1 Q112730228 orga…     2     NA pipe organ  NA        (-8528430 4763352)
#>  2 Q14691875  Batt…    23     NA monument    https://… (-8528461 4763352)
#>  3 Q7121864   Paca…    44     NA fire stati… https://… (-8528431 4763412)
#>  4 Q7891082   Unit…    68     NA courthouse  https://… (-8528370 4763418)
#>  5 Q4852806   Balt…    76     NA building    https://… (-8528520 4763398)
#>  6 Q7092512   One …    78     NA tower block https://… (-8528431 4763254)
#>  7 Q136488241 Equi…    82     NA building    https://… (-8528511 4763425)
#>  8 Q112697932 Tenc…    84     NA K–8 school… NA        (-8528522 4763296)
#>  9 Q5440386   Fede…   110     NA Federal Re… https://… (-8528531 4763455)
#> 10 Q19838194  2015…   113     NA protest     https://… (-8528300 4763421)

# Filter by class. "Q41176" is building; subclasses like skyscraper and city
# hall are included by default.
get_wikidata(
  location = bmore_city_hall,
  dist = 1,
  unit = "km",
  class = "Q41176",
  limit = 10
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528587 ymin: 4763182 xmax: -8528370 ymax: 4763455
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 10 × 7
#>    qid        label  dist height instance_of wikipedia           geometry
#>  * <chr>      <chr> <dbl>  <dbl> <chr>       <chr>            <POINT [m]>
#>  1 Q7121864   Paca…    44     NA fire stati… https://… (-8528431 4763412)
#>  2 Q7891082   Unit…    68     NA courthouse  https://… (-8528370 4763418)
#>  3 Q4852806   Balt…    76     NA building    https://… (-8528520 4763398)
#>  4 Q7092512   One …    78     NA tower block https://… (-8528431 4763254)
#>  5 Q136488241 Equi…    82     NA building    https://… (-8528511 4763425)
#>  6 Q5440386   Fede…   110     NA Federal Re… https://… (-8528531 4763455)
#>  7 Q4716584   Alex…   114     NA bank build… https://… (-8528464 4763211)
#>  8 Q114450330 Calv…   115     NA office bui… NA        (-8528569 4763299)
#>  9 Q7958765   Well…   128     NA tower bloc… https://… (-8528587 4763297)
#> 10 Q14692212  Merc…   136     NA building    https://… (-8528464 4763182)

# Set subclasses = FALSE to only include items that are directly an instance of
# the class
get_wikidata(
  location = bmore_city_hall,
  dist = 1,
  unit = "km",
  class = "Q41176",
  subclasses = FALSE,
  limit = 10
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528988 ymin: 4762851 xmax: -8527975 ymax: 4763772
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 10 × 7
#>    qid        label  dist height instance_of wikipedia           geometry
#>  * <chr>      <chr> <dbl>  <dbl> <chr>       <chr>            <POINT [m]>
#>  1 Q4852806   Balt…    76     NA building    https://… (-8528520 4763398)
#>  2 Q136488241 Equi…    82     NA building    https://… (-8528511 4763425)
#>  3 Q14692212  Merc…   136     NA building    https://… (-8528464 4763182)
#>  4 Q5033896   Cant…   206     NA building    https://… (-8528275 4763139)
#>  5 Q14692141  M&T …   295     NA building    https://… (-8528665 4763053)
#>  6 Q8030650   Woma…   418     NA building    https://… (-8528776 4763772)
#>  7 Q4975920   Brow…   435     NA building    https://… (-8528810 4763772)
#>  8 Q4856028   Bank…   453     NA building    https://… (-8528732 4762851)
#>  9 Q4986554   Buil…   478     NA building    https://… (-8527975 4763772)
#> 10 Q7077347   Odd …   525     NA building    https://… (-8528988 4763743)

# Use a polygon to set the search area automatically
bmore_area <- sf::st_buffer(
  sf::st_transform(bmore_city_hall, 26985),
  dist = 300
)

get_wikidata(
  location = bmore_area,
  limit = 10
)
#> New names:
#> • `` -> `...1`
#> Simple feature collection with 10 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -8528531 ymin: 4763254 xmax: -8528300 ymax: 4763455
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 10 × 7
#>    qid        label  dist height instance_of wikipedia           geometry
#>  * <chr>      <chr> <dbl>  <dbl> <chr>       <chr>            <POINT [m]>
#>  1 Q112730228 orga…     2     NA pipe organ  NA        (-8528430 4763352)
#>  2 Q14691875  Batt…    23     NA monument    https://… (-8528461 4763352)
#>  3 Q7121864   Paca…    44     NA fire stati… https://… (-8528431 4763412)
#>  4 Q7891082   Unit…    68     NA courthouse  https://… (-8528370 4763418)
#>  5 Q4852806   Balt…    76     NA building    https://… (-8528520 4763398)
#>  6 Q7092512   One …    78     NA tower block https://… (-8528431 4763254)
#>  7 Q136488241 Equi…    82     NA building    https://… (-8528511 4763425)
#>  8 Q112697932 Tenc…    84     NA K–8 school… NA        (-8528522 4763296)
#>  9 Q5440386   Fede…   110     NA Federal Re… https://… (-8528531 4763455)
#> 10 Q19838194  2015…   113     NA protest     https://… (-8528300 4763421)

# Return a data frame instead of an sf object and use another language
get_wikidata(
  location = bmore_city_hall,
  dist = 500,
  lang = "es",
  geometry = FALSE,
  limit = 10
)
#> New names:
#> • `` -> `...1`
#> # A tibble: 10 × 8
#>    qid        label                dist height instance_of wikipedia   lon   lat
#>    <chr>      <chr>               <dbl>  <dbl> <chr>       <chr>     <dbl> <dbl>
#>  1 Q112730228 organ of the Churc…     2     NA órgano de … NA        -76.6  39.3
#>  2 Q14691875  Battle Monument        23     NA monumento   NA        -76.6  39.3
#>  3 Q7121864   Paca Street Fireho…    44     NA estación d… NA        -76.6  39.3
#>  4 Q7891082   United States Post…    68     NA palacio de… NA        -76.6  39.3
#>  5 Q4852806   Baltimore City Cir…    76     NA edificio    NA        -76.6  39.3
#>  6 Q7092512   One Calvert Plaza      78     NA torre       https://… -76.6  39.3
#>  7 Q136488241 Equitable Building     82     NA edificio    NA        -76.6  39.3
#>  8 Q112697932 Tench Tilghman Ele…    84     NA Escuela K–… NA        -76.6  39.3
#>  9 Q5440386   Federal Reserve Ba…   110     NA Federal Re… NA        -76.6  39.3
#> 10 Q19838194  Disturbios de Balt…   113     NA protesta    https://… -76.6  39.3
```
