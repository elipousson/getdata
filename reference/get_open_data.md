# Get data from an open data portal (Socrata) for a location

get_socrata_data is get_open_data with source_type set to "socrata" (the
only currently supported option). get_open_data can return a selected
dataset using Socrata Query Language (SoQL) parameters as a tibble or sf
object. Details on SoQL queries are found in the Socrata API
documentation <https://dev.socrata.com/docs/queries/>.

## Usage

``` r
get_open_data(
  data = NULL,
  source_url = NULL,
  source_type = "socrata",
  select = NULL,
  where = NULL,
  query = NULL,
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  name_col = NULL,
  name = NULL,
  location_col = NULL,
  coords = c("longitude", "latitude"),
  geometry = FALSE,
  token = NULL,
  type = NULL,
  from_crs = 4326,
  crs = NULL,
  clean_names = TRUE,
  quiet = FALSE,
  .name_repair = janitor::make_clean_names
)

get_socrata_data(
  data = NULL,
  source_url = NULL,
  select = NULL,
  where = NULL,
  query = NULL,
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  name_col = NULL,
  name = NULL,
  location_col = NULL,
  coords = c("longitude", "latitude"),
  geometry = FALSE,
  token = NULL,
  type = NULL,
  from_crs = 4326,
  crs = NULL,
  clean_names = TRUE
)

get_socrata_metadata(source_url = NULL, data = NULL)

list_socrata_data(source_url)
```

## Arguments

- data:

  A data set identifier (known as a resource for Socrata) or a url for
  an individual dataset. If data is set to "list" and a valid source_url
  is provided, the function returns a list of all available resources.
  If data is a url, source_url must be NULL. get_socrata_metadata
  requires the data parameter.

- source_url:

  A data source url. For Socrata, this should the base url for the open
  data portal.

- source_type:

  Data source type; defaults to "socrata" which is currently the only
  supported option.

- select:

  Names of of columns to return or transformed, equivalent to a SELECT
  in SQL. Passed to SODA \$select parameter, see
  <https://dev.socrata.com/docs/queries/select.html> for more
  information.

- where:

  Condition to filters the rows to return, equivalent to WHERE in SQL.
  Passed to the SODA \$where parameter, see
  <https://dev.socrata.com/docs/queries/where.html> for more
  information.

- query:

  A full SoQL query string, all as one parameter. Passed to the SODA
  \$query parameter, see
  <https://dev.socrata.com/docs/queries/query.html> for more
  information.

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

- name, name_col:

  Name of column in Socrata data resource with location names (e.g.
  County) and name of location to return.

- location_col:

  Name of a "location" or "point" type column in a Socrata dataset.

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- geometry:

  If `TRUE` and coords are provided, return a `sf` object. Default
  `FALSE`.

- token, type:

  Access token or API Key and token type (name used to store token in
  .Renvironment). A token may be required to access data from Socrata
  and other open data portals but can be stored as an environment
  variable with
  [set_access_token](https://elipousson.github.io/getdata/reference/set_access_token.md).

- from_crs:

  Coordinate reference system used to match the location CRS to the
  source data.

- crs:

  Coordinate reference system of bounding box to return; defaults to
  `NULL` which maintains the crs of the input object.

- clean_names:

  If `TRUE`, clean names provided to nm or created based on value of col
  using
  [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
  If `FALSE`, use names as provided.

- quiet:

  If `TRUE`, suppress messages when downloading data. Defaults to
  `FALSE`.

- .name_repair:

  One of "unique", "universal", or "check_unique". See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

## Examples

``` r
## Get Q2 2020 vehicle crash data for Cecil County, Maryland
if (FALSE) { # \dontrun{
  get_open_data(
    source_url = "https://opendata.maryland.gov",
    data = "65du-s3qu",
    where = "(year = '2020') AND (quarter = 'Q2')",
    name_col = "county_desc",
    name = "Cecil",
    token = Sys.getenv("MARYLAND_OPEN_DATA_API_KEY")
  )
} # }
```
