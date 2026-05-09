# Use googlesheets4 to get a data frame or simple feature data from a Google Sheet

Use googlesheets4 to get a data frame or simple feature data from a
Google Sheet

## Usage

``` r
get_gsheet_data(
  url,
  sheet = NULL,
  ss = NULL,
  ask = FALSE,
  geometry = FALSE,
  location = NULL,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  asp = getOption("getdata.asp"),
  coords = getOption("getdata.coords", c("lon", "lat")),
  remove_coords = TRUE,
  address = getOption("getdata.address", "address"),
  geo = FALSE,
  from_crs = 4326,
  clean_names = TRUE,
  ...
)
```

## Arguments

- url:

  A Google Sheets url

- sheet:

  Sheet to read, in the sense of "worksheet" or "tab". You can identify
  a sheet by name, with a string, or by position, with a number. Ignored
  if the sheet is specified via `range`. If neither argument specifies
  the sheet, defaults to the first visible sheet.

- ss:

  Something that identifies a Google Sheet:

  - its file id as a string or
    [`drive_id`](https://googledrive.tidyverse.org/reference/drive_id.html)

  - a URL from which we can recover the id

  - a one-row
    [`dribble`](https://googledrive.tidyverse.org/reference/dribble.html),
    which is how googledrive represents Drive files

  - an instance of `googlesheets4_spreadsheet`, which is what
    [`gs4_get()`](https://googlesheets4.tidyverse.org/reference/gs4_get.html)
    returns

  Processed through
  [`as_sheets_id()`](https://googlesheets4.tidyverse.org/reference/sheets_id.html).

- ask:

  If `TRUE`, ask for the name of the Google Sheet to read if ss is not
  provided to
  [sfext::read_sf_gsheet](https://elipousson.github.io/sfext/reference/read_sf_ext.html).

- geometry:

  Type of geometry to include in data frame. options include "drop",
  "wkt", "centroid", "point", Default: 'centroid'.

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

- coords:

  Coordinate columns for input data.frame or output sf object (if
  geometry is 'centroid' or 'point') Default: c("lon", "lat").

- remove_coords:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html),
  if `TRUE`, remove the coordinate columns after converting a data frame
  to simple feature object; defaults to `FALSE`.

- address:

  Address column name passed to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
  or
  [tidygeocoder::geo](https://jessecambon.github.io/tidygeocoder/reference/geo.html)

- geo:

  If `TRUE`, use
  [`address_to_sf()`](https://elipousson.github.io/sfext/reference/address_to_sf.html)
  to geocode address column; defaults to `FALSE`.

- from_crs:

  For
  [`df_to_sf()`](https://elipousson.github.io/sfext/reference/sf_to_df.html),
  coordinate reference system used by coordinates or well known text in
  data frame.

- clean_names:

  If `TRUE`, clean names provided to nm or created based on value of col
  using
  [janitor::clean_names](https://sfirke.github.io/janitor/reference/clean_names.html).
  If `FALSE`, use names as provided.

- ...:

  Other parameters passed onto methods.
