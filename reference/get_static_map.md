# Use mapboxapi or bingmapr to get a static map image

Get a static map image using the [Mapbox Static Maps
API](https://www.mapbox.com/static-maps) using
[mapboxapi::static_mapbox](https://walker-data.com/mapboxapi/reference/static_mapbox.html)
or the [Bing Maps Static Map
API](https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map)
using
[bingmapr::get_map_image](https://elipousson.github.io/bingmapr/reference/bing_static_map.html).
An API key or access token is required for both services. Set the
bingmap API token using
[bingmapr::bing_maps_api_key](https://elipousson.github.io/bingmapr/reference/bing_maps_api_key.html)
and the Mapbox token with
[mapboxapi::mb_access_token](https://walker-data.com/mapboxapi/reference/mb_access_token.html)
or use `set_access_token` with `type = "BING_MAPS_API_KEY"` or
`type = "MAPBOX_PUBLIC_TOKEN"`.

## Usage

``` r
get_static_mapbox(
  location,
  dist = NULL,
  unit = "meter",
  style_url = "mapbox://styles/mapbox/light-v10",
  overlay_location = FALSE,
  overlay_sf = NULL,
  overlay_style = NULL,
  zoom = NULL,
  width = 600,
  height = 400,
  bearing = NULL,
  pitch = NULL,
  token = NULL,
  ...
)

get_osm_static_mapbox(
  id = NULL,
  key = NULL,
  level = NULL,
  location = NULL,
  dist = NULL,
  unit = "meter",
  overlay_location = TRUE,
  style_url = "mapbox://styles/mapbox/light-v10",
  overlay_sf = NULL,
  overlay_style = NULL,
  zoom = NULL,
  width = 600,
  height = 400,
  bearing = NULL,
  pitch = NULL,
  token = NULL,
  ...
)

get_location_static_mapbox(
  type,
  dist = NULL,
  unit = "meter",
  name = NULL,
  name_col = "name",
  id = NULL,
  id_col = "id",
  location = NULL,
  index = NULL,
  union = FALSE,
  overlay_location = TRUE,
  style_url = "mapbox://styles/mapbox/light-v10",
  overlay_sf = NULL,
  overlay_style = NULL,
  zoom = NULL,
  width = 600,
  height = 400,
  bearing = NULL,
  pitch = NULL,
  token = NULL,
  ...
)

get_static_bingmap(
  location = NULL,
  dist = NULL,
  unit = "m",
  imagery = "BirdsEye",
  zoom = NULL,
  width = 600,
  height = 400,
  bearing = NULL,
  token = NULL,
  ...
)
```

## Arguments

- location:

  An input location for which you would like to request tiles. Can be a
  length-4 vector representing a bounding box, or an `sf` object. If an
  input `sf` object is supplied, use the `buffer_dist` argument to
  control how much area you want to capture around the layer. While the
  input `sf` object can be in an arbitrary coordinate reference system,
  if a length-4 bounding box vector is supplied instead it must
  represent WGS84 longitude/latitude coordinates and be in the order
  `c(xmin, ymin, xmax, ymax)`.

- dist:

  Buffer distance passed to buffer_dist parameter of
  [`mapboxapi::static_mapbox()`](https://walker-data.com/mapboxapi/reference/static_mapbox.html)
  or to
  [`sfext::st_buffer_ext()`](https://elipousson.github.io/sfext/reference/st_buffer_ext.html)
  for `get_static_bingmap()`.

- unit:

  Unit of `dist` argument. Defaults to "meters".

- style_url:

  Style URL; defaults to "mapbox://styles/mapbox/light-v10"

- overlay_location:

  If `TRUE`, use the location (or OpenStreetMap feature) as the
  overlay_sf parameter. Default to `FALSE`. Ignored if overlay_sf is
  provided.

- overlay_sf:

  The overlay `sf` object (optional). The function will convert the `sf`
  object to GeoJSON then plot over the basemap style. Spatial data that
  are too large will trigger an error, and should be added to the style
  in Mapbox Studio instead.

- overlay_style:

  A named list of vectors specifying how to style the sf overlay.
  Possible names are "stroke", "stroke-width" (or "stroke_width"),
  "stroke-opacity" (or "stroke_opacity"), "fill", and "fill-opacity" (or
  "fill_opacity"). The fill and stroke color values can be specified as
  six-digit hex codes or color names, and the opacity and width values
  should be supplied as floating-point numbers. If overlay_style is
  `NULL`, the style values can be pulled from columns with the same
  names in `overlay_sf`.

- zoom:

  The map zoom. The map will infer this from the overlay unless
  longitude, latitude, and zoom are all specified.

- width, height:

  Map width and height; defaults to 600 px width and 400 px height.

- pitch, bearing:

  The map pitch and bearing; defaults to `NULL`. pitch can range from 0
  to 60, and bearing from -360 to 360.

- token:

  Optional token or API key. Recommend setting the Bing Maps API key
  using
  [`bingmapr::bing_maps_api_key()`](https://elipousson.github.io/bingmapr/reference/bing_maps_api_key.html)
  and the Mapbox access token with
  [`mapboxapi::mb_access_token()`](https://walker-data.com/mapboxapi/reference/mb_access_token.html).

- ...:

  Additional parameters passed to
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  if type is character and index is `NULL`.

- id:

  OpenStreetMap feature id with or without a type id prefix. If multiple
  id values are provided, they must use a single consistent value for
  geometry.

- key:

  Feature key for overpass API query.

- level:

  Numeric administrative level (admin_level) of boundary to return;
  defaults to `NULL`. If multiple levels are provided, the any admin
  levels between the min and max values of level is returned. See
  <https://wiki.openstreetmap.org/wiki/Key:admin_level> for more
  information. Only used for
  [`get_osm_boundaries()`](https://elipousson.github.io/getdata/reference/get_osm_data.md).

- type:

  For `get_osm_static_mapbox()`, type of feature with id; ("node",
  "way", or "relation"); for `get_location_static_mapbox()`, type of
  location (see
  [`get_location()`](https://elipousson.github.io/getdata/reference/get_location.md)
  for details.

- name:

  Location name to return.

- name_col:

  Column name in type with name values, Default: 'name' Required if name
  provided.

- id_col:

  Column name in type with id values, Default: 'id'. Required if id is
  provided.

- index:

  Optional list used to match type to data, Default: `NULL`

- union:

  If `TRUE`, the location geometry is unioned with
  [`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html)
  and the names are combined into a single value. Default: `FALSE`.

- imagery:

  String with imagery type, Default: 'BirdsEye' Supported values
  include:

  - Aerial: Aerial imagery.

  - AerialWithLabels: Aerial imagery with a road overlay.

  - AerialWithLabelsOnDemand: Aerial imagery with on-demand road
    overlay.

  - Streetside: Street-level imagery.

  - BirdsEye: Birds Eye (oblique-angle) imagery.

  - BirdsEyeWithLabels: Birds Eye (oblique-angle) imagery with a road
    overlay.

  - Road: Roads without additional imagery.

  - CanvasDark: A dark version of the road maps.

  - CanvasLight: A lighter version of the road maps which also has some
    of the details such as hill shading disabled.

  - CanvasGray: A grayscale version of the road maps

## Details

Variations on get_static_mapbox include

- get_location_static_mapbox wrapping
  [`get_location()`](https://elipousson.github.io/getdata/reference/get_location.md)

- get_osm_static_mapbox wrapping
  [`get_osm_data()`](https://elipousson.github.io/getdata/reference/get_osm_data.md),
  [`get_osm_id()`](https://elipousson.github.io/getdata/reference/get_osm_data.md),
  and
  [`get_osm_boundaries()`](https://elipousson.github.io/getdata/reference/get_osm_data.md)

In those cases, the ... parameters are passed on the getdata functions
rather than the static map function.

For `get_static_bingmap()`, parameter names are modified from
[`bingmapr::get_map_image()`](https://elipousson.github.io/bingmapr/reference/bing_static_map.html)
for consistency, so the bearing parameter passed to orientation and
token is passed to key.

## Examples

``` r
if (FALSE) { # \dontrun{
  get_osm_static_mapbox(
    id = "way/49664223",
    dist = 0.5,
    unit = "mi",
    overlay_style = list(
      stroke = "darkgreen",
      fill = "green",
      fill_opacity = 0.25
    )
  )

  nc <- sfext::read_sf_path(system.file("shape/nc.shp", package = "sf"))

  get_location_static_mapbox(
    type = nc,
    name = "Ashe",
    name_col = "NAME",
    dist = 50,
    unit = "mi"
  )
} # }
```
