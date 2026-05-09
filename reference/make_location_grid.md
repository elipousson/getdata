# Make a grid over the bounding box of a location

If location is a single feature sf object, the original columns of the
object are included in the output grid. If location has mutiple
features, the values of name_col are combined with
[sfext::st_union_ext](https://elipousson.github.io/sfext/reference/st_union_ext.html)
and other columns are dropped. The input sf object should not have
columns named id, rows, or cols.

## Usage

``` r
make_location_grid(location, name_col = "name", unit = NULL, ...)
```

## Arguments

- location:

  A sf, sfc, or bbox object passed to
  [sfext::st_make_grid_ext](https://elipousson.github.io/sfext/reference/st_make_grid_ext.html)

- name_col:

  Column name to collapse into new name_col value, Default: 'name'

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- ...:

  Arguments passed on to
  [`sfext::st_make_grid_ext`](https://elipousson.github.io/sfext/reference/st_make_grid_ext.html)

  `x`

  :   A `sf`, `sfc`, or `bbox` object, Default: `NULL`. Required.

  `crs`

  :   Coordinate reference system of bounding box to return; defaults to
      `NULL` which maintains the crs of the input object.

  `ncol,nrow`

  :   Used to set n if either are not `NULL`; defaults to `NULL`. row
      and id are added as columns to the grid if they are provided.

  `n`

  :   If n is NULL and square is `TRUE`, the grid is set automatically
      to be 10 cells wide, Default: `NULL`

  `gutter`

  :   Distance in units between each column cell; gutter effectively
      serves as a margin as the negative buffer is applied to all cells
      (including those at the edges of the grid).

  `desc`

  :   If TRUE, reverse standard order of cell id numbering; defaults
      `FALSE`

  `cellsize`

  :   numeric of length 1 or 2 with target cellsize: for square or
      rectangular cells the width and height, for hexagonal cells the
      distance between opposite edges (edge length is cellsize/sqrt(3)).
      A length units object can be passed, or an area unit object with
      area size of the square or hexagonal cell.

  `what`

  :   "polygons", "corners", "centers"; set to centers automatically if
      style is "circle", "circle_offset" but a buffer is applied to
      return circular polygons.

  `style`

  :   Style of cell to return with options including "rect", "square",
      "hex", "flat_top_hex", "circle", "circle_offset"

  `.id`

  :   A name to use for the cell id column. Defaults to "id".

  `filter`

  :   If `TRUE` (or if trim is `TRUE`) filter grid geometry by x using
      [st_filter_ext](https://elipousson.github.io/sfext/reference/st_filter_ext.html)

  `trim`

  :   If `TRUE`, x is trimmed to y with
      [`st_trim()`](https://elipousson.github.io/sfext/reference/st_erase.html).
