# Changelog

## getdata (development version)

- Add [arcgislayers](https://developers.arcgis.com/r-bridge) package to
  Suggests and move [esri2sf](https://github.com/elipousson/esri2sf/)
  package from Imports to Suggests (2024-10-28)
- Remove re-exports for
  [esri2sf](https://github.com/elipousson/esri2sf/) functions
  (2024-10-28)

## getdata 0.1.1 (2024-10-09)

- Export
  [`make_xwalk_list()`](https://elipousson.github.io/getdata/reference/make_xwalk_list.md)
  helper function (2023-08-14)
- Reexport functions from
  [esri2sf](https://github.com/elipousson/esri2sf/) and
  [sfext](https://github.com/elipousson/sfext) packages (2023-08-22)
- Add new
  [`cache_location_data()`](https://elipousson.github.io/getdata/reference/cache_location_data.md)
  function (2023-08-22)
- Add support for named `name` parameter for
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  (2023-09-07)
- Fix
  [`get_flickr_photos()`](https://elipousson.github.io/getdata/reference/get_flickr_photos.md)
  when making multipage requests or using a `NULL` location (2023-11-02)
- Add new
  [`get_elev_profile()`](https://elipousson.github.io/getdata/reference/get_elev_profile.md)
  function using [elevatr](https://github.com/usepa/elevatr/)
  (2023-11-06)
- Fix issue w/
  [`erase_data()`](https://elipousson.github.io/getdata/reference/format_sf_data.md)
  when `erase_data` input has no rows (2023-11-20)

## getdata 0.1.0.9002 (2023-05-02)

- Improve error if
  [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  returns empty data and pass to
  [`sfext::st_make_valid_ext()`](https://elipousson.github.io/sfext/reference/st_make_valid_ext.html)
  before returning data.
- Implement location filtering for
  [`get_tigris_data()`](https://elipousson.github.io/getdata/reference/get_state_tigris.md)
  using tigris (\> 2.0) `filter_by` parameter.
- Add
  [`check_date_range()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  function.
- Switch
  [`get_airtable_data()`](https://elipousson.github.io/getdata/reference/get_airtable_data.md)
  to use development version of the
  [rairtable](https://github.com/matthewjrogers/rairtable) package.

## getdata 0.1.0.9001 (2023-03-29)

- Add new
  [`bind_location_text_col()`](https://elipousson.github.io/getdata/reference/format_address_data.md)
  and
  [`between_date_range()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  functions.
- Export
  [`str_trim_squish_across()`](https://elipousson.github.io/getdata/reference/str_trim_squish_across.md)
  function.
- Add [lifecycle](https://lifecycle.r-lib.org/),
  [vctrs](https://vctrs.r-lib.org/),
  [tidyselect](https://tidyselect.r-lib.org), and
  [withr](https://withr.r-lib.org) to Imports (and remove
  [purrr](https://purrr.tidyverse.org/)).
- Update
  [`get_osm_id()`](https://elipousson.github.io/getdata/reference/get_osm_data.md)
  to handle vector id values.

## getdata 0.1.0.9000 (2022-12-23)

- First development version!
  [getdata](https://github.com/elipousson/getdata) works well and code
  coverage is above 60% but additional refactoring is expected to
  increase consistency of syntax across functions and overall
  performance.
