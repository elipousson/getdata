# getdata (development version)

- Add `{arcgislayers}` package to Suggests and move `{esri2sf}` package from Imports to Suggests (2024-10-28)
- Remove re-exports for `{esri2sf}` functions (2024-10-28)

# getdata 0.1.1 (2024-10-09)

- Export `make_xwalk_list()` helper function (2023-08-14)
- Reexport functions from `{esri2sf}` and `{sfext}` packages (2023-08-22)
- Add new `cache_location_data()` function (2023-08-22)
- Add support for named `name` parameter for `get_location_data()` (2023-09-07)
- Fix `get_flickr_photos()` when making multipage requests or using a `NULL` location (2023-11-02)
- Add new `get_elev_profile()` function using `{elevatr}` (2023-11-06)
- Fix issue w/ `erase_data()` when `erase_data` input has no rows (2023-11-20)

# getdata 0.1.0.9002 (2023-05-02)

- Improve error if `get_location_data()` returns empty data and pass to `sfext::st_make_valid_ext()` before returning data.
- Implement location filtering for `get_tigris_data()` using tigris (> 2.0) `filter_by` parameter.
- Add `check_date_range()` function.
- Switch `get_airtable_data()` to use development version of the `{rairtable}` package.

# getdata 0.1.0.9001 (2023-03-29)

* Add new `bind_location_text_col()` and `between_date_range()` functions.
* Export `str_trim_squish_across()` function.
* Add `{lifecycle}`,  `{vctrs}`, `{tidyselect}`, and `{withr}` to Imports (and remove `{purrr}`).
* Update `get_osm_id()` to handle vector id values.

# getdata 0.1.0.9000 (2022-12-23)

* First development version! `{getdata}` works well and code coverage is above 60% but additional refactoring is expected to increase consistency of syntax across functions and overall performance.
