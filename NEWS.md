# getdata (development version)

- Export `make_xwalk_list()` helper function
- Reexport functions from `{esri2sf}` and `{sfext}` packages
- Add new `cache_location_data()` function

# getdata 0.1.0.9002 (2023-05-02)

- Improve error if `get_location_data()` returns empty data and pass to `sfext::st_make_valid_ext()` before returning data.
- Implement location filtering for `get_tigris_data()` using tigris (> 2.0) filter_by parameter.
- Add `check_date_range()` function.
- Switch `get_airtable_data()` to rairtable package (forked development version).

# getdata 0.1.0.9001 (2023-03-29)

* Add new `bind_location_text_col()` and `between_date_range()` functions.
* Export `str_trim_squish_across()` function.
* Add `lifecycle`,  `vctrs`, `tidyselect`, and `withr` to Imports (and remove `purrr`).
* Update `get_osm_id()` to handle vector id values.

# getdata 0.1.0.9000 (2022-12-23)

* First development version! getdata works well and code coverage is above 60% but additional refactoring is expected to increase consistency of syntax across functions and overall performance.
