# getdata (development version)

- The minimum supported R version is now 4.1.0.
- `{arcgislayers}` is now in Suggests and `{esri2sf}` has moved from Imports to Suggests.
- Removed the re-exports for `{esri2sf}` functions.
- `get_esri_data()` now uses `{arcgislayers}` instead of `{esri2sf}` and has a new `call` argument.
- `get_wiki_data()` no longer errors when `details` has more than one value.
- `get_wiki_data()` now returns the API response when `list = "resp"`. Previously, the invalid value was passed on to the Wikipedia API.
- `get_wiki_data()` now uses the `getdata.crs` option (not `getdata.unit`) for the default `crs`.
- `get_wiki_data()` no longer fails when `radius = TRUE` is combined with `diag_ratio`. The radius is now based on the geodesic diagonal of the location bounding box.
- New `get_wikidata()` function gets Wikidata items with coordinates within a radius of a location using a SPARQL query to the Wikidata Query Service. Results can be filtered by class and include the item label, "instance of" labels, height (if available), and the English Wikipedia article URL.
- `make_location_data_list()` no longer uses `dplyr::case_when()` to recycle `location` and `data` and now reports the correct lengths when they do not match.
- `set_access_token()` no longer sets the environment variable using the literal name `default` instead of the requested name.

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
