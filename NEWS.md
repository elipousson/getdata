<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# getdata 0.1.0.9001 (2023-03-29)

- fix: correct outstanding issue w/ make_xwalk_list
- feat: export str_trim_squish_across
- fix: correct issue w/ passing range to as_sf
- refactor: import is_all_null
- fix: correct get_osm_id handling of vector ids
- test: add more tests for get_osm_data
- refactor: drop imports from purrr
- refactor: swap is_pkg_installed for renamed check_dev_installed
- refactor: remove purrr from Imports + move withr + lifecycle to Suggests + remove koki25ando/FlickrAPI from Remotes
- fix: correct handling of admin_level by get_osm_boundaries
- refactor: drop dependence on development version of osmdata (following new CRAN release)
- refactor: fix imports for rlang
- refactor: switch to development versions of FlickrAPI + osmdata
- feat: add details parameter to make_variable_dictionary
- refactor: improve fix_epoch_date handling of missing/unusable data
- refactor: minor improvement to has_same_name_col
- refactor: remove rlang::run_on_load() call
- fix: correct tidyselection
- test(format_data): add test for fix_epoch_date


# getdata

* Add new `bind_location_text_col()` and `between_date_range()` functions.
* Export `str_trim_squish_across()` function.
* Add `lifecycle`,  `vctrs`, `tidyselect`, and `withr` to Imports (and remove `purrr`).

# getdata 0.1.0.9000 (2022-12-23)

* First development version! getdata works well and code coverage is above 60% but additional refactoring is expected to increase consistency of syntax across functions and overall performance.
