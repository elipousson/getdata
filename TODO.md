# TODO

Running list of bugs and loose ends found while working on getdata. Add new
entries under **Open** with the function, the symptom, and how it was found.
When an item is fixed, move it to **Fixed** and add a bullet to `NEWS.md`
(function name first, alphabetical, no date, ending with a period, and an issue
number if there is one).

## Open

### get_osm_data()

- [ ] `nodes_only = TRUE` is deprecated in osmdata; `osmdata::opq()` warns to
  use `osm_types = "node"` instead (`R/get_osm_data.R`).
- [ ] `key = "wikidata"` errors ("not listed as Key in Map Features") because
  keys are validated against the OSM wiki list. Passing it through `features =`
  is untested.
- [ ] A `sf` location is only used as a bounding box. Results are not clipped
  to the actual geometry (e.g. a wedge), so the caller must clip.
- [ ] Repeated HTTP 504 Gateway Timeout from Overpass on a ~3 km and a ~1.5 km
  query (2026-11-05). Cause unconfirmed (likely server load). No retry or
  alternate-endpoint option.
- [ ] `tests/testthat/test-get_osm_data.R` is skipped ("Disabling broken test
  temporarily"). Re-enable after the deprecations are fixed.

### get_wiki_data()

- [ ] With `radius = TRUE` and neither `dist` nor `diag_ratio`, the error comes
  from `convert_dist_units(NULL)` and is not helpful. Check inputs up front.
- [ ] The API's limits (radius of 10-10,000 m; bounding box larger than roughly
  25 km across is "too big") are not validated before the request, so users get
  the API's error message.
- [ ] Results from a wide area hit the 500 `limit` cap with no paging.

### General

- [ ] `cli_abort_ifnot()` errors report `cli_abort_ifnot()` as the call instead
  of the user-facing function (visible in test snapshots).
- [ ] A `New names: ...1` message appears when calling `get_wiki_data()`.
  Probably from `{sfext}`, unconfirmed.
- [ ] Other tests are disabled: `test-get_static_map.R` (flakey) and
  `test-get_flickr_photos.R` (flakey).

## Fixed

- [x] `get_wiki_data()` error with multi-value `details` (2026-11-05)
- [x] `get_wiki_data(list = "resp")` sent an invalid `list` to the API
  (2026-11-05)
- [x] `get_wiki_data()` undefined `x` and degrees-vs-meters radius with
  `diag_ratio` (2026-11-05)
- [x] `get_wiki_data()` default `crs` read `getdata.unit` (2026-11-05)
- [x] `req_wiki_query()` missing error message when no location parameter is
  supplied (2026-11-05)
