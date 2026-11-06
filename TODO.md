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

### get_wikidata()

- [ ] **Enrich `get_wikidata()` results with `{tidywikidatar}` (separate
  session).** Goal: for a shortlist of items (e.g. visible landmarks), return
  trustworthy heights and other properties with their units and qualifiers.
  - *Why:* `get_wikidata()` returns one height per item (the maximum of the
    normalized SI value from `p:P2048/psn:P2048/wikibase:quantityAmount`) and
    drops qualifiers. Wikidata heights are sparse (15 of 788 items within 3 km
    of 417 E Fayette St, Baltimore) and sometimes wrong or ambiguous. Baltimore
    City Hall (Q805741) has one P2048 value, `+51.27`, with qualifier P459
    (determination method) = Q791801; the SPARQL normalized value is 15.6 m, so
    the unit appears to be feet and the value does not describe the dome.
  - *Starting point:* `tw_get_qualified_property()` in
    <https://github.com/elipousson/weirdunits/blob/main/R/tidywikidatar.R>. It
    takes QIDs (or a data frame with a `qid` column) and a property ID, then
    returns a list with `properties` (value, unit via
    `tw_get_property_with_details()`, converted to a `units` object) and
    `qualifiers` (labelled with `tw_get_label()` and
    `tw_get_property_label()`).
  - *Known problems to solve first:*
    - `tw_get_property_with_details()` returned only `id` and `p` columns (no
      `amount` or `unit`) for Q805741, Q1383405 and Q3381384, while
      `tw_get_qualifiers()` worked. In the weirdunits helper this skips the unit
      join entirely, so `unit_value` is never created. Check whether this is a
      `{tidywikidatar}` 0.6.2 issue, a cache issue (`tw_disable_cache()` was on
      in testing), or a data issue, and consider reading the unit from SPARQL
      (`psv:P2048/wikibase:quantityUnit`) instead.
    - `tw_get_qualifiers()` takes one item and property pair at a time. For
      more than a handful of items this means many API calls, so enrich only a
      shortlist and enable the `{tidywikidatar}` cache (`tw_enable_cache()`).
    - Qualifiers worth keeping for height: P459 (determination method), P518
      (applies to part, e.g. roof or spire), P585 (point in time).
  - *Design options:* (a) add an internal or exported helper such as
    `get_wikidata_property()` that takes `qid` and a property and returns tidy
    values plus qualifiers, with `{tidywikidatar}` in Suggests and
    `check_installed()`; or (b) skip `{tidywikidatar}` and extend the SPARQL
    query with `OPTIONAL` qualifier patterns (`p:P2048/pq:P518`,
    `p:P2048/pq:P459`) and return all height statements as list-columns.
    Option (b) is one request for all items; option (a) is more general.
  - *Tests:* use a canned response for offline tests and a live test (skipped
    offline and on CI) on Q805741, Q1383405 (100 Light Street) and Q3381384
    (Phoenix Shot Tower).
- [ ] When an item has several heights, only the maximum is returned. Heights
  are sparse and unreliable (see above), so treat as label detail only, not DSM
  input.
- [ ] Items with multiple coordinates (P625) keep one arbitrary coordinate.
- [ ] No filtering of non-physical items with coordinates (e.g. events,
  artworks, museum objects); use `class` for now.
- [ ] Search area is always a circle (no wedge/polygon filtering). Clip results
  by azimuth in the caller.

### General

- [ ] `cli_abort_ifnot()` errors report `cli_abort_ifnot()` as the call instead
  of the user-facing function (visible in test snapshots).
- [ ] A `New names: ...1` message appears when calling `get_wiki_data()`.
  Probably from `{sfext}`, unconfirmed.
- [ ] `test-get_esri_data.R:59` fails (2026-11-05): the test URL on
  `services.nconemap.gov` has an expired SSL certificate. Skip on this error or
  switch to a different test service.
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
