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

## Viewshed and landmark labelling (planning)

Goal: work out which landmarks are visible from a window (8th floor, 417 E
Fayette St, Baltimore), then label them on a photo or generated illustration.
Background, data checks and a candidate pipeline are in
`vignettes/viewshed.Rmd`. Scripts used for the checks are in `dev/`. Status as
of 2026-11-05: `get_wikidata()` and `get_wiki_data()` are done and committed;
the open decision is how to build the surface model (DSM) used for occlusion.

What we know (details in the vignette):

- OSM heights/levels cover only 3.1% of buildings within 3 km (7.3% in the 1 km
  core), mostly the tall towers. Overture adds almost nothing beyond OSM.
  Wikidata heights are sparse and sometimes wrong.
- Two 3DEP lidar projects cover downtown. `USGS_LPC_MD_PA_SandySupp_2014`
  (acquired 2014-12-15/16, leaf-off, ~8 points/m2) captures the City Hall dome
  (~70 m above ground).
- `terrainr::get_tiles()` returned a keyless 1 m bare-earth DEM for a 500 m
  window in ~4 s (11 m at City Hall; lidar ground ~10.8 m).
- `{elevatr}` 0.99.1 sources are `aws`, `gl3`, `gl1`, `alos` and `srtm15plus`
  (no direct 3DEP source).

### Option 1: lidar DSM for the whole area (needs discussion)

Build a ground surface and a surface model from the 2014 3DEP point cloud.

- *Steps:*
  1. Ground: `terrainr::get_tiles(services = "elevation", resolution = 1)`
     (see `dev/terrainr_dem_test.R`), reproject to EPSG:26985.
  2. Surface: pull EPT tiles for the study radius (`dev/ept_pull.py`),
     reproject from EPSG:3857, rasterize the highest return per 1 m cell after
     removing outliers (not by class code), and fill voids.
  3. Patch buildings built or demolished since 2014 using OSM `height` and
     `building:levels` (see the footprint-comparison note below).
- *Pros:* captures domes, spires, bridges (such as the elevated highway in the northeast photo) and
  trees; real geometry rather than modeled heights; one keyless public source.
- *Cons:*
  - 2014 vintage: newer buildings are missing and demolished ones remain.
  - Leaf-off (December) versus summer photos, so trees look thinner.
  - Download size: the 500 m test window was 22 MB and ~59 s serial. A rough
    linear extrapolation to a 3 km radius (~28 km2) is ~2.5 GB and ~2 hours
    serial (estimate, not tested; tile downloads can run in parallel).
  - Class codes are not reliable (buildings mostly class 17, class 18 includes
    ground-level points), so noise must be removed another way.
  - EPSG:3857 stretches horizontal distance by ~1.29 here; must reproject.
  - Currently depends on Python (`laspy`) outside the package.
- *Docs:*
  - USGS 3DEP lidar on AWS: <https://registry.opendata.aws/usgs-lidar/>
  - Project index: <https://usgs.entwine.io/boundaries/resources.geojson>
  - Index tooling: <https://github.com/hobuinc/usgs-lidar>
  - EPT specification:
    <https://entwine.io/en/latest/entwine-point-tile.html>
  - laspy: <https://laspy.readthedocs.io/>
  - terrainr: <https://docs.ropensci.org/terrainr/>
- *Open questions:* study radius needed for the far landmarks; whether
  `MD_Baltimore_2008` is worth using for comparison; how well a leaf-off DSM
  matches the summer photos.

### Option 2: replace the Python step with R tooling (needs discussion)

Keep the lidar approach but process points with `{lidR}` or `{lasR}` (or PDAL)
so the workflow is R-only.

- *Pros:*
  - `{lasR}` pipelines can produce DSM and DTM from a folder of point clouds
    (README example), including outlier removal, and read local or remote LAS,
    LAZ and COPC files.
  - `{lidR}` is on CRAN (4.3.3) with a book-length manual.
  - No Python or `uv` dependency for users.
- *Cons:*
  - Neither package lists EPT as a supported format (checked READMEs and
    `{lidR}` NEWS only; not exhaustive). We would still fetch the EPT node
    LAZ tiles ourselves (as `dev/ept_pull.py` does) or find LAZ/COPC sources.
    The public `usgs-lidar-public` bucket is EPT; the raw LAZ bucket is
    requester-pays and needs AWS credentials.
  - `{lasR}` is not on CRAN (r-universe, version 0.21.4 on 2026-11-05), which
    complicates Suggests/Remotes.
  - PDAL (`readers.ept`) would read EPT directly but is a system dependency
    (`brew install pdal`; not installed here).
  - Not tested at all yet.
- *Docs:*
  - lasR: <https://r-lidar.github.io/lasR/> and
    <https://github.com/r-lidar/lasR>
  - lidR: <https://CRAN.R-project.org/package=lidR> and
    <https://r-lidar.github.io/lidRbook/>
  - PDAL EPT reader: <https://pdal.io/en/stable/stages/readers.ept.html>
- *Open questions:* can EPT node tiles be merged and fed to `{lasR}` or
  `{lidR}` directly; are COPC versions of these projects available.

### Option 3: try gloBFPr and compare against the lidar DSM (needs discussion)

Install `{gloBFPr}` into a temporary library and compare its building heights
and fused DSM with the lidar surface and OSM-tagged heights for known buildings
(City Hall, 100 Light Street, the tall towers).

- *Pros:*
  - One call (`search_3dglobdf()`) returns footprints and heights for every
    building in a bounding box, so coverage is complete, with no large download.
  - Includes a canopy height model (`"metachm"` or `"ethCHM"`) through
    `get_fused_dsm()`; `get_3d_world()` can build 3D scenes.
  - Actively maintained (pushed 2026-09-04); on CRAN as 2.0.0 on 2026-11-05
    although the README still says "soon".
- *Cons:*
  - Heights are modeled (GlobalBuildingAtlas RMSE 1.5 to 8.9 m across
    continents; North America not broken out in what was read).
  - `get_fused_dsm()` gives each building one flat roof height, so the City
    Hall dome and tower tops would be lost.
  - The DEM comes from OpenTopography and needs an API key; 3DEP 1 m access is
    restricted to academic accounts. `get_fused_dsm()` has no `dem` argument
    (`get_3d_world()` does), so a `{terrainr}` DEM could not be used there.
  - Licenses are mixed: GlobalBuildingAtlas polygons are ODbL but heights and
    LoD1 models are CC BY-NC 4.0, which may rule out some uses.
  - Dataset vintage versus 2026 is unknown; experimental lifecycle.
- *Docs:*
  - <https://github.com/billbillbilly/gloBFPr>
  - 3D-GloBFP data: <https://zenodo.org/records/15487037>
  - GlobalBuildingAtlas paper: <https://essd.copernicus.org/articles/17/6647/2025/>
  - GlobalBuildingAtlas LoD1 Parquet:
    <https://source.coop/walkthru-earth/indices/building>
  - OpenTopography API key:
    <https://portal.opentopography.org/requestService?service=api>

### Comparison (for discussion)

| | 1. Lidar DSM | 2. R tooling for lidar | 3. gloBFPr |
|---|---|---|---|
| Roof detail (domes, spires) | Yes | Yes | No (flat roofs) |
| Post-2014 buildings | No (patch from OSM) | No (patch from OSM) | Depends on dataset vintage |
| Trees | Yes, leaf-off | Yes, leaf-off | Canopy model (metaCHM) |
| Height source | Measured | Measured | Modeled |
| Effort and size | Large download | Large download | Small |
| External dependency | Python | R only (`{lasR}` off CRAN) | R; OpenTopography key |
| Tested here | Yes (500 m window) | No | No |

A possible hybrid, to discuss: lidar DSM as the base, OSM heights to patch newer
buildings, and gloBFPr as a cross-check and gap filler.

### Related footprint packages (for completeness)

- [`{geomorphR}`](https://github.com/MakiJota/geomorphR) (CRAN 0.2.1, updated
  2026-10): `extract_geometric_features()` for per-footprint shape metrics and
  `compare_footprints()`, which compares two periods by intersection-over-union
  and returns new construction and demolitions. Could flag buildings added or
  removed since the lidar year so only those need an OSM height patch. Needs a
  footprint layer for the earlier period (e.g. derived from the lidar surface or
  the old City dataset). It does not estimate heights.
- [`{foot}`](https://wpgp.github.io/foot/) (WorldPop; GPL-3; not on CRAN; last
  updated 2022-06): building morphology metrics (area, shape, orientation,
  nearest-neighbor distance, clustering) for 2D footprints, plus tiled
  reading and writing. No heights, so low relevance here.

## Fixed

- [x] `get_wiki_data()` error with multi-value `details` (2026-11-05)
- [x] `get_wiki_data(list = "resp")` sent an invalid `list` to the API
  (2026-11-05)
- [x] `get_wiki_data()` undefined `x` and degrees-vs-meters radius with
  `diag_ratio` (2026-11-05)
- [x] `get_wiki_data()` default `crs` read `getdata.unit` (2026-11-05)
- [x] `req_wiki_query()` missing error message when no location parameter is
  supplied (2026-11-05)
