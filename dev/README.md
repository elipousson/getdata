# dev/

Throw-away development scripts for exploring the viewshed / landmark-labelling
idea (see `TODO.md` and `vignettes/viewshed.Rmd`). Not part of the package
(excluded in `.Rbuildignore`). Each script has a header with usage and the
results from when it was last run.

| Script | Purpose |
|---|---|
| `ept_pull.py` | Pull a small window of a USGS 3DEP point cloud from its EPT index without PDAL |
| `ept_analyze.py` | Crude look at the pulled points (class codes, nDSM); see its caveats |
| `terrainr_dem_test.R` | 1 m bare-earth DEM from the National Map with `{terrainr}` |
| `osm_building_height_coverage.R` | Height/levels coverage in a local OSM extract |
| `overture_building_height_coverage.sql` | Same check for Overture Maps via DuckDB |

The Python scripts expect an isolated environment at `dev/.venv` (ignored by
git); the setup commands are at the top of `ept_pull.py`.
