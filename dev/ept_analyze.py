# Quick look at points saved by ept_pull.py: class codes, a crude nDSM, and the
# height of a few known features. Development script; not part of the package.
#
# Usage: dev/.venv/bin/python -I dev/ept_analyze.py <out_dir>/window_points.npz
#
# CAVEATS (2026-11-05):
# * The ground surface is crude: class 2 minimum per cell, gaps filled with one
#   window-wide median. The "tallest peaks" it prints (two identical 183.8 m
#   cells) are probably artifacts. Replace with a real DTM (see
#   dev/terrainr_dem_test.R) before trusting any nDSM numbers.
# * Class codes in the 2014 project are not standard: building points are mostly
#   class 17 (bridge deck in ASPRS) and class 18 (high noise) includes
#   ground-level points. Do not rely on class codes.
# * Coordinates and the observer/City Hall locations are hard-coded.
import sys, numpy as np
from pyproj import Transformer
d = np.load(sys.argv[1]); x, y, z, c = d["x"], d["y"], d["z"], d["cls"]
t = Transformer.from_crs(4326, 3857, always_xy=True); inv = Transformer.from_crs(3857, 4326, always_xy=True)
lat0 = 39.2904; k = 1/np.cos(np.radians(lat0))
def near(lon, lat, r):
    cx, cy = t.transform(lon, lat); return (x-cx)**2 + (y-cy)**2 <= (r*k)**2
print("== class x z percentiles within 25 m of City Hall center (-76.6122, 39.2904) ==")
m = near(-76.6122, 39.2904, 25)
for cl in np.unique(c[m]):
    zz = z[m & (c == cl)]; print(f"class {cl:2d} n={len(zz):6d}  z p5/p50/p95/max = {np.percentile(zz,5):6.1f} {np.percentile(zz,50):6.1f} {np.percentile(zz,95):6.1f} {zz.max():6.1f}")
g = z[(c == 2)]
print("\nground (class 2) z p5/p50/p95 in window:", np.percentile(g, [5, 50, 95]).round(1))
print("class 17 z p5/p50/p95 in window:", np.percentile(z[c == 17], [5, 50, 95]).round(1), " class 1:", np.percentile(z[c == 1], [5, 50, 95]).round(1))
# DSM grid, 2 ground-meter cells, excluding noise classes
ok = ~np.isin(c, [7, 18]); cs = 2 * k
ix = ((x[ok] - x[ok].min()) // cs).astype(int); iy = ((y[ok] - y[ok].min()) // cs).astype(int)
nx, ny = ix.max() + 1, iy.max() + 1
dsm = np.full((ny, nx), -np.inf); np.maximum.at(dsm, (iy, ix), z[ok])
gm = c[ok] == 2
dtm = np.full((ny, nx), np.inf); np.minimum.at(dtm, (iy[gm], ix[gm]), z[ok][gm])
# fill DTM gaps with the window ground median (simple)
dtm[~np.isfinite(dtm)] = np.median(g)
ndsm = dsm - dtm; ndsm[~np.isfinite(ndsm)] = 0
print(f"\nDSM grid {nx}x{ny} cells (2 m); nDSM p50/p95/p99/max: {np.percentile(ndsm[ndsm>0],[50,95,99]).round(1)} {ndsm.max():.1f}")
# top local maxima separated by >= 40 m
order = np.dstack(np.unravel_index(np.argsort(-ndsm.ravel()), ndsm.shape))[0]
peaks = []
for r, q in order:
    if ndsm[r, q] < 40: break
    if all((r-pr)**2 + (q-pq)**2 > 20**2 for pr, pq, _ in peaks): peaks.append((r, q, ndsm[r, q]))
    if len(peaks) >= 12: break
print("\ntallest nDSM peaks (>40 m above local ground):")
for r, q, h in peaks:
    px = x[ok].min() + (q + .5) * cs; py = y[ok].min() + (r + .5) * cs; lo, la = inv.transform(px, py)
    print(f"  {h:6.1f} m  at lon {lo:.5f}, lat {la:.5f}")
# observer building
for name, lon, lat in [("observer 417 E Fayette", -76.60943, 39.29031)]:
    m = near(lon, lat, 20); mm = m & ok
    gz = np.median(z[m & (c == 2)]) if (m & (c == 2)).any() else np.nan
    print(f"\n{name}: ground z ~{gz:.1f}, top of surface within 20 m: {z[mm].max():.1f} => {z[mm].max()-gz:.1f} m above ground; n={mm.sum()}")
