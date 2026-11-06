# Pull a small window of a USGS 3DEP point cloud from its public EPT index
# without PDAL. Development script for the viewshed work (see TODO.md and
# vignettes/viewshed.Rmd); not part of the package.
#
# Setup (isolated environment, nothing installed system-wide):
#   uv venv dev/.venv
#   uv pip install --python dev/.venv/bin/python "laspy[lazrs]" numpy pyproj requests
#
# Usage:
#   dev/.venv/bin/python -I dev/ept_pull.py <project> <out_dir> <lon> <lat> <half_m>
#   dev/.venv/bin/python -I dev/ept_pull.py USGS_LPC_MD_PA_SandySupp_2014_LAS_2016 \
#     /tmp/ept_sandy2014 -76.6122 39.2904 250
#
# Project names come from https://usgs.entwine.io/boundaries/resources.geojson
# (MD_Baltimore_2008 and USGS_LPC_MD_PA_SandySupp_2014_LAS_2016 cover downtown
# Baltimore). Writes the LAZ tiles and window_points.npz (x, y, z, cls, rn, win)
# to <out_dir>. Coordinates are EPSG:3857: horizontal distances are stretched by
# 1/cos(lat) (about 1.29 at 39.3 N), so reproject before measuring. Run with
# python -I on downloaded data (it is untrusted input).
#
# Result on 2026-11-05 (500 m window around City Hall, 2014 project): 128 tiles,
# 22 MB, ~59 s, ~2 million points (~8 pts/m2), acquired 2014-12-15/16 (leaf-off).
import sys, json, time, os, datetime
import numpy as np, requests, laspy
from pyproj import Transformer

root = "https://s3-us-west-2.amazonaws.com/usgs-lidar-public/" + sys.argv[1]
out = sys.argv[2]
lon, lat, half = float(sys.argv[3]), float(sys.argv[4]), float(sys.argv[5])
os.makedirs(out, exist_ok=True)

ept = requests.get(root + "/ept.json", timeout=60).json()
rb = ept["bounds"]
t = Transformer.from_crs(4326, 3857, always_xy=True)
# window in 3857, sized in ground meters via the local scale factor
k = 1 / np.cos(np.radians(lat))
cx, cy = t.transform(lon, lat)
win = (cx - half * k, cy - half * k, cx + half * k, cy + half * k)

def node_xy(d, x, y):
    size = (rb[3] - rb[0]) / 2**d
    return (rb[0] + x * size, rb[1] + y * size, rb[0] + (x + 1) * size, rb[1] + (y + 1) * size)

def hits(d, x, y):
    b = node_xy(d, x, y)
    return not (b[2] < win[0] or b[0] > win[2] or b[3] < win[1] or b[1] > win[3])

t0 = time.time()
todo = {}
pending = ["0-0-0-0"]
seen_h = set()
while pending:
    hk = pending.pop()
    if hk in seen_h: continue
    seen_h.add(hk)
    h = requests.get(f"{root}/ept-hierarchy/{hk}.json", timeout=60).json()
    for key, n in h.items():
        d, x, y, z = map(int, key.split("-"))
        if not hits(d, x, y): continue
        if n == -1: pending.append(key)
        elif n > 0: todo[key] = n
print(f"nodes intersecting window: {len(todo)}, points in those nodes: {sum(todo.values()):,}, hierarchy time {time.time()-t0:.1f}s")

xs, ys, zs, cls, rn, gt = [], [], [], [], [], []
nbytes = 0
t1 = time.time()
for key in sorted(todo):
    p = os.path.join(out, key + ".laz")
    if not os.path.exists(p):
        r = requests.get(f"{root}/ept-data/{key}.laz", timeout=120)
        r.raise_for_status()
        open(p, "wb").write(r.content)
    nbytes += os.path.getsize(p)
    las = laspy.read(p)
    m = (las.x >= win[0]) & (las.x <= win[2]) & (las.y >= win[1]) & (las.y <= win[3])
    xs.append(np.asarray(las.x[m])); ys.append(np.asarray(las.y[m])); zs.append(np.asarray(las.z[m]))
    cls.append(np.asarray(las.classification[m])); rn.append(np.asarray(las.return_number[m])); gt.append(np.asarray(las.gps_time[m]))
x, y, z = map(np.concatenate, (xs, ys, zs)); cls = np.concatenate(cls); rn = np.concatenate(rn); gt = np.concatenate(gt)
print(f"downloaded {nbytes/1e6:.1f} MB in {time.time()-t1:.1f}s; points inside window: {len(x):,}")
area = (2 * half) ** 2
print(f"mean density: {len(x)/area:.1f} pts/m2 (all returns); first returns: {(rn==1).sum()/area:.1f} pts/m2")
print("classification counts:", dict(zip(*[a.tolist() for a in np.unique(cls, return_counts=True)])))
print(f"z range: {z.min():.1f} .. {z.max():.1f} m (vertical datum per ept srs)")
# acquisition dates
if gt.max() < 1e7:
    print("gps time looks like week seconds; cannot convert to a date")
else:
    ep = datetime.datetime(1980, 1, 6)
    off = 1e9 if gt.max() < 2e9 else 0
    d = lambda v: (ep + datetime.timedelta(seconds=float(v) + off)).date()
    print("acquisition dates (min/median/max):", d(gt.min()), d(np.median(gt)), d(gt.max()))
np.savez_compressed(os.path.join(out, "window_points.npz"), x=x, y=y, z=z, cls=cls, rn=rn, win=np.array(win))
