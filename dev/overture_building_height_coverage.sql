-- Same coverage check as dev/osm_building_height_coverage.R for Overture Maps
-- buildings, queried straight from S3 with DuckDB. Development script; not part
-- of the package.
--
-- Usage: duckdb < dev/overture_building_height_coverage.sql
-- Find the latest release at https://stac.overturemaps.org/catalog.json and
-- update the release string below. The bbox is ~3 km around 417 E Fayette St.
--
-- Result on 2026-11-05 (release 2026-10-21.0): 72,511 buildings, 2,662 with
-- height (3.7%), 2,699 with num_floors, 5,120 with either (7.1%). All rows come
-- from OpenStreetMap; 2,540 also carry a Microsoft ML height estimate (City Hall
-- is 19.5 m, too low). Adds little beyond OSM.
INSTALL httpfs; LOAD httpfs; INSTALL spatial; LOAD spatial;
SET s3_region='us-west-2';
CREATE TEMP TABLE ob AS
SELECT id, names.primary AS name, class, subtype, height, num_floors, min_height, roof_shape,
       list_transform(sources, lambda x: x.dataset) AS src, ST_Y(ST_Centroid(geometry)) lat, ST_X(ST_Centroid(geometry)) lon, ST_Area(geometry) area_deg
FROM read_parquet('s3://overturemaps-us-west-2/release/2026-10-21.0/theme=buildings/type=building/*', hive_partitioning=1)
WHERE bbox.xmin BETWEEN -76.6498 AND -76.5690 AND bbox.ymin BETWEEN 39.2633 AND 39.3173;
SELECT count(*) n, count(height) with_height, count(num_floors) with_floors, count(*) FILTER (height IS NOT NULL OR num_floors IS NOT NULL) either FROM ob;
SELECT unnest(src) AS dataset, count(*) n, count(height) h FROM ob GROUP BY 1 ORDER BY 2 DESC;
SELECT name, height, num_floors, class, src FROM ob WHERE height IS NOT NULL ORDER BY height DESC LIMIT 12;
SELECT name, height, num_floors, src FROM ob WHERE name ILIKE '%city hall%' OR name ILIKE '%100 light%' OR name ILIKE '%war memorial%' OR name ILIKE '%zion%' OR name ILIKE '%shot tower%';
