
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getdata <a href="https://elipousson.github.io/getdata/"><img src="man/figures/logo.png" align="right" height="118" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/elipousson/getdata/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elipousson/getdata?branch=main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of {getdata} is to make the experience of getting location data
easier and more consistent across a wide variety of sources. {getdata}
started as part of the
[{overedge}](https://elipousson.github.io/overedge/) package along with
[{maplayer}](https://elipousson.github.io/maplayer/) and
[{sfext}](https://elipousson.github.io/sfext/).

{getdata} is designed to work well with location-specific data packages
such as [{mapmaryland}](https://elipousson.github.io/mapmaryland/) and
[{mapbaltimore}](https://elipousson.github.io/mapbaltimore/) and to
support reproducible approaches to map-making and place-based data
analysis. Using data access functions from {sfext} and additional API
wrapper functions, this package supports data access for sources
including:

- ArcGIS FeatureServer and MapServer layers (using
  [{esri2sf}](https://github.com/yonghah/esri2sf))
- U.S. Census Bureau data (using
  [{tigris}](https://github.com/walkerke/tigris))
- OpenStreetMap (using [{osmdata}](https://docs.ropensci.org/osmdata/))
- Socrata Open Data resources (using
  [{RSocrata](https://github.com/Chicago/RSocrata)})
- Google Sheets (using
  [{googlesheets4}](https://googlesheets4.tidyverse.org/))
- Flickr photos (using
  [{FlickrAPI}](https://koki25ando.github.io/FlickrAPI/))
- Static map images from Mapbox (using
  [{mapboxapi}](https://walker-data.com/mapboxapi/))
- Airtable bases (using {httr2} and the [Airtable
  API](https://airtable.com/api))
- Wikipedia articles (using {httr2} and the [Wikipedia Geosearch
  API](https://www.mediawiki.org/wiki/Extension:GeoData))
- Other spatial data sources including Google MyMaps, GitHub gists, and
  any data source already supported by
  [sf::read_sf()](https://r-spatial.github.io/sf/reference/st_read.html)
  (see
  [sfext::read_sf_ext()](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
  for more details)

The advantage of using {getdata} is that it provides a consistent
interface for using a location to create a bounding box for spatial
filtering. Many functions also support querying spatial data by name or
id. Where possible, a spatial filter is used before importing or
downloading data to avoid the need to load large data files when you are
only need a small area. The package also provides a consistent approach
for handling API tokens and keys and for caching data locally (see
[set_access_token()](https://elipousson.github.io/getdata/reference/set_access_token.html)
or
[filenamr::get_data_dir()](https://elipousson.github.io/sfext/reference/get_data_dir.html)
for more details).

The related {sfext} package allows {getdata} to supports the easy
conversion of tabular data into spatial data. For example, if the source
data has coordinates, you can convert the data into an sf object. If
data has an address column, you can geocode the data using the
[{tidygeocoder}](https://jessecambon.github.io/tidygeocoder/) package.
If the data has a location name column, such as “neighborhood”, you can
join the data to a simple feature object with the related geometry. You
also can turn off these options by setting `geometry = FALSE` for most
data access functions.

Lastly, the
[format_data()](https://elipousson.github.io/getdata/reference/format_data.html)
and
[format_sf_data()](https://elipousson.github.io/getdata/reference/format_sf_data.html)
functions provide convenient options for working with the data after it
is downloaded. While advanced R users may prefer to create more custom
formatting scripts, these functions are designed to support the creation
of custom data formatting and access functions such as
[format_md_crash_data()](https://elipousson.github.io/mapmaryland/reference/format_md_sf.html)
and
[get_md_crash_data()](https://elipousson.github.io/mapmaryland/reference/get_md_open_data.html).

Fair warning: this package is *not* optimized for speed and I have no
plans to submit it to CRAN. This package imports {rlang} for both
non-standard evaluation and error handling and relies on {dplyr},
{purrr}, and other tidyverse packages. Suggestions for additional data
sources to support, new functions, or improvements to existing functions
are welcome.

## Installation

You can install the development version of getdata like so:

``` r
pak::pkg_install("elipousson/getdata")
```

## Basic usage

`get_location_data()` is a flexible function for reading and subsetting
data. In this example, data is a file path but it can also be a URL, the
name of a data set in another package, or a sf object.

``` r
library(getdata)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# location is optional
nc <- get_location_data(data = system.file("shape/nc.shp", package = "sf"))
```

You can use
[get_location()](https://elipousson.github.io/getdata/reference/) to get
a specific location from a larger simple feature collection that
includes a specific type of locations, such as counties in North
Carolina. The most basic approach is filtering by name or id:

``` r
# get_location works with a type sf object and name and id values
location <- get_location(type = nc, name = "Warren", name_col = "NAME")
```

You can then access data within or around this specific location. For
example, `get_location_data()` can return all counties within a
quarter-mile of Warren County.

``` r
nearby_counties <- get_location_data(
  data = nc,
  location = location,
  dist = 0.25,
  unit = "mi",
  crop = FALSE
)

glimpse(nearby_counties)
#> Rows: 6
#> Columns: 15
#> $ AREA      <dbl> 0.153, 0.118, 0.072, 0.190, 0.128, 0.142
#> $ PERIMETER <dbl> 2.206, 1.421, 1.085, 2.204, 1.554, 1.640
#> $ CNTY_     <dbl> 1832, 1836, 1842, 1846, 1897, 1913
#> $ CNTY_ID   <dbl> 1832, 1836, 1842, 1846, 1897, 1913
#> $ NAME      <chr> "Northampton", "Warren", "Vance", "Halifax", "Franklin", "Na…
#> $ FIPS      <chr> "37131", "37185", "37181", "37083", "37069", "37127"
#> $ FIPSNO    <dbl> 37131, 37185, 37181, 37083, 37069, 37127
#> $ CRESS_ID  <int> 66, 93, 91, 42, 35, 64
#> $ BIR74     <dbl> 1421, 968, 2180, 3608, 1399, 4021
#> $ SID74     <dbl> 9, 4, 4, 18, 2, 8
#> $ NWBIR74   <dbl> 1066, 748, 1179, 2365, 736, 1851
#> $ BIR79     <dbl> 1606, 1190, 2753, 4463, 1863, 5189
#> $ SID79     <dbl> 3, 2, 6, 17, 0, 7
#> $ NWBIR79   <dbl> 1197, 844, 1492, 2980, 950, 2274
#> $ geometry  <MULTIPOLYGON [°]> MULTIPOLYGON (((-77.21767 3..., MULTIPOLYGON (((-78.30876 3.…
```

This same approach of using names as an attribute query or locations
with buffers as a spatial filter works for most functions in this
package. You can access data from OpenStreetMap:

``` r
county_parks <- get_osm_data(
  location = nearby_counties[1, ],
  asp = 1,
  key = "leisure",
  value = "park",
  geometry = "polygons"
)
#> ℹ OpenStreetMap data is licensed under the Open Database License (ODbL).
#>   Attribution is required if you use this data.
#> • Learn more about the ODbL and OSM attribution requirements at
#>   <https://www.openstreetmap.org/copyright>
#> This message is displayed once every 8 hours.

glimpse(county_parks)
#> Rows: 41
#> Columns: 27
#> $ osm_id                    <chr> "33006375", "33006525", "33006552", "3300661…
#> $ name                      <chr> "Dwight Hall Recreation Park", "Rochelle Par…
#> $ `NHD:FCode`               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `NHD:FDate`               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `NHD:FTYPE`               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ access                    <chr> "yes", "yes", "yes", "yes", "yes", "yes", "y…
#> $ `addr:city`               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `addr:housenumber`        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `addr:postcode`           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `addr:state`              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `addr:street`             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ area                      <chr> NA, NA, "yes", NA, NA, NA, NA, NA, NA, "yes"…
#> $ ele                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `gnis:county_id`          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `gnis:created`            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `gnis:feature_id`         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `gnis:state_id`           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ leisure                   <chr> "park", "park", "park", "park", "park", "par…
#> $ `name:etymology:wikidata` <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ `nconemap:OWNER`          <chr> "GASTON", "ROANOKE RAPIDS", "ROANOKE RAPIDS"…
#> $ opening_hours             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ operator                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ ownership                 <chr> "municipal", "municipal", "municipal", "muni…
#> $ phone                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ source                    <chr> "NCOnemap", "NCOnemap", "NCOnemap", "NCOnema…
#> $ wikidata                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ geometry                  <POLYGON [°]> POLYGON ((-77.64057 36.4935..., POLY…
```

You can also access data from any public ArcGIS MapServer or
FeatureServer layers:

``` r
nps_park_url <- "https://carto.nationalmap.gov/arcgis/rest/services/govunits/MapServer/29"

nps_park <- get_esri_data(
  url = nps_park_url,
  name = "Cape Lookout National Seashore",
  name_col = "NAME",
  quiet = TRUE
)

glimpse(nps_park)
#> Rows: 1
#> Columns: 21
#> $ objectid              <int> 197631
#> $ permanent_identifier  <chr> "d9eec3c0-ae13-4ef9-a17b-dc4858cd39d4"
#> $ source_featureid      <chr> "CALO"
#> $ source_datasetid      <chr> "{562524A1-7D6A-40EA-AA44-2E9AEF4488EB}"
#> $ source_datadesc       <chr> "National Park Boundaries 3/2020"
#> $ source_originator     <chr> "National Park Service"
#> $ data_security         <int> 5
#> $ distribution_policy   <chr> "E4"
#> $ loaddate              <dbl> 1.586355e+12
#> $ gnis_id               <chr> "1000889"
#> $ name                  <chr> "Cape Lookout National Seashore"
#> $ areasqkm              <dbl> 115.005
#> $ ftype                 <int> 674
#> $ fcode                 <int> 67400
#> $ admintype             <int> 1
#> $ ownerormanagingagency <int> 13
#> $ shape_length          <dbl> 233295.3
#> $ shape_area            <dbl> 171056451
#> $ gnis_name             <chr> "Cape Lookout National Seashore"
#> $ globalid              <chr> "{652E570E-C428-4697-AD09-120539C4B71F}"
#> $ geoms                 <MULTIPOLYGON [m]> MULTIPOLYGON (((-8468201 41...
```

In some cases, an API key may be required for functions to work:

``` r
## Get Q2 2020 vehicle crash data for Cecil County, Maryland
get_open_data(
  source_url = "https://opendata.maryland.gov",
  data = "65du-s3qu",
  where = "(year = '2020') AND (quarter = 'Q2')",
  name_col = "county_desc",
  name = "Cecil",
  token = Sys.getenv("MARYLAND_OPEN_DATA_API_KEY")
)
```

You must set or provide an API token or key for `get_open_data()`,
`get_airtable_data()`, `get_flickr_photos()` to work.
`get_gsheet_data()` will require user authentication (handled
automatically by the {googlesheets4} package).

## Helper and utility functions

The package also includes a handful of helper and wrapper functions
designed that can be used for formatting, labelling, and other tasks.

For example, you can use `fix_epoch_date()` to convert columns with
[UNIX time](https://en.wikipedia.org/wiki/Unix_time) numeric values to
POSIXct values:

``` r
nps_park[["loaddate"]]
#> [1] 1.586355e+12

nps_park <- fix_epoch_date(nps_park)

nps_park[["loaddate"]]
#> [1] "2020-04-08 10:03:28 EDT"
```

You can use `make_variable_dictionary()` to make a custom dictionary:

``` r
make_variable_dictionary(
  nps_park[, c(10:12)],
  .labels = c(
    "Geographic Names Information System identifier",
    "Park name",
    "Area (sq km)",
    "Geometry"
  )
)
#>  pos variable label                                          col_type missing
#>  1   gnis_id  Geographic Names Information System identifier chr      0      
#>  2   name     Park name                                      chr      0      
#>  3   areasqkm Area (sq km)                                   dbl      0      
#>  4   geoms    Geometry                                       s_MULTIP 0      
#>  values
#>        
#>        
#>        
#> 
```

Or you can use `rename_with_xwalk()` to rename columns:

``` r
rename_with_xwalk(
  nps_park[, c(10:12)],
  xwalk = list(
    "gnis" = "gnis_id",
    "sq_km" = "areasqkm"
  )
)
#> Warning in xwalk == .x: longer object length is not a multiple of shorter
#> object length
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8534392 ymin: 4107119 xmax: -8463797 ymax: 4173979
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>      gnis                           name   sq_km                          geoms
#> 1 1000889 Cape Lookout National Seashore 115.005 MULTIPOLYGON (((-8468201 41...
```
