
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getdata

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/getdata)](https://CRAN.R-project.org/package=getdata)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of {getdata} is to make the experience of getting location data
easier and more consistent across a wide variety of sources. {getdata}
started as part of the
[overedge](https://elipousson.github.io/overedge/) package along with
[{maplayer}](https://elipousson.github.io/maplayer/) and
[{sfext}](https://elipousson.github.io/sfext/).

{getdata} is designed to work well with location-specific data packages
such as {mapmaryland} and {mapbaltimore} and to support reproducible
approaches to map-making and place-based data analysis. The package
supports downloading access from the following sources

-   ArcGIS FeatureServer and MapServer layers (using {esri2sf})

-   U.S. Census Bureau data (using {tigris})

-   OpenStreetMap (using {osmdata})

-   Socrata Open Data resources (using {RSocrata})

-   Google Sheets (using {googlesheets4})

-   Flickr photos (using {FlickrAPI})

-   Airtable bases (using {httr2} and the Airtable API)

-   Wikipedia articles (using {httr2} and the Wikipedia Geosearch API)

-   Other spatial and tabular data sources including Google MyMaps,
    GitHub gists, Excel sheets, and any data source already supported by
    [sf::read_sf()](https://r-spatial.github.io/sf/reference/st_read.html)
    (see
    [sfext::read_sf_ext()](https://elipousson.github.io/sfext/reference/read_sf_ext.html)
    for more details)

The advantage of using {getdata} is that it provides a consistent
interface for creating a bounding box for spatial filtering or for
querying spatial data by name or id. Where possible, a spatial filter is
used before importing or downloading data to avoid the need to load
large data files when you are only interested in a small area. The
package also provides a consistent approach for handling API tokens and
keys (inspired by both the tigris and mapboxapi packages) and for
caching data locally.

The related {sfext} package allows {getdata} to supports the easy
conversion of tabular data into spatial data. For example, if the source
data has coordinates, you can convert the data into an sf object. If
data has an address column, you can geocode the data using the
{tidygeocoder} package. If the data has a location name column, such as
“neighborhood”, you can join the data to a simple feature object with
the related geometry. You also can turn off these options by setting
`geometry = FALSE` for most data access functions.

Lastly, the
[format_data()](https://elipousson.github.io/getdata/reference/format_data.html)
and
[format_sf_data()](https://elipousson.github.io/getdata/reference/format_sf_data.html)
functions provide convenient options for working with the data after it
is downloaded. While advanced R users may prefer a more customizable
options, these functions are designed to support the creation of custom
data formatting and access functions such as
[format_md_crash_data()](https://elipousson.github.io/mapmaryland/reference/format_md_sf.html)
and
[get_md_crash_data()](https://elipousson.github.io/mapmaryland/reference/get_md_open_data.html).

This package imports rlang for both non-standard evaluation and error
handling and relies on dplyr, purrr, and other tidyverse packages.
Suggestions for additional data sources to support, new functions, or
improvements to existing functions are welcome.

## Installation

You can install the development version of getdata like so:

``` r
pak::pkg_install("elipousson/getdata")
```

## Example

[get_location_data()](https://elipousson.github.io/getdata/reference/get_location_data.html)
is a flexible function for reading and subsetting data. In this example,
data is a file path but it can also be a URL, the name of a data set in
another package, or a sf object.

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
nearby_counties <-
  get_location_data(
  data = nc,
  location = location,
  dist = 0.25,
  unit = "mi"
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
#> $ geometry  <POLYGON [°]> POLYGON ((-77.89524 36.5529..., POLYGON ((-78.13472 36.2365.…
```

A similar approach works with other sources although some may require an
API key to work.

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

You must set or provide an API token or key for
[get_open_data()](https://elipousson.github.io/getdata/reference/get_open_data.html),
[get_airtable_data()](https://elipousson.github.io/getdata/reference/get_airtable_data.html),
[get_flickr_photos()](https://elipousson.github.io/getdata/reference/get_flickr_photos.html)
to work.
[get_ghseet_data()](https://elipousson.github.io/getdata/reference/get_gsheet_data.html)
will require user authentication (handled automatically by the
googlesheets4 package).
