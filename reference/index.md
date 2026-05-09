# Package index

## Access spatial data based on location

- [`get_location()`](https://elipousson.github.io/getdata/reference/get_location.md)
  : Get location of a specified type based on name, id, or location
- [`get_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  [`map_location_data()`](https://elipousson.github.io/getdata/reference/get_location_data.md)
  : Get data for a location

## Download data from online sources

- [`get_airtable_data()`](https://elipousson.github.io/getdata/reference/get_airtable_data.md)
  [`get_airtable_metadata()`](https://elipousson.github.io/getdata/reference/get_airtable_data.md)
  **\[experimental\]** : Get data from an Airtable base and optionally
  convert to a sf object

- [`get_esri_data()`](https://elipousson.github.io/getdata/reference/get_esri_data.md)
  [`get_esri_layers()`](https://elipousson.github.io/getdata/reference/get_esri_data.md)
  [`get_esri_metadata()`](https://elipousson.github.io/getdata/reference/get_esri_data.md)
  : Use esri2sf to get data from an ArcGIS FeatureServer or MapServer
  for a location

- [`get_flickr_photos()`](https://elipousson.github.io/getdata/reference/get_flickr_photos.md)
  : Use FlickrAPI to get geotagged photos for a location

- [`get_gsheet_data()`](https://elipousson.github.io/getdata/reference/get_gsheet_data.md)
  : Use googlesheets4 to get a data frame or simple feature data from a
  Google Sheet

- [`get_open_data()`](https://elipousson.github.io/getdata/reference/get_open_data.md)
  [`get_socrata_data()`](https://elipousson.github.io/getdata/reference/get_open_data.md)
  [`get_socrata_metadata()`](https://elipousson.github.io/getdata/reference/get_open_data.md)
  [`list_socrata_data()`](https://elipousson.github.io/getdata/reference/get_open_data.md)
  : Get data from an open data portal (Socrata) for a location

- [`get_osm_data()`](https://elipousson.github.io/getdata/reference/get_osm_data.md)
  [`get_osm_id()`](https://elipousson.github.io/getdata/reference/get_osm_data.md)
  [`get_osm_boundaries()`](https://elipousson.github.io/getdata/reference/get_osm_data.md)
  : Use osmdata to get Open Street Map data for a location

- [`osm_common_tags`](https://elipousson.github.io/getdata/reference/osm_common_tags.md)
  : Common OpenStreetMap tags

- [`osm_building_tags`](https://elipousson.github.io/getdata/reference/osm_building_tags.md)
  : OpenStreetMap building tags

- [`get_tigris_data()`](https://elipousson.github.io/getdata/reference/get_state_tigris.md)
  : Use tigris to get state-level data from the U.S. Census Bureau

- [`get_wiki_data()`](https://elipousson.github.io/getdata/reference/get_wiki_data.md)
  : Get Wikipedia articles for a location

- [`get_static_mapbox()`](https://elipousson.github.io/getdata/reference/get_static_map.md)
  [`get_osm_static_mapbox()`](https://elipousson.github.io/getdata/reference/get_static_map.md)
  [`get_location_static_mapbox()`](https://elipousson.github.io/getdata/reference/get_static_map.md)
  [`get_static_bingmap()`](https://elipousson.github.io/getdata/reference/get_static_map.md)
  : Use mapboxapi or bingmapr to get a static map image

- [`get_elev_profile()`](https://elipousson.github.io/getdata/reference/get_elev_profile.md)
  **\[experimental\]** :

  Use
  [`elevatr::get_elev_point`](https://rdrr.io/pkg/elevatr/man/get_elev_point.html)
  to get the elevation along a profile

## Additional data formatting utilities and reference data

- [`bind_block_col()`](https://elipousson.github.io/getdata/reference/format_address_data.md)
  [`bind_address_col()`](https://elipousson.github.io/getdata/reference/format_address_data.md)
  [`bind_location_text_col()`](https://elipousson.github.io/getdata/reference/format_address_data.md)
  : Format data frames and simple features with address data

- [`format_data()`](https://elipousson.github.io/getdata/reference/format_data.md)
  [`rename_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)
  [`label_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)
  [`make_variable_dictionary()`](https://elipousson.github.io/getdata/reference/format_data.md)
  [`fix_epoch_date()`](https://elipousson.github.io/getdata/reference/format_data.md)
  : Format data frames and simple features using common approaches

- [`format_sf_data()`](https://elipousson.github.io/getdata/reference/format_sf_data.md)
  [`erase_data()`](https://elipousson.github.io/getdata/reference/format_sf_data.md)
  : Format simple feature data

- [`replace_with_xwalk()`](https://elipousson.github.io/getdata/reference/replace_with_xwalk.md)
  [`replace_street_suffixes()`](https://elipousson.github.io/getdata/reference/replace_with_xwalk.md)
  [`replace_street_dir_prefixes()`](https://elipousson.github.io/getdata/reference/replace_with_xwalk.md)
  : Replace values in a character vector or data frame with a crosswalk

- [`street_dir_prefixes`](https://elipousson.github.io/getdata/reference/street_dir_prefixes.md)
  : Street directional prefixes

- [`street_suffixes`](https://elipousson.github.io/getdata/reference/street_suffixes.md)
  : Street suffix abbreviations

- [`str_trim_squish_across()`](https://elipousson.github.io/getdata/reference/str_trim_squish_across.md)
  : Trim and squish across any character columns

- [`make_xwalk_list()`](https://elipousson.github.io/getdata/reference/make_xwalk_list.md)
  :

  Make a crosswalk list for use with
  [`label_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)
  or
  [`rename_with_xwalk()`](https://elipousson.github.io/getdata/reference/format_data.md)

## Additional data access utilities

- [`as_date_range()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  [`date_range_query()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  [`between_date_range()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  [`check_date_range()`](https://elipousson.github.io/getdata/reference/as_date_range.md)
  : Use lubridate to convert an object to a date range
- [`set_access_token()`](https://elipousson.github.io/getdata/reference/set_access_token.md)
  [`get_access_token()`](https://elipousson.github.io/getdata/reference/set_access_token.md)
  : Set or get an access token or API key to/from environment variables.
- [`set_pkg_options()`](https://elipousson.github.io/getdata/reference/set_pkg_options.md)
  : Set getdata or other package-specific options

## Additional utilities for locations

- [`make_location_data_list()`](https://elipousson.github.io/getdata/reference/make_location_data_list.md)
  : Make a list of data and corresponding locations

- [`make_location_grid()`](https://elipousson.github.io/getdata/reference/make_location_grid.md)
  : Make a grid over the bounding box of a location

- [`cache_location_data()`](https://elipousson.github.io/getdata/reference/cache_location_data.md)
  :

  Cache location data with
  [`sf::write_sf()`](https://r-spatial.github.io/sf/reference/st_write.html)
  or
  [`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html)
