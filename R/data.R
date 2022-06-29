#' OpenStreetMap building tags
#'
#' Used by [get_osm_data] if key = "building".
#'
#' More information on the building key
#' <https://wiki.openstreetmap.org/wiki/Key:building>
#'
#' @format A character vector with length of 84
"osm_building_tags"

#' U.S. State boundaries (1:5 mi scale, bbox and wkt)
#'
#' U.S. State boundaries data downloaded with [tigris::states].
#'
#' The geoid and wkt columns are labelled with the state abbreviation (abb) in
#' lower case.
#'
#' @format A data frame with 52 rows and 6 variables:
#' \describe{
#'   \item{`name`}{State name}
#'   \item{`geoid`}{State GeoID (labelled)}
#'   \item{`abb`}{State abbreviation (USPS)}
#'   \item{`est_pop`}{Estimated state population (B01001_001), American Community Survey 5 year, 2015-2019}
#'   \item{`statefp`}{State FIPS}
#'   \item{`bbox`}{Bounding box}
#'   \item{`wkt`}{Well known text (labelled)}
#' }
"us_states"

#' U.S. County boundaries (1:5 mi scale, bbox and wkt)
#'
#' U.S. County boundaries data downloaded with [tigris::counties].
#'
#' The geoid and wkt columns are labelled with a combination of an abbreviated
#' county name and state abbreviation in snake case. Population estimates
#' (est_pop) are not included for county equivalents in the U.S. Virgin Islands,
#' Guam, Northern Mariana Islands, and American Samoa.
#'
#' @format A data frame with 3,220 rows and 7 variables:
#' \describe{
#'   \item{`name`}{County name (tidycensus)}
#'   \item{`name_short`}{County name without state (tigris)}
#'   \item{`geoid`}{County GeoID (labelled)}
#'   \item{`abb_state`}{State abbreviation (USPS)}
#'   \item{`est_pop`}{Estimated county population (B01001_001), American Community Survey 5 year, 2015-2019}
#'   \item{`countyfp`}{County FIPS}
#'   \item{`statefp`}{State FIPS}
#'   \item{`bbox`}{Bounding box}
#'   \item{`wkt`}{Well known text (labelled)}
#' }
"us_counties"
