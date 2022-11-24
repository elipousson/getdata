#' OpenStreetMap building tags
#'
#' Used by [get_osm_data] if key = "building".
#'
#' More information on the building key
#' <https://wiki.openstreetmap.org/wiki/Key:building>
#'
#' @format A character vector with length of 84
"osm_building_tags"


#' Common OpenStreetMap tags
#'
#' A subset of tags scraped from the OpenStreetMap Wiki page on Map features:
#' <https://wiki.openstreetmap.org/wiki/Map_features>. Only those tags with a
#' url are included in this reference table.
#'
#' @format A data frame with 272 rows and 5 variables:
#' \describe{
#'   \item{\code{key}}{Key}
#'   \item{\code{value}}{Value}
#'   \item{\code{description}}{Description of tag/usage}
#'   \item{\code{category}}{Category}
#'   \item{\code{url}}{OSM Wiki url}
#' }
"osm_common_tags"


#' Street suffix abbreviations
#'
#' A data frame based on Appendix C1 Street Suffix Abbreviations from the U.S.
#' Postal Service Publication 28 - Postal Addressing Standards. This data
#' includes examples of suffix forms that are primary street suffix names,
#' common street suffixes or suffix abbreviations, and recommended official
#' U.S. Postal Service standard suffix abbreviations.
#'
#' @format A data frame with 206 rows and 3 variables:
#' \describe{
#'   \item{\code{street_suffix_abb}}{U.S. Postal Service standard suffix
#'   abbreviation}
#'   \item{\code{street_suffix}}{Street suffix name}
#'   \item{\code{street_suffix_common}}{List column with commonly used street
#'   suffix or abbreviation}
#' }
#' @details Source: <https://pe.usps.com/text/pub28/28apc_002.htm>
"street_suffixes"


#' Street directional prefixes
#'
#' A data frame based on Appendix I4 Directionals from the U.S. Postal Service
#' Publication 28 - Postal Addressing Standards. According to the U.S. Postal Service, "Directionals are not commonly used in
#' Puerto Rican addresses because other descriptions, such as the urbanization
#' name identify geographic areas. In the ZIP+4 file, the English equivalents
#' are used. Note: Although the Spanish word for West is Oeste, the abbreviation
#' W is used."
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{\code{street_dir_abb}}{Street directional abbreviation}
#'   \item{\code{street_dir_en}}{Street directional name (English)}
#'   \item{\code{street_dir_es}}{Street directional name (Spanish)}
#' }
#' @details Source: <https://pe.usps.com/text/pub28/28api_007.htm>
"street_dir_prefixes"
