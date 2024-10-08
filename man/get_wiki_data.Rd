% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_wiki_data.R
\name{get_wiki_data}
\alias{get_wiki_data}
\title{Get Wikipedia articles for a location}
\usage{
get_wiki_data(
  location,
  radius = FALSE,
  primary = NULL,
  details = NULL,
  limit = 50,
  list = "geosearch",
  lang = getOption("getdata.lang", default = "en"),
  geometry = TRUE,
  dist = getOption("getdata.dist"),
  diag_ratio = getOption("getdata.diag_ratio"),
  unit = getOption("getdata.unit", "meter"),
  asp = getOption("getdata.asp"),
  crs = getOption("getdata.unit", 3857),
  remove_coords = TRUE,
  clean_names = TRUE
)
}
\arguments{
\item{location}{sf object. If multiple areas are provided, they are unioned
into a single sf object using \code{\link[sf:geos_combine]{sf::st_union()}}}

\item{radius}{If \code{TRUE}, use dist as a buffer around the center of the
location; defaults to \code{FALSE}}

\item{primary}{If \code{NULL}, search for primary coordinates. Set primary to "all"
or "secondary" to search other coordinate types.}

\item{details}{Additional detailed to return with results. Options include
"type", "name", "country", "region"; defaults to \code{NULL}.}

\item{limit}{Number of pages to return (max 500); defaults to 50}

\item{list}{method to use for query; "geosearch" returns data, "resp" returns response}

\item{lang}{Language to search on Wikipedia; defaults to "en".}

\item{geometry}{If \code{TRUE}, return sf object. If \code{FALSE}, return data frame. Defaults to \code{FALSE}.}

\item{dist}{buffer distance in units. Optional.}

\item{diag_ratio}{ratio of diagonal distance of area's bounding box used as
buffer distance. e.g. if the diagonal distance is 3000 meters and the
"diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
provided.}

\item{unit}{Units for buffer. Supported options include "meter", "foot",
"kilometer", and "mile", "nautical mile" Common abbreviations (e.g. "km"
instead of "kilometer") are also supported. Distance in units is converted
to units matching GDAL units for x; defaults to "meter"}

\item{asp}{Aspect ratio of width to height as a numeric value (e.g. 0.33) or
character (e.g. "1:3"). If numeric, \code{\link[sfext:get_asp]{get_asp()}} returns the same value
without modification.}

\item{crs}{Coordinate reference system of bounding box to return; defaults to
\code{NULL} which maintains the crs of the input object.}

\item{remove_coords}{For \code{\link[sfext:df_to_sf]{df_to_sf()}}, if \code{TRUE}, remove the coordinate columns
after converting a data frame to simple feature object; defaults to
\code{FALSE}.}

\item{clean_names}{If \code{TRUE}, clean names provided to nm or created based on
value of col using \link[janitor:clean_names]{janitor::clean_names}. If \code{FALSE}, use names as
provided.}
}
\description{
Use the Wikipedia API geosearch API to get Wikipedia articles for a location.
See \url{https://www.mediawiki.org/wiki/Extension:GeoData} for more information.
Only returns Wikipedia articles with coordinates.
}
\details{
For this function, \code{location} can be either an \code{sf}, \code{sfc}, or \code{bbox} object
or the title of a Wikipedia article with a related location.
}
\seealso{
\code{GNfindNearbyWikipedia()} and \code{GNwikipediaBoundingBox()} functions
in the \href{https://docs.ropensci.org/geonames/}{geonames} package
}
