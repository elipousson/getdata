% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_pkg_options.R
\name{set_pkg_options}
\alias{set_pkg_options}
\title{Set getdata or other package-specific options}
\usage{
set_pkg_options(..., overwrite = FALSE, .pkg = "getdata")
}
\arguments{
\item{...}{Named list of options to set, e.g. "crs = 2804" with \code{.pkg = "getdata"} to set "getdata.crs" to 2804.}

\item{overwrite}{If \code{TRUE}, overwrite any existing option value.}

\item{.pkg}{Package name to append to option name. Defaults to "getdata".}
}
\description{
This function can set named options for a package using the convention of
"pkg.option". For examples \code{set_pkg_options(crs = 2804, .pkg = "getdata")} sets the option "getdata.crs" to 2804. If "getdata.crs" is
already set, overwrite must be \code{TRUE} to replace the existing value.
}
\section{Options for the getdata package}{


Implemented options (with defaults if used) for the getdata package include:
\itemize{
\item dist
\item diag_ratio
\item unit ("meter")
\item asp
\item crs (3857)
\item from_crs (4326)
\item address ("address")
\item package
\item filetype ("gpkg")
}

A similar convention is used for the maplayer package. The use of options is
not implemented across all functions and may be changed in the future.
}

