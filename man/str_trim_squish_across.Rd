% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_data.R
\name{str_trim_squish_across}
\alias{str_trim_squish_across}
\title{Trim and squish across any character columns}
\usage{
str_trim_squish_across(x)
}
\arguments{
\item{x}{A data.frame with character columns.}
}
\description{
Apply \code{\link[stringr:str_trim]{stringr::str_squish()}} and \code{\link[stringr:str_trim]{stringr::str_trim()}} to all character
columns in a data.frame.
}
