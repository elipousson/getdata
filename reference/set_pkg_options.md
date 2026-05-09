# Set getdata or other package-specific options

This function can set named options for a package using the convention
of "pkg.option". For examples
`set_pkg_options(crs = 2804, .pkg = "getdata")` sets the option
"getdata.crs" to 2804. If "getdata.crs" is already set, overwrite must
be `TRUE` to replace the existing value.

## Usage

``` r
set_pkg_options(..., overwrite = FALSE, .pkg = "getdata")
```

## Arguments

- ...:

  Named list of options to set, e.g. "crs = 2804" with
  `.pkg = "getdata"` to set "getdata.crs" to 2804.

- overwrite:

  If `TRUE`, overwrite any existing option value.

- .pkg:

  Package name to append to option name. Defaults to "getdata".

## Options for the getdata package

Implemented options (with defaults if used) for the getdata package
include:

- dist

- diag_ratio

- unit ("meter")

- asp

- crs (3857)

- from_crs (4326)

- address ("address")

- package

- filetype ("gpkg")

A similar convention is used for the maplayer package. The use of
options is not implemented across all functions and may be changed in
the future.
