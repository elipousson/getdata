# Use FlickrAPI to get geotagged photos for a location

`get_flickr_photos()` uses
[`FlickrAPI::get_photo_search()`](https://rdrr.io/pkg/FlickrAPI/man/getPhotoSearch.html)
to get a data frame or sf objects with photos from a specified location
or matching other photo search parameters. Set API key using
[`FlickrAPI::set_flickr_api_key()`](https://rdrr.io/pkg/FlickrAPI/man/setFlickrAPIKey.html)
or pass to the api_key parameter.

## Usage

``` r
get_flickr_photos(
  location = NULL,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  user_id = NULL,
  tags = NULL,
  license_id = "cc0",
  sort = "date-posted",
  desc = FALSE,
  img_size = "s",
  extras = c("description", "date_taken", "tags", "geo"),
  per_page = 100,
  page = 1,
  orientation = NULL,
  geometry = TRUE,
  crs = 4326,
  key = NULL
)
```

## Arguments

- location:

  A `sf` or `bbox` object to use in creating bounding box for getting
  photos from Flickr. Optional.

- dist:

  buffer distance in units. Optional.

- diag_ratio:

  ratio of diagonal distance of area's bounding box used as buffer
  distance. e.g. if the diagonal distance is 3000 meters and the
  "diag_ratio = 0.1" a 300 meter will be used. Ignored when `dist` is
  provided.

- unit:

  Units for buffer. Supported options include "meter", "foot",
  "kilometer", and "mile", "nautical mile" Common abbreviations (e.g.
  "km" instead of "kilometer") are also supported. Distance in units is
  converted to units matching GDAL units for x; defaults to "meter"

- asp:

  Aspect ratio of width to height as a numeric value (e.g. 0.33) or
  character (e.g. "1:3"). If numeric,
  [`get_asp()`](https://elipousson.github.io/sfext/reference/get_asp.html)
  returns the same value without modification.

- user_id:

  The NSID of the user with photos to search. If this parameter is NULL
  passed then all public photos will be searched.

- tags:

  A vector of tags to search for.

- license_id:

  The license id for photos. For possible values see the Flickr API
  method flickr.photos.licenses.getInfo or see details for more
  information.

- sort:

  Supported options include "date-posted","date-taken",
  "interestingness", or "relevance"

- desc:

  If `TRUE` return images in descending sort order, if `FALSE`, return
  in ascending sort order. Ignored if sort is set to "relevance".

- img_size:

  Image size; defaults to "s" (small). Options ranging from smallest to
  largest size include "sq" (square), "t", "s", "q", "m", "n", "z", "c",
  "l", and "o" (original).

- extras:

  Defaults to "description", "date_taken", "tags", and "geo".

- per_page:

  Photos to return per page of search results, Default: 100. Maximum 250
  if a location is provided or 500 otherwise.

- page:

  Page to return. If page is greater than length 1, loop over all pages.
  This may cause issues with API access if a large page range is
  provided. Default: 1

- orientation:

  If img_size is length 1, photos are filtered to one or more of the
  supported orientations ("portrait", "landscape", and "square");
  defaults to `NULL`.

- geometry:

  If `TRUE`, include "geo" in extras and convert photos data frame to
  `sf` object. Passed to geo parameter of
  [`FlickrAPI::get_photo_search()`](https://rdrr.io/pkg/FlickrAPI/man/getPhotoSearch.html)

- crs:

  Coordinate reference system of `sf` object to return if geometry is
  `TRUE`.

- key:

  Flickr API key. If api_key is `NULL`, the
  [FlickrAPI::getPhotoSearch](https://rdrr.io/pkg/FlickrAPI/man/getPhotoSearch.html)
  uses
  [`FlickrAPI::getFlickrAPIKey()`](https://rdrr.io/pkg/FlickrAPI/man/setFlickrAPIKey.html)
  to use the environment variable "FLICKR_API_KEY" as the key. Use
  [`set_access_token()`](https://elipousson.github.io/getdata/reference/set_access_token.md)
  w/ `type = "FLICKR_API_KEY"` or
  [`FlickrAPI::setFlickrAPIKey()`](https://rdrr.io/pkg/FlickrAPI/man/setFlickrAPIKey.html)

## Value

A data frame with photo information or `sf` object with geometry based
on latitude and longitude of geocoded photos.

## Details

License id options:

license_id can be an integer from 0 to 10 or a corresponding license
code including:

- "c" (All Rights Reserved),

- "by-bc-sa" (Attribution-NonCommercial-ShareAlike),

- "by-nc" (Attribution-NonCommercial),

- "by-nc-nd" (Attribution-NonCommercial-NoDerivs),

- "by" (Attribution),

- "by-sa" (Attribution-ShareAlike),

- "by-nd" (Attribution-NoDerivs),

- "nkc" (No known copyright restrictions),

- "pd-us" (United States Government Work),

- "cc0" (Public Domain Dedication),

- or "pd" (Public Domain Mark).

## See also

[`FlickrAPI::getPhotoSearch()`](https://rdrr.io/pkg/FlickrAPI/man/getPhotoSearch.html)
