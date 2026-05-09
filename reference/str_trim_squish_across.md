# Trim and squish across any character columns

Apply
[`stringr::str_squish()`](https://stringr.tidyverse.org/reference/str_trim.html)
and
[`stringr::str_trim()`](https://stringr.tidyverse.org/reference/str_trim.html)
to all character columns in a data.frame.

## Usage

``` r
str_trim_squish_across(x)
```

## Arguments

- x:

  A data.frame with character columns.
