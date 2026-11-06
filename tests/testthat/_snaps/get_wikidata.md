# get_wikidata warns when no items are found

    Code
      data <- suppressMessages(get_wikidata(location, dist = 100))
    Condition
      Warning:
      No Wikidata items found for `location`.

# get_wikidata checks inputs

    Code
      get_wikidata()
    Condition
      Error in `get_wikidata()`:
      ! `location` is absent but must be supplied.

---

    Code
      suppressMessages(get_wikidata(sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)),
      crs = 4326)))
    Condition
      Error in `cli_abort_ifnot()`:
      ! `dist` or `diag_ratio` must be provided for a point `location`.
      i Use a polygon `location` to set the search area automatically.

---

    Code
      suppressMessages(get_wikidata(sf::st_sfc(sf::st_point(c(-76.6122, 39.2904)),
      crs = 4326), dist = 100, class = "building"))
    Condition
      Error in `cli_abort_ifnot()`:
      ! `class` must be Wikidata identifiers like "Q41176".

# wikidata_query_around validates arguments

    Code
      wikidata_query_around(-76.6, 39.3, 500, class = "building")
    Condition
      Error in `cli_abort_ifnot()`:
      ! `class` must be Wikidata identifiers like "Q41176".

---

    Code
      wikidata_query_around(-76.6, 39.3, 500, class = 41176)
    Condition
      Error:
      ! `class` must be a character vector, not the number 41176.

---

    Code
      wikidata_query_around(-76.6, 39.3, 500, limit = 0)
    Condition
      Error:
      ! `limit` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      wikidata_query_around(-76.6, 39.3, -1)
    Condition
      Error:
      ! `radius` must be a number larger than or equal to 0, not the number -1.

---

    Code
      wikidata_query_around(-76.6, 39.3, 500, lang = 1)
    Condition
      Error:
      ! `lang` must be a single string, not the number 1.

