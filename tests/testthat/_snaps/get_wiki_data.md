# get_wiki_data checks inputs

    Code
      get_wiki_data(location, list = "search")
    Condition
      Error in `cli_abort_ifnot()`:
      ! `list` must be "geosearch" or "resp".
      i Support for additional options may be added in the future.

---

    Code
      get_wiki_data()
    Condition
      Error in `get_wiki_data()`:
      ! `location` is absent but must be supplied.

---

    Code
      get_wiki_data(1)
    Condition
      Error in `get_wiki_data()`:
      ! `location` must be a character vector, not the number 1.

---

    Code
      suppressMessages(get_wiki_data(location, radius = TRUE, dist = 0))
    Condition
      Error in `cli_abort_ifnot()`:
      ! radius `dist` must be greater than 1.

# req_wiki_query validates arguments

    Code
      req_wiki_query(lang = "en", gspage = "Baltimore", details = "height")
    Condition
      Error:
      ! `details` must be one of "name", "type", "dim", "scale", "region", "country", or "globe", not "height".

---

    Code
      req_wiki_query(lang = "en", gspage = "Baltimore", primary = "none")
    Condition
      Error:
      ! `primary` must be one of "primary", "all", or "secondary", not "none".

---

    Code
      req_wiki_query(lang = "en")
    Condition
      Error in `cli_abort_ifnot()`:
      ! One of `gsbbox`, `gscoord`, or `gspage` must be supplied.

---

    Code
      req_wiki_query(lang = 1, gspage = "Baltimore")
    Condition
      Error:
      ! `lang` must be a single string, not the number 1.

# req_wiki_query resets limit to the max

    Code
      req <- req_wiki_query(lang = "en", gspage = "Baltimore", limit = 1000)
    Condition
      Warning:
      `limit` can't be greater than "500".
      v Resetting provided 1000 limit value to max.

