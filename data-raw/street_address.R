url <- "https://docs.google.com/spreadsheets/d/1WTGwGbxMMo1wNxrzeHKmcU3EAaTOea6_nyGLXVDwMtw/edit?usp=sharing"

street_suffixes <- googlesheets4::read_sheet(url)

street_suffixes <- street_suffixes %>%
  tidyr::fill(street_suffix, street_suffix_abb)

street_suffixes <-
  street_suffixes %>%
  dplyr::group_by(street_suffix_abb, street_suffix) %>%
  dplyr::summarize(
    street_suffix_common = list(street_suffix_common)
  )

usethis::use_data(street_suffixes, overwrite = TRUE)

street_dir_prefixes <-
  tibble::tribble(
    ~street_dir_abb, ~street_dir_en, ~street_dir_es,
    "N", "NORTH", "NORTE",
    "NE", "NORTHEAST", "NORESTE",
    "NW", "NORTHWEST", "NOROESTE",
    "S", "SOUTH", "SUR",
    "SE", "SOUTHEAST", "SURESTE",
    "SW", "SOUTHWEST", "SUROESTE",
    "E", "EAST", "ESTE",
    "W", "WEST", "OESTE"
  )

usethis::use_data(street_dir_prefixes, overwrite = TRUE)
