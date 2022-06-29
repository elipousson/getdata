
osm_building_tags <-
  osmdata::available_tags("building")

osm_building_tags <-
  stringr::str_remove(osm_building_tags, "(?<=&)[:graph:]+") |>
  stringr::str_remove("&")

usethis::use_data(
  osm_building_tags,
  overwrite = TRUE
)
