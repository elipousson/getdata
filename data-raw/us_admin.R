## code to prepare `tigris` dataset goes here

us_states <-
  tigris::states(
    cb = TRUE,
    resolution = "5m"
  )

us_states <-
  us_states %>%
  janitor::clean_names("snake") %>%
  sf::st_transform(3857)

nest_states <-
  us_states %>%
  dplyr::group_by(geoid) %>%
  dplyr::group_nest(keep = TRUE)

us_states <-
  purrr::map_dfr(
    .x = nest_states$data,
    ~ tibble::tibble(
      name = unique(.x$name),
      geoid = unique(.x$geoid),
      statefp = unique(.x$statefp),
      abb = unique(.x$stusps),
      bbox = list(sf::st_bbox(.x)),
      wkt = sf::st_as_text(sf::st_as_sfc(.x))
    )
  )


get_state_pop <-
  function(x) {
    tidycensus::get_acs(
      geography = "state",
      variables = "B01001_001",
      year = 2019,
      state = x,
      cache_table = TRUE
    ) %>%
      janitor::clean_names("snake")
  }


possibly_get_state_pop <-
  purrr::possibly(
    ~ get_state_pop(.x),
    NULL,
    quiet = FALSE
  )

us_states_census <-
  purrr::map_dfr(
    us_states$statefp,
    ~ possibly_get_state_pop(.x)
  )

us_states_census <-
  us_states_census %>%
  dplyr::select(-c(name, variable, moe)) %>%
  dplyr::rename(est_pop = estimate)

us_states <- us_states %>%
  dplyr::left_join(
    us_states_census,
    by = "geoid"
  ) %>%
  dplyr::relocate(
    abb,
    est_pop,
    .after = geoid
  )

us_states$wkt <- textclean::replace_non_ascii(us_states$wkt)
names(us_states$geoid) <- us_states$abb
names(us_states$wkt) <- us_states$abb

usethis::use_data(
  us_states,
  overwrite = TRUE,
  internal = FALSE
)

us_counties <-
  purrr::map_dfr(
    us_states$statefp,
    ~ tigris::counties(
      state = .x,
      cb = TRUE,
      resolution = "5m"
    )
  )

us_counties <-
  us_counties %>%
  janitor::clean_names("snake") %>%
  sf::st_transform(3857)

us_counties <-
  us_counties %>%
  dplyr::left_join(
    dplyr::select(us_states, state_abb = abb, statefp),
    by = "statefp"
  )

nest_counties <- us_counties %>%
  dplyr::group_by(geoid) %>%
  dplyr::group_nest(keep = TRUE)

us_counties <-
  purrr::map_dfr(
    .x = nest_counties$data,
    ~ tibble::tibble(
      name_short = unique(.x$name),
      geoid = unique(.x$geoid),
      countyfp = unique(.x$countyfp),
      statefp = unique(.x$statefp),
      abb_state = unique(.x$state_abb),
      bbox = list(sf::st_bbox(.x)),
      wkt = sf::st_as_text(sf::st_as_sfc(.x))
    )
  )

get_county_pop <-
  function(x, y) {
    tidycensus::get_acs(
      geography = "county",
      variables = "B01001_001",
      year = 2019,
      state = x,
      county = y,
      cache_table = TRUE
    )
  }

possibly_get_county_pop <-
  purrr::possibly(
    ~ get_county_pop(.x, .y),
    NULL,
    quiet = FALSE
  )


us_counties_census <-
  purrr::map2_dfr(
    us_counties$statefp,
    us_counties$countyfp,
    ~ possibly_get_county_pop(
      .x,
      .y
    )
  ) %>%
  janitor::clean_names("snake")

us_counties <-
  us_counties %>%
  dplyr::left_join(us_counties_census, by = "geoid") %>%
  dplyr::select(-c(moe, variable)) %>%
  dplyr::rename(
    est_pop = estimate
  ) %>%
  dplyr::relocate(
    abb_state, est_pop,
    .after = name_short
  ) %>%
  dplyr::relocate(
    name,
    .before = dplyr::everything()
  ) %>%
  dplyr::mutate(
    name = dplyr::case_when(
      (is.na(name) && state_abb == "VI") ~ paste0(name_short, ", ", "U.S. Virgin Islands"),
      (is.na(name) && state_abb == "GU") ~ paste0(name_short, ", ", "Guam"),
      (is.na(name) && state_abb == "MP") ~ paste0(name_short, ", ", "Northern Mariana Islands"),
      (is.na(name) && state_abb == "AS") ~ paste0(name_short, ", ", "American Samoa"),
      TRUE ~ name
    )
  ) %>%
  dplyr::mutate(
    label = stringr::str_extract(name, ".+(?=,)"),
    label = janitor::make_clean_names(paste0(label, ", ", abb_state))
  ) %>%
  dplyr::mutate(
    label = stringr::str_replace(
      label, "_borough_", "_bor_"
    ),
    label = stringr::str_replace(
      label, "_county_", "_co_"
    ),
    label = stringr::str_replace(
      label, "_parish_", "_par_"
    ),
    label = stringr::str_replace(
      label, "_township_", "_twp_"
    ),
    label = stringr::str_replace(
      label, "_census_area_", "_"
    ),
    label = stringr::str_replace(
      label, "_municipio_", "_muni_"
    )
  )

names(us_counties$wkt) <- us_counties$label
names(us_counties$geoid) <- us_counties$label

us_counties <- dplyr::select(
  us_counties,
  -label
)

usethis::use_data(
  us_counties,
  overwrite = TRUE,
  internal = FALSE
)
