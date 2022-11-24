osm_tags_tables <-
  htmldf::html_df("https://wiki.openstreetmap.org/wiki/Map_features")

osm_tags_links <- osm_tags_tables$links[[1]] %>%
  dplyr::distinct(href, .keep_all = TRUE) %>%
  dplyr::filter(stringr::str_detect(href, "wiki/Tag%3A")) %>%
  tidyr::separate(
    href,
    into = c("baseurl", "key", "value"),
    sep = "%3A|=",
    remove = FALSE
  ) %>%
  dplyr::select(-c(baseurl, text))

osm_common_tags <-
  purrr::map(
    osm_tags_tables$tables[[1]],
    ~ dplyr::select(
      .x,
      dplyr::any_of(c("Key", "Value", "Element", "Comment", "Description"))
    )
  ) %>%
  dplyr::bind_rows() %>%
  janitor::clean_names("snake") %>%
  dplyr::mutate(
    description = dplyr::coalesce(comment, description),
    category = dplyr::if_else(
      key == value,
      key,
      NA_character_
    ),
    subheader = !is.na(category)
  ) %>%
  tidyr::fill(category) %>%
  dplyr::filter(!subheader, element != "Element") %>%
  dplyr::select(-c(subheader, comment, element)) %>%
  dplyr::left_join(osm_tags_links, by = c("key", "value")) %>%
  dplyr::rename(
    url = href
  ) %>%
  dplyr::filter(!is.na(url)) %>%
  dplyr::distinct(url, .keep_all = TRUE)

usethis::use_data(osm_common_tags, overwrite = TRUE)
