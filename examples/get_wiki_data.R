# Get articles within a radius of a point
bmore_city_hall <- sf::st_sfc(
  sf::st_point(c(-76.6122, 39.2904)),
  crs = 4326
)

get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 5
)

# A numeric radius is used as dist (set unit to change the distance unit)
get_wiki_data(
  location = bmore_city_hall,
  radius = 0.25,
  unit = "mi",
  limit = 5
)

# Search within the bounding box of a polygon or other sf object
bmore_area <- sf::st_buffer(
  sf::st_transform(bmore_city_hall, 26985),
  dist = 300
)

get_wiki_data(
  location = bmore_area,
  limit = 5
)

# Expand the bounding box with dist or diag_ratio
get_wiki_data(
  location = bmore_area,
  dist = 200,
  unit = "meter",
  limit = 5
)

get_wiki_data(
  location = bmore_area,
  radius = TRUE,
  diag_ratio = 0.5,
  limit = 5
)

# Return additional coordinate details for each article
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  details = c("type", "name"),
  limit = 5
)

# Return a data frame with longitude and latitude columns instead of an sf
# object
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 5,
  geometry = FALSE,
  remove_coords = FALSE
)

# Use the title of an article with coordinates as the location
get_wiki_data(
  location = "Baltimore City Hall",
  limit = 5
)

# Search another language edition of Wikipedia
get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  lang = "es",
  limit = 5
)

# Return the full API response instead of a data frame
resp <- get_wiki_data(
  location = bmore_city_hall,
  radius = TRUE,
  dist = 500,
  limit = 2,
  list = "resp"
)

str(resp, max.level = 2)
