bmore_city_hall <- sf::st_sfc(
  sf::st_point(c(-76.6122, 39.2904)),
  crs = 4326
)

# Get items within 500 meters of a point
get_wikidata(
  location = bmore_city_hall,
  dist = 500,
  limit = 10
)

# Filter by class. "Q41176" is building; subclasses like skyscraper and city
# hall are included by default.
get_wikidata(
  location = bmore_city_hall,
  dist = 1,
  unit = "km",
  class = "Q41176",
  limit = 10
)

# Set subclasses = FALSE to only include items that are directly an instance of
# the class
get_wikidata(
  location = bmore_city_hall,
  dist = 1,
  unit = "km",
  class = "Q41176",
  subclasses = FALSE,
  limit = 10
)

# Use a polygon to set the search area automatically
bmore_area <- sf::st_buffer(
  sf::st_transform(bmore_city_hall, 26985),
  dist = 300
)

get_wikidata(
  location = bmore_area,
  limit = 10
)

# Return a data frame instead of an sf object and use another language
get_wikidata(
  location = bmore_city_hall,
  dist = 500,
  lang = "es",
  geometry = FALSE,
  limit = 10
)
