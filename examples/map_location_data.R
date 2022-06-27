map_location_data(
  data = c("streets", "mta_bus_lines"),
  package = "mapbaltimore",
  location = get_location(
    type = "council_districts",
    id = 1,
    package = "mapbaltimore"),
  load = TRUE
)
