get_states(
  location = "Maryland"
)

get_counties(
  name = "Baltimore city, Maryland"
)

# Using short names with abbreviated state names for look-up may result in inexact matches
get_counties(
  county = "Baltimore, MD"
)

# Two-digit integer GeoIDs are supported
# bbox and wkt columns are dropped when returning class "sf"
get_states(
  geoid = 24,
  class = "sf"
)

# sf locations are used as a spatial filter
# Example not currently working
#  get_counties(
#    location = get_states("MD", class = "sf"),
#    class = "sf"
#  ),
#  "sf"
# )
