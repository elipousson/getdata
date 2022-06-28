dc <-
  get_tigris_data(
  type = "census places",
  state = "District of Columbia",
)

get_wiki_data(
  location = dc,
  radius = 0.1,
  unit = "mi",
  limit = 5
)
