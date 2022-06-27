baltimore_md <-
  get_tigris_data(
    state = "MD",
    type = "counties",
    name = "Baltimore city",
    cb = TRUE
  )

get_wiki_data(
  location = baltimore_md,
  limit = 5
)
