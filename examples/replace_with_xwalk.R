address_df <-
  data.frame(
    "bldg_num" = c("100", "1415", "600", "10"),
    "street_dir_prefix" = c(NA, NA, "N", NA),
    "street_name" = c("Holiday", "Key", "Charles", "Art Museum"),
    "street_type" = c("St", "Hwy", "St", "Dr")
  )

replace_street_suffixes(
  c("Street", "Highway", "Avenue", "Drive")
)

replace_street_suffixes(
  address_df,
  abb = FALSE,
  case = "sentence"
)

replace_street_dir_prefixes(
  c("North", "East", "West")
)

replace_street_dir_prefixes(
  c("S", "W", "N"),
  abb = FALSE,
  case = "sentence"
)
