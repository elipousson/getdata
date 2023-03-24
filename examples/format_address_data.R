address_df <-
  data.frame(
    "bldg_num" = c("100", "1415", "600"),
    "street_dir_prefix" = c(NA, NA, "N"),
    "street_name" = c("Holiday", "Key", "Charles"),
    "street_type" = c("Street", "Highway", "St")
  )

address_df <-
  bind_block_col(
    x = address_df,
    street_col = "street_address"
  )

address_df[1,]

address_df <-
  bind_address_col(
    address_df,
    city = "Baltimore",
    state = "MD"
  )

address_df[2,]

location_df <-
  data.frame(
    "text" = c(
      "100 Holiday St.",
      "1400 block Key Highway (north side)",
      "Charles St. from E. Centre St. to E. Madison St."
    )
)

location_df <- bind_location_text_col(location_df)

location_df
