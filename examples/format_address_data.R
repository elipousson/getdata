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

bind_address_col(
  address_df,
  city = "Baltimore",
  state = "MD"
)
