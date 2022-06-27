## Get Q2 2020 vehicle crash data for Cecil County, Maryland
\dontrun{
  get_open_data(
    source_url = "https://opendata.maryland.gov",
    data = "65du-s3qu",
    where = "(year = '2020') AND (quarter = 'Q2')",
    name_col = "county_desc",
    name = "Cecil",
    token = Sys.getenv("MARYLAND_OPEN_DATA_API_KEY")
  )
}
