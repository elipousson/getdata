\dontrun{
  get_osm_static_mapbox(
    id = "way/49664223",
    dist = 0.5,
    unit = "mi",
    overlay_style = list(
      stroke = "darkgreen",
      fill = "green",
      fill_opacity = 0.25
    )
  )

  nc <- sfext::read_sf_path(system.file("shape/nc.shp", package = "sf"))

  get_location_static_mapbox(
    type = nc,
    name = "Ashe",
    name_col = "NAME",
    dist = 50,
    unit = "mi"
  )
}
