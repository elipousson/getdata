library(sf)

nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
nc_county <- nc[2,]

# Transform coordinate reference system
st_crs(nc)$epsg
st_crs(format_sf_data(nc, crs = 3857))$epsg

# Simplify and smooth geometry
plot(nc_county, max.plot = 1)
nc_county_simple <- format_sf_data(nc_county, dTolerance = 5000, smooth = TRUE)
plot(nc_county_simple, max.plot = 1)

# Erase data
nc_co_water <- get_tigris_data(type = "area water", state = "NC", county = nc_county$NAME)
plot(format_sf_data(nc_county, erase_data = nc_co_water), max.plot = 1)

# Relocate sf column with a helper function function
relocate_sf_col(nc, .after = contains("AREA"))[,c(1, 2, 6)]

# If sf_req is set to FALSE, use any object that can be converted with sfext::as_sf
nc_bbox <- st_bbox(nc)
plot(format_sf_data(nc_bbox, erase_data = nc_county_simple, sf_req = FALSE))

# Bind area units (converting square meters to square miles)
bind_units_col(nc, st_area(nc), units = "mi^2")[,c(1,5,15)]

