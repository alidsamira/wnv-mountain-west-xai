# =============================================================================
# 01_extract_counties.R  -  PIPELINE: county boundaries + centroids
# Output: data/derived/counties.gpkg   (281 counties, polygons)
#         data/derived/county_location.csv  (fips, lat, lon, area)
# =============================================================================
source("00_setup.R")

counties <- tigris::counties(state = states, cb = TRUE, year = 2020) |>
  transmute(fips = GEOID, state = STUSPS, county_name = NAME) |>
  st_transform(crs_albers)

stopifnot(nrow(counties) == 281)   # 8 Mountain West states

# Centroid inside each polygon (lat/long in degrees for the model)
pts <- st_point_on_surface(counties) |> st_transform(4326)
xy  <- st_coordinates(pts)

location <- counties |>
  st_drop_geometry() |>
  mutate(lon = xy[, 1], lat = xy[, 2],
         area_km2 = as.numeric(st_area(counties)) / 1e6)

st_write(counties, file.path(dir_der, "counties.gpkg"), delete_dsn = TRUE)
write_csv(location, file.path(dir_der, "county_location.csv"))
message("Counties saved: ", nrow(counties))
