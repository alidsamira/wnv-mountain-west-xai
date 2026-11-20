# =============================================================================
# 08_extract_elevation.R  -  PIPELINE: mean elevation of populated tracts
# Elevation (AWS Terrain Tiles via elevatr) at each 2020 census tract center
# of population, averaged per county weighted by tract population.
# Input : data/derived/tract_centers.csv (from 03_population_annual.R)
# Output: data/derived/elevation_county.csv
# =============================================================================
source("00_setup.R")

tracts <- read_csv(file.path(dir_der, "tract_centers.csv"),
                   col_types = cols(fips = "c", tract = "c")) |>
  filter(census == 2020)

pts  <- st_as_sf(tracts, coords = c("lon", "lat"), crs = 4326)
elev <- elevatr::get_elev_point(pts, src = "aws", z = 9)   # ~300 m tiles

elevation <- elev |>
  st_drop_geometry() |>
  group_by(fips) |>
  summarise(elev_pop_m = weighted.mean(elevation, pop, na.rm = TRUE))

write_csv(elevation, file.path(dir_der, "elevation_county.csv"))
message("Counties with elevation: ", nrow(elevation))
