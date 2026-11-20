# =============================================================================
# 04_climate_county_year.R  -  PIPELINE: station climate -> county-year climate
# Input : stations_mountain_west.csv, monthly_station_summary.csv (GHCN-Daily),
#         tract_centers.csv from 03_population_annual.R
# Steps : 1. station-year seasonal values (only complete seasons)
#         2. each census tract gets an inverse-distance-weighted value from its
#            5 nearest stations reporting that year (within 150 km)
#         3. county value = population-weighted mean of its tracts
#         4. normals (2002-2025 county mean), same-year and previous-year anomalies
# Output: data/derived/climate_county_year.csv
# =============================================================================
source("00_setup.R")

k_stations <- 5      # stations averaged per tract
max_km     <- 150    # ignore stations farther than this

stations <- read_csv(file_stations, show_col_types = FALSE)
monthly  <- read_csv(file_monthly, show_col_types = FALSE) |>
  filter(year >= 2001)                         # 2001 needed for 2002 lag
tracts   <- read_csv(file.path(dir_der, "tract_centers.csv"),
                     col_types = cols(fips = "c", tract = "c"))

# ---- 1. Seasonal values per station-year ------------------------------------
# A month counts only with >= 20 days of data; counts (GDD, heat days) are
# scaled up to the full month. A season needs every one of its months.
monthly <- monthly |>
  mutate(ndays   = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month] +
                   (month == 2 & year %% 4 == 0),
         t_ok    = temp_days >= 20 & !is.na(tmean_c),
         p_ok    = prcp_days >= 20,
         gdd     = `gdd_base14.3` * ndays / pmax(temp_days, 1),
         heat    = days_tmax_ge32c * ndays / pmax(temp_days, 1))

specs <- tribble(
  ~var,           ~col,          ~months, ~fun,  ~ok,
  "tmean_spring", "tmean_c",     3:5,     "mean", "t_ok",
  "tmean_summer", "tmean_c",     6:8,     "mean", "t_ok",
  "tmax_summer",  "tmax_mean_c", 6:8,     "mean", "t_ok",
  "ppt_spring",   "prcp_total_mm", 3:5,   "sum",  "p_ok",
  "ppt_summer",   "prcp_total_mm", 6:8,   "sum",  "p_ok",
  "heat_days",    "heat",        6:9,     "sum",  "t_ok",
  "gdd",          "gdd",         4:9,     "sum",  "t_ok"
)

station_year <- pmap(specs, \(var, col, months, fun, ok) {
  monthly |>
    filter(month %in% months, .data[[ok]], !is.na(.data[[col]])) |>
    group_by(station, year) |>
    filter(n() == length(months)) |>
    summarise(value = if (fun == "mean") mean(.data[[col]]) else sum(.data[[col]])) |>
    mutate(var = var)
}) |> bind_rows() |> ungroup()

# ---- 2. Tract values by inverse-distance weighting --------------------------
to_xy <- function(d) st_as_sf(d, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs_albers) |> st_coordinates()
st_xy <- to_xy(stations)

# Tract layout of the nearest census: <2005 -> 2000, <2015 -> 2010, else 2020
census_for <- function(y) ifelse(y < 2005, 2000, ifelse(y < 2015, 2010, 2020))

tract_values <- map(c(2000, 2010, 2020), \(cen) {
  tr <- filter(tracts, census == cen)
  nn <- FNN::get.knnx(st_xy, to_xy(tr), k = 15)   # candidate stations
  pairs <- tibble(tid = rep(seq_len(nrow(tr)), 15),
                  station = stations$id[as.vector(nn$nn.index)],
                  km = as.vector(nn$nn.dist) / 1000) |>
    filter(km <= max_km)
  yrs <- (2001:2025)[census_for(2001:2025) == cen]
  pairs |>
    inner_join(filter(station_year, year %in% yrs), by = "station",
               relationship = "many-to-many") |>
    group_by(tid, year, var) |>
    slice_min(km, n = k_stations, with_ties = FALSE) |>
    summarise(value = weighted.mean(value, 1 / pmax(km, 1)^2)) |>
    ungroup() |>
    mutate(fips = tr$fips[tid], pop = tr$pop[tid])
}) |> bind_rows()

# ---- 3. Population-weighted county means ------------------------------------
county <- tract_values |>
  group_by(fips, year, var) |>
  summarise(value = weighted.mean(value, pop)) |>
  ungroup()

# ---- 4. Normals and anomalies -----------------------------------------------
climate <- county |>
  group_by(fips, var) |>
  mutate(norm = mean(value[year >= 2002], na.rm = TRUE),
         anom = value - norm) |>
  arrange(year, .by_group = TRUE) |>
  mutate(anom_lag1 = if_else(lag(year) == year - 1, lag(anom), NA_real_)) |>
  ungroup() |>
  filter(year >= 2002) |>
  select(fips, year, var, norm, anom, anom_lag1) |>
  pivot_wider(names_from = var, values_from = c(norm, anom, anom_lag1),
              names_glue = "{var}_{.value}")

message("County-years: ", nrow(climate), " | missing values per column:")
print(colSums(is.na(climate))[colSums(is.na(climate)) > 0])
write_csv(climate, file.path(dir_der, "climate_county_year.csv"))
