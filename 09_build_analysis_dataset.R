# =============================================================================
# 09_build_analysis_dataset.R  -  Join everything into one county-year table
# Required : county_location (01), cases (02), population (03), climate (04)
# Optional : land cover (05), NDVI (06), irrigation (07), elevation (08)
#            -> skipped with a warning if not run yet, so you can start modelling
# Output   : data/derived/analysis_county_year.csv  (281 x 17 = 4,777 rows)
# =============================================================================
source("00_setup.R")

rd <- function(name) {
  f <- file.path(dir_der, name)
  if (!file.exists(f)) { warning("Not found, skipped: ", name); return(NULL) }
  read_csv(f, col_types = cols(fips = "c"), show_col_types = FALSE)
}
join_if <- function(d, x, by) if (is.null(x)) d else left_join(d, x, by = by)

location <- rd("county_location.csv")
cy <- c("fips", "year")

dat <- expand_grid(fips = location$fips, year = years) |>
  left_join(select(location, fips, state, county_name, lat, lon), by = "fips") |>
  left_join(rd("cases_county_year.csv"),      by = cy) |>
  left_join(rd("population_county_year.csv"), by = cy) |>
  left_join(rd("climate_county_year.csv"),    by = cy) |>
  join_if(rd("landcover_county_year.csv"),    by = cy) |>
  join_if(rd("ndvi_county_year.csv"),         by = cy) |>
  join_if(rd("irrigation_county_year.csv"),   by = cy) |>
  join_if(rd("elevation_county.csv"),         by = "fips") |>
  mutate(rate = neuro_cases / pop * 1e5)       # neuroinvasive per 100,000

# NASS withholds irrigated acres "(D)" for a few small counties -> 0
if ("irrigated_share" %in% names(dat))
  dat <- mutate(dat, irrigated_share = replace_na(irrigated_share, 0))

# Quick checks 
stopifnot(nrow(dat) == 281 * length(years))
message("Rows: ", nrow(dat), " | Neuroinvasive cases 2009-2025: ", sum(dat$neuro_cases))
message("Share of county-years with zero cases: ", round(mean(dat$neuro_cases == 0), 3))
na_counts <- colSums(is.na(dat))
if (any(na_counts > 0)) { message("Columns with missing values:"); print(na_counts[na_counts > 0]) }

write_csv(dat, file.path(dir_der, "analysis_county_year.csv"))
