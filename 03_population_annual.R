# =============================================================================
# 03_population_annual.R  -  PIPELINE: annual county population 2002-2025
# County totals from the 2000, 2010 and 2020 Census tract centers of population
# (USpopcenters package, no download needed), linearly interpolated between
# censuses and extended past 2020 with each county's 2010-2020 trend.
# 2018 county file is used as a check.
# Outputs: data/derived/population_county_year.csv
#          data/derived/tract_centers.csv (tract points + pop, used for climate)
# =============================================================================
source("00_setup.R")
library(USpopcenters)

pop18 <- read_csv(file_pop2018, col_types = cols(fips = "c"))

tracts <- bind_rows(
  tract2000 |> mutate(census = 2000),
  tract2010 |> mutate(census = 2010),
  tract2020 |> mutate(census = 2020)
) |>
  transmute(census, fips = paste0(STATEFP, COUNTYFP), tract = TRACTCE,
            pop = POPULATION, lat = LATITUDE, lon = LONGITUDE) |>
  filter(fips %in% pop18$fips, pop > 0)
write_csv(tracts, file.path(dir_der, "tract_centers.csv"))

census_pop <- tracts |> group_by(fips, census) |> summarise(pop = sum(pop)) |> ungroup()

# Linear between census points; beyond them, continue the nearest segment's slope.
interp_pop <- function(x, y, xout) {
  out <- approx(x, y, xout = xout, rule = 2)$y
  n <- length(x)
  if (n >= 2) {
    hi <- xout > max(x); lo <- xout < min(x)
    out[hi] <- y[n] + (xout[hi] - x[n]) * (y[n] - y[n - 1]) / (x[n] - x[n - 1])
    out[lo] <- y[1] - (x[1] - xout[lo]) * (y[2] - y[1]) / (x[2] - x[1])
  }
  pmax(round(out), 1)
}

pop <- census_pop |>
  group_by(fips) |>
  reframe(year = 2002:2025, pop = interp_pop(census, pop, 2002:2025))

# ---- Check against 2018 file -------------------------------------------
chk <- pop |> filter(year == 2018) |> inner_join(pop18, by = "fips") |>
  mutate(pct_diff = 100 * (pop - pop2018) / pop2018)
message("2018 interpolated vs your file: median abs. difference ",
        round(median(abs(chk$pct_diff)), 1), "%")
print(chk |> arrange(desc(abs(pct_diff))) |> select(fips, county, pop, pop2018, pct_diff) |> head(5))
stopifnot(n_distinct(pop$fips) == 281)

write_csv(pop, file.path(dir_der, "population_county_year.csv"))
