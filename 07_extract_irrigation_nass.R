# =============================================================================
# 07_extract_irrigation_nass.R  -  PIPELINE: irrigated land (USDA Ag Census)
# County irrigated acres from the Census of Agriculture (2002, 2007, 2012,
# 2017, 2022) via the NASS Quick Stats API, converted to a share of county
# area and linearly interpolated to every year 2009-2025.
# Output: data/derived/irrigation_county_year.csv
#
# ONE-TIME SETUP: free API key from https://quickstats.nass.usda.gov/api
#   (click "Request API Key", enter your email; the key arrives by email).
#   Optional, so you are not asked every session: run usethis::edit_r_environ(),
#   add the line   NASS_API_KEY=yourkey   save, restart R.
# =============================================================================
source("00_setup.R")
library(rnassqs)

# API key: read from NASS_API_KEY (set once in .Renviron, see top of file),
# otherwise asked for in a pop-up for this session.
nass_key <- Sys.getenv("NASS_API_KEY")
if (nass_key == "") nass_key <- askpass::askpass("NASS Quick Stats API key")
if (is.null(nass_key) || !nzchar(nass_key))
  stop("No API key. Get a free one at https://quickstats.nass.usda.gov/api")
nassqs_auth(key = nass_key)

location <- read_csv(file.path(dir_der, "county_location.csv"),
                     col_types = cols(fips = "c"))

raw <- nassqs(list(
  source_desc    = "CENSUS",
  short_desc     = "AG LAND, IRRIGATED - ACRES",
  domain_desc    = "TOTAL",
  agg_level_desc = "COUNTY",
  state_alpha    = states,
  year           = c(2002, 2007, 2012, 2017, 2022)
))
# If this returns 0 rows, check the exact label with:
#   nassqs_param_values("short_desc", commodity_desc = "AG LAND")

irr_census <- raw |>
  transmute(
    fips  = paste0(state_fips_code, county_code),
    year  = as.integer(year),
    # "(D)" = withheld to protect farms -> NA; "(Z)" = less than half unit -> 0
    acres = case_when(str_detect(Value, "\\(Z\\)") ~ 0,
                      TRUE ~ suppressWarnings(as.numeric(gsub(",", "", Value))))
  ) |>
  filter(fips %in% location$fips)

# Interpolate between census years; carry the last census forward after 2022
irrigation <- irr_census |>
  complete(fips = location$fips, year = 2002:2025) |>
  group_by(fips) |>
  mutate(acres = if (sum(!is.na(acres)) >= 2)
                   approx(year, acres, xout = year, rule = 2)$y else acres) |>
  ungroup() |>
  filter(year %in% years) |>
  left_join(select(location, fips, area_km2), by = "fips") |>
  mutate(irrigated_share = acres * 0.0040468564 / area_km2) |>  # acres -> km2
  select(fips, year, irrigated_share)

write_csv(irrigation, file.path(dir_der, "irrigation_county_year.csv"))
message("Counties with no irrigation data: ",
        sum(is.na(irrigation$irrigated_share)) / length(years))
