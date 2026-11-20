# =============================================================================
# 02_prepare_cases.R  -  PIPELINE: CDC ArboNET cases -> county-year table
# Input : wnv_hist_hum_nonhum_yearly.csv  (all US counties, 1999-2025; a county
#         appears only in years with human or non-human WNV activity)
# Output: data/derived/cases_county_year.csv  (all 281 counties x 2002-2025,
#         zero-filled; 2002-2008 kept for descriptive use, models use 2009+)
# =============================================================================
source("00_setup.R")

pop18 <- read_csv(file_pop2018, col_types = cols(fips = "c"))   # 281 study counties

raw <- read_csv(file_cases, col_types = cols(Location = "c"), show_col_types = FALSE)

cases <- raw |>
  transmute(fips = str_pad(Location, 5, pad = "0"),
            year = Year,
            total_cases = `Total human disease cases`,
            neuro_cases = `Neuroinvasive disease cases`) |>
  filter(fips %in% pop18$fips, year >= 2002) |>
  group_by(fips, year) |>
  summarise(total_cases = sum(total_cases), neuro_cases = sum(neuro_cases)) |>
  ungroup() |>
  complete(fips = pop18$fips, year = 2002:2025,
           fill = list(total_cases = 0, neuro_cases = 0))

# ---- Checks against the proposal -------------------------------------------
message("Counties: ", n_distinct(cases$fips), " | rows: ", nrow(cases))
message("Total human cases 2002-2025: ", sum(cases$total_cases), " (proposal: 15,786)")
ut <- cases |> filter(str_starts(fips, "49"), year %in% c(2017, 2025)) |>
  group_by(year) |> summarise(neuro = sum(neuro_cases))
print(ut)   # proposal: 39 neuroinvasive cases in both years

write_csv(cases, file.path(dir_der, "cases_county_year.csv"))
