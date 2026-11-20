# Input data

| File | In this repo? | Source |
|---|---|---|
| `wnv_hist_hum_nonhum_yearly.csv` | Yes | CDC ArboNET, historical West Nile virus data by county and year |
| `mw_county_pop2018.csv` | Yes | US Census Bureau county population, 2018 (used only as a check) |
| `stations_mountain_west.csv` | Yes | NOAA GHCN-Daily station list for the 8 states |
| `monthly_station_summary.csv` | No (56 MB) | Monthly summaries of NOAA GHCN-Daily records, 2000-2025 |
| `daily_AZ.csv` ... `daily_WY.csv` | No (60-135 MB each) | NOAA GHCN-Daily records for each state |

The large climate files are too big for GitHub. Download daily station data from
NOAA GHCN-Daily (https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily)
for the stations in `stations_mountain_west.csv`, then summarise by station, year and month
(total precipitation, mean temperature, days of data, growing degree-days above 14.3 C,
days with maximum temperature of at least 32 C).

These layers are downloaded by the scripts themselves:

| Layer | Script | Source |
|---|---|---|
| County boundaries | `01_extract_counties.R` | US Census (tigris package) |
| Census tract population centers | `03_population_annual.R` | US Census (USpopcenters package) |
| Land cover | `05_extract_landcover_nlcd.R` | USGS Annual NLCD - download the 2010, 2015, 2020 and 2024 Land Cover GeoTIFFs from https://www.mrlc.gov/data |
| Vegetation (NDVI) | `06_extract_ndvi_modis.R` | NASA AppEEARS, MODIS MOD13A3 (free Earthdata account) |
| Irrigation | `07_extract_irrigation_nass.R` | USDA NASS Quick Stats (free API key) |
| Elevation | `08_extract_elevation.R` | AWS Terrain Tiles (elevatr package) |
