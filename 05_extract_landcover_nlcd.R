# =============================================================================
# 05_extract_landcover_nlcd.R  -  PIPELINE: USGS Annual NLCD land cover shares
# For each county and NLCD year: share of cultivated crops, pasture/hay,
# wetlands, developed land and open (surface) water.
# Output: data/derived/landcover_county_year.csv
#
# Land cover changes slowly, so a few NLCD years are extracted and each
# county-year gets the nearest NLCD year.
#
# WHERE THE RASTERS COME FROM (no FedData: its links point to the retired
# Collection 1.0 files and fail with "file does not exist"):
#   1. A local file, if you downloaded one: any .tif whose name contains
#      "LndCov_<year>" (e.g. Annual_NLCD_LndCov_2020_CU_C1V2.tif) anywhere in
#      the working directory or data/raw/nlcd/ (sub-folders are searched too).
#      Download: https://www.mrlc.gov/data  ->  Annual NLCD,
#      Land Cover, CONUS, pick the year (zip; unzip the .tif into that folder).
#   2. Otherwise the USGS cloud copy (Cloud-Optimized GeoTIFF) is read over
#      the internet; only the blocks covering each county are fetched.
# =============================================================================
source("00_setup.R")
library(terra); library(exactextractr)

counties   <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE)
nlcd_years <- c(2010, 2015, 2020, 2024)
nlcd_dir   <- file.path(dir_raw, "nlcd")
dir.create(nlcd_dir, showWarnings = FALSE, recursive = TRUE)

# NLCD class codes -> variable names
classes <- list(
  crop_share      = 82,
  pasture_share   = 81,
  wetland_share   = c(90, 95),
  developed_share = c(21, 22, 23, 24),
  water_share     = 11
)

# Faster, anonymous reads of the public cloud files
Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", AWS_NO_SIGN_REQUEST = "YES",
           CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif", GDAL_HTTP_MAX_RETRY = "5",
           GDAL_HTTP_RETRY_DELAY = "3")

# ---- Find the raster for a year: local file first, then cloud versions -------
open_nlcd <- function(yr) {
  # searches data/raw/nlcd AND the working directory, including sub-folders
  # such as ./Annual_NLCD_LndCov_2010_CU_C1V2/
  local <- list.files(c(nlcd_dir, "."), pattern = paste0("LndCov_", yr, ".*\\.tif$"),
                      recursive = TRUE, full.names = TRUE)
  local <- unique(normalizePath(local[!grepl("aux|ovr", basename(local))]))
  if (length(local) == 1) { message("  using ", local); return(rast(local)) }
  if (length(local) > 1) {               # tiles (e.g. from EarthExplorer) -> mosaic
    message("  mosaicking ", length(local), " tiles")
    return(vrt(local))
  }

  for (v in c(2, 1, 0)) {          # newest collection version first
    f   <- sprintf("Annual_NLCD_LndCov_%d_CU_C1V%d.tif", yr, v)
    url <- sprintf("/vsicurl/https://usgs-landcover.s3.us-west-2.amazonaws.com/annual-nlcd/c1/v%d/cu/mosaic/%s", v, f)
    r <- tryCatch(rast(url), error = function(e) NULL)
    if (!is.null(r)) { message("  using cloud file ", f); return(r) }
  }
  stop("No NLCD land cover file found for ", yr, ".\n",
       "Download 'Annual NLCD Land Cover ", yr, " CONUS' from https://www.mrlc.gov/data,\n",
       "unzip it and put the .tif in ", normalizePath(nlcd_dir), ", then run again.")
}

# ---- Class shares per county -----------------------------------
extract_year <- function(yr) {
  message("NLCD ", yr)
  r   <- open_nlcd(yr)
  cty <- st_transform(counties, crs(r))
  fr  <- exact_extract(r, cty, "frac", progress = TRUE)   # one column per class
  names(fr) <- sub("frac_", "", names(fr))

  res <- tibble(fips = cty$fips, nlcd_year = yr)
  for (v in names(classes)) {
    cols <- intersect(as.character(classes[[v]]), names(fr))
    res[[v]] <- if (length(cols)) rowSums(fr[, cols, drop = FALSE]) else 0
  }
  write_csv(res, file.path(nlcd_dir, paste0("county_shares_", yr, ".csv")))  # checkpoint
  res
}

# Re-use finished years if the script is re-run
lc <- map(nlcd_years, \(yr) {
  done <- file.path(nlcd_dir, paste0("county_shares_", yr, ".csv"))
  if (file.exists(done)) read_csv(done, col_types = cols(fips = "c")) else extract_year(yr)
}) |> bind_rows()

# Sanity check: shares are between 0 and 1
stopifnot(all(between(as.matrix(select(lc, ends_with("_share"))), 0, 1.0001)))

# ---- Assign each study year to the nearest NLCD year -------------------------
nearest <- tibble(year = years) |>
  mutate(nlcd_year = map_dbl(year, \(y) nlcd_years[which.min(abs(nlcd_years - y))]))

landcover <- nearest |>
  left_join(lc, by = "nlcd_year", relationship = "many-to-many") |>
  select(-nlcd_year)

write_csv(landcover, file.path(dir_der, "landcover_county_year.csv"))
message("Land cover rows: ", nrow(landcover))
