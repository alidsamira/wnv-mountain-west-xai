# =============================================================================
# 06_extract_ndvi_modis.R  -  PIPELINE: MODIS NDVI (MOD13A3, monthly, 1 km)
# Downloads monthly NDVI through NASA AppEEARS, then averages it per county:
#   ndvi_spring  = mean NDVI April-June     (same year)
#   ndvi_summer  = mean NDVI July-September (same year)
#   ndvi_spring_anom / ndvi_summer_anom = difference from the county's mean
# Output: data/derived/ndvi_county_year.csv
#
# ONE-TIME SETUP: free NASA Earthdata account (https://urs.earthdata.nasa.gov)
#   1. Tell R your Earthdata USERNAME: add  EARTHDATA_USER=yourname  to .Renviron
#      (usethis::edit_r_environ()), or type it in earthdata_user below.
#   2. Optional (so you are not asked every session): run
#        usethis::edit_r_environ()
#      add the line   EARTHDATA_PASSWORD=yourpassword   save, restart R.
# AppEEARS tasks run on NASA servers; each year can take 10-60 minutes.
# =============================================================================
source("00_setup.R")
library(terra); library(exactextractr); library(appeears)

# Earthdata USERNAME (not email): set EARTHDATA_USER in .Renviron, or type it here
earthdata_user <- Sys.getenv("EARTHDATA_USER", "your_earthdata_user")
ndvi_dir <- file.path(dir_raw, "ndvi")
dir.create(ndvi_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 0. Earthdata login ------------------------------------------------------
# The appeears package's use of the Windows Credential Manager fails
# ("Element not found"), so the password is kept in memory for this R session
# only. It is read from EARTHDATA_PASSWORD (set once in .Renviron, see below)
# or asked for in a pop-up.
if (!nzchar(earthdata_user) || grepl("^your_", earthdata_user))   # placeholder not replaced
  stop("Set earthdata_user (near the top of this script) to your NASA Earthdata username (not email).")
options(keyring_backend = "env")
pw <- Sys.getenv("EARTHDATA_PASSWORD")
if (pw == "") pw <- askpass::askpass(paste("Earthdata password for", earthdata_user))
suppressWarnings(rs_set_key(user = earthdata_user, password = pw))
rm(pw)

counties <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE)
roi <- counties |> st_transform(crs_albers) |> st_union() |>   # union in metres
  st_simplify(dTolerance = 2000) |> st_transform(4326) |> st_as_sf()

# ---- 1. Download (safe to re-run; picks up where it stopped) -----------------
# A year is complete when its 6 monthly NDVI images (Apr-Sep) are on disk.
ndvi_files <- function() {
  f <- list.files(ndvi_dir, pattern = "NDVI.*doy\\d{7}.*\\.tif$",
                  recursive = TRUE, full.names = TRUE)
  # delete broken files left by an interrupted download
  ok <- map_lgl(f, \(x) file.size(x) > 0 && !is.null(tryCatch(rast(x), error = \(e) NULL)))
  if (any(!ok)) { message("Removing ", sum(!ok), " broken file(s)"); file.remove(f[!ok]) }
  f[ok]
}
missing_years <- function() {
  doy <- str_match(basename(ndvi_files()), "doy(\\d{4})(\\d{3})")
  n <- table(factor(doy[, 2], levels = years))[as.character(years)]
  years[n < 6]
}
my_tasks <- function() {
  t <- suppressWarnings(rs_list_task(user = earthdata_user))
  if (is.null(t) || !nrow(t)) data.frame(task_name = character(), task_id = character(),
                                         status = character()) else t
}

# 1a. Submit ALL missing years at once (NASA processes them in parallel).
#     Years already submitted earlier are not sent again.
tasks <- my_tasks()
for (yr in missing_years()) {
  task_name <- paste0("ndvi_", yr)
  if (task_name %in% tasks$task_name) next
  df <- data.frame(
    task = task_name, subtask = "mountain_west",
    start = paste0(yr, "-04-01"), end = paste0(yr, "-09-30"),
    product = "MOD13A3.061", layer = "_1_km_monthly_NDVI"
  )
  task <- rs_build_task(df = df, roi = roi, format = "geotiff")
  suppressWarnings(rs_request(request = task, user = earthdata_user, transfer = FALSE,
                              path = ndvi_dir, verbose = FALSE))
  message("Submitted ", task_name)
}

# 1b. Every 5 minutes: download finished years, retry failed downloads.
started <- Sys.time()
while (length(todo <- missing_years()) > 0) {
  tasks <- my_tasks()
  for (yr in todo) {
    t <- tasks[tasks$task_name == paste0("ndvi_", yr), , drop = FALSE]
    done <- t[t$status == "done", , drop = FALSE]
    if (nrow(done)) {
      message("Downloading ", yr)
      dir.create(file.path(ndvi_dir, paste0("ndvi_", yr)), showWarnings = FALSE)
      tryCatch(suppressWarnings(rs_transfer(task_id = done$task_id[1], user = earthdata_user,
                           path = file.path(ndvi_dir, paste0("ndvi_", yr)))),
               error = \(e) message("  download failed, will retry: ", conditionMessage(e)))
    } else if (any(t$status == "error")) {
      stop("NASA reports an error for ", yr, ". Delete that request on the AppEEARS ",
           "website ('Explore Requests') and re-run to resubmit it.")
    }
  }
  todo <- missing_years()
  if (!length(todo)) break
  if (difftime(Sys.time(), started, units = "hours") > 12)
    stop("Still waiting after 12 h for: ", paste(todo, collapse = ", "), ". Re-run later.")
  message(format(Sys.time(), "%H:%M"), " waiting for: ", paste(todo, collapse = ", "))
  Sys.sleep(300)
}
message("All NDVI years downloaded.")

# ---- 2. County means for each monthly file ---------------------------------
files <- ndvi_files()
stopifnot(length(files) > 0)

monthly <- map(files, \(f) {
  doy  <- str_match(basename(f), "doy(\\d{4})(\\d{3})")
  date <- as.Date(as.numeric(doy[3]) - 1, origin = paste0(doy[2], "-01-01"))
  r <- rast(f)
  r[r < -2000] <- NA                       # fill values
  if (global(r, "max", na.rm = TRUE)[1, 1] > 1) r <- r * 1e-4  # scale factor
  cty <- st_transform(counties, crs(r))
  tibble(fips = cty$fips, year = as.integer(doy[2]),
         month = as.integer(format(date, "%m")),
         ndvi = exact_extract(r, cty, "mean", progress = FALSE))
}) |> bind_rows()

# ---- 3. Seasonal means and anomalies ---------------------------------------
ndvi <- monthly |>
  mutate(season = case_when(month %in% 4:6 ~ "ndvi_spring",
                            month %in% 7:9 ~ "ndvi_summer")) |>
  filter(!is.na(season)) |>
  group_by(fips, year, season) |>
  summarise(ndvi = mean(ndvi, na.rm = TRUE)) |>
  pivot_wider(names_from = season, values_from = ndvi) |>
  group_by(fips) |>
  mutate(ndvi_spring_anom = ndvi_spring - mean(ndvi_spring, na.rm = TRUE),
         ndvi_summer_anom = ndvi_summer - mean(ndvi_summer, na.rm = TRUE)) |>
  ungroup()

write_csv(ndvi, file.path(dir_der, "ndvi_county_year.csv"))
message("NDVI rows: ", nrow(ndvi))
