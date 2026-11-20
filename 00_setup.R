# =============================================================================
# 00_setup.R  -  Shared settings, packages and helper functions
# Project: Explaining West Nile Virus Risk in the Mountain West with XAI
#
# Every other script starts with source("00_setup.R").
# Edit the PATHS and INPUT FILES sections to match your computer.
# =============================================================================

# ---- Packages ---------------------------------------------------------------
pkgs <- c(
  # data + spatial
  "dplyr", "tidyr", "readr", "purrr", "stringr", "sf", "terra", "tigris",
  "exactextractr", "FedData", "appeears", "rnassqs", "elevatr",
  "USpopcenters", "FNN",
  # models
  "MASS", "ranger", "xgboost",
  # explanations + plots
  "treeshap", "shapviz", "lime", "ggplot2", "patchwork",
  # app
  "shiny", "leaflet"
)
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(sf); library(ggplot2)
})
options(tigris_use_cache = TRUE, dplyr.summarise.inform = FALSE)
sf_use_s2(FALSE)

# ---- Paths ------------------------------------------------------------------
dir_raw  <- "data/raw"        # downloads (NLCD, MODIS, ...)
dir_der  <- "data/derived"    # county-level tables made by the pipelines
dir_out  <- "output"          # model results, figures, tables
for (d in c(dir_raw, dir_der, dir_out, file.path(dir_out, "figures")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- Study design -----------------------------------------------------------
states      <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY")
years       <- 2009:2025
train_years <- 2009:2021
test_years  <- 2022:2025
surge_years <- c(2017, 2021, 2025)
crs_albers  <- 5070          # CONUS Albers equal-area (metres)
set.seed(6960)

# ---- INPUT FILES (your data; put them in data/input/) ------------------------
dir_in        <- "data/input"
file_cases    <- file.path(dir_in, "wnv_hist_hum_nonhum_yearly.csv")   # CDC ArboNET
file_pop2018  <- file.path(dir_in, "mw_county_pop2018.csv")            # check only
file_stations <- file.path(dir_in, "stations_mountain_west.csv")       # GHCN-D stations
file_monthly  <- file.path(dir_in, "monthly_station_summary.csv")      # station-month climate
# (the daily_XX.csv files are not needed: the monthly summary is built from them)

# ---- Collinear variables to DROP before modelling ---------------------------
# Summer temperature, heat days and GDD are r = 0.81-0.93: keep one per group.
# Regular expression; default keeps mean summer temperature and drops the rest.
# 10_prepare_features.R prints every pair with |r| > 0.8 so you can revise it.
drop_pattern <- "^(heat_days|gdd|tmax_summer)_|^tmean_spring_norm$"


# ---- Readable labels for figures --------------------------------------------
var_labels <- c(
  lon = "Longitude", lat = "Latitude", elev_pop_m = "Elevation where people live (m)",
  tmean_summer_norm = "Summer temperature, normal (\u00b0C)",
  tmean_summer_anom = "Summer temperature anomaly (\u00b0C)",
  tmean_summer_anom_lag1 = "Last year's summer temperature anomaly (\u00b0C)",
  tmean_spring_anom = "Spring temperature anomaly (\u00b0C)",
  tmean_spring_anom_lag1 = "Last year's spring temperature anomaly (\u00b0C)",
  ppt_spring_norm = "Spring rainfall, normal (mm)", ppt_summer_norm = "Summer rainfall, normal (mm)",
  ppt_spring_anom = "Spring rainfall anomaly (mm)", ppt_summer_anom = "Summer rainfall anomaly (mm)",
  ppt_spring_anom_lag1 = "Last year's spring rainfall anomaly (mm)",
  ppt_summer_anom_lag1 = "Last year's summer rainfall anomaly (mm)",
  irrigated_share = "Irrigated land (share of county)", crop_share = "Cropland (share of county)",
  pasture_share = "Pasture/hay (share of county)", wetland_share = "Wetlands (share of county)",
  developed_share = "Developed land (share of county)", water_share = "Open water (share of county)",
  ndvi_spring = "Spring greenness (NDVI)", ndvi_summer = "Summer greenness (NDVI)",
  ndvi_spring_anom = "Spring greenness anomaly (NDVI)", ndvi_summer_anom = "Summer greenness anomaly (NDVI)"
)
# variable name -> readable label (unknown names are returned unchanged)
nice_var <- function(x) ifelse(x %in% names(var_labels), var_labels[x], x)
model_labels  <- c(NB = "Negative binomial", RF = "Random forest", XGB = "XGBoost")
scheme_labels <- c(temporal = "Temporal (test 2022-2025)", spatial = "Spatial blocks",
                   loyo = "Leave-one-year-out")
state_labels  <- c(AZ = "Arizona", CO = "Colorado", ID = "Idaho", MT = "Montana",
                   NV = "Nevada", NM = "New Mexico", UT = "Utah", WY = "Wyoming")
model_colours <- c("Negative binomial" = "#4C78A8", "Random forest" = "#F58518",
                   "XGBoost" = "#54A24B", "Observed" = "black")

# shapviz object with readable feature names (for plotting only)
pretty_sv <- function(sv) {
  S <- shapviz::get_shap_values(sv); X <- shapviz::get_feature_values(sv)
  colnames(S) <- nice_var(colnames(S)); names(X) <- nice_var(names(X))
  shapviz::shapviz(S, X = X, baseline = shapviz::get_baseline(sv))
}
theme_report <- function() theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

# ---- Helper functions -------------------------------------------------------
id_cols <- c("fips", "year", "state", "county_name", "neuro_cases",
             "total_cases", "pop", "rate", "lon_km", "lat_km", "block")

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
mae  <- function(obs, pred) mean(abs(obs - pred), na.rm = TRUE)

# Feature column names = everything that is not an ID / outcome column
feature_names <- function(df) setdiff(names(df), id_cols)

# --- Model 1: negative binomial baseline (counts with log-population offset)
fit_nb <- function(train, feats) {
  f <- as.formula(paste("neuro_cases ~", paste(feats, collapse = " + "),
                        "+ offset(log(pop))"))
  MASS::glm.nb(f, data = train, control = glm.control(maxit = 100))
}
pred_nb <- function(model, newdata) {
  # returns rate per 100,000
  predict(model, newdata = newdata, type = "response") / newdata$pop * 1e5
}

# --- Model 2: random forest on rate, weighted by population
fit_rf <- function(train, feats, num.trees = 500) {
  ranger::ranger(
    x = train[, feats], y = train$rate,
    case.weights = train$pop,
    num.trees = num.trees, mtry = max(1, floor(length(feats) / 3)),
    # depth limits keep exact TreeSHAP fast and reduce over-fitting to
    # single outbreak years
    min.node.size = 10, max.depth = 12,
    importance = "permutation", seed = 6960
  )
}
pred_rf <- function(model, newdata, feats)
  predict(model, data = newdata[, feats])$predictions

# --- Model 3: XGBoost, Poisson objective on counts, population offset
# The offset is log(pop x average training rate), not log(pop): with log(pop)
# alone the model starts at ~1 case per person and, with small learning steps,
# never comes down to the real rate (~1 per 100,000) -> predictions stuck ~59.
fit_xgb <- function(train, feats, nrounds = 300) {
  r0 <- sum(train$neuro_cases) / sum(train$pop)        # average rate per person
  dtrain <- xgboost::xgb.DMatrix(as.matrix(train[, feats]),
                                 label = train$neuro_cases,
                                 base_margin = log(train$pop * r0))
  booster <- xgboost::xgb.train(
    params = list(objective = "count:poisson", eta = 0.05, max_depth = 4,
                  subsample = 0.8, colsample_bytree = 0.8,
                  min_child_weight = 5),
    data = dtrain, nrounds = nrounds, verbose = 0
  )
  list(booster = booster, r0 = r0)                      # keep r0 for prediction
}
pred_xgb <- function(model, newdata, feats) {
  d <- xgboost::xgb.DMatrix(as.matrix(newdata[, feats]),
                            base_margin = log(newdata$pop * model$r0))
  predict(model$booster, d) / newdata$pop * 1e5        # rate per 100,000
}

# One call to fit any model on 'train' and predict rates on 'test'
fit_predict <- function(model_name, train, test, feats) {
  switch(model_name,
    NB  = pred_nb(fit_nb(train, feats), test),
    RF  = pred_rf(fit_rf(train, feats), test, feats),
    XGB = pred_xgb(fit_xgb(train, feats), test, feats)
  )
}

# Read the modelling table produced by 10_prepare_features.R
load_model_data <- function() readRDS(file.path(dir_der, "model_data.rds"))
