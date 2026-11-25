# =============================================================================
# 10_prepare_features.R  -  Collinearity check, feature list, CV folds
# Output: data/derived/model_data.rds  (list: data, feats, folds)
# =============================================================================
source("00_setup.R")

dat <- read_csv(file.path(dir_der, "analysis_county_year.csv"),
                col_types = cols(fips = "c")) |>
  filter(!is.na(pop), pop > 0)

# Lat/long are kept as features (as in the proposal); the projected
# coordinates below are only used to build spatial CV blocks.
xy <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(crs_albers) |> st_coordinates()
dat <- dat |> mutate(lon_km = xy[, 1] / 1000, lat_km = xy[, 2] / 1000)

# 1. Collinearity: print every pair with |r| > 0.8 
feats_all <- setdiff(feature_names(dat), "county_name")
num <- dat[, feats_all] |> select(where(is.numeric))
cm  <- cor(num, use = "pairwise.complete.obs")
high <- which(abs(cm) > 0.8 & upper.tri(cm), arr.ind = TRUE)
pairs <- tibble(var1 = rownames(cm)[high[, 1]], var2 = colnames(cm)[high[, 2]],
                r = round(cm[high], 2)) |> arrange(desc(abs(r)))
message("Highly correlated pairs (|r| > 0.8) - revise drop_pattern in 00_setup.R if needed:")
print(pairs, n = Inf)
write_csv(pairs, file.path(dir_out, "correlated_pairs.csv"))

# 2. Final feature list ---------------------------------------------
feats <- names(num)[!str_detect(names(num), drop_pattern)]
feats <- feats[!str_detect(feats, "case")]
dat <- dat |> drop_na(all_of(feats))
message("Features (", length(feats), "): ", paste(feats, collapse = ", "))
message("Rows after removing missing values: ", nrow(dat))

# 3. Validation folds -------------------------------------
# a) temporal: train 2009-2021, test 2022-2025
# b) spatial blocks: k-means on county centroids -> 10 groups of neighbours
# c) leave-one-year-out
cents <- dat |> distinct(fips, lon_km, lat_km)
km <- kmeans(cents[, c("lon_km", "lat_km")], centers = 10, nstart = 25)
cents$block <- km$cluster
dat <- dat |> left_join(select(cents, fips, block), by = "fips")
write_csv(select(cents, fips, block), file.path(dir_der, "spatial_blocks.csv"))

folds <- list(
  temporal = ifelse(dat$year %in% test_years, "test", "train"),
  spatial  = dat$block,
  loyo     = dat$year
)

# map of spatial blocks (to check they are contiguous)
counties <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE) |>
  left_join(select(cents, fips, block), by = "fips")
p <- ggplot(counties) + geom_sf(aes(fill = factor(block)), colour = "white", linewidth = 0.1) +
  labs(fill = "Spatial CV block", title = "Spatial cross-validation blocks") + theme_void()
ggsave(file.path(dir_out, "figures", "spatial_cv_blocks.png"), p, width = 7, height = 6)

saveRDS(list(data = dat, feats = feats, folds = folds),
        file.path(dir_der, "model_data.rds"))
