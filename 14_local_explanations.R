# =============================================================================
# 14_local_explanations.R  -  Q3: why did the model flag a county-year?
#   1. SHAP waterfall plots for selected county-years
#      (Salt Lake County 2017, Cache County 2025 + any you add)
#   2. LIME for the same cases, repeated 10 times and averaged
#   3. Agreement between SHAP and LIME (rank correlation, sign, top-5 overlap)
#   4. Utah surges 2017 & 2025: share of the predicted rise due to climate,
#      vegetation, land cover and location (compare with 21% and 39%)
# =============================================================================
source("00_setup.R")
library(shapviz); library(lime); library(patchwork)

md  <- load_model_data(); dat <- md$data
fm  <- readRDS(file.path(dir_out, "final_model.rds")); feats <- fm$feats
sv  <- readRDS(file.path(dir_out, "shap_values.rds"))
X   <- as.data.frame(dat[, feats])

cases <- tribble(
  ~fips,   ~year, ~label,
  "49035", 2017,  "Salt Lake County, UT 2017",
  "49005", 2025,  "Cache County, UT 2025"
  # add more rows here, e.g. "08123", 2023, "Weld County, CO 2023"
) |> mutate(row = map2_int(fips, year, \(f, y) which(dat$fips == f & dat$year == y)))

# ---- 1. SHAP waterfalls ------------------------------------------------------
wf <- map2(cases$row, cases$label, \(r, l)
  sv_waterfall(pretty_sv(sv), row_id = r, max_display = 10) + ggtitle(l) +
    labs(x = "Predicted rate (cases per 100,000)") + theme_report())
ggsave(file.path(dir_out, "figures", "shap_waterfalls.png"),
       wrap_plots(wf, ncol = 1), width = 8, height = 5 * nrow(cases))

# ---- 2. LIME (random forest), repeated 10 times -----------------------------
if (fm$type != "RF") stop("LIME section is written for the random forest model.")
model_type.ranger    <- function(x, ...) "regression"
predict_model.ranger <- function(x, newdata, type, ...)
  data.frame(Response = predict(x, data = newdata)$predictions)

explainer <- lime(X, fm$model, bin_continuous = TRUE, n_bins = 4)
lime_runs <- map(1:10, \(s) {
  set.seed(s)
  explain(X[cases$row, ], explainer, n_features = 8, n_permutations = 5000) |>
    mutate(run = s)
}) |> bind_rows()

lime_avg <- lime_runs |>
  mutate(row = as.integer(case)) |>   # lime keeps the original row names
  group_by(row, feature) |>
  summarise(lime_weight = sum(feature_weight) / 10,   # 0 when not selected
            times_selected = n()) |>
  ungroup()
write_csv(lime_avg, file.path(dir_out, "lime_average_weights.csv"))

# ---- 3. SHAP vs LIME agreement ----------------------------------------------
S <- get_shap_values(sv)
agree <- map(seq_len(nrow(cases)), \(i) {
  r <- cases$row[i]
  shap_i <- tibble(feature = feats, shap = S[r, feats])
  comp <- shap_i |> inner_join(filter(lime_avg, row == r), by = "feature")
  tibble(
    case = cases$label[i],
    spearman_rho   = cor(comp$shap, comp$lime_weight, method = "spearman"),
    sign_agreement = mean(sign(comp$shap) == sign(comp$lime_weight)),
    top5_overlap   = length(intersect(
      slice_max(shap_i, abs(shap), n = 5)$feature,
      slice_max(comp, abs(lime_weight), n = 5)$feature)) / 5
  )
}) |> bind_rows()
print(agree)
write_csv(agree, file.path(dir_out, "shap_lime_agreement.csv"))

p_cmp <- map(seq_len(nrow(cases)), \(i) {
  r <- cases$row[i]
  tibble(feature = feats, SHAP = S[r, feats]) |>
    inner_join(filter(lime_avg, row == r) |> select(feature, LIME = lime_weight), by = "feature") |>
    pivot_longer(c(SHAP, LIME)) |>
    group_by(name) |> mutate(value = value / max(abs(value))) |>   # scale to compare
    ggplot(aes(value, reorder(nice_var(feature), value), fill = name)) +
    geom_col(position = "dodge") + geom_vline(xintercept = 0) +
    scale_fill_manual(values = c(SHAP = "#4C78A8", LIME = "#BAB0AC")) +
    labs(x = "Scaled contribution (largest = 1)", y = NULL, fill = NULL, title = cases$label[i]) +
    theme_report()
})
ggsave(file.path(dir_out, "figures", "shap_vs_lime.png"),
       wrap_plots(p_cmp, ncol = 1), width = 8, height = 4 * nrow(cases))

# ---- 4. Utah surges: what share of the predicted rise is climate? -----------
group_of <- function(f) case_when(
  str_detect(f, "ndvi") ~ "vegetation",
  str_detect(f, "crop|pasture|wetland|developed|water|irrig") ~ "land cover",
  str_detect(f, "^lat$|^lon$|elev") ~ "location / terrain",
  TRUE ~ "climate")

ut <- as_tibble(S) |> mutate(state = dat$state, year = dat$year, pop = dat$pop) |>
  filter(state == "UT") |>
  pivot_longer(all_of(feats), names_to = "feature", values_to = "shap") |>
  mutate(group = group_of(feature)) |>
  group_by(year, group) |> summarise(shap = weighted.mean(shap, pop))  # per-capita view

surge_share <- map(c(2017, 2025), \(y) {
  base <- ut |> filter(!year %in% c(2017, 2021, 2025)) |>
    group_by(group) |> summarise(base = mean(shap))
  ut |> filter(year == y) |> left_join(base, by = "group") |>
    mutate(rise = shap - base) |> ungroup() |>
    mutate(share_of_rise = rise / sum(rise), year = y)
}) |> bind_rows()
print(surge_share)
write_csv(surge_share, file.path(dir_out, "utah_surge_attribution.csv"))
message("Earlier statistical result: climate explained ~21% (2017) and ~39% (2025).")
