# =============================================================================
# 12_shap_global.R  -  Q2 (part 1): which variables drive predictions?
# Exact TreeSHAP for the final model on every county-year, then:
#   - ranking by mean |SHAP|  (bar plot)
#   - SHAP summary (beeswarm) plot
#   - dependence plots for the top variables
# Outputs: output/shap_values.rds (shapviz object)
#          output/shap_long.csv   (fips, year, feature, value, shap)
#          output/shap_importance.csv, figures
# Units: RF -> SHAP in cases per 100,000.
#        XGB (Poisson) -> SHAP on the log-rate scale.
# =============================================================================
source("00_setup.R")
library(shapviz); library(patchwork)
md  <- load_model_data(); dat <- md$data
fm  <- readRDS(file.path(dir_out, "final_model.rds")); feats <- fm$feats
X   <- as.data.frame(dat[, feats])

# ---- 1. SHAP values ---------------------------------------------------------
if (fm$type == "RF") {
  unified <- treeshap::ranger.unify(fm$model, X)
  ts      <- treeshap::treeshap(unified, X, verbose = TRUE)
  S       <- as.matrix(ts$shaps)
  pred    <- pred_rf(fm$model, dat, feats)
  baseline <- mean(pred - rowSums(S))   # so that baseline + sum(SHAP) = prediction
  sv <- shapviz(S, X = X, baseline = baseline)
} else {
  sv <- shapviz(fm$model$booster, X_pred = as.matrix(X), X = X)
}
saveRDS(sv, file.path(dir_out, "shap_values.rds"))

shap_long <- as_tibble(get_shap_values(sv)) |>
  mutate(fips = dat$fips, year = dat$year, state = dat$state) |>
  pivot_longer(all_of(feats), names_to = "feature", values_to = "shap") |>
  left_join(X |> mutate(fips = dat$fips, year = dat$year) |>
              pivot_longer(all_of(feats), names_to = "feature", values_to = "value"),
            by = c("fips", "year", "feature"))
write_csv(shap_long, file.path(dir_out, "shap_long.csv"))

# ---- 2. Global importance ---------------------------------------------------
imp <- shap_long |> group_by(feature) |>
  summarise(mean_abs_shap = mean(abs(shap))) |> arrange(desc(mean_abs_shap))
print(imp, n = Inf)
write_csv(imp, file.path(dir_out, "shap_importance.csv"))

fig <- \(name, p, w = 7, h = 6) ggsave(file.path(dir_out, "figures", name), p, width = w, height = h)
svp <- pretty_sv(sv)                      # same values, readable names
fig("shap_importance_bar.png", sv_importance(svp, kind = "bar", max_display = 15) +
      labs(x = "Average effect on predicted rate (cases per 100,000)") + theme_report(), w = 9)
fig("shap_summary_beeswarm.png", sv_importance(svp, kind = "beeswarm", max_display = 15) +
      labs(x = "Effect on predicted rate (cases per 100,000)") + theme_report(), w = 9)

# ---- 3. Dependence plots for the top 6 variables ----------------------------
top <- head(imp$feature, 6)
fig("shap_dependence_top6.png",
    sv_dependence(svp, v = nice_var(top), color_var = NULL) & theme_report() &
      labs(y = "Effect on predicted rate"),
    w = 12, h = 8)
