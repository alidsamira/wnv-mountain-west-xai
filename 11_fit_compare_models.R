# =============================================================================
# 11_fit_compare_models.R  -  Q1: How accurate are RF and XGBoost vs NB?
# Validation : (a) train 2009-2021 / test 2022-2025
#              (b) spatial block CV (10 groups of neighbouring counties)
#              (c) leave-one-year-out (LOYO)
# Metrics    : RMSE and MAE of the rate per 100,000 (plain and
#              population-weighted), overall and for large vs small counties;
#              rank of the surge years 2017, 2021, 2025.
# Outputs    : output/cv_predictions.csv, output/model_metrics.csv,
#              output/surge_year_ranks.csv, output/final_model.rds
# =============================================================================
source("00_setup.R")
md <- load_model_data(); dat <- md$data; feats <- md$feats
models <- c("NB", "RF", "XGB")

# Standardise features for NB (keeps glm.nb stable); trees do not need it
dat_s <- dat |> mutate(across(all_of(feats), \(x) as.numeric(scale(x))))

# ---- 1. Out-of-sample predictions for every scheme and model ----------------
run_cv <- function(fold, scheme) {
  groups <- if (scheme == "temporal") "test" else sort(unique(fold))
  map(groups, \(g) {
    tr <- which(fold != g); te <- which(fold == g)
    map(models, \(m) {
      d <- if (m == "NB") dat_s else dat
      tibble(scheme = scheme, model = m, row = te,
             pred = fit_predict(m, d[tr, ], d[te, ], feats))
    }) |> bind_rows()
  }) |> bind_rows()
}

preds <- imap(md$folds, run_cv) |> bind_rows() |>
  left_join(dat |> mutate(row = row_number()) |>
              select(row, fips, state, county_name, year, pop, neuro_cases, rate),
            by = "row")
write_csv(preds, file.path(dir_out, "cv_predictions.csv"))

# ---- 2. Accuracy -------------------------------------------------------------
score <- function(d) d |> summarise(
  n = n(),
  RMSE = rmse(rate, pred), MAE = mae(rate, pred),
  RMSE_popw = sqrt(weighted.mean((rate - pred)^2, pop)),
  MAE_popw  = weighted.mean(abs(rate - pred), pop))

metrics <- bind_rows(
  preds |> group_by(scheme, model) |> score() |> mutate(counties = "all"),
  preds |> mutate(counties = ifelse(pop >= 1e5, "large (>=100k)", "small (<100k)")) |>
    group_by(scheme, model, counties) |> score()
) |> arrange(scheme, counties, RMSE)
print(metrics, n = Inf)
write_csv(metrics, file.path(dir_out, "model_metrics.csv"))

ggplot(filter(metrics, counties == "all") |>
         mutate(model = model_labels[model], scheme = scheme_labels[scheme]),
       aes(model, RMSE_popw, fill = model)) +
  geom_col(width = 0.7) + facet_wrap(~scheme) +
  geom_text(aes(label = sprintf("%.2f", RMSE_popw)), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = model_colours) +
  labs(y = "Prediction error (cases per 100,000)", x = NULL,
       title = "Out-of-sample prediction error by test",
       subtitle = "Population-weighted RMSE; lower is better") +
  theme_report() + theme(legend.position = "none")
ggsave(file.path(dir_out, "figures", "model_rmse.png"), width = 8, height = 4)

# ---- 3. Do models rank the surge years as high-risk? ----
yearly <- preds |> filter(scheme == "loyo") |>
  group_by(model, year) |>
  summarise(obs_rate  = sum(neuro_cases) / sum(pop) * 1e5,
            pred_rate = weighted.mean(pred, pop)) |>
  mutate(obs_rank = rank(-obs_rate), pred_rank = rank(-pred_rate))
surge <- filter(yearly, year %in% surge_years) |> arrange(model, year)
print(surge)
write_csv(yearly, file.path(dir_out, "surge_year_ranks.csv"))

bind_rows(yearly |> transmute(series = model_labels[model], year, rate = pred_rate),
          yearly |> filter(model == "NB") |> transmute(series = "Observed", year, rate = obs_rate)) |>
  ggplot(aes(year, rate, colour = series, linewidth = series == "Observed")) +
  geom_vline(xintercept = surge_years, linetype = 2, colour = "grey70") +
  geom_line() +
  scale_colour_manual(values = model_colours, name = NULL) +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.7), guide = "none") +
  labs(y = "Cases per 100,000 people", x = NULL,
       title = "Observed vs predicted West Nile rate, each year left out in turn",
       subtitle = "Dashed lines: surge years 2017, 2021 and 2025") +
  theme_report() + theme(legend.position = "top")
ggsave(file.path(dir_out, "figures", "surge_years.png"), width = 8, height = 4)

# ---- 4. Final model for explanation (trained on all years) -----------------
best_model <- "RF"
final <- if (best_model == "RF") fit_rf(dat, feats) else fit_xgb(dat, feats)
saveRDS(list(model = final, type = best_model, feats = feats),
        file.path(dir_out, "final_model.rds"))
message("Saved final ", best_model, " model.")
