# =============================================================================
# 15_report_figures.R  -  All figures used in the final report
# Reads only saved results (output/ and data/derived/), so it runs in seconds
# and does not refit any model. Uses readable labels from 00_setup.R.
# Output: output/figures/report_fig01_... to report_fig11_...png
# =============================================================================
source("00_setup.R")
library(patchwork)

out <- function(n, name, p, w = 9, h = 5)
  ggsave(file.path(dir_out, "figures", sprintf("report_fig%02d_%s.png", n, name)),
         p, width = w, height = h, dpi = 200, bg = "white")

rd <- function(f) read_csv(file.path(dir_out, f), show_col_types = FALSE,
                           col_types = cols(.default = col_guess(), fips = col_character()))
shap_long <- rd("shap_long.csv")
metrics   <- rd("model_metrics.csv")
yearly    <- rd("surge_year_ranks.csv")
bench     <- rd("benchmark_slopes_by_state.csv")
lime_avg  <- rd("lime_average_weights.csv")
baseline  <- readRDS(file.path(dir_out, "shap_values.rds"))$baseline
counties  <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE)
blocks    <- read_csv(file.path(dir_der, "spatial_blocks.csv"), col_types = cols(fips = "c"))

group_of <- function(f) case_when(
  str_detect(f, "ndvi") ~ "Vegetation",
  str_detect(f, "crop|pasture|wetland|developed|water|irrig") ~ "Land cover & irrigation",
  f %in% c("lat", "lon", "elev_pop_m") ~ "Location",
  TRUE ~ "Climate")
group_colours <- c("Climate" = "#4C78A8", "Land cover & irrigation" = "#F58518",
                   "Location" = "#9D9DA1", "Vegetation" = "#54A24B")

# ---- Fig 1: spatial cross-validation blocks ---------------------------------
p1 <- counties |> left_join(blocks, by = "fips") |>
  ggplot() + geom_sf(aes(fill = factor(block)), colour = "white", linewidth = 0.15) +
  scale_fill_brewer(palette = "Set3", name = "Test block") +
  labs(title = "Counties grouped into 10 neighbouring blocks for spatial testing") +
  theme_void() + theme(plot.title = element_text(face = "bold"))
out(1, "spatial_blocks", p1, w = 7, h = 6)

# ---- Fig 2: prediction error by model and test -------------------------------
p2 <- metrics |> filter(counties == "all") |>
  mutate(model = factor(model_labels[model], levels = model_labels),
         scheme = factor(scheme_labels[scheme], levels = scheme_labels)) |>
  ggplot(aes(model, RMSE_popw, fill = model)) +
  scale_x_discrete(labels = \(x) str_wrap(x, 10)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", RMSE_popw)), vjust = -0.4, size = 3.6) +
  facet_wrap(~scheme) + scale_fill_manual(values = model_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "All three models make similar errors",
       subtitle = "Population-weighted prediction error (RMSE); lower is better",
       x = NULL, y = "Cases per 100,000 people") +
  theme_report() + theme(legend.position = "none")
out(2, "model_error", p2, w = 10, h = 4.5)

# ---- Fig 3: observed vs predicted by year -----------------------------------
d3 <- bind_rows(
  yearly |> transmute(series = model_labels[model], year, rate = pred_rate),
  yearly |> filter(model == "NB") |> transmute(series = "Observed", year, rate = obs_rate)) |>
  mutate(series = factor(series, levels = c("Observed", model_labels)))
p3 <- ggplot(d3, aes(year, rate, colour = series, linewidth = series == "Observed")) +
  annotate("rect", xmin = surge_years - 0.4, xmax = surge_years + 0.4,
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_line() + geom_point(size = 1.4) +
  scale_colour_manual(values = model_colours, name = NULL) +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.7), guide = "none") +
  scale_x_continuous(breaks = seq(2009, 2025, 2)) +
  labs(title = "The models catch the 2021 rise but not its size",
       subtitle = "Each year predicted by a model that never saw it; shaded = surge years",
       x = NULL, y = "Cases per 100,000 people") +
  theme_report() + theme(legend.position = "top")
out(3, "surge_years", p3, w = 9, h = 4.5)

# ---- Fig 4: importance --------------------------------------------------------
imp <- shap_long |> group_by(feature) |> summarise(m = mean(abs(shap))) |>
  arrange(desc(m)) |> head(15) |>
  mutate(label = nice_var(feature), group = group_of(feature))
p4 <- ggplot(imp, aes(m, reorder(label, m), fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", m)), hjust = -0.15, size = 3.3) +
  scale_fill_manual(values = group_colours, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "What the model relies on most",
       subtitle = "Average size of each variable's effect (mean |SHAP|), top 15",
       x = "Cases per 100,000 people", y = NULL) +
  theme_report() + theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))
out(4, "importance", p4, w = 9, h = 6)

# ---- Fig 5: summary (beeswarm-style) -------------------------------------------
top15 <- imp$feature
d5 <- shap_long |> filter(feature %in% top15) |>
  group_by(feature) |> mutate(level = percent_rank(value)) |> ungroup() |>
  mutate(label = factor(nice_var(feature), levels = rev(nice_var(top15))))
set.seed(1)
p5 <- ggplot(d5, aes(shap, label, colour = level)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_jitter(height = 0.25, width = 0, size = 0.6, alpha = 0.6) +
  scale_colour_viridis_c(option = "plasma", breaks = c(0, 1), labels = c("Low", "High"),
                         name = "Variable\nvalue") +
  labs(title = "Direction of each effect",
       subtitle = "Each dot is one county-year; right of zero raises predicted risk",
       x = "Effect on predicted rate (cases per 100,000)", y = NULL) +
  theme_report()
out(5, "summary", p5, w = 9, h = 6.5)

# ---- Fig 6: dependence, top 6 ----------------------------------------------------
top6 <- head(imp$feature, 6)
p6 <- shap_long |> filter(feature %in% top6) |>
  mutate(label = factor(nice_var(feature), levels = nice_var(top6))) |>
  ggplot(aes(value, shap)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(alpha = 0.15, size = 0.7, colour = "#4C78A8") +
  stat_summary_bin(fun = median, bins = 15, geom = "line", colour = "#E45756", linewidth = 0.9) +
  facet_wrap(~label, scales = "free", ncol = 3) +
  labs(title = "How predicted risk changes as each variable increases",
       subtitle = "Six most important variables; red line = median effect within bins of the variable",
       x = "Variable value", y = "Effect on predicted rate") +
  theme_report() + theme(strip.text = element_text(face = "bold", size = 9))
out(6, "dependence", p6, w = 11, h = 6.5)

# ---- Fig 7: Utah vs Arizona, summer heat ---------------------------------------
p7 <- shap_long |> filter(feature == "tmean_summer_anom", state %in% c("UT", "AZ")) |>
  mutate(state = state_labels[state]) |>
  ggplot(aes(value, shap, colour = state)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  scale_colour_manual(values = c(Arizona = "#d95f02", Utah = "#1b9e77"), name = NULL) +
  labs(title = "Unusual summers raise predicted risk a little, slightly more in Utah",
       x = "Summer temperature anomaly (\u00b0C; 0 = normal)",
       y = "Effect on predicted rate (cases per 100,000)") +
  theme_report() + theme(legend.position = "top")
out(7, "utah_vs_arizona", p7, w = 8, h = 5)

# ---- Fig 8: effect by state -------------------------------------------------------
p8 <- bench |> mutate(feature = nice_var(feature), state = state_labels[state]) |>
  ggplot(aes(state, slope_per_sd, fill = slope_per_sd > 0)) +
  geom_col(width = 0.75) + geom_hline(yintercept = 0) +
  facet_wrap(~feature, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#C44E52", `FALSE` = "#4C78A8"), guide = "none") +
  labs(title = "How each variable's effect differs between states",
       subtitle = "Change in predicted rate per 1 standard deviation increase; red raises, blue lowers risk",
       x = NULL, y = "Cases per 100,000 per SD") +
  theme_report() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))
out(8, "effect_by_state", p8, w = 10, h = 6.5)

# ---- Fig 9: maps --------------------------------------------------------------------
map_vars <- c("lon", "tmean_summer_anom_lag1", "irrigated_share",
              "pasture_share", "tmean_summer_anom", "ndvi_summer")
map_one <- function(v) {
  cs <- shap_long |> filter(feature == v) |> group_by(fips) |> summarise(ms = mean(shap))
  lim <- max(abs(cs$ms))
  counties |> left_join(cs, by = "fips") |>
    ggplot() + geom_sf(aes(fill = ms), colour = "grey85", linewidth = 0.08) +
    scale_fill_distiller(palette = "RdBu", limits = c(-lim, lim), name = NULL) +
    labs(title = nice_var(v)) + theme_void() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          legend.key.height = unit(0.5, "cm"))
}
p9 <- wrap_plots(map(map_vars, map_one), ncol = 3) +
  plot_annotation(title = "Where each variable raises (red) or lowers (blue) predicted risk",
                  subtitle = "County average effect, 2009-2025, cases per 100,000; each map has its own scale",
                  theme = theme(plot.title = element_text(face = "bold")))
out(9, "maps", p9, w = 12, h = 8)

# ---- Fig 10: waterfalls ---------------------------------------------------------------
cases <- tibble(fips = c("49035", "49005"), year = c(2017, 2025),
                label = c("Salt Lake County, 2017", "Cache County, 2025"))
waterfall <- function(f, y, ttl) {
  x <- shap_long |> filter(fips == f, year == y) |> arrange(desc(abs(shap)))
  w <- bind_rows(head(x, 8) |> transmute(lbl = nice_var(feature), shap),
                 tibble(lbl = paste("Other", nrow(x) - 8, "variables"), shap = sum(x$shap[-(1:8)]))) |>
    mutate(end = baseline + cumsum(shap), start = end - shap, id = row_number(),
           dir = ifelse(shap > 0, "Raises", "Lowers"))
  pred <- baseline + sum(x$shap)
  ggplot(w) +
    geom_rect(aes(xmin = pmin(start, end), xmax = pmax(start, end),
                  ymin = id - 0.4, ymax = id + 0.4, fill = dir)) +
    geom_text(aes(x = pmax(start, end), y = id, label = sprintf("%+.2f", shap)),
              hjust = -0.15, size = 3.2) +
    geom_vline(xintercept = baseline, linetype = 2, colour = "grey40") +
    geom_vline(xintercept = pred) +
    scale_y_reverse(breaks = w$id, labels = w$lbl) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    scale_fill_manual(values = c(Raises = "#C44E52", Lowers = "#4C78A8"), guide = "none") +
    labs(title = ttl,
         subtitle = sprintf("Starts at the regional average (%.2f, dashed) and ends at the prediction (%.2f, solid)",
                            baseline, pred),
         x = "Predicted rate (cases per 100,000)", y = NULL) +
    theme_report()
}
p10 <- wrap_plots(pmap(cases, \(fips, year, label) waterfall(fips, year, label)), ncol = 1)
out(10, "waterfalls", p10, w = 9, h = 8.5)

# ---- Fig 11: SHAP vs LIME ---------------------------------------------------------------
row_key <- shap_long |> distinct(fips, year) |> mutate(row = row_number())
p11 <- lime_avg |> left_join(row_key, by = "row") |>
  inner_join(cases, by = c("fips", "year")) |>
  left_join(shap_long |> select(fips, year, feature, shap), by = c("fips", "year", "feature")) |>
  group_by(label) |>
  mutate(SHAP = shap / max(abs(shap)), LIME = lime_weight / max(abs(lime_weight))) |>
  ungroup() |> pivot_longer(c(SHAP, LIME), names_to = "method") |>
  mutate(label = factor(label, levels = cases$label), var = nice_var(feature)) |>
  ggplot(aes(value, reorder(var, value), fill = method)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40") +
  facet_wrap(~label, scales = "free_y") +
  scale_fill_manual(values = c(SHAP = "#4C78A8", LIME = "#BAB0AC"), name = NULL) +
  labs(title = "SHAP and LIME agree on which variables matter, less on direction",
       subtitle = "Each method scaled so its largest effect = 1",
       x = "Scaled effect", y = NULL) +
  theme_report() + theme(legend.position = "top")
out(11, "shap_vs_lime", p11, w = 11, h = 5.5)

message("Report figures written to ", file.path(dir_out, "figures"))
