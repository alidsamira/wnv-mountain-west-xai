# =============================================================================
# 13_shap_maps_places.R  -  Q2 (part 2): where do variables matter, and do
# their effects differ between places (Utah vs Arizona)?
#   1. Maps of county mean SHAP for key variables
#   2. Utah vs Arizona dependence plots for summer temperature
#   3. SHAP-value slopes by state for the benchmark variables, compared with
#      the earlier statistical results
# =============================================================================
source("00_setup.R")
library(patchwork)

shap_long <- read_csv(file.path(dir_out, "shap_long.csv"), col_types = cols(fips = "c"))
imp       <- read_csv(file.path(dir_out, "shap_importance.csv"))
counties  <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE)
feats     <- unique(shap_long$feature)

# ---- EDIT: names of your benchmark variables --------------------------------
pick <- function(name, pattern) if (name %in% feats) name else
  feats[str_detect(feats, pattern)][1]
temp_var   <- pick("tmean_summer_anom", "^t.*summer|summer.*temp")
spring_var <- pick("ppt_spring_anom",   "ppt.*spring|spring.*pr")
irrig_var  <- pick("irrigated_share",   "irrig")
ndvi_var   <- pick("ndvi_summer",       "ndvi")
message("Benchmark variables: ", paste(c(temp_var, spring_var, irrig_var, ndvi_var), collapse = ", "))

# ---- 1. County maps of mean SHAP -------------------------------------------
map_vars <- unique(na.omit(c(head(imp$feature, 4), temp_var, ndvi_var, irrig_var)))

county_shap <- shap_long |> filter(feature %in% map_vars) |>
  group_by(fips, feature) |> summarise(mean_shap = mean(shap))

map_one <- function(v) {
  d <- counties |> left_join(filter(county_shap, feature == v), by = "fips")
  lim <- max(abs(d$mean_shap), na.rm = TRUE)
  ggplot(d) + geom_sf(aes(fill = mean_shap), colour = "grey80", linewidth = 0.1) +
    scale_fill_distiller(palette = "RdBu", limits = c(-lim, lim), name = "Effect") +
    labs(title = nice_var(v)) + theme_void() +
    theme(plot.title = element_text(size = 11, face = "bold"))
}
p_maps <- wrap_plots(map(map_vars, map_one), ncol = 3) +
  plot_annotation(title = "Where each variable raises or lowers predicted risk (county average, 2009-2025)",
                  subtitle = "Red raises and blue lowers the predicted rate (cases per 100,000); each map has its own scale")
ggsave(file.path(dir_out, "figures", "shap_maps.png"), p_maps, width = 14, height = 9)

# ---- 2. Utah vs Arizona: temperature effect --------------------------------
ua <- shap_long |> filter(feature == temp_var, state %in% c("UT", "AZ"))
p_ua <- ggplot(ua |> mutate(state = state_labels[state]), aes(value, shap, colour = state)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = TRUE) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_colour_manual(values = c(Arizona = "#d95f02", Utah = "#1b9e77"), name = NULL) +
  labs(x = nice_var(temp_var), y = "Effect on predicted rate (cases per 100,000)",
       title = "Utah vs Arizona: how summer heat changes predicted risk") +
  theme_report() + theme(legend.position = "top")
ggsave(file.path(dir_out, "figures", "shap_dependence_UT_vs_AZ.png"), p_ua, width = 8, height = 5)

# ---- 3. Benchmark check: SHAP slope per SD of each variable, by state -------
bench <- shap_long |>
  filter(feature %in% c(temp_var, spring_var, irrig_var, ndvi_var)) |>
  group_by(feature) |> mutate(value_sd = as.numeric(scale(value))) |>
  group_by(feature, state) |>
  summarise(slope_per_sd = coef(lm(shap ~ value_sd))[2],
            mean_abs_shap = mean(abs(shap))) |>
  arrange(feature, desc(slope_per_sd))
print(bench, n = Inf)
write_csv(bench, file.path(dir_out, "benchmark_slopes_by_state.csv"))

p_b <- ggplot(bench |> mutate(feature = nice_var(feature)),
              aes(state, slope_per_sd, fill = slope_per_sd > 0)) +
  geom_col() + facet_wrap(~feature, scales = "free_y") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c(`TRUE` = "#b2182b", `FALSE` = "#2166ac"), guide = "none") +
  labs(y = "Change in effect per 1 SD increase", x = NULL,
       title = "How each variable's effect differs by state",
       subtitle = "Red: higher values raise predicted risk; blue: they lower it") +
  theme_report()
ggsave(file.path(dir_out, "figures", "benchmark_slopes.png"), p_b, width = 10, height = 6)

# Irrigated plains: top 15 counties where irrigation/cropland raise risk most
lc_vars <- feats[str_detect(feats, "irrig|crop|pasture")]
top_lc <- shap_long |> filter(feature %in% lc_vars) |>
  group_by(fips, state) |> summarise(landcover_shap = sum(shap) / n_distinct(year)) |>
  arrange(desc(landcover_shap)) |> head(15) |>
  left_join(st_drop_geometry(counties) |> select(fips, county_name), by = "fips")
print(top_lc)
write_csv(top_lc, file.path(dir_out, "top_irrigated_counties_shap.csv"))
