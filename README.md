# Explaining West Nile Virus Risk in the Mountain West with Explainable AI

R code and results for a county-level study of severe (neuroinvasive) West Nile virus
in Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah and Wyoming, 2009-2025.
Three models (negative binomial, random forest, XGBoost) predict each county's yearly
rate, and SHAP and LIME explain what drives the predictions.


## Main findings

- The machine learning models were only slightly more accurate than the regression model
  (population-weighted error about 2.8 cases per 100,000 for all three).
- No model predicted the size of the 2021 epidemic.
- Location, last year's summer temperature and irrigated or farmed land drive the predictions.
- Irrigation and wetter springs raised predicted risk in every state.

## Repository contents

```
00_setup.R ... 15_report_figures.R   R scripts, run in order
app.R                                optional Shiny map (prototype)
data/input/                          small input files + DATA_SOURCES.md
output/                              all results (tables as .csv, models as .rds)
output/figures/                      all figures (report_fig*.png are used in the report)
```

## Scripts

| Script | Step |
|---|---|
| `00_setup.R` | Packages, paths, settings, model functions, figure labels |
| `01_extract_counties.R` | County boundaries and centers |
| `02_prepare_cases.R` | WNV cases per county and year |
| `03_population_annual.R` | Yearly county population |
| `04_climate_county_year.R` | Weather at weather stations -> county averages where people live |
| `05_extract_landcover_nlcd.R` | Land cover shares |
| `06_extract_ndvi_modis.R` | Vegetation greenness (NDVI) |
| `07_extract_irrigation_nass.R` | Irrigated land |
| `08_extract_elevation.R` | Elevation where people live |
| `09_build_analysis_dataset.R` | Joins everything (281 counties x 17 years) |
| `10_prepare_features.R` | Removes overlapping variables; sets up test groups |
| `11_fit_compare_models.R` | Fits and tests the three models |
| `12_shap_global.R` | SHAP values and overall importance |
| `13_shap_maps_places.R` | Maps and state comparisons |
| `14_local_explanations.R` | SHAP and LIME for Salt Lake 2017 and Cache 2025 |
| `15_report_figures.R` | Figures for the report |

## Results files (`output/`)

| File | What it contains |
|---|---|
| `model_metrics.csv` | Prediction error of each model in each test |
| `surge_year_ranks.csv` | Observed vs predicted rate and rank for every year |
| `cv_predictions.csv` | Every out-of-sample prediction |
| `shap_importance.csv` | Average effect size of each variable |
| `shap_long.csv` | SHAP value of every variable for every county-year |
| `benchmark_slopes_by_state.csv` | Effect of key variables by state |
| `top_irrigated_counties_shap.csv` | Counties where farmland raises risk most |
| `shap_lime_agreement.csv`, `lime_average_weights.csv` | SHAP vs LIME for two counties |
| `utah_surge_attribution.csv` | What explains Utah's 2017 and 2025 surges |
| `correlated_pairs.csv` | Variable pairs with correlation above 0.8 |
| `final_model.rds`, `shap_values.rds` | Fitted random forest and its SHAP values |

## How to run

1. Install R 4.3 or later. `00_setup.R` installs the packages it needs.
2. Put the input files in `data/input/` (see `data/input/DATA_SOURCES.md`).
3. Add free account details to your `.Renviron` file (`usethis::edit_r_environ()`), never to the code:
   ```
   EARTHDATA_USER=your_username
   EARTHDATA_PASSWORD=your_password
   NASS_API_KEY=your_key
   ```
4. Run the scripts in order from this folder.

## License

MIT - see `LICENSE`.
