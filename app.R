# =============================================================================
# app.R  -  Interactive decision-support tool (Shiny)
# Pick a year, see predicted WNV neuroinvasive risk by county, click a county
# to see WHY the model predicts that risk (SHAP waterfall + plain-language list).
# Run after scripts 01-12:   shiny::runApp()
# =============================================================================
source("00_setup.R")
library(shiny); library(leaflet); library(shapviz)

md  <- load_model_data(); dat <- md$data
fm  <- readRDS(file.path(dir_out, "final_model.rds")); feats <- fm$feats
sv  <- readRDS(file.path(dir_out, "shap_values.rds"))
S   <- get_shap_values(sv)

dat$pred <- if (fm$type == "RF") pred_rf(fm$model, dat, feats) else pred_xgb(fm$model, dat, feats)
counties <- st_read(file.path(dir_der, "counties.gpkg"), quiet = TRUE) |> st_transform(4326)

ui <- fluidPage(
  titlePanel("West Nile virus risk in the Mountain West: what drives each prediction?"),
  sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput("year", "Year", min = min(dat$year), max = max(dat$year),
                  value = 2025, step = 1, sep = ""),
      radioButtons("show", "Map shows",
                   c("Predicted rate" = "pred", "Observed rate" = "rate")),
      hr(),
      helpText("Rates are neuroinvasive cases per 100,000 people.",
               "Click a county to see what drives its prediction.",
               "Explanations describe the model, not proven causes.")
    ),
    mainPanel(width = 9,
      leafletOutput("map", height = 450),
      h4(textOutput("title")),
      fluidRow(column(7, plotOutput("waterfall", height = 380)),
               column(5, tableOutput("drivers")))
    )
  )
)

server <- function(input, output, session) {
  yr_data <- reactive({
    counties |> left_join(filter(dat, year == input$year) |>
                            select(fips, pred, rate, pop, neuro_cases), by = "fips")
  })
  sel <- reactiveVal("49035")   # Salt Lake County by default

  output$map <- renderLeaflet({
    d <- yr_data(); v <- d[[input$show]]
    pal <- colorBin("YlOrRd", domain = c(dat$pred, dat$rate),
                    bins = c(0, 0.5, 1, 2, 5, 10, 20, Inf))
    leaflet(d) |> addProviderTiles("CartoDB.Positron") |>
      addPolygons(layerId = ~fips, fillColor = pal(v), fillOpacity = 0.8,
                  color = "white", weight = 0.5,
                  label = ~sprintf("%s, %s: predicted %.1f, observed %.1f (%d cases)",
                                   county_name, state, pred, rate, neuro_cases)) |>
      addLegend(pal = pal, values = v, title = "per 100,000")
  })
  observeEvent(input$map_shape_click, sel(input$map_shape_click$id))

  row_id <- reactive(which(dat$fips == sel() & dat$year == input$year))

  output$title <- renderText({
    r <- row_id(); d <- dat[r, ]
    sprintf("%s, %s (%d): predicted %.1f vs observed %.1f per 100,000",
            d$county_name, d$state, d$year, d$pred, d$rate)
  })
  output$waterfall <- renderPlot(sv_waterfall(sv, row_id = row_id(), max_display = 10))
  output$drivers <- renderTable({
    r <- row_id()
    tibble(Variable = feats, Value = unlist(dat[r, feats]), SHAP = S[r, feats]) |>
      arrange(desc(abs(SHAP))) |> head(8) |>
      mutate(Effect = ifelse(SHAP > 0, "raises risk", "lowers risk"),
             Value = signif(Value, 3), SHAP = round(SHAP, 2))
  })
}

shinyApp(ui, server)
