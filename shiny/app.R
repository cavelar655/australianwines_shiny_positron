library(shiny)
library(fpp3)
library(readr)

# ---- Data ("*" marks missing values; Rose has 2, in the last months) ----
wines_ts <- read_csv("AustralianWines.csv", na = "*", show_col_types = FALSE) |>
  mutate(Month = yearmonth(my(Month))) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

varietals <- unique(wines_ts$Varietal)
dates     <- as.Date(range(wines_ts$Month))

# ---- UI ----
ui <- fluidPage(
  titlePanel("Australian Wine Sales"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("varietal", "Varietal(s)", varietals, selected = varietals[1:2]),
      dateRangeInput("dates", "Date range", start = dates[1], end = dates[2],
                     min = dates[1], max = dates[2], format = "yyyy-mm"),
      sliderInput("holdout", "Validation window (months)", 6, 24, 12),
      sliderInput("horizon", "Forecast horizon (months)", 6, 36, 12)
    ),
    mainPanel(tabsetPanel(
      tabPanel("Overview",      plotOutput("overview")),
      tabPanel("Seasonality",   plotOutput("season")),
      tabPanel("Decomposition", plotOutput("decomp")),
      tabPanel("Forecasts",     plotOutput("forecast", height = "600px")),
      tabPanel("Models",        tableOutput("specs")),
      tabPanel("Accuracy",      tableOutput("acc"))
    ))
  )
)

# ---- Server ----
server <- function(input, output) {

  data <- reactive({
    req(input$varietal)
    wines_ts |>
      filter(Varietal %in% input$varietal,
             Month >= yearmonth(input$dates[1]),
             Month <= yearmonth(input$dates[2]))
  })

  # train/validation split: the last `holdout` months are held out
  split <- reactive(max(data()$Month) - input$holdout)
  train <- reactive(filter(data(), Month <= split()))

  fit <- reactive({
    validate(need(n_distinct(train()$Month) >= 36,
                  "Select a longer date range (training needs at least 36 months)."))
    train() |>
      model(tslm  = TSLM(Sales ~ trend() + season()),
            ets   = ETS(Sales),
            arima = ARIMA(Sales)) |>
      mutate(combo = (ets + arima) / 2)   # extra: combination forecast
  })

  fc <- reactive(forecast(fit(), h = input$horizon))

  # ---- Exploration plots ----
  output$overview <- renderPlot(
    autoplot(data(), Sales) +
      facet_wrap(~Varietal, scales = "free_y") +
      theme(legend.position = "none")
  )

  output$season <- renderPlot(gg_season(data(), Sales))

  output$decomp <- renderPlot({
    validate(need(!anyNA(data()$Sales),
                  "Selection has missing values (Rose, mid-1994): end the date range before Jul 1994."),
             need(n_distinct(data()$Month) >= 24, "Select at least 24 months."))
    data() |>
      model(STL(Sales ~ season(window = "periodic"))) |>
      components() |>
      autoplot()
  })

  # ---- Forecast plot ----
  output$forecast <- renderPlot(
    autoplot(fc(), filter(data(), Month > split() - 48), level = 95) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1)
  )

  # ---- Model specifications ----
  output$specs <- renderTable({
    as_tibble(fit()) |>
      pivot_longer(-Varietal, names_to = "Model", values_to = "Specification") |>
      mutate(Specification = vapply(Specification, format, character(1)))
  })

  # ---- Accuracy: training vs validation ----
  output$acc <- renderTable({
    valid_fc <- filter(fc(), Month <= max(data()$Month))
    bind_rows(
      accuracy(fit())               |> mutate(Set = "Training"),
      accuracy(valid_fc, data())    |> mutate(Set = "Validation")
    ) |>
      select(Varietal, Model = .model, Set, RMSE, MAE, MAPE) |>
      arrange(Varietal, Set, RMSE)
  }, digits = 1)
}

shinyApp(ui, server)