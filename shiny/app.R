library(shiny)
library(bslib)
library(fpp3)
library(readr)
library(gt)

message("---- ARIMA DIAGNOSTICS ----")
for (p in c("fable", "fabletools", "feasts", "urca", "tsibble", "forecast")) {
  message(p, ": ",
          if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "NOT INSTALLED")
}
message("R: ", R.version.string)
message("ndiffs test: ",
        tryCatch(feasts::unitroot_ndiffs(rnorm(60)), error = function(e) paste("ERROR:", conditionMessage(e))))
message("nsdiffs test: ",
        tryCatch(feasts::unitroot_nsdiffs(ts(rnorm(60), frequency = 12)), error = function(e) paste("ERROR:", conditionMessage(e))))
message("---------------------------")



# ---------------------------------------------------------------------------
# Data import (from the .qmd) -- AustralianWines.csv must sit next to app.R
# ---------------------------------------------------------------------------
wines_ts <- read_csv("AustralianWines.csv", na = "*", show_col_types = FALSE) |>
  mutate(Month = yearmonth(my(Month))) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  as_tsibble(index = Month, key = Varietal)

varietals  <- sort(unique(wines_ts$Varietal))
first_date <- as.Date(min(wines_ts$Month))
last_date  <- as.Date(max(wines_ts$Month))
# Default train cutoff = 1 year before the last observation
default_cutoff <- as.Date(max(wines_ts$Month) - 12)

model_choices <- c("All models" = "all",
                   "TSLM (trend + season)" = "tslm",
                   "ARIMA" = "arima",
                   "ETS" = "ets")

# Render a gt table as raw HTML (avoids gt_output's shared input/output ID error)
gt_html <- function(tbl) HTML(as_raw_html(tbl, inline_css = TRUE))

metric_table <- function(acc) {
  acc |>
    as_tibble() |>
    select(Varietal, .model, RMSE, MAE, MAPE) |>
    arrange(.model, RMSE) |>
    gt() |>
    fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2)
}

message("---- ARIMA FIT TEST ----")
test_fit <- tryCatch({
  x <- wines_ts |> filter(Varietal == "Red", Month <= yearmonth("1993 Aug"))
  f <- model(x, arima = ARIMA(Sales))
  message("Model summary: ", model_sum(f$arima[[1]]))
  message(paste(capture.output(str(f$arima[[1]], max.level = 3)), collapse = "\n"))
}, error = function(e) message("OUTER ERROR: ", conditionMessage(e)))
message("------------------------")

# ---------------------------------------------------------------------------
# UI -- three tabs, each with its own inputs
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "Australian Wine Forecast",
  theme = bs_theme(version = 5),

  # ---- Tab 1: Visualization -------------------------------------------------
  nav_panel(
    "Visualization",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        checkboxGroupInput("varietal", "Select Varietals:",
                           choices = varietals, selected = varietals),
        dateRangeInput(
          "dates", "Training/Forecast Range:",
          start = default_cutoff, end = last_date,
          min = first_date, max = as.Date(max(wines_ts$Month) + 24),
          format = "yyyy-mm-dd", startview = "year"
        ),
        p("First date: Training cutoff (default 1 year before end)"),
        p("Second date: End of forecast horizon")
      ),
      card(card_header("Wine Data Visualization"),
           plotOutput("ts_plot", height = "700px"))
    )
  ),

  # ---- Tab 2: Model Building ------------------------------------------------
  nav_panel(
    "Model Building",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        input_switch("show_specs", "Show model specifications", value = FALSE),
        input_switch("show_train_acc", "Show training accuracy", value = FALSE)
      ),
      uiOutput("model_tab_msg"),
      conditionalPanel(
        "input.show_specs",
        card(card_header("Model specifications"), uiOutput("spec_tbl"))
      ),
      conditionalPanel(
        "input.show_train_acc",
        card(card_header("Training accuracy"), uiOutput("train_acc_tbl"))
      )
    )
  ),

  # ---- Tab 3: Forecast ------------------------------------------------------
  nav_panel(
    "Forecast",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput("fc_model", "Model for the forecast", choices = model_choices,
                    selected = "all")
      ),
      card(card_header("Forecast accuracy on held-out months"),
           uiOutput("fc_acc_tbl")),
      card(card_header("Forecast visualization"),
           plotOutput("fc_plot", height = "650px"))
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  cutoff  <- reactive(yearmonth(input$dates[1]))
  fc_end  <- reactive(yearmonth(input$dates[2]))

  selected <- reactive({
    validate(need(length(input$varietal) > 0, "Select at least one varietal."))
    input$varietal
  })

  # Train / test split from the cutoff date
  wines_trn <- reactive({
    validate(need(!is.na(input$dates[1]) && !is.na(input$dates[2]),
                  "Choose a train cutoff and a forecast end date."),
             need(cutoff() < fc_end(),
                  "The forecast end must be after the train cutoff."))
    out <- wines_ts |> filter(Month <= cutoff())
    validate(need(nrow(out) / length(varietals) >= 24,
                  "Training data needs at least 24 months. Move the cutoff later."))
    out
  })

  wines_tst <- reactive(wines_ts |> filter(Month > cutoff()))

  # Fit all six varietals once per cutoff; filter for display afterwards
  wines_fit <- reactive({
    withProgress(message = "Fitting models...", value = 0.5, {
      wines_trn() |>
        model(
          tslm  = TSLM(Sales ~ trend() + season()),
          arima = ARIMA(Sales),
          ets   = ETS(Sales)
        )
    })
  })

  wines_fc <- reactive({
    h <- as.integer(as.numeric(fc_end()) - as.numeric(cutoff()))
    wines_fit() |> forecast(h = h)
  })

  # ---- Tab 1 ----
  output$ts_plot <- renderPlot({
    selected()
    wines_ts |>
      filter(Varietal %in% input$varietal) |>
      ggplot(aes(x = Month, y = Sales, colour = Varietal)) +
      geom_line() +
      geom_vline(xintercept = as.Date(cutoff()), linetype = "dashed",
                 colour = "red") +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      labs(title = "Wine Data with Training Cutoff",
           x = "Month", y = "Sales (thousands of litres)",
           caption = "Red dashed line indicates training cutoff") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            strip.text = element_text(face = "bold", size = 11),
            panel.grid = element_blank(),
            legend.position = "right")
  })

  # ---- Tab 2 ----
  output$model_tab_msg <- renderUI({
    if (!isTruthy(input$show_specs) && !isTruthy(input$show_train_acc)) {
      p(class = "text-muted",
        "Turn on a toggle in the sidebar to see the model specifications or ",
        "the training accuracy.")
    }
  })

  output$spec_tbl <- renderUI({
    selected()
    fit <- wines_fit() |> filter(Varietal %in% input$varietal)
    tb  <- as_tibble(fit)
    for (col in setdiff(names(tb), "Varietal")) {
      tb[[col]] <- vapply(tb[[col]], function(m)
        tryCatch(model_sum(m), error = function(e) NA_character_), character(1))
    }
    gt_html(gt(tb))
  })

  output$train_acc_tbl <- renderUI({
    selected()
    wines_fit() |>
      filter(Varietal %in% input$varietal) |>
      accuracy() |>
      metric_table() |>
      gt_html()
  })

  # ---- Tab 3 ----
  fc_selected <- reactive({
    selected()
    fc <- wines_fc() |> filter(Varietal %in% input$varietal)
    if (input$fc_model != "all") fc <- fc |> filter(.model == input$fc_model)
    fc
  })

  output$fc_acc_tbl <- renderUI({
    fc  <- fc_selected()
    tst <- wines_tst() |> filter(Varietal %in% input$varietal)
    validate(need(nrow(tst) > 0,
                  "No observed data after the cutoff to score the forecasts against."))
    fc |>
      filter(Month <= max(tst$Month)) |>
      accuracy(tst) |>
      metric_table() |>
      gt_html()
  })

  output$fc_plot <- renderPlot({
    fc_selected() |>
      autoplot(filter(wines_ts, Month >= yearmonth("1992 Jan"),
                      Varietal %in% input$varietal), level = 95) +
      labs(title = "Australian Wine Sales Forecasts",
           y = "Sales (in thousands of litres)", x = "Year") +
      guides(colour = guide_legend(title = "Forecast Model")) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 2) +
      theme_minimal()
  })
}

shinyApp(ui, server)