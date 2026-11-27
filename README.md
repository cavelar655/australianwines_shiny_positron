# Australian Wine Sales Forecasting

An interactive Shiny app for exploring and forecasting monthly Australian
wine sales by varietal.

**Live app:** https://connect.posit.cloud/cavelar/content/01a21f03-e6e9-403c-b1e4-c0f91328afab

## Data
`AustralianWines.csv` has monthly sales (thousands of litres) for six
varietals from Jan 1980 to Aug 1994. Two Rose values (Jul and Aug 1994)
are missing in the source file, marked `*`.

## What the app does
- Select varietals and a date range
- Overview, seasonality, and decomposition plots
- Forecasts from TSLM, ETS, and ARIMA with prediction intervals
- Model specifications and accuracy (training and validation)
- Extra feature: a combination forecast (average of ETS and ARIMA)

## Modeling choices
Describe the split, the models, and how you compare them.

## Run locally
1. Install packages: `install.packages(c("fpp3", "gt", "readr"))`
2. Put `AustralianWines.csv` in the same folder as `app.R`
3. Run `shiny::runApp()`

## Files
- `app.R`: the Shiny app
- `AustralianWines.csv`: the data
- `Assignment_5_1.qmd`: the analysis notebook

## Author
Cassandra Avelar
