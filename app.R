#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# --- LIBRARIES ---
library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)
library(forecast)

# --- FUNCTIONS DOWNLOADING WEATHER DATA FROM API ---
get_coordinates <- function(city_name) {
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(city_name), "&format=json&limit=1")
  response <- httr::GET(url, user_agent("R (Shiny App)"))
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  if (length(result) > 0) {
    lat <- as.numeric(result$lat[1])
    lon <- as.numeric(result$lon[1])
    return(list(lat = lat, lon = lon))
  } else {
    return(NULL)
  }
}

get_weather_history <- function(lat, lon, start_date, end_date) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat,
    "&longitude=", lon,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&daily=temperature_2m_max",
    "&timezone=auto"
  )
  
  response <- httr::GET(url)
  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  if (!is.null(data$daily)) {
    return(data.frame(
      date = as.Date(data$daily$time),
      temp_max = data$daily$temperature_2m_max
    ))
  } else {
    return(NULL)
  }
}

# --- FUNCTION FOR ARIMA MODEL TEMPERATURE PREDICTION ---
make_forecast_arima <- function(df, h, var) {
  df_month_filtered <- df %>% dplyr::filter(month(date) == month(Sys.Date()))
  df_full_history <- df  
  
  if (nrow(df_month_filtered) < 10) {
    df_month_filtered <- df_full_history
  }
  
  ts_month <- ts(df_month_filtered[[var]], frequency = 12)
  ts_full <- ts(df_full_history[[var]], frequency = 12)
  
  arima_month_model <- tryCatch({ forecast::auto.arima(ts_month, stepwise = TRUE, approximation = TRUE) }, error = function(e) { return(NULL) })
  arima_season_model <- tryCatch({ forecast::auto.arima(ts_full, stepwise = TRUE, approximation = TRUE) }, error = function(e) { return(NULL) })
  
  if (is.null(arima_month_model) || is.null(arima_season_model)) {
    return(NULL)
  }
  
  fc_month <- forecast::forecast(arima_month_model, h = h)
  fc_season <- forecast::forecast(arima_season_model, h = h)
  
  dates_month <- seq.Date(from = max(df_month_filtered$date) + 1, by = "day", length.out = h)
  fc_season_filtered <- fc_season$mean[which(format(dates_month, "%m") == format(Sys.Date(), "%m"))]
  
  h <- min(length(fc_month$mean), length(fc_season_filtered))
  
  weight_month <- min(1, nrow(df_month_filtered) / 10)
  weight_season <- 1 - weight_month
  
  prediction_combined <- fc_month$mean[seq_len(h)] * weight_month + fc_season_filtered[seq_len(h)] * weight_season
  lower_combined <- fc_month$lower[seq_len(h), 1] * weight_month + fc_season$lower[seq_len(h), 1] * weight_season
  upper_combined <- fc_month$upper[seq_len(h), 1] * weight_month + fc_season$upper[seq_len(h), 1] * weight_season
  
  return(data.frame(
    date = seq.Date(from = max(df_month_filtered$date) + 1, by = "day", length.out = h),
    prediction = round(prediction_combined, 1),
    lower = round(lower_combined, 1),
    upper = round(upper_combined, 1)
  ))
}

# --- UI ---
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f4f4f4; font-family: Arial, sans-serif; }
    .title { font-size: 24px; font-weight: bold; color: #0078D7; }
    .sidebar { background-color: #ffffff; padding: 20px; border-radius: 10px; box-shadow: 0px 0px 10px #ccc; }
    .button { background-color: #0078D7; color: #ffffff; font-weight: bold; border-radius: 5px; padding: 10px; }
  "))),
  
  titlePanel(span("🌡 Temperature forecast", class = "title")),
  
  sidebarLayout(
    sidebarPanel(class = "sidebar",
                 textInput("city", "City:", value = "Warszawa"),
                 dateRangeInput("dates", "Historical scope:", start = Sys.Date() - years(10), end = Sys.Date()),
                 numericInput("forecast_days", "Days of forecast:", value = 7, min = 1, max = 30),
                 selectInput("tempType", "Select value:", 
                             choices = c("Mean" = "prediction", "Min" = "lower", "Max" = "upper")),
                 actionButton("go", "🔍 Generate forecast", class = "button"),
                 hr(),
                 textOutput("debug_text")
    ),
    
    mainPanel(
      plotOutput("forecastPlot", height = "500px"),  
      hr(),
      h4("📊 Historical data:"),
      DT::dataTableOutput("dataTable")
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  observeEvent(input$go, {
    req(input$city, input$dates)
    
    coordinates <- get_coordinates(input$city)
    if (is.null(coordinates)) return(NULL)
    
    history_data <- get_weather_history(coordinates$lat, coordinates$lon, as.character(input$dates[1]), as.character(input$dates[2]))
    if (is.null(history_data)) return(NULL)
    
    fc_arima <- make_forecast_arima(history_data, input$forecast_days, "temp_max")
    
    if (is.null(fc_arima)) {
      output$debug_text <- renderText("Can't generate forecast")
      return()
    }
    
    
    # GGPLOT 
    output$forecastPlot <- renderPlot({
      selected_temp <- switch(input$tempType, 
                              "prediction" = fc_arima$prediction,
                              "lower" = fc_arima$lower,
                              "upper" = fc_arima$upper)
      
      ggplot(fc_arima, aes(x = date, y = selected_temp)) +
        geom_line(color = "green", linewidth = 1) +
        geom_point(color = "blue", size = 2) +
        labs(title = paste("📈 Forecast:", input$tempType),
             x = "Data", y = "Temperature [°C]") +
        theme_minimal()
    })
    
    output$dataTable <- DT::renderDataTable({ history_data })
  })
}

# --- RUN ---
shinyApp(ui = ui, server = server)
