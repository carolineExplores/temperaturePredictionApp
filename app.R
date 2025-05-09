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
library(plotly)
library(lutz)

# --- FUNKCJE ---
get_coordinates <- function(city_name) {
  url <- paste0("https://geocoding-api.open-meteo.com/v1/search?name=", URLencode(city_name), "&count=1")
  tryCatch({
    response <- httr::GET(url)
    if (httr::http_error(response)) {
      showNotification(paste("Error:", httr::http_status(response)$message), type = "error")
      return(NULL)
    }
    result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    if (is.null(result$results) || length(result$results) == 0) {
      showNotification("City not found.", type = "error")
      return(NULL)
    }
    if (is.data.frame(result$results)) {
      lat <- result$results$latitude[1]
      lon <- result$results$longitude[1]
    } else if (is.list(result$results) && length(result$results) > 0) {
      lat <- result$results[[1]]$latitude
      lon <- result$results[[1]]$longitude
    } else {
      showNotification("Unexpected response format from geocoding API", type = "error")
      return(NULL)
    }
    if (is.null(lat) || is.null(lon)) {
      showNotification("Unable to extract coordinates from the response", type = "error")
      return(NULL)
    }
    return(list(lat = lat, lon = lon))
  }, error = function(e) {
    showNotification(paste("Error in get_coordinates:", e$message), type = "error")
    return(NULL)
  })
}

get_timezone <- function(lat, lon) {
  tryCatch({
    lutz::tz_lookup_coords(lat, lon, method = "accurate")
  }, error = function(e) {
    "UTC"
  })
}

get_weather_history <- function(lat, lon, forecast_start, days_back = 14, years_back = 10) {
  all_data <- list()
  for (i in 0:years_back) {
    base_day <- forecast_start - years(i) - days(1)
    start_date <- base_day - days(days_back - 1)
    end_date <- base_day
    url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&daily=temperature_2m_max",
      "&timezone=auto"
    )
    response <- tryCatch({
      GET(url)
    }, error = function(e) {
      showNotification(paste("Error in API:", e$message), type = "error")
      return(NULL)
    })
    if (is.null(response) || http_error(response)) {
      showNotification(paste("Error fetching historical data for", format(start_date)), type = "warning")
      next
    }
    data <- tryCatch({
      fromJSON(content(response, "text", encoding = "UTF-8"))
    }, error = function(e) {
      showNotification(paste("Error parssing JSON:", e$message), type = "error")
      return(NULL)
    })
    if (!is.null(data$daily)) {
      df <- data.frame(
        date = as_date(data$daily$time),
        temp_max = data$daily$temperature_2m_max,
        year = year(as_date(data$daily$time))
      )
      all_data[[length(all_data) + 1]] <- df
    }
  }
  if (length(all_data) > 0) bind_rows(all_data) else NULL
}

get_weather_forecast <- function(lat, lon, start_date, end_date, timezone) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast?",
    "latitude=", lat,
    "&longitude=", lon,
    "&hourly=temperature_2m",
    "&daily=temperature_2m_max,temperature_2m_min",
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&timezone=", URLencode(timezone)
  )
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    showNotification(paste("Error in API:", e$message), type = "error")
    return(NULL)
  })
  if (is.null(response) || http_error(response)) {
    showNotification("Error in downloading forecasting data", type = "error")
    return(NULL)
  }
  data <- tryCatch({
    fromJSON(content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    showNotification(paste("Error parssing JSON:", e$message), type = "error")
    return(NULL)
  })
  if (is.null(data$hourly) || is.null(data$daily)) {
    showNotification("Invalid data format from the API", type = "error")
    return(NULL)
  }
  # Time parssing
  hourly_df <- data.frame(
    datetime = lubridate::ymd_hm(data$hourly$time, tz = timezone),
    temperature = data$hourly$temperature_2m
  )
  daily_df <- data.frame(
    date = as.Date(data$daily$time),
    temp_max = data$daily$temperature_2m_max,
    temp_min = data$daily$temperature_2m_min
  )
  return(list(hourly = hourly_df, daily = daily_df))
}

make_forecast_arima <- function(df, h, var) {
  df <- arrange(df, date)
  if (nrow(df) < 10) {
    showNotification("Insufficient historical data for accurate ARIMA modeling", type = "warning")
    return(NULL)
  }
  df$trend <- as.numeric(df$date - min(df$date))
  ts_data <- ts(df[[var]], frequency = 365/30)
  model <- tryCatch({
    auto.arima(ts_data, xreg = df$trend, seasonal = FALSE)
  }, error = function(e) {
    showNotification(paste("Error in ARIMA modeling:", e$message), type = "warning")
    return(NULL)
  })
  if (is.null(model)) return(NULL)
  future_trend <- max(df$trend) + seq_len(h)
  fc <- tryCatch({
    forecast(model, xreg = future_trend, h = h)
  }, error = function(e) {
    showNotification(paste("Error in forecast:", e$message), type = "warning")
    return(NULL)
  })
  data.frame(
    date = seq.Date(from = max(df$date) + 1, by = "day", length.out = h),
    prediction = round(fc$mean, 1),
    lower = round(fc$lower[, 1], 1),
    upper = round(fc$upper[, 1], 1)
  )
}

make_forecast_hourly <- function(lat, lon, forecast_date, timezone, days_back = 14) {
  start_date <- forecast_date - days(days_back)
  end_date <- forecast_date - days(1)
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat,
    "&longitude=", lon,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&hourly=temperature_2m",
    "&timezone=", URLencode(timezone)
  )
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    showNotification(paste("Error in API:", e$message), type = "error")
    return(NULL)
  })
  if (is.null(response) || http_error(response)) {
    showNotification("Error fetching historical hourly data", type = "error")
    return(NULL)
  }
  data <- tryCatch({
    fromJSON(content(response, "text", encoding = "UTF-8"))
  }, error = function(e) {
    showNotification(paste("Error parssing JSON:", e$message), type = "error")
    return(NULL)
  })
  if (is.null(data$hourly)) {
    showNotification("No hourly data available from the API", type = "warning")
    return(NULL)
  }
  # Time parssing
  df <- data.frame(
    datetime = lubridate::ymd_hm(data$hourly$time, tz = timezone),
    temperature = data$hourly$temperature_2m
  )
  hourly_avg <- df %>%
    mutate(hour = hour(datetime)) %>%
    group_by(hour) %>%
    summarise(temp_avg = mean(temperature, na.rm = TRUE))
  forecast_hours <- data.frame(
    datetime = seq(from = as.POSIXct(paste(forecast_date, "00:00:00"), tz = timezone),
                   by = "hour", length.out = 24),
    hour = 0:23
  )
  forecast <- left_join(forecast_hours, hourly_avg, by = "hour") %>%
    select(datetime, temp_avg) %>%
    rename(predicted_temperature = temp_avg)
  return(forecast)
}

# --- UI ---
ui <- fluidPage(
  titlePanel("🌡️Temperature forecast"),
  sidebarLayout(
    sidebarPanel(
      textInput("city", "City:", value = "Warsaw"),
      numericInput("forecast_days", "Days of forecast (max 14):", 7, min = 1, max = 14),
      actionButton("go", "🔍 Generate forecast", class = "btn-primary"),
      br(), br(),
      helpText("This application uses the Open-Meteo API for weather data collection and ARIMA modeling for temperature forecasting."),
      hr(),
      verbatimTextOutput("debug_text"),
      uiOutput("emoji_legend")
    ),
    mainPanel(
      uiOutput("locationInfo"),
      uiOutput("emoji_text"),
      tabsetPanel(
        tabPanel("Daily Forecast",
                 br(),
                 plotlyOutput("forecastPlot", height = "500px")),
        tabPanel("Hourly Forecast",
                 br(),
                 plotlyOutput("hourlyTemperaturePlot", height = "500px")),
        tabPanel("Historical Data",
                 br(),
                 DTOutput("historyTable"),
                 br(),
                 downloadButton("downloadForecast", "Pobierz Prognozę CSV"),
                 downloadButton("downloadHistory", "Pobierz Historię CSV"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  forecast_data <- reactiveVal(NULL)
  history_data <- reactiveVal(NULL)
  location_info <- reactiveVal(NULL)
  hourly_forecast <- reactiveVal(NULL)
  arima_forecast <- reactiveVal(NULL)
  
  observeEvent(input$go, {
    req(input$city)
    withProgress(message = 'Generate forecast...', {
      city_name <- input$city
      forecast_days <- min(input$forecast_days, 14)
      incProgress(0.1, detail = "Searching for coordinates")
      coordinates <- get_coordinates(city_name)
      if (is.null(coordinates)) {
        showNotification("Unable to find coordinates for the city", type = "error")
        return()
      }
      lat <- coordinates$lat
      lon <- coordinates$lon
      incProgress(0.1, detail = "Download timezone")
      timezone <- get_timezone(lat, lon)
      location_info(list(city = city_name, lat = lat, lon = lon, timezone = timezone))
      forecast_date <- Sys.Date()
      forecast_end <- forecast_date + days(forecast_days - 1)
      incProgress(0.2, detail = "Download historical data")
      hist_data <- get_weather_history(lat, lon, forecast_date)
      history_data(hist_data)
      if (is.null(hist_data) || nrow(hist_data) < 10) {
        showNotification("Insufficient historical data", type = "warning")
      } else {
        incProgress(0.2, detail = "Building ARIMA model")
        fc_arima <- make_forecast_arima(hist_data, forecast_days, "temp_max")
        arima_forecast(fc_arima)
        if (is.null(fc_arima)) {
          showNotification("Unable to generate ARIMA forecast", type = "error")
        }
      }
      incProgress(0.2, detail = "Download API forecast")
      api_forecast <- get_weather_forecast(lat, lon, forecast_date, forecast_end, timezone)
      forecast_data(api_forecast)
      incProgress(0.2, detail = "Generate hourly forecast")
      hourly_fc <- make_forecast_hourly(lat, lon, forecast_date, timezone)
      hourly_forecast(hourly_fc)
    })
  })
  
  output$locationInfo <- renderUI({
    info <- location_info()
    if (!is.null(info)) {
      tags$div(
        style = "margin-bottom: 15px;",
        tags$h4(paste("📍", info$city)),
        tags$p(paste("Coordinates:", round(info$lat, 4), ", ", round(info$lon, 4))),
        tags$p(paste("Timezone:", info$timezone))
      )
    }
  })
  
  output$emoji_text <- renderUI({
    fc <- arima_forecast()
    hist <- history_data()
    if (!is.null(fc) && !is.null(hist) && nrow(hist) > 0) {
      recent_hist <- hist %>%
        filter(year(date) >= year(Sys.Date()) - 3) %>%
        group_by(doy = yday(date)) %>%
        summarise(avg_temp = mean(temp_max, na.rm = TRUE))
      forecast_doys <- yday(fc$date)
      avg_historical_temp <- mean(recent_hist$avg_temp[match(forecast_doys, recent_hist$doy)], na.rm = TRUE)
      avg_forecast_temp <- mean(fc$prediction, na.rm = TRUE)
      diff <- avg_forecast_temp - avg_historical_temp
      emoji <- if (diff > 2) {
        "☀️😎 Warmer than usual!"
      } else if (diff < -2) {
        "🥶☁️ Colder than usual!"
      } else {
        "😐🌤 The same as usual."
      }
      tags$div(style = "font-size: 24px; font-weight: bold; margin: 15px 0;", emoji)
    }
  })
  
  output$forecastPlot <- renderPlotly({
    fc <- arima_forecast()
    api_fc <- forecast_data()
    if (is.null(fc) && is.null(api_fc)) {
      return(NULL)
    }
    # English locals for plot
    old_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    
    if (!is.null(fc)) {
      fc_long <- pivot_longer(fc, cols = c("prediction", "lower", "upper"),
                              names_to = "type", values_to = "value")
      p <- ggplot(fc_long, aes(x = date, y = value, color = type)) +
        geom_line() + geom_point() +
        scale_color_manual(values = c("prediction" = "darkgreen", "lower" = "blue", "upper" = "red")) +
        labs(title = "📈 Daily temperature forecast",
             subtitle = paste("ARIMA model with prediction intervals"),
             x = "Date", y = "Temperature (°C)", color = "Type") +
        theme_minimal() +
        scale_x_date(date_labels = "%d %b", date_breaks = "1 day")
      if (!is.null(api_fc) && !is.null(api_fc$daily)) {
        api_daily <- api_fc$daily %>%
          select(date, temp_max) %>%
          mutate(type = "API forecast") %>%
          rename(value = temp_max)
        p <- p + geom_line(data = api_daily, aes(x = date, y = value, color = "API forecast"), linetype = "dashed") +
          scale_color_manual(values = c("prediction" = "darkgreen", "lower" = "blue", "upper" = "red", "API forecast" = "purple"))
      }
      plotly_obj <- ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
    } else if (!is.null(api_fc) && !is.null(api_fc$daily)) {
      api_daily <- api_fc$daily
      p <- ggplot(api_daily, aes(x = date)) +
        geom_line(aes(y = temp_max, color = "Maximum")) +
        geom_line(aes(y = temp_min, color = "Minimum")) +
        labs(title = "📈 Daily temperature forecast",
             subtitle = "Data API forecast",
             x = "Date", y = "Temperature (°C)", color = "Type") +
        theme_minimal() +
        scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
        scale_color_manual(values = c("Maximum" = "red", "Minimum" = "blue"))
      plotly_obj <- ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
    }
    Sys.setlocale("LC_TIME", old_locale)
    plotly_obj
  })
  
  output$hourlyTemperaturePlot <- renderPlotly({
    api_fc <- forecast_data()
    hourly_fc <- hourly_forecast()
    info <- location_info()
    if (is.null(api_fc) || is.null(hourly_fc) || is.null(info)) {
      return(NULL)
    }
    
    # Set locale to English (US)
    old_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
    
    first_day_data <- api_fc$hourly %>%
      filter(date(datetime) == Sys.Date())
    
    plot_data <- full_join(
      first_day_data %>% rename(api_temperature = temperature),
      hourly_fc,
      by = "datetime"
    )
    
    plot_data <- plot_data %>% arrange(datetime)
    all_hours <- seq.POSIXt(from = as.POSIXct(paste(Sys.Date(), "00:00:00"), tz = info$timezone),
                            by = "hour", length.out = 24)
    plot_data <- right_join(data.frame(datetime = all_hours), plot_data, by = "datetime") %>%
      arrange(datetime)
    
    plot_data$hour_label <- format(plot_data$datetime, "%H:%M")
    
    p <- plot_ly() %>%
      add_trace(
        data = plot_data,
        x = ~datetime,
        y = ~api_temperature,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecast API",
        line = list(color = "#2E86C1", width = 3),
        marker = list(size = 8, symbol = "circle"),
        text = ~hour_label,
        hovertemplate = "%{text}<br>%{y}°C<extra>Forecast API</extra>"
      ) %>%
      add_trace(
        data = plot_data,
        x = ~datetime,
        y = ~predicted_temperature,
        type = "scatter",
        mode = "lines+markers",
        name = "Historical mean",
        line = list(color = "#E74C3C", width = 2.5, dash = "dash"),
        marker = list(size = 8, symbol = "diamond"),
        text = ~hour_label,
        hovertemplate = "%{text}<br>%{y}°C<extra>Average temperature recent days</extra>"
      ) %>%
      layout(
        title = list(
          text = paste0("Hourly forecast for ", format(Sys.Date(), "%A, %d %B"), " in ", info$city),
          font = list(size = 22)
        ),
        xaxis = list(
          title = "Hour",
          tickformat = "%H:%M",
          type = "date"
        ),
        yaxis = list(
          title = "Temperature (°C)"
        ),
        plot_bgcolor = "rgba(250, 250, 250, 0.8)",
        legend = list(
          orientation = "h",
          yanchor = "bottom",
          y = -0.15,
          xanchor = "center",
          x = 0.5
        ),
        hovermode = "x unified"
      )
    
    # Reset locale to original
    Sys.setlocale("LC_TIME", old_locale)
    
    return(p)
  })
  
  output$emoji_legend <- renderUI({
    tags$div(
      style = "margin-top: 15px; font-size: 16px;",
      tags$h5("🌡️ Emoji Legend – Compared to Historical Average:"),
      tags$ul(
        tags$li("☀️😎 – Warmer than usual: more than +2°C above the historical average"),
        tags$li("😐🌤 – Similar to usual: within ±2°C of the historical average"),
        tags$li("🥶☁️ – Colder than usual: more than -2°C below the historical average")
      )
    )
  })
  
  output$historyTable <- renderDT({
    hist <- history_data()
    if (!is.null(hist)) {
      hist <- hist %>%
        arrange(desc(date)) %>%
        select(date, year, temp_max)
      datatable(hist,
                options = list(pageLength = 10,
                               order = list(list(0, 'desc')),
                               scrollY = "400px"),
                caption = "Historical Max Temperatures") %>%
        formatRound('temp_max', 1)
    }
  })
  
  output$downloadForecast <- downloadHandler(
    filename = function() {
      info <- location_info()
      paste0("forecast_", ifelse(is.null(info), "location", info$city), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(arima_forecast(), file, row.names = FALSE)
    }
  )
  
  output$downloadHistory <- downloadHandler(
    filename = function() {
      info <- location_info()
      paste0("history_", ifelse(is.null(info), "location", info$city), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(history_data(), file, row.names = FALSE)
    }
  )
}

# --- RUN APP ---
shinyApp(ui, server, options = list(host = "0.0.0.0", port = 5000))