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

# --- FUNCTIONS ---
# Function to get coordinates for a city
get_coordinates <- function(city_name) {
  url <- paste0("https://geocoding-api.open-meteo.com/v1/search?name=", URLencode(city_name), "&count=10")
  response <- tryCatch(GET(url), error = function(e) return(NULL))
  if (is.null(response) || http_error(response)) return(NULL)
  
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  if (is.null(result$results) || nrow(result$results) == 0) return(NULL)
  
  coords <- result$results
  latitudes <- coords$latitude
  longitudes <- coords$longitude
  
  # Use reverse geocoding to get more reliable regions
  regions <- mapply(get_region_by_coords, latitudes, longitudes, SIMPLIFY = TRUE)
  
  list(
    cities = coords$name,
    coords = data.frame(lat = latitudes, lon = longitudes),
    countries = coords$country,
    regions = regions
  )
}

# Get timezone based on coordinates
get_timezone <- function(lat, lon) {
  tz <- tryCatch(tz_lookup_coords(lat, lon, method = "accurate"), error = function(e) "UTC")
  if (is.na(tz) || tz == "") tz <- "UTC"
  tz
}

# Reverse geocoding to get region by coordinates
get_region_by_coords <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", lat, "&lon=", lon, "&zoom=10&addressdetails=1")
  result <- tryCatch(jsonlite::fromJSON(url, flatten = TRUE), error = function(e) NULL)
  if (!is.null(result$address$state)) {
    return(result$address$state)
  } else if (!is.null(result$address$county)) {
    return(result$address$county)
  } else {
    return("Unknown")
  }
}

# Historical weather data
get_weather_history <- function(lat, lon, forecast_start) {
  all_data <- list()
  
  # Calculate date ranges - get 5 years of data
  end_date <- forecast_start - days(2)  # yesterday
  start_date <- end_date - years(5)     # 5 years ago
  
  # Make a single API call for the entire period
  url <- paste0("https://archive-api.open-meteo.com/v1/archive?latitude=", lat,
                "&longitude=", lon,
                "&start_date=", start_date,
                "&end_date=", end_date,
                "&daily=temperature_2m_max,temperature_2m_min",
                "&timezone=auto")
  
  response <- tryCatch(GET(url), error = function(e) NULL)
  if (is.null(response) || http_error(response)) {
    return(NULL)
  }
  
  data <- tryCatch(fromJSON(content(response, "text", encoding = "UTF-8")), 
                   error = function(e) NULL)
  
  if (!is.null(data$daily)) {
    df <- data.frame(
      date = as_date(data$daily$time),
      temp_max = data$daily$temperature_2m_max,
      temp_min = data$daily$temperature_2m_min,
      year = year(as_date(data$daily$time))
    )
    
    # Add day of year for seasonal analysis
    df$doy <- yday(df$date)
    
    return(df %>% arrange(desc(date)))
  }
  
  return(NULL)
}

# Weather forecast
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
  response <- tryCatch(GET(url), error = function(e) NULL)
  if (is.null(response) || http_error(response)) return(NULL)
  data <- tryCatch(fromJSON(content(response, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(data$hourly) || is.null(data$daily)) return(NULL)
  hourly_df <- data.frame(
    datetime = lubridate::ymd_hm(data$hourly$time, tz = timezone),
    temperature = data$hourly$temperature_2m
  )
  daily_df <- data.frame(
    date = as.Date(data$daily$time),
    temp_max = data$daily$temperature_2m_max,
    temp_min = data$daily$temperature_2m_min
  )
  list(hourly = hourly_df, daily = daily_df)
}

# ARIMA modeling
make_forecast_arima <- function(df, h, vars = c("temp_max", "temp_min")) {
  tryCatch({
    df <- arrange(df, date)
    if (nrow(df) < 10) return(NULL)
    
    result <- data.frame(
      date = seq.Date(from = max(df$date) + 2, by = "day", length.out = h)
    )
    
    # Process each temperature type (max and min) separately
    for (var in vars) {
      print(paste("Starting processing for:", var)) 
      
      if (!var %in% names(df)) {
        print(paste("Missing variable:", var))
        next
      }
      
      # Remove NA values and check length
      ts_df <- df %>%
        select(date, !!sym(var)) %>%
        filter(!is.na(!!sym(var)))
      
      if (nrow(ts_df) < 10) {
        print(paste("Insufficient data for:", var))
        next
      }
      
      # Create time series with seasons
      ts_data <- ts(ts_df[[var]], 
                    frequency = 365/12,
                    start = c(year(min(ts_df$date)), isoweek(min(ts_df$date))))
      
      model <- tryCatch({
        print(paste("Fitting ARIMA model for:", var))  
        auto.arima(ts_data, 
                   seasonal = TRUE,
                   D = 1,
                   max.P = 1,
                   max.Q = 1,
                   stepwise = TRUE,
                   approximation = TRUE)
      }, error = function(e) {
        print(paste("ARIMA error for", var, ":", e$message))
        NULL
      })
      
      if (!is.null(model)) {
        print(paste("Successfully fit ARIMA model for:", var))  
        
        fc <- tryCatch({
          forecast(model, h = h)
        }, error = function(e) {
          print(paste("Forecast error for", var, ":", e$message))
          NULL
        })
        
        if (!is.null(fc)) {
          print(paste("Generated forecast for:", var)) 
          
          # Store predictions with clear naming
          col_prefix <- if(var == "temp_max") "temp_max" else "temp_min"
          result[[paste0(col_prefix, "_prediction")]] <- round(fc$mean, 1)
          result[[paste0(col_prefix, "_lower")]] <- round(fc$lower[, 1], 1)
          result[[paste0(col_prefix, "_upper")]] <- round(fc$upper[, 1], 1)
          
          print(paste("✅ ARIMA complete for:", var))
        } else {
          print(paste("❌ forecast() returned NULL for:", var))
        }
      } else {
        print(paste("❌ auto.arima() failed for:", var))
      }
    }
    
    
    # Return result if we have at least one prediction
    if (any(c("temp_max_prediction", "temp_min_prediction") %in% names(result))) {
      return(result)
    }
    
    print("No predictions generated")
    return(NULL)
    
  }, error = function(e) {
    print(paste("General error:", e$message))
    return(NULL)
  })
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
  titlePanel("🌡️Temperature Forecast"),
  
  # CSS Styles
  tags$style(HTML("
    .custom-text-input {
      background-color: #f5f5f5;
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 15px;
    }
    .city-select {
      margin-top: 10px;
      margin-bottom: 10px;
    }
    .selectize-input {
      min-height: 38px;
      padding: 8px 12px;
      border-radius: 4px;
      border: 1px solid #ccc;
    }
    .selectize-dropdown {
      border-radius: 4px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.1);
    }
    .selectize-dropdown-content {
      max-height: 300px;
    }
    .selectize-dropdown-content .option {
      padding: 8px 12px;
      border-bottom: 1px solid #f0f0f0;
    }
    .selectize-dropdown-content .option:hover {
      background-color: #f5f5f5;
    }
    .alert {
      padding: 10px;
      margin-top: 10px;
      border-radius: 4px;
    }
  ")),
  
  fluidRow(
    # SIDEBAR (left)
    column(
      width = 3,
      style = "padding: 20px;",
      wellPanel(
        style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 8px; padding: 15px;",
        div(
          class = "custom-text-input",
          textInput("city", "Search city:",
                    value = "",
                    placeholder = "Type city name..."),
          style = "margin-bottom: 10px;"
        ),
        actionButton("search_button", "Search",
                     class = "btn btn-primary btn-block",
                     style = "margin-bottom: 10px;"),
        uiOutput("city_select"),
        tags$div(
          id = "search-results",
          style = "max-height: 200px; overflow-y: auto;"
        ),
        numericInput("forecast_days",
                     "Days of forecast (max 10):",
                     7, min = 1, max = 10),
        actionButton("go", "🔍 Generate forecast",
                     class = "btn btn-primary btn-block"),
        br(), br(),
        helpText("This application uses the Open-Meteo API for weather data collection and ARIMA modeling for temperature forecasting."),
        hr(),
        verbatimTextOutput("debug_text"),
        uiOutput("emoji_legend")
      )
    ),
    
    # MAIN PANEL (right)
    column(
      width = 9,
      style = "padding: 20px;",
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
                 downloadButton("downloadForecast", "Download Forecast CSV"),
                 downloadButton("downloadHistory", "Download History CSV"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  # Reactive values
  forecast_data <- reactiveVal(NULL)
  history_data <- reactiveVal(NULL)
  location_info <- reactiveVal(NULL)
  hourly_forecast <- reactiveVal(NULL)
  arima_forecast <- reactiveVal(NULL)
  locations_df <- reactiveVal(NULL)
  
  # Function to reset all reactive values
  reset_reactives <- function() {
    forecast_data(NULL)
    history_data(NULL)
    location_info(NULL)
    hourly_forecast(NULL)
    arima_forecast(NULL)
  }
  
  # Search for cities and update dropdown
  observeEvent(input$search_button, {
    req(input$city)
    # Reset all reactive values when searching for a new city
    reset_reactives()
    
    withProgress(message = 'Searching cities...', {
      city_name <- input$city
      result <- get_coordinates(city_name)
      
      if (!is.null(result) && length(result$cities) > 0) {
        # Create location labels
        city_labels <- mapply(
          function(city, region, country) {
            region <- if (is.na(region) || region == "") "Unknown" else region
            sprintf("%s (%s, %s)", city, region, country)
          },
          result$cities,
          result$regions,
          result$countries,
          SIMPLIFY = TRUE
        )
        
        # Store location data
        locations_df(data.frame(
          index = seq_along(result$cities),
          city = result$cities,
          region = result$regions,
          country = result$countries,
          lat = result$coords$lat,
          lon = result$coords$lon,
          stringsAsFactors = FALSE
        ))
        
        # Update select input
        output$city_select <- renderUI({
          selectInput("selected_city",
                      "Select city:",
                      choices = setNames(seq_along(city_labels), city_labels),
                      selected = 1,
                      width = "100%")
        })
      } else {
        output$city_select <- renderUI({
          div(
            class = "alert alert-warning",
            "No cities found. Please try a different search."
          )
        })
      }
    })
  })
  
  # City selection observer
  observeEvent(input$selected_city, {
    req(input$selected_city, locations_df())
    
    # Reset forecast-related values when selecting a new city
    forecast_data(NULL)
    history_data(NULL)
    hourly_forecast(NULL)
    arima_forecast(NULL)
    
    idx <- as.numeric(input$selected_city)
    loc <- locations_df()[idx, ]
    
    location_info(list(
      city = loc$city,
      lat = loc$lat,
      lon = loc$lon,
      region = loc$region,
      country = loc$country
    ))
  })
  
  # Generate forecasts
  observeEvent(input$go, {
    req(location_info())  # Ensure location info is available
    info <- location_info()  # Get the selected city's details
    
    withProgress(message = 'Generate forecast...', {
      forecast_days <- min(input$forecast_days, 10)
      
      # Validate coordinates
      if (is.null(info$lat) || is.null(info$lon)) {
        showNotification("Invalid coordinates for the selected city.", type = "error")
        return()
      }
      
      # Fetch timezone
      incProgress(0.1, detail = "Fetching timezone")
      timezone <- get_timezone(info$lat, info$lon)
      location_info(modifyList(info, list(timezone = timezone)))
      
      # Fetch historical data
      incProgress(0.2, detail = "Fetching historical data")
      hist_data <- get_weather_history(info$lat, info$lon, Sys.Date())
      history_data(hist_data)
      if (is.null(hist_data) || nrow(hist_data) < 10) {
        showNotification("Insufficient historical data for ARIMA model.", type = "warning")
        return()
      }
      
      # Build ARIMA forecast
      incProgress(0.2, detail = "Building ARIMA model")
      fc_arima <- make_forecast_arima(hist_data, forecast_days, c("temp_max", "temp_min"))
      arima_forecast(fc_arima)
      if (is.null(fc_arima)) {
        showNotification("Failed to generate ARIMA forecast.", type = "error")
        return()
      }
      
      # Fetch API forecast
      incProgress(0.2, detail = "Fetching API forecast")
      forecast_end <- Sys.Date() + forecast_days - 1
      api_forecast <- get_weather_forecast(info$lat, info$lon, Sys.Date(), forecast_end, timezone)
      forecast_data(api_forecast)
      
      # Generate hourly forecast
      incProgress(0.2, detail = "Generating hourly forecast")
      hourly_fc <- make_forecast_hourly(info$lat, info$lon, Sys.Date(), timezone)
      hourly_forecast(hourly_fc)
    })
  })
  
  # Render location info
  output$locationInfo <- renderUI({
    info <- location_info()
    if (!is.null(info)) {
      tags$div(
        style = "margin-bottom: 15px;",
        tags$h4(paste("📍", info$city)),
        tags$p(paste("Country:", ifelse(!is.null(info$country), info$country, "Unknown"))),
        tags$p(paste("Region:", ifelse(!is.null(info$region), info$region, "Unknown"))),
        tags$p(paste("Coordinates:", round(info$lat, 4), ",", round(info$lon, 4)))
      )
    }
  })
  
  # Render emoji text
  output$emoji_text <- renderUI({
    fc <- arima_forecast()
    hist <- history_data()
    
    if (!is.null(fc) && !is.null(hist) && nrow(hist) > 0) {
      recent_hist <- hist %>%
        filter(year(date) >= year(Sys.Date()) - 3) %>%
        group_by(doy = yday(date)) %>%
        summarise(
          avg_max = mean(temp_max, na.rm = TRUE),
          avg_min = mean(temp_min, na.rm = TRUE)
        )
      
      forecast_doys <- yday(fc$date)
      avg_historical_max <- mean(recent_hist$avg_max[match(forecast_doys, recent_hist$doy)], na.rm = TRUE)
      avg_forecast_max <- mean(fc$temp_max_prediction, na.rm = TRUE)
      
      diff <- avg_forecast_max - avg_historical_max
      emoji <- if (diff > 2) {
        "☀️😎 Warmer than usual!"
      } else if (diff < -2) {
        "🥶☁️ Colder than usual!"
      } else {
        "😐🌤 Similar to usual temperatures."
      }
      
      tags$div(
        style = "font-size: 24px; font-weight: bold; margin: 15px 0;",
        emoji
      )
    }
  })

          output$forecastPlot <- renderPlotly({
            fc <- arima_forecast()
            api_fc <- forecast_data()
            
            if (is.null(fc) && is.null(api_fc)) {
              return(NULL)
            }
            
            # Set locale to English (US)
            old_locale <- Sys.getlocale("LC_TIME")
            Sys.setlocale("LC_TIME", "en_US.UTF-8")
            
            tryCatch({
              if (!is.null(api_fc) && !is.null(api_fc$daily)) {
                # Create API forecast data
                api_daily_long <- api_fc$daily %>%
                  select(date, temp_max, temp_min) %>%
                  pivot_longer(
                    cols = c(temp_max, temp_min),
                    names_to = "metric",
                    values_to = "value"
                  ) %>%
                  mutate(
                    type = if_else(metric == "temp_max", "API Maximum", "API Minimum"),
                    date = as.Date(date)
                  ) %>%
                  filter(date >= Sys.Date())
                
                # Base plot with API data
                p <- ggplot() +
                  geom_line(data = api_daily_long, 
                            aes(x = date, y = value, color = type),
                            size = 1) +
                  geom_point(data = api_daily_long,
                             aes(x = date, y = value, color = type),
                             size = 3)
                
                # Add ARIMA predictions if available
                if (!is.null(fc)) {
                  fc_long <- data.frame()
                  
                  # Add max temperature prediction if available
                  if ("temp_max_prediction" %in% names(fc)) {
                    fc_long <- bind_rows(
                      fc_long,
                      data.frame(
                        date = fc$date,
                        value = fc$temp_max_prediction,
                        type = "ARIMA Maximum"
                      )
                    )
                  }
                  
                  # Add min temperature prediction if available
                  if ("temp_min_prediction" %in% names(fc)) {
                    fc_long <- bind_rows(
                      fc_long,
                      data.frame(
                        date = fc$date,
                        value = fc$temp_min_prediction,
                        type = "ARIMA Minimum"
                      )
                    )
                  }
                  
                  if (nrow(fc_long) > 0) {
                    p <- p +
                      geom_line(data = fc_long,
                                aes(x = date, y = value, color = type),
                                linetype = "dashed",
                                size = 1) +
                      geom_point(data = fc_long,
                                 aes(x = date, y = value, color = type),
                                 shape = 1,
                                 size = 3)
                  }
        }
        
        # Add styling
        p <- p +
          scale_color_manual(
            values = c(
              "API Maximum" = "red",
              "API Minimum" = "blue",
              "ARIMA Maximum" = "darkred",
              "ARIMA Minimum" = "darkblue"
            ),
            name = "Temperature Type",
            labels = c(
              "API Maximum" = "API Forecast (Max)",
              "API Minimum" = "API Forecast (Min)",
              "ARIMA Maximum" = "ARIMA Prediction (Max)",
              "ARIMA Minimum" = "ARIMA Prediction (Min)"
            )
          ) +
          labs(
            title = "📈 Daily Temperature Forecast",
            subtitle = paste("Forecast from", format(Sys.Date(), "%d %B %Y")),
            x = "Date",
            y = "Temperature (°C)"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)
          ) +
          scale_x_date(
            date_labels = "%d %b",
            date_breaks = "1 day",
            limits = c(Sys.Date(), max(fc$date))
          )
        
        plotly_obj <- ggplotly(p) %>%
          layout(
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              y = -0.2,
              x = 0.5,
              xanchor = "center"
            ),
            hovermode = "x unified"
          )
        
        Sys.setlocale("LC_TIME", old_locale)
        return(plotly_obj)
      }
      
      return(NULL)
    }, error = function(e) {
      Sys.setlocale("LC_TIME", old_locale)
      return(NULL)
    })
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
          font = list(size = 16)
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
          y = -0.25,
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
        select(date, year, temp_max, temp_min) %>%
        rename(
          "Date" = date,
          "Year" = year,
          "Max Temp (°C)" = temp_max,
          "Min Temp (°C)" = temp_min
        )
      
      datatable(hist,
                options = list(
                  pageLength = 10,
                  order = list(list(0, 'desc')),
                  scrollY = "400px"
                ),
                caption = "Historical Temperatures") %>%
        formatRound(c('Max Temp (°C)', 'Min Temp (°C)'), 1)
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