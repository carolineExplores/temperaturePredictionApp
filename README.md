# temperaturePredictionApp

Temperature Forecast Prediction App 

This is a powerful weather forecasting application built using Shiny in R, which leverages both historical temperature data and forecast data from the Open Meteo API. The app uses the ARIMA model to predict future temperatures by analyzing temperature data from the last 5 years (including the current year).

You can explore the interactive Shiny app by clicking the link below:

👉 [Launch the App](https://carolineexplores.shinyapps.io/app_r/)

This application allows you to visualize and predict temperature trends using historical data. No installation is required – it runs directly in your browser.

# Key Features:

Temperature Predictions: Users can generate temperature forecasts (minimum and maximum temperature) for 1 to 10 days ahead for cities worldwide.

Hourly Forecast: The app also provides hourly temperature predictions for the following day.

Historical Data Access: Users can view and interact with a detailed table of the historical data used for model predictions.

CSV Export: Users can download the forecast data or historical data for any given period as CSV files.

# How It Works: 
Forecasting with ARIMA: The app analyzes temperature data from the past days over 5 years, packed in a month time series (including the current year) to train an ARIMA model. Based on this model, it generates forecasts for future temperatures.

Forecast Data: The app retrieves forecast data from Open Meteo and combines it with historical temperature data to produce accurate predictions.

Global Coverage: Forecasts are available for cities across the globe, allowing users to input any city for temperature predictions.

Interactive Table: A table displays the historical data used in the forecast model, allowing users to better understand the basis of the predictions.

CSV Export: The app gives users the ability to export the forecast data or historical data to CSV files for further analysis or record-keeping.

# Use Case: 
The app is ideal for users looking to get accurate and reliable weather forecasts for up to 14 days, based on a combination of historical data and advanced statistical modeling. Whether you're planning a trip, managing outdoor events, or simply curious about future weather trends, this app provides valuable insights into the expected weather conditions.
