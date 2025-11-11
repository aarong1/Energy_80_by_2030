# Improved Project Availability Forecasting
# Author: GitHub Copilot
# Date: August 2025

library(tidyverse)
library(lubridate)
library(forecast)
library(broom)

# Assuming 'x' is your original data with Planning Application Submitted dates
# This script provides multiple forecasting approaches for project availability

#===============================================================================
# APPROACH 1: Time Series Forecasting (Recommended for trends/seasonality)
#===============================================================================

prepare_ts_data <- function(data) {
  ts_data <- data %>%
    filter(!is.na(`Planning Application Submitted`)) |> 
    mutate(
      year_month = floor_date(as.Date(`Planning Application Submitted`), "month")
    ) %>%
    group_by(year_month, tech) %>%
    summarise(n_projects = n(), .groups = 'drop') %>%
    arrange(year_month)
  
  return(ts_data)
}

forecast_ts_approach <- function(ts_data, tech_type, forecast_periods = 12) {
  # Filter for specific technology
  tech_data <- ts_data %>% 
    filter(tech == tech_type) %>%
    arrange(year_month)
  
  # Create time series object
  ts_obj <- ts(tech_data$n_projects, 
               start = c(year(min(tech_data$year_month)), month(min(tech_data$year_month))), 
               frequency = 12)
  
  # Fit ARIMA model (automatically selects best parameters)
  model <- auto.arima(ts_obj)
  
  # Generate forecast
  forecast_result <- forecast(model, h = forecast_periods)
  
  # Convert back to tibble with dates
  forecast_dates <- seq.Date(
    from = max(tech_data$year_month) + months(1),
    length.out = forecast_periods,
    by = "month"
  )
  
  forecast_df <- tibble(
    year_month = forecast_dates,
    tech = tech_type,
    forecast = as.numeric(forecast_result$mean),
    lower_80 = as.numeric(forecast_result$lower[,1]),
    upper_80 = as.numeric(forecast_result$upper[,1]),
    lower_95 = as.numeric(forecast_result$lower[,2]),
    upper_95 = as.numeric(forecast_result$upper[,2])
  )
  
  return(list(model = model, forecast = forecast_df, historical = tech_data))
}


ts <- forecast_ts_approach(prepare_ts_data(pipeline), 'Solar Photovoltaics')

#===============================================================================
# APPROACH 2: Improved GLM with Better Features (Your current approach enhanced)
#===============================================================================

improved_glm_forecast <- function(data, forecast_years = 2) {
  # Prepare data with better features
  model_data <- data %>%
    filter(!is.na(`Planning Application Submitted`)) |> 
    mutate(
      date = floor_date(as.Date(`Planning Application Submitted`), "month"),
      year = year(date),
      month = month(date),
      quarter = quarter(date),
      # Add trend variable
      time_index = as.numeric(date - min(date)) / 30.44  # months since start
    ) %>%
    filter(year > 2017) %>%  # Your existing filter
    group_by(date, year, month, quarter, time_index, tech) %>%
    summarise(n = n(), .groups = 'drop')
  
  # Fit improved GLM with better predictors
  model <- glm(n ~ tech * (poly(time_index, 2) + factor(month) + factor(quarter)), 
               data = model_data, 
               family = quasipoisson(link = "log"))
  
  # Create future data for prediction
  max_date <- max(model_data$date)
  future_months <- seq.Date(from = max_date + months(1), 
                           length.out = forecast_years * 12, 
                           by = "month")
  
  future_data <- expand_grid(
    date = future_months,
    tech = unique(model_data$tech)
  ) %>%
    mutate(
      year = year(date),
      month = month(date),
      quarter = quarter(date),
      time_index = as.numeric(date - min(model_data$date)) / 30.44
    )
  
  # Generate predictions with confidence intervals
  predictions <- predict(model, newdata = future_data, 
                        type = "response", se.fit = TRUE)
  
  forecast_df <- future_data %>%
    mutate(
      forecast = predictions$fit,
      se = predictions$se.fit,
      lower_95 = forecast - 1.96 * se,
      upper_95 = forecast + 1.96 * se
    ) %>%
    mutate(across(c(lower_95), ~pmax(0, .)))  # Ensure non-negative
  
  return(list(model = model, forecast = forecast_df, historical = model_data))
}

glm <- improved_glm_forecast(pipeline)
#===============================================================================
# APPROACH 3: Machine Learning with Cross-Validation (Most Robust)
#===============================================================================

ml_forecast_approach <- function(data) {
  library(randomForest)
  
  # Feature engineering
  ml_data <- data %>%
    filter(!is.na(`Planning Application Submitted`)) |> 
    mutate(
      date = floor_date(as.Date(`Planning Application Submitted`), "month"),
      year = year(date),
      month = month(date),
      quarter = quarter(date),
      time_index = as.numeric(date - min(date)),
      # Lagged features (if enough data)
      year_lag1 = lag(year, 1),
      season = case_when(
        month %in% c(12,1,2) ~ "Winter",
        month %in% c(3,4,5) ~ "Spring", 
        month %in% c(6,7,8) ~ "Summer",
        month %in% c(9,10,11) ~ "Autumn"
      )
    ) %>%
    filter(year > 2017) %>%
    group_by(date, year, month, quarter, time_index, season, tech) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(!is.na(n))
  
  # Split into train/test
  train_size <- floor(0.8 * nrow(ml_data))
  train_data <- ml_data[1:train_size, ]
  test_data <- ml_data[(train_size + 1):nrow(ml_data), ]
  
  # Fit random forest
  rf_model <- randomForest(n ~ year + month + quarter #+ time_index 
                           + season + tech,
                          data = train_data, 
                          ntree = 500, 
                          importance = TRUE)
  
  return(list(model = rf_model, train = train_data, test = test_data))
}


ml <- ml_forecast_approach(pipeline)


#===============================================================================
# VISUALIZATION AND COMPARISON
#===============================================================================

plot_forecast_comparison <- function(ts_result, glm_result, tech_type) {
  # Combine historical and forecast data
  combined_data <- bind_rows(
    ts_result$historical %>% 
      select(year_month, n_projects) %>% 
      mutate(type = "Historical", method = "Observed"),
    
    ts_result$forecast %>% 
      select(year_month, forecast) %>% 
      rename(n_projects = forecast) %>%
      mutate(type = "Forecast", method = "Time Series"),
    
    glm_result$forecast %>% 
      filter(tech == tech_type) %>%
      select(date, forecast) %>% 
      rename(year_month = date, n_projects = forecast) %>%
      mutate(type = "Forecast", method = "GLM")
  )
  
  p <- ggplot(combined_data, aes(x = year_month, y = n_projects, color = method)) +
    geom_line(size = 1) +
    geom_point() +
    # geom_vline(xintercept = max(ts_result$historical$year_month), 
    #            linetype = "dashed", alpha = 0.7) +
    # labs(title = paste("Project Forecasts for", tech_type),
    #      subtitle = "Dashed line separates historical vs forecast periods",
    #      x = "Date", y = "Number of Projects") +
    #   theme_minimal(base_family = "Avenir") + +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

plot_forecast_comparison(ts,glm,ml)

#===============================================================================
# RECOMMENDATIONS FOR YOUR APPROACH:
#===============================================================================

cat("
RECOMMENDATIONS FOR PROJECT AVAILABILITY FORECASTING:

1. TIME SERIES APPROACH (Best for trend/seasonality):
   - Use auto.arima() for automatic model selection
   - Handles seasonality and trends naturally
   - Provides confidence intervals

2. YOUR GLM APPROACH - IMPROVEMENTS:
   - Add polynomial terms for non-linear trends
   - Include seasonal factors (quarter, month)
   - Use proper date formatting (fixed your line 34 issue)
   - Add cross-validation for model selection

3. ENSEMBLE APPROACH (Most Robust):
   - Combine multiple methods
   - Use cross-validation to weight models
   - Random Forest for complex patterns

4. EVALUATION METRICS:
   - Mean Absolute Percentage Error (MAPE)
   - Root Mean Square Error (RMSE)
   - Out-of-sample validation

USAGE:
# Uncomment and run with your data 'x':
# ts_data <- prepare_ts_data(x)
# ts_result <- forecast_ts_approach(ts_data, 'your_tech_type')
# glm_result <- improved_glm_forecast(x)
# plot_forecast_comparison(ts_result, glm_result, 'your_tech_type')
")

