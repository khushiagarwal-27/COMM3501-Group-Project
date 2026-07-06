#=========================================================
# TIME SERIES ANALYSIS
# Purpose: Forecast monthly NEOS recommendation count
# Based on COMM3501 M3 Time Series methods
#=========================================================

#---------------------------------------------------------
# 0. Load packages
#---------------------------------------------------------

packages <- c("dplyr", "ggplot2", "lubridate", "tidyr", "forecast", "zoo")

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p)
  }
}

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(forecast)
library(zoo)


#---------------------------------------------------------
# 1. Check cleaned data exists
#---------------------------------------------------------

if (!exists("insurance_clean")) {
  stop("insurance_clean does not exist. Please run the data cleaning script first.")
}


#=========================================================
# 2. Create monthly NEOS recommendation count
#=========================================================

monthly_neos <- insurance_clean %>%
  mutate(
    Date = as.Date(Date),
    Month = floor_date(Date, unit = "month")
  ) %>%
  filter(Underwriter == "NEOS Life") %>%
  group_by(Month) %>%
  summarise(
    NEOS_Count = n(),
    Avg_AnnualisedPremium = mean(AnnualisedPremium, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Month)

# Fill missing months with 0 recommendations
monthly_neos <- monthly_neos %>%
  complete(
    Month = seq.Date(
      from = min(Month),
      to = max(Month),
      by = "month"
    ),
    fill = list(
      NEOS_Count = 0,
      Avg_AnnualisedPremium = NA
    )
  ) %>%
  arrange(Month) %>%
  mutate(
    t = row_number(),
    Month_Num = month(Month),
    Month_Factor = factor(
      month(Month),
      levels = 1:12,
      labels = month.abb
    )
  )

# Check the time series data
monthly_neos
dim(monthly_neos)


#=========================================================
# 3. Plot original time series
# Purpose: visually inspect trend, seasonality, and noise
#=========================================================

ggplot(monthly_neos, aes(x = Month, y = NEOS_Count)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly NEOS Recommendation Count",
    x = "Month",
    y = "Number of NEOS Recommendations"
  )


#=========================================================
# 4. Moving average smoothing
# Purpose: smooth short-term noise
#=========================================================

monthly_neos <- monthly_neos %>%
  mutate(
    MA_3 = rollmean(
      NEOS_Count,
      k = 3,
      fill = NA,
      align = "right"
    )
  )

ggplot(monthly_neos, aes(x = Month)) +
  geom_line(aes(y = NEOS_Count, linetype = "Actual")) +
  geom_line(aes(y = MA_3, linetype = "3-Month Moving Average")) +
  labs(
    title = "Monthly NEOS Recommendations with Moving Average",
    x = "Month",
    y = "Number of NEOS Recommendations",
    linetype = "Series"
  )


#=========================================================
# 5. Time-based training / validation split
# Important: do not randomly split time series data
#=========================================================

train_size <- floor(0.8 * nrow(monthly_neos))

train_ts <- monthly_neos[1:train_size, ]
valid_ts <- monthly_neos[(train_size + 1):nrow(monthly_neos), ]

train_ts
valid_ts


#=========================================================
# 6. Define accuracy metrics
# MSE, MAD, and MAPE are used in the lecture
#=========================================================

mse <- function(actual, predicted) {
  mean((actual - predicted)^2, na.rm = TRUE)
}

mad <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

mape <- function(actual, predicted) {
  mean(
    ifelse(actual == 0, NA, abs((actual - predicted) / actual)),
    na.rm = TRUE
  ) * 100
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}


#=========================================================
# 7. Model 1: Moving Average Forecast
#=========================================================

rolling_ma_forecast <- function(train_values, valid_values, k = 3) {
  
  history <- train_values
  predictions <- numeric(length(valid_values))
  
  for (i in seq_along(valid_values)) {
    predictions[i] <- mean(tail(history, k), na.rm = TRUE)
    
    # After each validation period, actual value becomes known
    history <- c(history, valid_values[i])
  }
  
  return(predictions)
}

valid_ts$pred_moving_average <- rolling_ma_forecast(
  train_values = train_ts$NEOS_Count,
  valid_values = valid_ts$NEOS_Count,
  k = 3
)


#=========================================================
# 8. Model 2: Simple Exponential Smoothing
#=========================================================

start_year <- year(min(train_ts$Month))
start_month <- month(min(train_ts$Month))

train_ts_object <- ts(
  train_ts$NEOS_Count,
  frequency = 12,
  start = c(start_year, start_month)
)

ses_model <- ses(
  train_ts_object,
  h = nrow(valid_ts)
)

valid_ts$pred_exponential_smoothing <- as.numeric(ses_model$mean)

summary(ses_model)


#=========================================================
# 9. Model 3: Linear Trend Model
#=========================================================

linear_trend_model <- lm(
  NEOS_Count ~ t,
  data = train_ts
)

summary(linear_trend_model)

valid_ts$pred_linear_trend <- predict(
  linear_trend_model,
  newdata = valid_ts
)


#=========================================================
# 10. Model 4: Quadratic Trend Model
#=========================================================

quadratic_trend_model <- lm(
  NEOS_Count ~ t + I(t^2),
  data = train_ts
)

summary(quadratic_trend_model)

valid_ts$pred_quadratic_trend <- predict(
  quadratic_trend_model,
  newdata = valid_ts
)


#=========================================================
# 11. Optional Model 5: Trend + Seasonal Dummy Model
# Only run if training data includes all 12 months
#=========================================================

use_seasonal_model <- length(unique(train_ts$Month_Num)) == 12

if (use_seasonal_model) {
  
  seasonal_trend_model <- lm(
    NEOS_Count ~ t + Month_Factor,
    data = train_ts
  )
  
  summary(seasonal_trend_model)
  
  valid_ts$pred_seasonal_trend <- predict(
    seasonal_trend_model,
    newdata = valid_ts
  )
  
} else {
  
  valid_ts$pred_seasonal_trend <- NA
  
  print("Seasonal dummy model was skipped because training data does not include all 12 months.")
}


#=========================================================
# 12. Compare model accuracy on validation set
#=========================================================

time_series_comparison <- data.frame(
  Model = c(
    "Moving Average",
    "Exponential Smoothing",
    "Linear Trend",
    "Quadratic Trend",
    "Trend + Seasonal Dummies"
  ),
  MSE = c(
    mse(valid_ts$NEOS_Count, valid_ts$pred_moving_average),
    mse(valid_ts$NEOS_Count, valid_ts$pred_exponential_smoothing),
    mse(valid_ts$NEOS_Count, valid_ts$pred_linear_trend),
    mse(valid_ts$NEOS_Count, valid_ts$pred_quadratic_trend),
    mse(valid_ts$NEOS_Count, valid_ts$pred_seasonal_trend)
  ),
  RMSE = c(
    rmse(valid_ts$NEOS_Count, valid_ts$pred_moving_average),
    rmse(valid_ts$NEOS_Count, valid_ts$pred_exponential_smoothing),
    rmse(valid_ts$NEOS_Count, valid_ts$pred_linear_trend),
    rmse(valid_ts$NEOS_Count, valid_ts$pred_quadratic_trend),
    rmse(valid_ts$NEOS_Count, valid_ts$pred_seasonal_trend)
  ),
  MAD = c(
    mad(valid_ts$NEOS_Count, valid_ts$pred_moving_average),
    mad(valid_ts$NEOS_Count, valid_ts$pred_exponential_smoothing),
    mad(valid_ts$NEOS_Count, valid_ts$pred_linear_trend),
    mad(valid_ts$NEOS_Count, valid_ts$pred_quadratic_trend),
    mad(valid_ts$NEOS_Count, valid_ts$pred_seasonal_trend)
  ),
  MAPE = c(
    mape(valid_ts$NEOS_Count, valid_ts$pred_moving_average),
    mape(valid_ts$NEOS_Count, valid_ts$pred_exponential_smoothing),
    mape(valid_ts$NEOS_Count, valid_ts$pred_linear_trend),
    mape(valid_ts$NEOS_Count, valid_ts$pred_quadratic_trend),
    mape(valid_ts$NEOS_Count, valid_ts$pred_seasonal_trend)
  )
)

# Remove skipped seasonal model if it has NA results
time_series_comparison <- time_series_comparison %>%
  filter(!is.na(MSE))

time_series_comparison


#=========================================================
# 13. Select best model
# We choose the model with the lowest RMSE
#=========================================================

best_model_name <- time_series_comparison$Model[
  which.min(time_series_comparison$RMSE)
]

best_model_name


#=========================================================
# 14. Plot actual vs predicted values on validation set
#=========================================================

validation_plot_data <- valid_ts %>%
  select(
    Month,
    Actual = NEOS_Count,
    Moving_Average = pred_moving_average,
    Exponential_Smoothing = pred_exponential_smoothing,
    Linear_Trend = pred_linear_trend,
    Quadratic_Trend = pred_quadratic_trend,
    Seasonal_Trend = pred_seasonal_trend
  ) %>%
  pivot_longer(
    cols = -Month,
    names_to = "Series",
    values_to = "Count"
  ) %>%
  filter(!is.na(Count))

ggplot(validation_plot_data, aes(x = Month, y = Count, linetype = Series)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Validation Results: Actual vs Forecasted NEOS Recommendations",
    x = "Month",
    y = "Number of NEOS Recommendations",
    linetype = "Series"
  )


#=========================================================
# 15. Refit selected model on full data and forecast next 3 months
#=========================================================

future_months <- data.frame(
  Month = seq.Date(
    from = max(monthly_neos$Month) %m+% months(1),
    by = "month",
    length.out = 3
  )
) %>%
  mutate(
    t = (max(monthly_neos$t) + 1):(max(monthly_neos$t) + 3),
    Month_Num = month(Month),
    Month_Factor = factor(
      month(Month),
      levels = 1:12,
      labels = month.abb
    )
  )


# Helper function for future moving average forecast
future_ma_forecast <- function(values, h = 3, k = 3) {
  
  history <- values
  predictions <- numeric(h)
  
  for (i in 1:h) {
    predictions[i] <- mean(tail(history, k), na.rm = TRUE)
    history <- c(history, predictions[i])
  }
  
  return(predictions)
}


if (best_model_name == "Moving Average") {
  
  future_forecast <- future_months %>%
    mutate(
      Forecast_NEOS_Count = future_ma_forecast(
        values = monthly_neos$NEOS_Count,
        h = 3,
        k = 3
      )
    )
  
} else if (best_model_name == "Exponential Smoothing") {
  
  full_ts_object <- ts(
    monthly_neos$NEOS_Count,
    frequency = 12,
    start = c(
      year(min(monthly_neos$Month)),
      month(min(monthly_neos$Month))
    )
  )
  
  final_ses_model <- ses(
    full_ts_object,
    h = 3
  )
  
  future_forecast <- future_months %>%
    mutate(
      Forecast_NEOS_Count = as.numeric(final_ses_model$mean)
    )
  
} else if (best_model_name == "Linear Trend") {
  
  final_linear_model <- lm(
    NEOS_Count ~ t,
    data = monthly_neos
  )
  
  future_forecast <- future_months %>%
    mutate(
      Forecast_NEOS_Count = predict(
        final_linear_model,
        newdata = future_months
      )
    )
  
} else if (best_model_name == "Quadratic Trend") {
  
  final_quadratic_model <- lm(
    NEOS_Count ~ t + I(t^2),
    data = monthly_neos
  )
  
  future_forecast <- future_months %>%
    mutate(
      Forecast_NEOS_Count = predict(
        final_quadratic_model,
        newdata = future_months
      )
    )
  
} else if (best_model_name == "Trend + Seasonal Dummies") {
  
  final_seasonal_model <- lm(
    NEOS_Count ~ t + Month_Factor,
    data = monthly_neos
  )
  
  future_forecast <- future_months %>%
    mutate(
      Forecast_NEOS_Count = predict(
        final_seasonal_model,
        newdata = future_months
      )
    )
}


# Recommendation count cannot be negative
future_forecast <- future_forecast %>%
  mutate(
    Forecast_NEOS_Count = pmax(0, Forecast_NEOS_Count),
    Forecast_NEOS_Count_Rounded = round(Forecast_NEOS_Count, 0)
  )

future_forecast


#=========================================================
# 16. Plot historical data and future forecast
#=========================================================

forecast_plot_data <- monthly_neos %>%
  select(Month, NEOS_Count) %>%
  rename(Count = NEOS_Count) %>%
  mutate(Type = "Historical") %>%
  bind_rows(
    future_forecast %>%
      transmute(
        Month,
        Count = Forecast_NEOS_Count,
        Type = "Forecast"
      )
  )

ggplot(forecast_plot_data, aes(x = Month, y = Count, linetype = Type)) +
  geom_line() +
  geom_point() +
  labs(
    title = "NEOS Recommendation Trend and 3-Month Forecast",
    x = "Month",
    y = "Number of NEOS Recommendations",
    linetype = "Series"
  )


#=========================================================
# 17. Save output files
#=========================================================

write.csv(
  monthly_neos,
  "monthly_neos_recommendations.csv",
  row.names = FALSE
)

write.csv(
  time_series_comparison,
  "time_series_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  future_forecast,
  "neos_3_month_forecast.csv",
  row.names = FALSE
)
