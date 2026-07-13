# ============================================================
# Load Forecasting Package
# Purpose:
# Load functions used to build and evaluate time series models
# ============================================================

library(forecast)

# ============================================================
# Time Series Data Preparation
# Purpose:
# Create monthly NEOS recommendation rates to analyse how
# adviser recommendations of NEOS Life change over time
# ============================================================

source("Final_Clean_Code.R")

neos_monthly <- data_clean %>%
  group_by(year, month) %>%
  summarise(
    total_recommendations = n(),
    neos_recommendations = sum(underwriter == "NEOS Life"),
    neos_recommendation_rate =
      neos_recommendations / total_recommendations,
    .groups = "drop"
  ) %>%
  arrange(year, month)

dim(neos_monthly)
head(neos_monthly)
tail(neos_monthly)

# ============================================================
# Create Monthly Date Variable
# Purpose:
# Convert year and month into a proper date format for
# time series analysis
# ============================================================

neos_monthly <- neos_monthly %>%
  mutate(
    month_date = as.Date(
      paste("01", month, year),
      format = "%d %b %Y"
    )
  ) %>%
  arrange(month_date)

# Check the time range
range(neos_monthly$month_date)

# ============================================================
# Plot Monthly NEOS Recommendation Rate
# Purpose:
# Visualise the monthly trend in NEOS recommendation rate
# from March 2022 to March 2023
# ============================================================

ggplot(
  neos_monthly,
  aes(
    x = month_date,
    y = neos_recommendation_rate
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly NEOS Recommendation Rate",
    x = "Month",
    y = "NEOS Recommendation Rate"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal()

# ============================================================
# Create Monthly Time Series Object
# Purpose:
# Convert the monthly NEOS recommendation rate into a formal
# time series object for trend modelling and forecasting
# ============================================================

neos_rate_ts <- ts(
  neos_monthly$neos_recommendation_rate,
  start = c(2022, 3),
  frequency = 12
)

# Display the time series values
neos_rate_ts

# Plot the formal time series object
plot(
  neos_rate_ts,
  main = "NEOS Monthly Recommendation Rate Time Series",
  xlab = "Time",
  ylab = "Recommendation Rate"
)

# ============================================================
# Split Time Series into Training and Testing Sets
# Purpose:
# Use the first 10 months to train forecasting models and
# the final 3 months to evaluate forecast accuracy
# ============================================================

train_ts <- window(
  neos_rate_ts,
  end = c(2022, 12)
)

test_ts <- window(
  neos_rate_ts,
  start = c(2023, 1)
)

# Check the training and testing periods
train_ts
test_ts

# ============================================================
# Build Time Series Forecasting Models
# Purpose:
# Build five forecasting models covered in the course:
# linear trend, quadratic trend, exponential trend,
# simple exponential smoothing and Holt's method
# ============================================================

library(forecast)

# Create a numerical time index for the training data
train_time <- 1:length(train_ts)

# Create future time values for the 3-month testing period
future_time <- (length(train_ts) + 1):
  (length(train_ts) + length(test_ts))


# ------------------------------------------------------------
# Model 1: Linear Trend
# Purpose:
# Estimate a straight upward or downward trend over time
# ------------------------------------------------------------

linear_model <- lm(
  as.numeric(train_ts) ~ train_time
)

linear_forecast <- predict(
  linear_model,
  newdata = data.frame(
    train_time = future_time
  )
)


# ------------------------------------------------------------
# Model 2: Quadratic Trend
# Purpose:
# Allow the trend to curve rather than remain a straight line
# ------------------------------------------------------------

quadratic_model <- lm(
  as.numeric(train_ts) ~
    train_time +
    I(train_time^2)
)

quadratic_forecast <- predict(
  quadratic_model,
  newdata = data.frame(
    train_time = future_time
  )
)


# ------------------------------------------------------------
# Model 3: Exponential Trend
# Purpose:
# Model proportional rather than fixed changes over time
# ------------------------------------------------------------

exponential_model <- lm(
  log(as.numeric(train_ts)) ~ train_time
)

exponential_log_forecast <- predict(
  exponential_model,
  newdata = data.frame(
    train_time = future_time
  )
)

# Convert the log forecasts back to recommendation rates
# and apply the bias-adjustment method from the course
exponential_forecast <- exp(
  exponential_log_forecast +
    summary(exponential_model)$sigma^2 / 2
)


# ------------------------------------------------------------
# Model 4: Simple Exponential Smoothing
# Purpose:
# Forecast the series using its smoothed level without
# explicitly modelling a trend
# ------------------------------------------------------------

ses_model <- ses(
  train_ts,
  h = length(test_ts)
)

ses_forecast <- as.numeric(
  ses_model$mean
)


# ------------------------------------------------------------
# Model 5: Holt Exponential Smoothing
# Purpose:
# Forecast the series using both the current level and trend
# ------------------------------------------------------------

holt_model <- holt(
  train_ts,
  h = length(test_ts),
  damped = FALSE
)

holt_forecast <- as.numeric(
  holt_model$mean
)

# ============================================================
# Evaluate Forecast Accuracy
# Purpose:
# Compare the five models using the final 3 months of data
# Lower MSE, RMSE, MAD and MAPE indicate better predictions
# ============================================================

actual_values <- as.numeric(test_ts)

# Function used to calculate forecast accuracy
calculate_accuracy <- function(actual, predicted) {
  
  errors <- actual - predicted
  
  data.frame(
    MSE = mean(errors^2),
    RMSE = sqrt(mean(errors^2)),
    MAD = mean(abs(errors)),
    MAPE = mean(abs(errors / actual)) * 100
  )
}


# Calculate testing accuracy for each model
linear_accuracy <- calculate_accuracy(
  actual_values,
  linear_forecast
)

quadratic_accuracy <- calculate_accuracy(
  actual_values,
  quadratic_forecast
)

exponential_accuracy <- calculate_accuracy(
  actual_values,
  exponential_forecast
)

ses_accuracy <- calculate_accuracy(
  actual_values,
  ses_forecast
)

holt_accuracy <- calculate_accuracy(
  actual_values,
  holt_forecast
)


# Combine all accuracy results into one table
ts_model_comparison <- rbind(
  data.frame(
    Model = "Linear Trend",
    linear_accuracy
  ),
  
  data.frame(
    Model = "Quadratic Trend",
    quadratic_accuracy
  ),
  
  data.frame(
    Model = "Exponential Trend",
    exponential_accuracy
  ),
  
  data.frame(
    Model = "Simple Exponential Smoothing",
    ses_accuracy
  ),
  
  data.frame(
    Model = "Holt Exponential Smoothing",
    holt_accuracy
  )
)

print(ts_model_comparison)

# ============================================================
# Compare Trend Model Explanatory Power
# Purpose:
# Compare adjusted R-squared for the trend models because
# more complex polynomial models can produce lower training
# errors simply by including additional terms
# ============================================================

trend_model_comparison <- data.frame(
  Model = c(
    "Linear Trend",
    "Quadratic Trend",
    "Exponential Trend"
  ),
  
  Adjusted_R_Squared = c(
    summary(linear_model)$adj.r.squared,
    summary(quadratic_model)$adj.r.squared,
    summary(exponential_model)$adj.r.squared
  )
)

print(trend_model_comparison)

# ============================================================
# Final Forecast Using the Best Model
# Purpose:
# Refit the Holt exponential smoothing model using all
# available monthly data and forecast the next 3 months
# ============================================================

final_holt_model <- holt(
  neos_rate_ts,
  h = 3,
  damped = FALSE
)

# Display the forecast values and prediction intervals
final_holt_model

# Display only the point forecasts
final_forecast_values <- as.numeric(
  final_holt_model$mean
)

print(final_forecast_values)

# ============================================================
# Create Final Forecast Plot
# Purpose:
# Create a cleaner report-ready chart by connecting the
# historical series to the forecast and clearly separating
# the forecast period
# ============================================================

# Create future monthly dates
future_dates <- seq(
  from = max(neos_monthly$month_date) %m+% months(1),
  by = "month",
  length.out = 3
)

# Historical data
historical_plot_data <- neos_monthly %>%
  select(
    month_date,
    neos_recommendation_rate
  )

# Forecast data
forecast_plot_data <- data.frame(
  month_date = future_dates,
  neos_recommendation_rate = as.numeric(final_holt_model$mean),
  lower_80 = as.numeric(final_holt_model$lower[, "80%"]),
  upper_80 = as.numeric(final_holt_model$upper[, "80%"])
)

# Add the last historical observation to the forecast line
forecast_line_data <- bind_rows(
  historical_plot_data %>%
    slice_tail(n = 1),
  forecast_plot_data %>%
    select(
      month_date,
      neos_recommendation_rate
    )
)

# Plot historical trend and future forecast
ggplot() +
  geom_ribbon(
    data = forecast_plot_data,
    aes(
      x = month_date,
      ymin = lower_80,
      ymax = upper_80
    ),
    alpha = 0.15
  ) +
  geom_line(
    data = historical_plot_data,
    aes(
      x = month_date,
      y = neos_recommendation_rate
    ),
    linewidth = 0.9
  ) +
  geom_line(
    data = forecast_line_data,
    aes(
      x = month_date,
      y = neos_recommendation_rate
    ),
    linewidth = 0.9,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = max(historical_plot_data$month_date),
    linetype = "dotted"
  ) +
  geom_point(
    data = forecast_plot_data,
    aes(
      x = month_date,
      y = neos_recommendation_rate
    ),
    size = 2.5
  ) +
  labs(
    title = "NEOS Recommendation Rate Forecast",
    subtitle = "Historical monthly rate and Holt forecast for April–June 2023",
    x = NULL,
    y = "Recommendation Rate",
    caption = "Dashed line represents forecast; shaded area is the 80% prediction interval"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.08, 0.16)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )