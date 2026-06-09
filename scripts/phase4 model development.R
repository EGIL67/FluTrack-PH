# ============================================================
# FluTrack PH - Phase 4 Model Development and Evaluation
# ============================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(forecast)
library(xgboost)
library(Metrics)
library(ggplot2)

# Create output folders
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

# Load raw FluNet data
flu_raw <- read_csv(
  "data/raw/VIW_FNT.csv",
  show_col_types = FALSE
) %>%
  clean_names()

# Filter Philippines data and clean variables
flu_ph <- flu_raw %>%
  filter(str_detect(str_to_lower(country_area_territory), "phil")) %>%
  mutate(
    iso_weekstartdate = as.Date(iso_weekstartdate),
    iso_year = as.integer(iso_year),
    iso_week = as.integer(iso_week),
    inf_a = replace_na(as.numeric(inf_a), 0),
    inf_b = replace_na(as.numeric(inf_b), 0),
    inf_all = as.numeric(inf_all),
    inf_all = if_else(is.na(inf_all), inf_a + inf_b, inf_all)
  ) %>%
  group_by(iso_year, iso_week, iso_weekstartdate) %>%
  summarise(
    total_cases = sum(inf_all, na.rm = TRUE),
    inf_a = sum(inf_a, na.rm = TRUE),
    inf_b = sum(inf_b, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(iso_weekstartdate)

# Feature engineering
model_data <- flu_ph %>%
  arrange(iso_weekstartdate) %>%
  mutate(
    ab_ratio = if_else(inf_b == 0, inf_a, inf_a / inf_b),
    month = lubridate::month(iso_weekstartdate),
    lag_1 = lag(total_cases, 1),
    lag_2 = lag(total_cases, 2),
    lag_4 = lag(total_cases, 4),
    rolling_4 = zoo::rollmean(total_cases, k = 4, fill = NA, align = "right")
  ) %>%
  drop_na(lag_1, lag_2, lag_4, rolling_4)

write_csv(model_data, "data/processed/model_ready_flu.csv")

# Train-test split
train_data <- model_data %>%
  filter(iso_year >= 2010, iso_year <= 2023)

test_data <- model_data %>%
  filter(iso_year == 2024)

# ============================================================
# ARIMA MODEL
# ============================================================

train_ts <- ts(
  train_data$total_cases,
  frequency = 52
)

arima_model <- auto.arima(
  train_ts,
  seasonal = TRUE,
  stepwise = TRUE,
  approximation = TRUE
)

arima_forecast <- forecast(
  arima_model,
  h = nrow(test_data)
)

arima_pred <- as.numeric(arima_forecast$mean)

arima_rmse <- rmse(test_data$total_cases, arima_pred)
arima_mae <- mae(test_data$total_cases, arima_pred)

arima_mape <- mean(
  abs(
    (test_data$total_cases - arima_pred) /
      pmax(test_data$total_cases, 1)
  )
) * 100

arima_results <- tibble(
  Model = "ARIMA",
  RMSE = arima_rmse,
  MAE = arima_mae,
  MAPE = arima_mape
)

# ============================================================
# XGBOOST MODEL
# ============================================================

x_features <- c(
  "iso_week",
  "month",
  "lag_1",
  "lag_2",
  "lag_4",
  "rolling_4",
  "inf_a",
  "inf_b",
  "ab_ratio"
)

x_train <- as.matrix(train_data[, x_features])
y_train <- train_data$total_cases

x_test <- as.matrix(test_data[, x_features])
y_test <- test_data$total_cases

xgb_model <- xgboost(
  x = x_train,
  y = y_train,
  objective = "reg:squarederror",
  nrounds = 100,
  max_depth = 4,
  learning_rate = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_pred <- predict(
  xgb_model,
  x_test
)

xgb_rmse <- rmse(y_test, xgb_pred)
xgb_mae <- mae(y_test, xgb_pred)

xgb_mape <- mean(
  abs(
    (y_test - xgb_pred) /
      pmax(y_test, 1)
  )
) * 100

xgb_results <- tibble(
  Model = "XGBoost",
  RMSE = xgb_rmse,
  MAE = xgb_mae,
  MAPE = xgb_mape
)

# ============================================================
# MODEL COMPARISON OUTPUT
# ============================================================

model_comparison <- bind_rows(arima_results, xgb_results)

write_csv(model_comparison, "outputs/model_comparison.csv")

figure5 <- model_comparison %>%
  ggplot(aes(x = Model, y = RMSE)) +
  geom_col() +
  labs(
    title = "Figure 5. Forecasting Model Comparison",
    subtitle = "XGBoost achieved lower RMSE than ARIMA on 2024 test data",
    x = "Model",
    y = "RMSE"
  ) +
  theme_minimal()

ggsave(
  "outputs/figure5_model_comparison.png",
  plot = figure5,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# PREDICTED VS ACTUAL OUTPUT
# ============================================================

predicted_vs_actual <- test_data %>%
  select(date = iso_weekstartdate, actual_cases = total_cases) %>%
  mutate(
    ARIMA = arima_pred,
    XGBoost = xgb_pred
  )

write_csv(predicted_vs_actual, "outputs/predicted_vs_actual_2024.csv")

pred_plot <- predicted_vs_actual %>%
  pivot_longer(
    cols = c(actual_cases, ARIMA, XGBoost),
    names_to = "Series",
    values_to = "Cases"
  ) %>%
  ggplot(aes(x = date, y = Cases, color = Series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted vs Actual Weekly Influenza Cases, 2024",
    x = "Date",
    y = "Weekly cases"
  ) +
  theme_minimal()

ggsave(
  "outputs/predicted_vs_actual_2024.png",
  plot = pred_plot,
  width = 9,
  height = 5,
  dpi = 300
)

# ============================================================
# XGBOOST FEATURE IMPORTANCE
# ============================================================

importance <- xgb.importance(
  feature_names = x_features,
  model = xgb_model
)

write_csv(
  as_tibble(importance),
  "outputs/xgboost_feature_importance.csv"
)

importance_plot <- importance %>%
  as_tibble() %>%
  arrange(desc(Gain)) %>%
  ggplot(
    aes(
      x = reorder(Feature, Gain),
      y = Gain
    )
  ) +
  geom_col() +
  coord_flip() +
  labs(
    title = "XGBoost Feature Importance",
    x = "Feature",
    y = "Gain"
  ) +
  theme_minimal()

ggsave(
  "outputs/xgboost_feature_importance.png",
  plot = importance_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# ============================================================
# FOUR-WEEK FORECAST OUTPUT
# ============================================================

latest_data <- model_data %>%
  arrange(iso_weekstartdate) %>%
  tail(1)

future_forecasts <- tibble()
current_data <- latest_data

for (i in 1:4) {
  next_date <- current_data$iso_weekstartdate + lubridate::weeks(1)
  next_week <- lubridate::isoweek(next_date)
  next_year <- lubridate::isoyear(next_date)
  next_month <- lubridate::month(next_date)
  
  next_row <- current_data %>%
    mutate(
      iso_weekstartdate = next_date,
      iso_year = next_year,
      iso_week = next_week,
      month = next_month,
      lag_4 = lag_2,
      lag_2 = lag_1,
      lag_1 = total_cases,
      rolling_4 = mean(c(total_cases, lag_1, lag_2, lag_4), na.rm = TRUE)
    )
  
  x_future <- as.matrix(next_row[, x_features])
  predicted_cases <- predict(xgb_model, x_future)
  
  risk_level <- case_when(
    predicted_cases >= quantile(model_data$total_cases, 0.75, na.rm = TRUE) ~ "High",
    predicted_cases >= quantile(model_data$total_cases, 0.50, na.rm = TRUE) ~ "Moderate",
    TRUE ~ "Low"
  )
  
  future_forecasts <- bind_rows(
    future_forecasts,
    tibble(
      forecast_week = i,
      iso_year = next_year,
      iso_week = next_week,
      date = next_date,
      predicted_cases = round(as.numeric(predicted_cases), 0),
      risk_level = risk_level
    )
  )
  
  current_data <- next_row %>%
    mutate(total_cases = as.numeric(predicted_cases))
}

write_csv(
  future_forecasts,
  "outputs/xgboost_4week_forecast.csv"
)

# Print final outputs
print(model_comparison)
print(future_forecasts, width = Inf)
print(importance)
list.files("outputs")