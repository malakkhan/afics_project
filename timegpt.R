library(dplyr)
library(lubridate)
library(nixtlar)
library(Metrics)

train_data <- readRDS("AFCS-Project/train_data.rds")
val_data   <- readRDS("AFCS-Project/validation_data.rds")

stopifnot(
  "item_id" %in% names(train_data),
  "date"    %in% names(train_data),
  "units"   %in% names(train_data)
)

stopifnot(
  "item_id" %in% names(val_data),
  "date"    %in% names(val_data),
  "units"   %in% names(val_data)
)

train_data$date <- as.Date(train_data$date)
val_data$date   <- as.Date(val_data$date)

nixtla_set_api_key("nixak-x72bdiwGFCJZJheii9ZpMh49431M3dgCi0Z43v1hWjWtPnKdIv1U8TVfUX2h6S2F6G3VaivBZ0zYwnUz")

h_val <- length(unique(val_data$date))

train_data <- train_data %>%
  rename(
    unique_id = item_id,
    ds = date,
    y = units
  )

val_data <- val_data %>%
  rename(
    unique_id = item_id,
    ds = date,
    y = units
  )

xreg_cols <- intersect(c("sell_price", "snap_TX"), names(train_data))

fc_val <- nixtla_client_forecast(
  df        = train_data,
  h         = h_val,
  freq      = "D",
  X_df      = if(length(xreg_cols) > 0) train_data[, xreg_cols, drop = FALSE] else NULL
)

xreg_cols <- intersect(
  c("sell_price", "snap_TX", "event_name_1", "event_type_1",
    "event_name_2", "event_type_2", "weekday", "wday", "month", "year"),
  names(train_data)
)

# Prepare X_df
X_df_full <- if(length(xreg_cols) > 0) train_data[, xreg_cols, drop = FALSE] else NULL

fcst <- nixtla_client_forecast(
  df = train_data,
  h = h_val,
  freq = "D", # Use 'H' for hourly, 'D' for daily, 'M' for monthly, etc.
  level = c(80, 95), # Optional: Add confidence intervals
  finetune_steps = 2 # Optional: Fine-tune on your data for better accuracy
)

rmse_val <- rmse(val_data$y, fcst$TimeGPT)
mae_val  <- mae(val_data$y, fcst$TimeGPT)
mape_val <- mape(val_data$y, fcst$TimeGPT)

cat("Validation RMSE:", rmse_val, "\n")
cat("Validation MAE :", mae_val,  "\n")
cat("Validation MAPE:", mape_val, "\n")

