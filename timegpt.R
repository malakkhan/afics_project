library(dplyr)
library(lubridate)
library(nixtlar)
library(Metrics)

train_data <- readRDS("AFCS-Project/train_data.rds")
val_data   <- readRDS("AFCS-Project/validation_data.rds")
test_data   <- readRDS("AFCS-Project/test_data.rds")

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

stopifnot(
  "item_id" %in% names(test_data),
  "date"    %in% names(test_data),
  "units"   %in% names(test_data)
)

train_data$date <- as.Date(train_data$date)
val_data$date   <- as.Date(val_data$date)
test_data$date   <- as.Date(test_data$date)

nixtla_set_api_key("nixak-x72bdiwGFCJZJheii9ZpMh49431M3dgCi0Z43v1hWjWtPnKdIv1U8TVfUX2h6S2F6G3VaivBZ0zYwnUz")

h_test <- length(unique(test_data$date))

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

full_data <- bind_rows(train_data, val_data)

fcst <- nixtla_client_forecast(
  df = full_data,
  h = h_test,
  freq = "D", 
  level = c(80, 95), 
  finetune_steps = 10
)

rmse_val <- rmse(test_data$units, fcst$TimeGPT)
mae_val  <- mae(test_data$units, fcst$TimeGPT)
mape_val <- mape(test_data$units, fcst$TimeGPT)

cat("RMSE:", rmse_val, "\n")
cat("MAE :", mae_val,  "\n")
cat("MAPE:", mape_val, "\n")

saveRDS(fcst, file = "timeGPTSubmission.rds")
