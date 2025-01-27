# Q2.1
library(fpp3)
library(tidyverse)

Usage <- read_csv("https://jagelves.github.io/Data/ElectricityBill.csv")

# Q2.2
Usage <- Usage %>%
  mutate(date = yearmonth(seq(from = ymd("2021-06-01"),length.out = 38, by = "1 month")))

# Q2.3
usage_ts <- Usage %>%
  rename(kwh = `Usage (kWh)`) %>%
  select(date, kwh) %>%
  as_tsibble(index = date)

# Q2.4
usage_ts %>%
  autoplot(kwh) +
  ggtitle("Electric Usage") +
  theme_classic()

# Q2.5
train_usage <- usage_ts %>%
  filter_index("2021 Jul" ~ "2023 Apr")
test_usage <- usage_ts %>%
  filter_index("2023 May" ~ .)

# Q2.6
fit <- train_usage %>%
  model(ARIMA = ARIMA(kwh), ETS = ETS(kwh), TSLM = TSLM(kwh ~ trend() + season()))

# Q2.7
accuracy_results <- fit %>%
  forecast(h = 4) %>%
  accuracy(test_usage) %>%
  arrange(RMSE)

accuracy_results

# Q2.8
fit %>%
  glance() %>%
  select(.model, AIC, AICc, BIC)

# Q2.9
forecast_plot <- usage_ts %>%
  model(TSLM = TSLM(kwh ~ trend() + season())) %>%
  forecast(h = 4) %>%
  autoplot(usage_ts, level = 95) +
  autolayer(usage_ts, color = "blue") +
  ggtitle("Monthly Electric Usage in kWh") +
  theme_classic() +
  labs(x = NULL, y = NULL)

forecast_plot

# Q3
# load old Netflix data from M6
Netflix <- read_csv("https://jagelves.github.io/Data/Netflix.csv")

# add on the missing data for 2024 Q3 from "Final-Q3-24-Shareholder-Letter.pdf"
Netflix <- Netflix %>%
  bind_rows(tibble(Quarter = "2024 Q3", Subscribers = 282.72))

# convert to tsibble
Netflix_ts <- Netflix %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

# fit with TSLM as this had lowest AIC during M6 assignment
fit_Netflix <- Netflix_ts %>%
  model(TSLM_Best = TSLM(Subscribers ~ trend() + I(trend()^2) + season()))

# forecast next quarter (2024 Q4)
forecast_Netflix <- fit_Netflix %>%
  forecast(h = 1, level = c(90))

# forecast table predicts 291 million subscribers with sd of 86
forecast_Netflix

# forecasst visualization
forecast_Netflix %>%
  autoplot(Netflix_ts, level = 90) +
  ggtitle("Netflix Subscribers Forecast", subtitle = "2013 Q1 to 2024 Q4 (estimated)") +
  theme_classic() +
  labs(x = NULL, y = "Subscribers (Millions)")
