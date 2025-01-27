# Q2.1
library(fpp3)
library(tidyverse)
Netflix <- read_csv("https://jagelves.github.io/Data/Netflix.csv")

# Q2.2
Netflix_ts <- Netflix %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)

# Q2.3
Netflix_ts %>%
  autoplot(Subscribers) +
  ggtitle("Netflix World Wide Subscribers (Millions)",
          subtitle = "2013 Q1 to 2023 Q2")

# Q2.4
train_net <- Netflix_ts %>%
  filter_index("2013 Q1" ~ "2021 Q4")
test_net <- Netflix_ts %>%
  filter_index("2022 Q1" ~ "2023 Q2")

# Q2.5
cv <- train_net %>%
  stretch_tsibble(.init = 12, .step = 4) %>%
  model(
    ETS_AAdA = ETS(Subscribers ~ error("A") + trend("Ad") + season("A")),
    ETS_Auto = ETS(Subscribers),
    TSLM_Trend = TSLM(Subscribers ~ trend() + I(trend()^2)),
    TSLM_Season = TSLM(Subscribers ~ trend() + I(trend()^2) + season())) %>%
  forecast(h = 4) %>%
  accuracy(test_net)


# Q2.6
# Lowest RMSE (~12.5) ETS(A,Ad,A)


# Q2.7:
fit <- Netflix_ts %>%
  model(
    ETS_Best = ETS(Subscribers ~ error("A") + trend("Ad") + season("A")),
    TSLM_Best = TSLM(Subscribers ~ trend() + I(trend()^2) + season()))

# Q2.8
fit %>%
  glance() %>%
  select(.model, AIC, AICc, BIC)

# Q2.9
forecast <- fit %>%
  forecast(h = 4, level = c(90))

# Q2.10
forecast %>%
  autoplot(Netflix_ts, level = 90) +
  ggtitle("Netflix Subscribers", subtitle = "2013 Q1 to 2024 Q2 (estimated)") +
  theme_classic() +
  labs(x = NULL, y = NULL)

