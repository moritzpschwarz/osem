library(conflicted)
library(eurostat)
library(gets)
library(midasr)
library(forecast)
library(tidyverse)
library(janitor)

conflict_prefer("filter", "dplyr")
conflict_prefer("as.zoo.data.frame", "zoo")

saving <- FALSE

# AT
q_GDP <- c(
  "namq_10_gdp", # Hauptkomponenten
  "namq_10_fcs", # Konsum
  "namq_10_exi" # Exporte
)


# Umwelt
env <- c(
  "env_ac_aibrid_r2", # Übergang zu Emissionsinventaren
  "t2020_35", # ESD
  "env_air_gge" # THG nach Quellsektor
)

# EU
env_eu <- "env_ac_aigg_q"




overall <- tibble()
for (var in q_GDP) {
  full <- get_eurostat(id = var)

  full %>%
    filter(geo == "AT") %>%
    label_eurostat() -> intermed


  overall <- bind_rows(overall, intermed)
}

overall %>%
  filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  ggplot(aes(x = time, y = values, color = na_item)) +
  geom_line() +
  facet_wrap(~na_item, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

if (saving) {
  write_csv(overall, "Quarterly GDP Components.csv")
}

# THG nach Quellsektor
env_air_gge_raw <- get_eurostat("env_air_gge")
env_air_gge_raw %>%
  filter(geo == "AT") %>%
  filter(airpol %in% c("GHG", "CO2"), unit == "MIO_T") -> env_air_gge
# label_eurostat() %>%  #distinct(src_crf) %>% View


# ESD Emissions
t2020_35_raw <- get_eurostat("t2020_35", time_format = "raw")
t2020_35_raw %>%
  filter(geo == "AT", indic_eu == "T2020_35T") %>%
  select(-indic_eu) %>%
  mutate(unit = "MIO_T") -> t2020_35 # %>% #label_eurostat()


overall_env <- tibble()
for (var in env) {
  full <- get_eurostat(id = var, time_format = "raw")

  full %>%
    filter(geo == "AT") %>%
    label_eurostat() -> intermed


  overall_env <- bind_rows(overall_env, intermed)
}


overall_env %>%
  filter(unit == "Tonne")

filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  ggplot(aes(x = time, y = values, color = na_item)) +
  geom_line() +
  facet_wrap(~na_item, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

#
#
#
#
# full_df <- get_eurostat(id = "namq_10_gdp")
#
# full_df %>%
#   filter(geo == "AT") %>%
#   label_eurostat() -> df_AT
#
# df_AT %>%
#   distinct(na_item) %>% View


# df_AT %>%
overall %>%
  filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  ggplot(aes(x = time, y = values, color = na_item)) +
  geom_line() +
  facet_wrap(~na_item, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")


df_AT %>%
  filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year, na_item) %>%
  summarise(values = mean(values), .groups = "drop") %>%
  ggplot(aes(x = year, y = values, color = na_item)) +
  geom_line() +
  facet_wrap(~na_item, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")





# Trial gets --------------------------------------------------------------
gdp_vars_to_estimate <- c(
  "Gross capital formation",
  "Final consumption expenditure of general government",
  "Final consumption expenditure",
  "Value added, gross",
  "Gross domestic product at market prices",
  "Exports of goods",
  "Final consumption expenditure of households"
)

overall %>%
  filter(time >= as.Date("2005-01-01") & time < as.Date("2021-01-01")) %>%
  filter(na_item %in% gdp_vars_to_estimate) %>%
  filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  mutate(year = lubridate::year(time)) %>%
  group_by(year, na_item) %>%
  summarise(values = mean(values), .groups = "drop") %>%
  pivot_wider(year, names_from = na_item, values_from = values) %>%
  janitor::clean_names() %>%
  arrange(year) %>%
  select(-year) -> gdp_vars_annual


tibble(
  orig = gdp_vars_to_estimate,
  new = overall %>% filter(na_item %in% gdp_vars_to_estimate) %>%
    filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
    pivot_wider(time, names_from = na_item, values_from = values, values_fn = list) %>% select(-time) %>%
    clean_names() %>%
    names()
) -> dictionary

gets_result <- gets(arx(y = t2020_35$values, mxreg = as.data.frame(gdp_vars_annual)))
gdp_vars_to_estimate_gets <- dictionary %>%
  filter(new %in% gets_result$aux$mXnames) %>%
  pull(orig)

# Trial MiDaS -------------------------------------------------------------

# https://github.com/mpiktas/midasr-user-guide/blob/master/midasr-user-guide.pdf
library(midasr)


#
# data("USrealgdp")
# data("USunempr")
#
# y.ar <- diff(log(USrealgdp))
# xx <- window(diff(USunempr), start = 1949)
# trend <- 1:length(y.ar)
#
# ##Fit AR(1) model
# mr_ar <- midas_r(y.ar ~ trend + mls(y.ar, 1, 1) + fmls(xx, 11, 12, nealmon), start = list(xx = rep(0, 3)))
#
# ##First order MIDAS-AR* restricted model
# mr_arstar <-  midas_r(y.ar ~ trend + mls(y.ar, 1, 1, "*") + fmls(xx, 11, 12, nealmon), start = list(xx = rep(0, 3)))
#



overall %>%
  filter(time >= as.Date("2005-01-01") & time < as.Date("2021-01-01")) %>%
  filter(na_item %in% gdp_vars_to_estimate_gets) %>%
  filter(s_adj == "Seasonally and calendar adjusted data", unit == "Chain linked volumes (2015), million euro") %>%
  pivot_wider(id_cols = time, names_from = na_item, values_from = values) %>%
  janitor::clean_names() %>%
  arrange(time) %>%
  select(-time) %>%
  ts(start = c(2005, 1), frequency = 4) -> gdp_vars


y.var <- ts(t2020_35$values, start = 2005, frequency = 1)


# test_fun <- function(data){
#   output <- list()
#   for(i in 1:ncol(data)){
#     fmls(data[,i],3,4,nealmon)
#   }
#   fmls(
#
#   start_list <- rep(list(rep(0,4)),length(colnames(data)))
#   names(start_list) <- colnames(data)
#
#
# }

# midas_r(y.var <- fmls(gdp_vars, 3, 4, nealmon))
# gdp_vars %>%
#   as.list %>%
#   lapply(FUN = function(x){fmls(x,3,4,nealmon)}) -> x_vars



arima_outcome <- auto.arima(y = y.var)
plot(forecast(arima_outcome, h = 5))

n_ahead <- 12
data <- gdp_vars

estimate_and_predict <- function(data, n_ahead) {
  start_list <- rep(list(rep(0, 4)), length(colnames(data)))
  names(start_list) <- colnames(data)

  formula_to_use <- as.formula(paste0("y.var ~ ", paste0("fmls(", colnames(data), ",3,4,nealmon)", collapse = " + ")))

  midas_model <- midas_r(formula_to_use, start = start_list, data = append(list(y.var = y.var), as.list(data)))


  newdata <- data %>%
    as_tibble() %>%
    slice(rep(n(), n_ahead)) %>%
    as.list()
  fcast <- forecast(midas_model, newdata = newdata, se = TRUE, show_progress = TRUE, method = "static")
  summary(fcast)
  plot(fcast)

  out <- list()
  out$model <- midas_model
  out$forecast <- fcast
  return(out)
}


estimate_and_predict(gdp_vars, 12)

# midas_r(formula_to_use, start = list(value_added_gross = rep(0, 4),
#                                                                      gross_domestic_product_at_market_prices = rep(0,4)))
# test1 <- fmls(gdp_vars[,c(1)], 3, 4, nealmon)
# test2 <- fmls(gdp_vars[,c(2)], 3, 4, nealmon)
#
# midas_r(y.var ~ test1 + test2, start = list(test1 = rep(0,4),
#                                             test2 = rep(10,4)))
#
#
# midas_lstr_plain(y = y.var,X = test1, start_x = rep(0,3), start_lstr = 0, weight = )
#
#
# apply(gdp_vars, MARGIN = 2, FUN = function(x){fmls(x,3,4,nealmon)})


# data("USrealgdp")
# data("USunempr")
#
# y <- diff(log(USrealgdp))
# x <- window(diff(USunempr), start = 1949)
# trend <- 1:length(y)
#
# ##24 high frequency lags of x included
# mr <- midas_r(y ~ trend + fmls(x, 23, 12, nealmon), start = list(x = rep(0, 3)))
#
# ##Forecast horizon
# h <- 3
# ##Declining unemployment
# xn <- rep(-0.1, 12*h)
# ##New trend values
# trendn <- length(y) + 1:h
#
# ##Static forecasts combining historic and new high frequency data
# forecast(mr, list(trend = trendn, x = xn), method = "static")
#
