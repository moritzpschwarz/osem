devtools::load_all()
library(tidyverse)
library(readr)
library(magrittr)
library(forecast)

# test of auto.airma forecasting

specification <- dplyr::tibble(
  type = c(
    "n"
  ),
  dependent = c(
    "FinConsExpHH"
  ),
  independent = c(
    "FinConsExpGov + HICP_Gas"
  )
)

set.seed(123)
testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                          FinConsExpGov = rnorm(mean = 100, n = length(time)),
                          #HICP_Gas = rnorm(mean = 200, n = length(time)),
                          # simulate an AR1 process with rho = 0.3 and call it HICP_Gas
                          HICP_Gas = as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 30, mean = 200)),
                          L1.HICP_Gas = lag(HICP_Gas),
                          FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas -0.2 * L1.HICP_Gas +
                            as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 0.2, mean = 0)))
#rnorm(length(time), mean = 0, sd = 0.2))
testdata_2 <- testdata %>%
  dplyr::mutate(L2.HICP_Gas = lag(L1.HICP_Gas))

testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

lag <- 12     # number of lags
H <- 12      # number of forecast horizons
W <- 1       #  Local window smoothing - usually set equal to frequency of series
trend = TRUE  # linear trend?

model <- run_model(specification = specification,
                 dictionary = dict,
                 inputdata_directory = testdata,
                 primary_source = "local")
forecast <- forecast_model(
  model,
  exog_predictions = NULL,
  n.ahead = 10,
  ci.levels = c(0.5, 0.66, 0.95),
  exog_fill_method = "clements_hendry",
  ar.fill.max = 4,
  plot = TRUE,
  uncertainty_sample = 100,
  quiet = FALSE,
  window = 4,
  lag = 4,
  trend = trend
)






