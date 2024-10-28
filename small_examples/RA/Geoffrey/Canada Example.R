devtools::load_all()
library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

spec <- dplyr::tibble(
  type = c(
    "d",
    "n"
  ),
  dependent = c(
    "IndProdGDP",
    "HICP_Energy"
  ),
  independent = c(
    "HICP_GAS + HICP_Energy + WORLD_OIL",
    "IndProd"
  )
)

dictionary <- dict_statcan %>% bind_rows(dict_imf)


#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)


model_result <- run_model(specification = spec, dictionary = dictionary, primary_source = "download")

model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "last")

plot <- plot.osem.forecast(model_forecast,order.as.run = TRUE)


hind_cast <- forecast_insample(
  model_result,
  sample_share = 0.9,
  uncertainty_sample = 100,
  exog_fill_method = "last"
)



