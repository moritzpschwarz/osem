library(tidyverse)
library(readr)
library(magrittr)

#toy specs for example
spec <- dplyr::tibble(
  type = c(
    "d"
  ),
  dependent = c(
    "EmiCO2Industry"
  ),
  independent = c(
    "HICP_GAS + HICP_Energy"

  )
)

#toy dictionary for example

test_imf <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col, ~nace_r2, ~geo, ~unit, ~s_adj,~freq,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
  "HICP_GAS","The PCPS","imf",NA,"PCPS","na_item", NA, NA, NA, NA,"M","W00","POILAPSP","USD",NA,NA,
  "HICP_Energy","The TEST","imf",NA,"PCPS","na_item", NA, NA, NA, NA,"M","W00","POILAPSP","USD",NA,NA
)

bind_rows(test_imf, dict_imf) %>%
  janitor::remove_empty("cols") %>%
  select(-var_col) -> dict_imf_ready

#dict_statCan$variable_code <- NA
dict_imf <- as.data.frame(dict_imf)



dictionary <- dict_imf #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_imf)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)


#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)


model_result <- run_model(specification = spec, dictionary = dict_imf_ready, primary_source = "download")
browser()
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot = FALSE)

plot <- plot(model_forecast,order.as.run = TRUE)

hind_cast <- forecast_insample(
  model_result,
  sample_share = 0.98,
  uncertainty_sample = 100,
  exog_fill_method = "AR",
  plot = TRUE
)



