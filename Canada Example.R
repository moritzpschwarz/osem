library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

#toy specs for example
spec <- dplyr::tibble(
  type = c(
    "d"
  ),
  dependent = c(
    "TOTS"
  ),
  independent = c(
    "GDP - Debt"
  )
)

#toy dictionary for example

dict_statCan <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~dataset_id, ~freq, ~var_col, ~found, ~`North American Industry Classification System (NAICS)`, ~unit,  ~geo, ~`Seasonal adjustment`, ~Prices, ~nace_r2,~s_adj,~`Central government debt`,
  "GDP", "Gross domestic product", "statcan", "36-10-0434-02", "q", "na_item", TRUE, "All industries [T001]", "millions", "Canada", "Seasonally adjusted at annual rates", "2017 constant prices",NA,NA,NA,
  "Debt", "Central Government Debt", "statcan", "10-10-0002-01","q", "na_item", TRUE, NA, "millions", "Canada", NA, NA,NA,NA,"A. Federal debt (accumulated deficit), (B - E)"
)
dict_statCan$variable_code <- NA
dict_statCan <- as.data.frame(dict_statCan)



dictionary <- dict_statCan #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCan)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

browser()
#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)


model_result <- run_model(specification = spec, dictionary = dict_statCan, primary_source = "download")
browser()
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot.forecast = FALSE)

plot <- plot.aggmod.forecast(model_forecast,order.as.run = TRUE)




