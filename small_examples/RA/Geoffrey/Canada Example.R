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

dict_statCan <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
  "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,
  "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,NA,NA,NA,NA,NA,
  "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at self service filling stations", NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
  "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
  "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
  "WORLD_OIL", "World Oil Price USD", "imf", NA, "PCPS", "na_item","M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"W00","POILAPSP","USD",NA,NA,
  "FISH", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,

)
dict_statCan <- as.data.frame(dict_statCan)

dictionary <- dict_statCan #aggregate.model::statcan_dict


browser()
#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)

browser()
model_result <- run_model(specification = spec, dictionary = dict_statCan, primary_source = "download")
browser()
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "last")
browser()
plot <- plot.osem.forecast(model_forecast,order.as.run = TRUE)


hind_cast <- forecast_insample(
  model_result,
  sample_share = 0.5,
  uncertainty_sample = 100,
  exog_fill_method = "last"
)



