devtools::load_all()

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


#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=test_imf)

model_result <- run_model(specification = spec, dictionary = test_imf, primary_source = "download")

model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot = FALSE)

plot <- plot(model_forecast,order.as.run = TRUE)

hind_cast <- forecast_insample(
  model_result,
  #sample_share = 0.95,
  sample_share = 0.98,
  uncertainty_sample = 100,
  exog_fill_method = "AR",
  plot = TRUE
)



