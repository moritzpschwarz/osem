test_that('statCan_load_or_download works', {

  skip_on_cran()
  skip_on_ci()

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

  dictionary <- dict_statCan


  actual_cols = colnames(dictionary)
  # basic functionality
  module_order <- check_config_table(spec)
  to_obtain <- determine_variables(specification = module_order,
                                   dictionary = dictionary)
  data <- download_statcan(to_obtain = to_obtain,
                           #column_filters = additional_filters,
                           column_filters = actual_cols,
                           quiet = FALSE)
  imf_data <- download_imf(to_obtain = to_obtain,
                           #column_filters = additional_filters,
                           column_filters = actual_cols,
                           quiet = FALSE)

  expect_length(data, 2)
  expect_type(data, "list")
  expect_named(data, c("df", "to_obtain"))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_GAS") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProd") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(imf_data$df %>% dplyr::filter(na_item == "WORLD_OIL") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProdGDP") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))



})

test_that('statcan_load_and_download_forecasting_functionality',{

  skip_on_cran()
  skip_on_ci()

  #setup
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


  dictionary <- dict_statCan

  df <- read.csv(test_path("testdata", "canada_imf_data", "canada_imf_stable_data.csv"))

  #run the model
  model_run <- run_model(specification = spec,
                         dictionary = dictionary,
                         inputdata_directory = df,
                         primary_source = "local",
                         quiet = TRUE)


  #forcast the model
  model_forecast <- forecast_model(model_run, plot = FALSE)

  #plot model
  expect_s3_class(plot(model_forecast,order.as.run = TRUE),class = c("gg","ggplot"))

  bb_df <- plot(model_forecast, return.data = TRUE)
  expect_s3_class(bb_df, class = "tbl_df")
  expect_true(all(names(bb_df) == c("time", "na_item", "values", "type", "p95", "p05", "p975",
                                    "p025", "p75", "p25")))
  expect_true(all(unique(bb_df$type) == c("Forecast", "Insample Fit", "Observation")))

  expect_s3_class(plot(model_forecast, return.data = TRUE), class = "tbl_df")


  #hindcast model
  hind_cast <- forecast_insample(
    model_run,
    sample_share = 0.99,
    uncertainty_sample = 100,
    exog_fill_method = "last"
  )
  expect_s3_class(hind_cast$plot,class = c("gg","ggplot"))


})
#
# ##TODO
# ##STRESS TESTS
