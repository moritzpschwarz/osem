test_that('statCan_load_or_download works', {

  skip_on_cran()
  skip_on_ci()

  #setup
  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "EmiCO2Industry",
      "IndProd"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + IndProd",
      "GAS"

    )
  )

  module_order <- aggregate.model:::check_config_table(spec)
  dictionary <- aggregate.model::statcan_dict
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)

  actual_cols = colnames(dictionary)
  # basic functionality
  data <- aggregate.model:::download_statcan(to_obtain = to_obtain,
                                          #column_filters = additional_filters,
                                          column_filters = actual_cols,
                                          quiet = FALSE)

  expect_length(data, 2)
  expect_type(data, "list")
  expect_named(data, c("df", "to_obtain"))
  expect_true(all(data$to_obtain[which(to_obtain$database == "statcan"), "found"]))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_GAS") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProd") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))


})

test_that('statcan_load_and_download_forecasting_functionality',{
  #library("tinytest")
  #using("tinysnapshot")
  skip_on_cran()
  skip_on_ci()

  #setup
  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "EmiCO2Industry",
      "IndProd"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + IndProd",
      "GAS"

    )
  )
  # basic functionality: running a model
  dictionary <- aggregate.model::statcan_dict


  #run the model
  model_run <- run_model(
      specification = spec,
      dictionary = dictionary,
      max.ar = 4,
      max.dl = 4,
      primary_source = "download",
      quiet = TRUE
    )

  set.seed(123)
  #forcast the model
  expect_message(model_forecast <- forecast_model(model_run, plot.forecast = FALSE), regexp = "No exogenous values")
  skip_on_ci()

  #plot model
  expect_is(plot.aggmod.forecast(model_forecast,order.as.run = TRUE),class = c("gg","ggplot"))

  #hindcast model
  hind_cast <- forecast_insample(
    model_run,
    sample_share = 0.5,
    uncertainty_sample = 100,
    exog_fill_method = "AR",
    plot.forecast = TRUE
  )

  expect_is(hind_cast$plot,class = c("gg","ggplot"))


})

##TODO
##STRESS TESTS
