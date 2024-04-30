

test_that("Checking nowcasting issue when one independent variable is not available for a period",{

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
                            HICP_Gas = rnorm(mean = 200, n = length(time)),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.2))

  # now modify this to simluate the effect of all exogenous variables just not existing for the period that we
  # need nowcasting for
  # so first we set FinConsExpHH (our dependent variable) to NA to necessitate nowcasting
  # this is because of course if we have a value for our dependent variable, we don't need to nowcast it
  # then we remove an exogenous independent variable on the last date
  # this used to produce an error - should be fixed now
  testdata %>%
    dplyr::mutate(FinConsExpHH = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                                  TRUE ~ FinConsExpHH)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") %>%
    dplyr::filter(!(time == as.Date("2023-10-01") & na_item == "FinConsExpGov")) -> testdata_modified_long


  expect_silent(model <- run_model(specification = specification,
                                   dictionary = dict,
                                   inputdata_directory = testdata_modified_long,
                                   primary_source = "local",
                                   present = FALSE,
                                   quiet = TRUE,
                                   selection.tpval = 0.001,
                                   constrain.to.minimum.sample = FALSE))

  expect_message(f1 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE), regexp = "No exogenous values")

  expect_equal(model$module_collection$model[[1]]$aux$mXnames[!model$module_collection$model[[1]]$aux$mXnames %in% c("mconst","time")],
               names(f1$forecast$data[[1]])[!names(f1$forecast$data[[1]]) %in% c("mconst","time")])
})









test_that("Checking nowcasting issue when no independent variable is available for a period",{

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
                            HICP_Gas = rnorm(mean = 200, n = length(time)),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.2))

  # now modify this to simluate the effect of an exogenous variable just not existing for the period that we
  # need nowcasting for
  # so first we set FinConsExpHH (our dependent variable) to NA to necessitate nowcasting
  # this is because of course if we have a value for our dependent variable, we don't need to nowcast it
  # then we remove all exogenous independent variables on the last date --> DIFFERENCE TO ABOVE
  # this used to produce an error - should be fixed now
  testdata %>%
    dplyr::mutate(FinConsExpHH = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                                  TRUE ~ FinConsExpHH)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") %>%

    #difference to above here: we remove all ind variables
    dplyr::filter(!(time == as.Date("2023-10-01") & (na_item == "FinConsExpGov" | na_item == "HICP_Gas"))) -> testdata_modified_long


  expect_silent(model <- run_model(specification = specification,
                                   dictionary = dict,
                                   inputdata_directory = testdata_modified_long,
                                   primary_source = "local",
                                   present = FALSE,
                                   quiet = TRUE,
                                   selection.tpval = 0.001,
                                   constrain.to.minimum.sample = FALSE))

  expect_message(f1 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE), regexp = "No exogenous values")

  expect_equal(model$module_collection$model[[1]]$aux$mXnames[!model$module_collection$model[[1]]$aux$mXnames %in% c("mconst","time")],
               names(f1$forecast$data[[1]])[!names(f1$forecast$data[[1]]) %in% c("mconst","time")])

})








test_that("Testing that scenario changes to the exogenous data work",{

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
                            HICP_Gas = rnorm(mean = 200, n = length(time)),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.2))

  # now modify this to simluate the effect of providing external exogenous data but
  # where we do not provide any values for the values that would otherwise be nowcasted
  # so first we get the exogenous values based of a base case
  # then we set one value in the historical data to NA
  testdata %>%
    dplyr::mutate(FinConsExpHH = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                              TRUE ~ FinConsExpHH)) %>%
    dplyr::mutate(HICP_Gas = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                              TRUE ~ HICP_Gas)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") -> testdata_modified_long

  model <- run_model(specification = specification,
                     dictionary = dict,
                     inputdata_directory = testdata_modified_long,
                     primary_source = "local",
                     present = FALSE,
                     quiet = TRUE,
                     selection.tpval = 0.001,
                     constrain.to.minimum.sample = FALSE)


  f1 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE)
  #f1$exog_data
  #f1$exog_data_nowcast

  expect_error(f2.1 <- forecast_model(model, exog_predictions = f1$exog_data, plot.forecast = FALSE),
               regexp = "Forecasting has failed, likely due to a nowcasting issue")

  expect_equal(model$module_collection$model[[1]]$aux$mXnames[!model$module_collection$model[[1]]$aux$mXnames %in% c("mconst","time")],
               names(f1$forecast$data[[1]])[!names(f1$forecast$data[[1]]) %in% c("mconst","time")])

})
