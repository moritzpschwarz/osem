

test_that("Checking some earlier nowcasting issues",{

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
  # so first we set FinConsExpHH to NA to necessitate nowcasting
  # then we remove an exogenous independet variable on the last date
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

})
