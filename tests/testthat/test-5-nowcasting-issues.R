

test_that("Checking some earlier nowcasting issues",{

  dict %>%
    dplyr::bind_rows(dplyr::tibble(
      model_varname = "IndProd", # this is free to choose but must be unique
      full_name = "An index of Industrial Production",
      database  = "eurostat",
      variable_code = "PROD", # in this case use the bt_indicator information here
      dataset_id = "sts_inpr_q",
      var_col = "indic_bt", # here we specify what the column with the variables is called
      freq = "q", # for quarterly data, 'm' would be monthly
      geo = "AT",
      unit = "I15", # for index of 2015 = 100
      s_adj = "NSA", # not seasonally adjusted
      nace_r2 = "B-D")) -> new_dict


  specification <- dplyr::tibble(
    type = c(
      #"n",
      #"n",
      #"n",
      "n"
    ),
    dependent = c(
      #"EmiCO2Industry",
      #"EmiCO2Combustion",
      #"EmiCH4Livestock",
      "FinConsExpHH"
    ),
    independent = c(
      #"HICP_Gas + HICP_Electricity + IndProd + Export",
      #"FinConsExpHH + HICP_Electricity + EmiCO2Industry",
      #"FinConsExpHH + Export + GValueAddAgri",
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
                                   dictionary = new_dict,
                                   inputdata_directory = testdata_modified_long,
                                   primary_source = "local",
                                   present = FALSE,
                                   quiet = TRUE,
                                   selection.tpval = 0.001,
                                   constrain.to.minimum.sample = FALSE))

  expect_message(f1 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE), regexp = "No exogenous values")

})
