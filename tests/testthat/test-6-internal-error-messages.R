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
                          FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time), mean = 0, sd = 0.2))
testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")




test_that("Error messages for saturation, selection",{

  expect_error(run_model(specification = specification, gets_selection = NULL), "must be logical")
  expect_error(run_model(specification = specification, gets_selection = "NULL"), "must be logical")
  expect_error(run_model(specification = specification, gets_selection = 1), "must be logical")

  expect_error(run_model(specification = specification, saturation = FALSE), "character vector that can take the values 'IIS', 'SIS', or 'TIS'")
  expect_error(run_model(specification = specification, saturation = "MIS"), "character vector that can take the values 'IIS', 'SIS', or 'TIS'")
  expect_error(run_model(specification = specification, saturation = 0.5), "character vector that can take the values 'IIS', 'SIS', or 'TIS'")

  expect_error(run_model(specification = specification, saturation.tpval = "A"), "saturation.tpval' must be either NULL or numeric between 0 and 1")
  expect_error(run_model(specification = specification, saturation.tpval = 2), "saturation.tpval' must be either NULL or numeric between 0 and 1")
  expect_error(run_model(specification = specification, saturation.tpval = -2), "saturation.tpval' must be either NULL or numeric between 0 and 1")

  expect_error(run_model(specification = specification, selection.tpval = "A"), "selection.tpval' must be either NULL or numeric between 0 and 1")
  expect_error(run_model(specification = specification, selection.tpval = 2), "selection.tpval' must be either NULL or numeric between 0 and 1")
  expect_error(run_model(specification = specification, selection.tpval = -2), "selection.tpval' must be either NULL or numeric between 0 and 1")


  expect_silent(mod <- run_model(specification = specification,
                                 dictionary = dict,
                                 input = testdata,
                                 primary_source = "local",
                                 present = FALSE,
                                 quiet = TRUE, saturation = "IIS"))
  expect_equal(nrow(mod$module_order), 1)
  expect_equal(mod$module_collection$model[[1]]$ISnames, "iis2007-10-01")


  expect_silent(mod <- run_model(specification = specification,
                                 dictionary = dict,
                                 input = testdata,
                                 primary_source = "local",
                                 present = FALSE,
                                 quiet = TRUE, saturation = c("SIS","TIS")))

  expect_equal(nrow(mod$module_order), 1)
  expect_equal(mod$module_collection$model[[1]]$ISnames, c("sis2007-10-01", "sis2008-01-01"))

})


test_that("Check that arx estimation works (i.e. saturation = NULL)",{

  # check that saturation = NULL works
  expect_silent(mod <- run_model(specification = specification,
                                 dictionary = dict,
                                 input = testdata,
                                 primary_source = "local",
                                 present = FALSE,
                                 quiet = TRUE, saturation = NULL))

  # check that all elements of mod$module_collection$model[[1]]$aux$mX have column names
  expect_true(!is.null(colnames(mod$module_collection$model[[1]]$aux$mX)))

  # check that ar argument is contained in mod$module_collection$model[[1]]$aux$args
  expect_true("ar" %in% names(mod$module_collection$model[[1]]$aux$args))

  # check that saturation = NULL and gets_selection = NULL works
  expect_silent(mod <- run_model(specification = specification,
                                 dictionary = dict,
                                 input = testdata,
                                 primary_source = "local",
                                 present = FALSE,
                                 quiet = TRUE,

                                 saturation = NULL,
                                 gets_selection = FALSE))

})



test_that("Error messages for log specifications", {

  expect_error(run_model(specification = specification, use_logs = "abc"), "The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'")
  expect_error(run_model(specification = specification, use_logs = 0), "The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'")
  expect_error(run_model(specification = specification, use_logs = NULL), "The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'")
  expect_error(run_model(specification = specification, use_logs = c("both", "none")), "The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'")

  mod <- run_model(specification = specification,
                   dictionary = dict,
                   input = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE,
                   use_logs = "both",
                   gets_selection = FALSE,
                   max.ar = 0,
                   max.dl = 0)

  expect_true(all(c("ln.FinConsExpGov", "ln.HICP_Gas") %in% colnames(mod$module_collection$model[[1]]$aux$mX)))
  expect_true(all(log(testdata$values[testdata$na_item == "FinConsExpGov"]) == mod$module_collection$model[[1]]$aux$mX[,"ln.FinConsExpGov"]))
  expect_true(all(log(testdata$values[testdata$na_item == "HICP_Gas"]) == mod$module_collection$model[[1]]$aux$mX[,"ln.HICP_Gas"]))
  expect_true(all(log(testdata$values[testdata$na_item == "FinConsExpHH"]) == mod$module_collection$model[[1]]$aux$y))

  mod <- run_model(specification = specification,
                   dictionary = dict,
                   input = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE,
                   use_logs = "x",
                   gets_selection = FALSE,
                   max.ar = 0,
                   max.dl = 0)


  expect_true(all(c("ln.FinConsExpGov", "ln.HICP_Gas") %in% colnames(mod$module_collection$model[[1]]$aux$mX)))
  expect_true(all(log(testdata$values[testdata$na_item == "FinConsExpGov"]) == mod$module_collection$model[[1]]$aux$mX[,"ln.FinConsExpGov"]))
  expect_true(all(log(testdata$values[testdata$na_item == "HICP_Gas"]) == mod$module_collection$model[[1]]$aux$mX[,"ln.HICP_Gas"]))
  expect_true(all(testdata$values[testdata$na_item == "FinConsExpHH"] == mod$module_collection$model[[1]]$aux$y))

  mod <- run_model(specification = specification,
                   dictionary = dict,
                   input = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE,
                   use_logs = "y",
                   gets_selection = FALSE,
                   max.ar = 0,
                   max.dl = 0)


  expect_true(all(c("FinConsExpGov", "HICP_Gas") %in% colnames(mod$module_collection$model[[1]]$aux$mX)))
  expect_true(all(testdata$values[testdata$na_item == "FinConsExpGov"] == mod$module_collection$model[[1]]$aux$mX[,"FinConsExpGov"]))
  expect_true(all(testdata$values[testdata$na_item == "HICP_Gas"] == mod$module_collection$model[[1]]$aux$mX[,"HICP_Gas"]))
  expect_true(all(log(testdata$values[testdata$na_item == "FinConsExpHH"]) == mod$module_collection$model[[1]]$aux$y))


  mod <- run_model(specification = specification,
                   dictionary = dict,
                   input = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE,
                   use_logs = "none",
                   gets_selection = FALSE,
                   max.ar = 0,
                   max.dl = 0)

  expect_true(all(c("FinConsExpGov", "HICP_Gas") %in% colnames(mod$module_collection$model[[1]]$aux$mX)))
  expect_true(all(testdata$values[testdata$na_item == "FinConsExpGov"] == mod$module_collection$model[[1]]$aux$mX[,"FinConsExpGov"]))
  expect_true(all(testdata$values[testdata$na_item == "HICP_Gas"] == mod$module_collection$model[[1]]$aux$mX[,"HICP_Gas"]))
  expect_true(all(testdata$values[testdata$na_item == "FinConsExpHH"] == mod$module_collection$model[[1]]$aux$y))


  # test that the log transformation is correctly applied to the dependent variable
  # even when saturation = NULL (i.e. arx is used)

  mod <- run_model(specification = specification,
                   dictionary = dict,
                   input = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE,
                   gets_selection = FALSE,
                   saturation = NULL,
                   max.ar = 0,
                   max.dl = 0)
  # forecast_model(mod, plot = TRUE)

  mod$full_data %>%
    dplyr::filter(grepl("FinConsExpHH", na_item), time == as.Date("2005-01-01")) %>%
    dplyr::pull(values) %>%
    round(3) -> dat_not_logged

  #expect_identical(dat_not_logged, c(80.37, 4.39)) # this used to be case
  expect_identical(dat_not_logged, c(80.369, 80.366)) # now estimated and predicted are nearly equal

})


test_that("Error messages for ecm/ardl", {

  expect_error(run_model(specification = specification, ardl_or_ecm = "abc"), "argument 'ardl_or_ecm' must be a character vector and can only be one of 'ardl' or 'ecm'")
  expect_error(run_model(specification = specification, ardl_or_ecm = 0), "argument 'ardl_or_ecm' must be a character vector and can only be one of 'ardl' or 'ecm'")
  expect_error(run_model(specification = specification, ardl_or_ecm = NULL), "argument 'ardl_or_ecm' must be a character vector and can only be one of 'ardl' or 'ecm'")
  expect_error(run_model(specification = specification, ardl_or_ecm = c("ardl","ecm")), "argument 'ardl_or_ecm' must be a character vector and can only be one of 'ardl' or 'ecm'")

})


test_that("Check that all variables are in the dictionary",{


  specification <- dplyr::tibble(
    type = c(
      "n",
      "n"
    ),
    dependent = c(
      "FinConsExpHH",
      "y"
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas",
      "x1 + x2"
    )
  )

  set.seed(123)
  dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                FinConsExpGov = rnorm(mean = 100, n = length(time)),
                HICP_Gas = rnorm(mean = 200, n = length(time)),
                FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.2),
                x1 = rnorm(mean = 10, n = length(time)),
                x2 = rnorm(mean = 20, n = length(time)),
                y = 0.5 - 0.2*x1 + 0.6 * x2 + rnorm(length(time),mean = 0, sd = 0.3)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") -> testdata_modified_long



  expect_error(model <- run_model(specification = specification,
                                  dictionary = dict,
                                  input = testdata_modified_long,
                                  primary_source = "local",
                                  present = FALSE,
                                  quiet = TRUE,
                                  selection.tpval = 0.001,
                                  constrain.to.minimum.sample = FALSE), regexp = "Not all model variables found in the dictionary")

})




test_that("Check that a missing dictionary entry is not triggering an error for an identity",{


  specification <- dplyr::tibble(
    type = c(
      "n",
      "d"
    ),
    dependent = c(
      "FinConsExpHH",
      "NewIdent" # this identity is not defined in the dictionary
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas",
      "FinConsExpHH + HICP_Gas"
    )
  )

  set.seed(123)
  testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                            FinConsExpGov = rnorm(mean = 100, n = length(time)),
                            #HICP_Gas = rnorm(mean = 200, n = length(time)),
                            # simulate an AR1 process with rho = 0.3 and call it HICP_Gas
                            HICP_Gas = as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 30, mean = 200)),
                            L1.HICP_Gas = lag(HICP_Gas),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas -0.2 * L1.HICP_Gas +
                              as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 0.2, mean = 0)))

  testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

  expect_silent(mod <- run_model(specification = specification,
                                 dictionary = dict,
                                 input = testdata,
                                 primary_source = "local",
                                 present = FALSE,
                                 quiet = TRUE, saturation = "IIS"))

  expect_true("NewIdent" %in% unique(mod$full_data$na_item))


  # check that dictionary stops with an error message for incorrect database

  dict %>%
    dplyr::mutate(database = "incorrect") -> dict_incorrect

  expect_error(run_model(specification = specification, dictionary = dict_incorrect, input = testdata, quiet = TRUE),
               regexp = "Currently, only allow data bases 'eurostat', 'edgar', 'statcan', 'imf', or 'local' files")



})


test_that("Check the keep argument",{


  specification <- dplyr::tibble(
    type = c(
      "n",
      "d"
    ),
    dependent = c(
      "FinConsExpHH",
      "NewIdent" # this identity is not defined in the dictionary
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas + test",
      "FinConsExpHH + HICP_Gas"
    )
  )

  set.seed(123)
  testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                            FinConsExpGov = rnorm(mean = 100, n = length(time)),
                            #HICP_Gas = rnorm(mean = 200, n = length(time)),
                            # simulate an AR1 process with rho = 0.3 and call it HICP_Gas
                            HICP_Gas = as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 30, mean = 200)),
                            L1.HICP_Gas = lag(HICP_Gas),
                            test = rnorm(length(time)),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas -0.2 * L1.HICP_Gas +
                              as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 0.2, mean = 0)))

  testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

  expect_error(mod <- run_model(specification = specification,
                                dictionary = dict,
                                input = testdata,
                                primary_source = "local",
                                present = FALSE,
                                keep = 2,
                                quiet = TRUE, saturation = "IIS"), regexp = "must be a character vector")

  expect_error(mod <- run_model(specification = specification,
                                dictionary = dict,
                                input = testdata,
                                primary_source = "local",
                                present = FALSE,
                                keep = c("sdklf0", "asdfkj"),
                                quiet = TRUE, saturation = "IIS"), regexp = "not a vector")


  # check that test is not in the final model
  nokeep <- run_model(specification = specification,
                      dictionary = dict %>% dplyr::add_row(model_varname = "test", database = "local"),
                      input = testdata,
                      primary_source = "local",
                      present = FALSE,
                      plot = FALSE,
                      gets_selection = TRUE,
                      quiet = TRUE, saturation = "IIS")

  wkeep <- run_model(specification = specification,
                     dictionary = dict %>% dplyr::add_row(model_varname = "test", database = "local"),
                     input = testdata,
                     primary_source = "local",
                     present = FALSE,
                     plot = FALSE,
                     gets_selection = TRUE,
                     keep = "test",
                     quiet = TRUE, saturation = "IIS")


  expect_true(!any(grepl("test",row.names(nokeep$module_collection$model[[1]]$mean.results))))
  expect_true(any(grepl("test",row.names(wkeep$module_collection$model[[1]]$mean.results))))

})


