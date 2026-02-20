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

# now modify this to simluate TIS
testdata %>%
  # add a step and a trend to FinConsExpHH from 2008-01-01 and then revert that trend to negative from 2015-07-01
  dplyr::mutate(FinConsExpHH = dplyr::case_when(time >= as.Date("2008-01-01") & time < as.Date("2015-07-01") ~ FinConsExpHH + .3 * as.numeric(difftime(time, as.Date("2008-01-01"), units = "weeks"))/52,
                                                time >= as.Date("2015-07-01") ~ FinConsExpHH -.2 * as.numeric(difftime(time, as.Date("2015-07-01"), units = "weeks"))/52,
                                                TRUE ~ FinConsExpHH)) %>%
  tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") -> testdata_modified_long

#testdata_modified_long %>% ggplot(aes(x = time, y =values, color = na_item)) + geom_line()

test_that("TIS and Super Saturation works", {
  expect_silent(tis_model <- run_model(specification = specification,
                                       dictionary = dict,
                                       input = testdata_modified_long,
                                       primary_source = "local",
                                       present = FALSE,
                                       quiet = TRUE,
                                       plot = FALSE,
                                       selection.tpval = 0.001,
                                       saturation = "TIS",
                                       constrain.to.minimum.sample = FALSE))

  fcst_tis <- forecast_model(tis_model, quiet = TRUE, plot = FALSE)

  expect_equal(round(fcst_tis$forecast$central.estimate[[1]]$ln.FinConsExpHH,5),
               c(4.36492, 4.36777, 4.36645, 4.36395, 4.36488, 4.36402, 4.36322,
                 4.36265, 4.36176, 4.36155))

  expect_silent(super_sat_model <- run_model(specification = specification,
                                             dictionary = dict,
                                             input = testdata_modified_long,
                                             primary_source = "local",
                                             present = FALSE,
                                             quiet = TRUE,
                                             plot = FALSE,
                                             selection.tpval = 0.001,
                                             saturation = c("TIS","SIS","IIS"),
                                             constrain.to.minimum.sample = FALSE))


  fcst_supersat <- forecast_model(super_sat_model, quiet = TRUE, plot = FALSE)

  expect_equal(round(fcst_supersat$forecast$central.estimate[[1]]$ln.FinConsExpHH,5),
               c(4.36491, 4.36776, 4.36644, 4.36394, 4.36487, 4.36401, 4.36321,
                 4.36264, 4.36175, 4.36154))



  specification <- dplyr::tibble(
    type = c(
      "n","n"
    ),
    dependent = c(
      "FinConsExpHH",
      "FinConsExpGov"
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas",""
    )
  )

  test <- run_model(specification = specification,
                         dictionary = dict,
                         input = testdata_modified_long,
                         primary_source = "local",
                         present = FALSE,
                         quiet = TRUE,
                         plot = FALSE,
                         selection.tpval = 0.001,
                         saturation = "TIS",
                         constrain.to.minimum.sample = FALSE)

  forecast_model(test, quiet = TRUE, plot = FALSE)

})
