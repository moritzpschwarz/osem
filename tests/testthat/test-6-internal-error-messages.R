

test_that("Error messages for saturation, selection",{

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


  run_model(specification = specification,
            dictionary = dict,
            inputdata_directory = testdata %>%
              tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values"),
            primary_source = "local",
            present = FALSE,
            quiet = TRUE, saturation = "IIS")


  run_model(specification = specification,
            dictionary = dict,
            inputdata_directory = testdata %>%
              tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values"),
            primary_source = "local",
            present = FALSE,
            quiet = TRUE, saturation = c("SIS","TIS"))

})
