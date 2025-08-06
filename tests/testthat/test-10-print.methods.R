

specification <- dplyr::tibble(
  type = c(
    "n",
    "n"
  ),
  dependent = c(
    "FinConsExpHH",
    "FinConsExpGov"
  ),
  independent = c(
    "FinConsExpGov + HICP_Gas",
    ""
  )
)

set.seed(123)
testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                          FinConsExpGov = rnorm(mean = 100, n = length(time)),
                          HICP_Gas = rnorm(mean = 200, n = length(time)),
                          FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time), mean = 0, sd = 0.2))
testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")


mod <- run_model(specification = specification,
                 dictionary = dict,
                 input = testdata,
                 primary_source = "local",
                 present = FALSE,
                 quiet = TRUE, saturation = "IIS")

mod_fcst <- forecast_model(model = mod, quiet = TRUE)


test_that("print methods are working", {


  # create a number of tests that evaluate whether the print.osem and print.osem.forecast works
  expect_output(print(mod, plot = FALSE), "Relationships considered")
  expect_output(print(mod, plot = FALSE), "Diagnostics")
  expect_output(str(print(mod)), "ggplot")
  expect_output(str(print(mod, plot = FALSE)))

  expect_output(str(print(mod_fcst)), "ggplot")
  expect_output(print(mod_fcst, plot = FALSE), "Central Forecast Estimates")
  expect_output(print(mod_fcst, plot = FALSE), "Forecast Method")
  expect_output(print(mod_fcst, plot = FALSE), "AR Model")
  expect_output(print(mod_fcst, plot = FALSE), "FinConsExpGov")
  expect_output(print(mod_fcst, plot = FALSE), "100")
  expect_output(print(mod_fcst, plot = FALSE), "80.3")

})


test_that("Testing model_table function",{
  skip_on_cran()
  skip_on_ci()

  # create a number of tests that evaluate whether the model_table function works
  expect_s3_class(model_table(mod), "tinytable")
  # check whether the output is a data.frame
  expect_s3_class(model_table(mod, output = "data.frame"), "data.frame")
  # expect whether this would be html code
  expect_s3_class(model_table(mod,output = "html"), "tinytable")
  # expect latex (this will, if run for the first time, create )
  #expect_warning(model_table(mod, output = "latex"))
  options("modelsummary_format_numeric_latex" = "plain")
  expect_s3_class(model_table(mod, output = "latex"), "character")
})


