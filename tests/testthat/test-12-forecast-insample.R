# To use expect_snapshot_file() you'll typically need to start by writing
# a helper function that creates a file from your code, returning a path
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")

  if(ggplot2::is.ggplot(code)){
    ggplot2::ggsave(filename = path, plot = code, width = 7, height = 5)
  } else {
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
  }
  path
}

# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  #skip_on_os("windows")
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name)
}




df <- read.csv(test_path("testdata", "ragged_edge", "ragged_edge_emissions_data.csv"))

spec <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "EmiCO2Combustion"
  ),
  independent = c(
    "FinConsExpHH + GCapitalForm",
    "",
    "GValueAdd",
    "FinConsExpHH + GCapitalForm + GValueAdd"
  )
)

mod <- run_model(specification = spec,
                 dictionary = dict,
                 inputdata_directory = df,
                 primary_source = "local",
                 present = FALSE,
                 quiet = TRUE, saturation = "IIS")

set.seed(1298)
mod_insfc <- forecast_insample(mod, sample_share = 0.95, quiet = TRUE,plot = FALSE,
                               exog_fill_method = c("AR","auto"))

test_that("Testing general insample forecasting", {


  # testing the output
  expect_s3_class(mod_insfc$central, "tbl_df")
  expect_s3_class(mod_insfc, "osem.forecast.insample")

  # test the print output
  expect_output(print(mod_insfc, plot = FALSE), "Forecast Within Uncertainty")
  expect_output(print(mod_insfc, plot = FALSE), "AR")
  expect_output(print(mod_insfc, plot = FALSE), "auto")
  expect_output(print(mod_insfc, plot = FALSE), "Total RMSFE")
  expect_output(print(mod_insfc, plot = FALSE), "AR Model run with auto.arima")
  expect_output(print(mod_insfc, plot = FALSE), "Outlier and Step Shift Corrected")

  expect_identical(round(mod_insfc$central$values,4),
                   c(18848.7039, 17813.3682, 17057.7254, 16122.894, 36483.1274,
                     36460.9715, 36606.3708, 36698.6771, 40878.7839, 39650.6983, 38892.9975,
                     37953.3681, 24200.5961, 17198.5765, 15322.7043, 22854.2847, 18740.6821,
                     18405.4553, 18164.7252, 17994.0518, 36483.1274, 36460.9715, 36606.3708,
                     36698.6771, 40770.7161, 40250.4249, 40045.3962, 39949.2589, 24200.5961,
                     17198.5765, 15322.7043, 22854.2847, 19094.9165, 17749.3509, 16505.3234,
                     35448.4381, 35440.3447, 35577.0081, 40883.3706, 39169.0473, 37732.6796,
                     17588.2429, 15369.8796, 22675.5375, 19904.2415, 19236.7078, 18753.6772,
                     35448.4381, 35440.3447, 35577.0081, 41715.5158, 40752.7452, 40205.8723,
                     17588.2429, 15369.8796, 22675.5375, 20697.2741, 21421.1899, 36533.2545,
                     37264.5981, 45604.978, 46962.4844, 16104.8977, 24249.7797, 19486.6619,
                     19017.8148, 36533.2545, 37264.5981, 44545.2428, 44782.5123, 16104.8977,
                     24249.7797, 19797.1984, 38374.0713, 45805.7061, 24294.673, 18794.16,
                     38374.0713, 44875.0549, 24294.673))

  skip_on_ci()
  skip_on_cran()
  expect_snapshot_plot("insample_forecast",code = plot(mod_insfc))




})




test_that("Testing insample forecasting error messages",{


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


  model <- run_model(specification = specification,
                     dictionary = dict,
                     inputdata_directory = testdata,
                     primary_source = "local",
                     present = FALSE,
                     quiet = TRUE,
                     saturation = "IIS")

  expect_error(forecast_insample(model, sample_share = 0.99, quiet = TRUE),
               regexp = "Not enough models available for insample comparison")



})
