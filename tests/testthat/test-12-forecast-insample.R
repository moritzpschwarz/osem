# To use expect_snapshot_file() you'll typically need to start by writing
# a helper function that creates a file from your code, returning a path
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")

  if(ggplot2::is_ggplot(code)){
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
                 input = df,
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
                     37953.3681, 24200.5961, 17198.5765, 15322.7043, 22854.2847, 18834.5303,
                     18447.7795, 18216.5211, 18036.0675, 36483.1274, 36460.9715, 36606.3708,
                     36698.6771, 40864.6234, 40294.6977, 40099.8152, 39994.3761, 24200.5961,
                     17198.5765, 15322.7043, 22854.2847, 19094.9165, 17749.3509, 16505.3234,
                     35448.4381, 35440.3447, 35577.0081, 40883.3706, 39169.0473, 37732.6796,
                     17588.2429, 15369.8796, 22675.5375, 19873.3993, 19175.4593, 18695.2089,
                     35448.4381, 35440.3447, 35577.0081, 41684.1266, 40688.8358, 40142.6319,
                     17588.2429, 15369.8796, 22675.5375, 20697.2741, 21421.1899, 36533.2545,
                     37264.5981, 45604.978, 46962.4844, 16104.8977, 24249.7797, 19447.7447,
                     19039.9766, 36533.2545, 37264.5981, 44510.5183, 44801.2147, 16104.8977,
                     24249.7797, 19797.1984, 38374.0713, 45805.7061, 24294.673, 18827.5626,
                     38374.0713, 44906.5244, 24294.673))


  expect_error(plot(mod_insfc, first_date = "asd"))
  expect_error(plot(mod_insfc, first_date_insample_model = "asd"))
  expect_error(plot(mod_insfc, first_date_insample_model = 20))

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
                     input = testdata,
                     primary_source = "local",
                     present = FALSE,
                     quiet = TRUE,
                     saturation = "IIS")

  expect_error(forecast_insample(model, sample_share = 0.99, quiet = TRUE),
               regexp = "Not enough models available for insample comparison")



})
