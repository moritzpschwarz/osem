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


mod <- run_model(specification = specification,
                 dictionary = dict,
                 inputdata_directory = testdata,
                 primary_source = "local",
                 present = FALSE,
                 quiet = TRUE, saturation = "IIS")

test_that("Testing plot.osem functionality",{

  skip_on_ci()

  expect_snapshot_plot("plot.osem_1",code = plot(mod))
  expect_snapshot_plot("plot.osem_2",code = plot(mod, exclude.exogenous = FALSE))
  expect_snapshot_plot("plot.osem_3",code = plot(mod, exclude.exogenous = FALSE,grepl_variables = "HICP"))
  expect_snapshot_plot("plot.osem_4",code = plot(mod, exclude.exogenous = TRUE, order.as.run = TRUE))
  expect_snapshot_plot("plot.osem_5",code = plot(mod, exclude.exogenous = TRUE, order.as.run = TRUE, first_date = "2020-01-01"))

})


