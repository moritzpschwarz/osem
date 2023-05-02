options(timeout=1000)

# # You'd then also provide a helper that skips tests where you can't
# # be sure of producing exactly the same output
# expect_snapshot_plot <- function(name, code) {
#   # Other packages might affect results
#   skip_if_not_installed("ggplot2", "2.0.0")
#   # Or maybe the output is different on some operation systems
#   skip_on_ci()
#   # You'll need to carefully think about and experiment with these skips
#
#   name <- paste0(name, ".png")
#
#   # Announce the file before touching `code`. This way, if `code`
#   # unexpectedly fails or skips, testthat will not auto-delete the
#   # corresponding snapshot file.
#   announce_snapshot_file(name = name)
#
#   # To use expect_snapshot_file() you'll typically need to start by writing
#   # a helper function that creates a file from your code, returning a path
#   save_png <- function(code, width = 400, height = 400) {
#     path <- tempfile(fileext = ".png")
#     png(path, width = width, height = height)
#     on.exit(dev.off())
#     code
#
#     path
#   }
#
#   path <- save_png(code)
#   expect_snapshot_file(path, name)
# }


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


# test_that("Test that forecasting works",{
#
#   ## Test AR1 and fully exogenous ----
#
#   spec <- dplyr::tibble(
#     type = c(
#       "d",
#       "d",
#       "n",
#       "n",
#       "n",
#       "n",
#       "n"
#     ),
#     dependent = c(
#       "StatDiscrep",
#       "TOTS",
#       "Import",
#       "FinConsExpHH",
#       "GCapitalForm",
#       "Emissions",
#       "GDP"
#     ),
#     independent = c(
#       "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#       "GValueAdd + Import",
#       "FinConsExpHH + GCapitalForm",
#       "FinConsExpGov",
#       "Emissions",
#       "GDP",
#       ""
#     )
#   )
#
#   fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR", varcolumn = "na_item")
#   fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR", varcolumn = "na_item")
#   fc <- list(geo = "AT", unit = "THS_T", nace_r2 = "TOTAL_HH", varcolumn = "airpol")
#   filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa, "GHG" = fc, "B1GQ" = fa)
#
#   expect_warning(
#     b <- run_model(
#       specification = spec,
#       dictionary = NULL,
#       inputdata_directory = NULL,
#       filter_list = filter_list,
#       download = TRUE,
#       save_to_disk = NULL,
#       present = FALSE,
#       quiet = TRUE
#     ))
#
#   set.seed(123)
#   expect_message(forecast_model(b), regexp = "No exogenous values")
#
#   skip_on_ci()
#   expect_snapshot_plot("Forecast_plot",plot(forecast_model(b)))
#
#
# })


test_that("Test that forecasting works - with fixed data",{

  ## Test AR1 and fully exogenous ----

  spec <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm",
      "Emissions",
      "GDP"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "FinConsExpGov",
      "Emissions",
      "GDP",
      ""
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR", varcolumn = "na_item")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR", varcolumn = "na_item")
  fc <- list(geo = "AT", unit = "THS_T", nace_r2 = "TOTAL_HH", varcolumn = "airpol")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa, "GHG" = fc, "B1GQ" = fa)

  expect_silent(
    b <- run_model(
      specification = spec,
      dictionary = NULL,
      inputdata_directory = sample_input,
      primary_source = "local",
      quiet = TRUE
    ))

  set.seed(123)
  expect_message(bb <- forecast_model(b, plot.forecast = FALSE), regexp = "No exogenous values")

  skip_on_ci()
  expect_snapshot_plot("Forecast_plot",code = plot(bb))
})
