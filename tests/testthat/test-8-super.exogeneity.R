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
                          #HICP_Gas = rnorm(mean = 200, n = length(time)),
                          # simulate an AR1 process with rho = 0.3 and call it HICP_Gas
                          HICP_Gas = as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 30, mean = 200)),
                          L1.HICP_Gas = lag(HICP_Gas),
                          FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas -0.2 * L1.HICP_Gas +
                            as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 0.2, mean = 0)))
                            #rnorm(length(time), mean = 0, sd = 0.2))
testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

# plot testdata using ggplot
# ggplot2::ggplot(data = testdata, ggplot2::aes(x = time, y = values, color = na_item)) +
#   ggplot2::geom_line() +
#   ggplot2::theme_minimal() +
#   ggplot2::facet_wrap(~na_item, scales = "free")



test_that("Super Exogeneity Tests", {


  mod <- run_model(specification = specification,
                   dictionary = dict,
                   inputdata_directory = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE, saturation = "IIS")

  expect_true(!is.null(mod$module_collection$diagnostics[[1]]$super.exogeneity))
  expect_s3_class(mod$module_collection$diagnostics[[1]]$super.exogeneity, "htest")
  expect_equal(mod$module_collection$diagnostics[[1]]$super.exogeneity$statistic, c("F-Stat" = 6.996028))
  expect_equal(round(as.numeric(mod$module_collection$diagnostics[[1]]$super.exogeneity$p.value),9), 0.024863487)


  # run it with a tighter significance to ensure at least one variable is not testable
  mod <- run_model(specification = specification,
                   dictionary = dict,
                   inputdata_directory = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE, saturation = "IIS",
                   saturation.tpval = 0.001)

  expect_true(!is.null(mod$module_collection$diagnostics[[1]]$super.exogeneity))
  expect_s3_class(mod$module_collection$diagnostics[[1]]$super.exogeneity, "htest")


  # run it with a very tight significance to ensure no test is possible
  mod <- run_model(specification = specification,
                   dictionary = dict,
                   inputdata_directory = testdata,
                   primary_source = "local",
                   present = FALSE,
                   quiet = TRUE, saturation = "IIS",
                   saturation.tpval = 0.000001)

  expect_true(!is.null(mod$module_collection$diagnostics[[1]]$super.exogeneity))
  expect_equal(mod$module_collection$diagnostics[[1]]$super.exogeneity, NA)
  expect_equal(diagnostics_model(mod)$`Super Exogeneity`, NA)

})
