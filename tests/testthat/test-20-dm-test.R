test_that("dm_test: basic structure, options, and input validation", {

  skip_on_cran()

  # --- small synthetic dataset (fast) ---
  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("FinConsExpHH", "FinConsExpGov"),
    independent = c("FinConsExpGov + HICP_Gas", "")
  )

  set.seed(123)
  testdata <- dplyr::tibble(
    time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2012-10-01"), by = "quarter"),
    FinConsExpGov = rnorm(length(time), mean = 5, sd = 1) * 0.01 * 1:length(time), # increasing trend
    HICP_Gas      = arima.sim(length(time), mean = 200, sd = 1, model = list(ar = c(0.5))),
    FinConsExpHH  = 0.5 + 0.2 * FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time), mean = 0, sd = 0.2)
  )

  # add a few missing values to exercise drop_na paths
  testdata$HICP_Gas[length(testdata$HICP_Gas)] <- NA_real_
  testdata$FinConsExpGov[c(1,2)] <- NA_real_

  testdata_long <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

  mod <- run_model(
    specification = specification,
    dictionary = dict,
    input = testdata_long,
    primary_source = "local",
    present = FALSE,
    quiet = TRUE,
    saturation = "IIS"
  )

  # --- 1) smoke test: runs and returns expected structure (no VAR/BVAR in tests) ---
  res <- dm_test(
    model = mod,
    insample_methods = c("ets", "auto"),
    insample_main_comparison = "ets",
    insample_sample_share = 0.8,
    comparison_methods = c("RW", "ar"),   # fast comparisons; avoid VAR/BVAR in unit tests
    dm.horizons = 4,
    lags = 2,
    quiet = TRUE
  )

  expect_type(res, "list")
  expect_true(all(c("dm_forecast", "dat", "insample_model") %in% names(res)))

  expect_s3_class(res$dm_forecast, "tbl_df")
  expect_true(all(c("variable", "Horizon", "model_1", "model_2", "statistic", "p_value", "n") %in% names(res$dm_forecast)))
  expect_true(all(res$dm_forecast$Horizon >= 1 & res$dm_forecast$Horizon <= 4))
  expect_true(all(res$dm_forecast$model_1 == "OSEM ets"))
  expect_true(all(res$dm_forecast$model_2 %in% c("RW", "ar")))

  expect_s3_class(res$dat, "tbl_df")
  expect_true(all(c("Origin_Date", "Horizon", "na_item", "forecast_type", "time", "value", "hist_value", "diff") %in% names(res$dat)))

  # --- 2) grepl_variables filters the DM results (should reduce variables) ---
  res_filtered <- dm_test(
    model = mod,
    insample_methods = c("ets", "auto"),
    insample_main_comparison = "ets",
    insample_sample_share = 0.8,
    comparison_methods = c("RW"),
    dm.horizons = 3,
    grepl_variables = "FinConsExpHH",
    quiet = TRUE
  )

  expect_true(all(res_filtered$dm_forecast$variable == "FinConsExpHH" | res_filtered$dm_forecast$var == "FinConsExpHH"))

  # --- 3) using provided insample_model works and is consistent ---
  # Build insample forecasts once and pass them in
  ins_mod <- forecast_insample(
    mod,
    sample_share = 0.8,
    exog_fill_method = c("ets"),
    plot = FALSE,
    quiet = TRUE
  )

  res_with_ins <- dm_test(
    model = mod,
    insample_model = ins_mod,
    insample_main_comparison = "ets",
    comparison_methods = c("RW"),
    dm.horizons = 3,
    quiet = TRUE
  )

  expect_type(res_with_ins, "list")
  expect_true(all(c("dm_forecast", "dat") %in% names(res_with_ins)))
  expect_false("insample_model" %in% names(res_with_ins)) # since you only attach it when you computed it inside

  # must contain OSEM ets since insample_main_comparison is ets
  expect_true(any(res_with_ins$dat$forecast_type == "OSEM ets", na.rm = TRUE))

  # --- 4) parallel.cores should not change results (fast: 1 core vs serial) ---
  skip_if_not_installed("parallel") # only used if you run the parallel.cores check
  res_serial <- dm_test(
    model = mod,
    insample_methods = c("ets"),
    insample_main_comparison = "ets",
    insample_sample_share = 0.8,
    comparison_methods = c("RW"),
    dm.horizons = 3,
    parallel.cores = NULL,
    quiet = TRUE
  )

  res_parallel1 <- dm_test(
    model = mod,
    insample_methods = c("ets"),
    insample_main_comparison = "ets",
    insample_sample_share = 0.8,
    comparison_methods = c("RW"),
    dm.horizons = 3,
    parallel.cores = 2,
    quiet = TRUE
  )

  # compare stable parts only (DM table + dat), ignoring insample_model internals
  expect_equal(res_parallel1$dm_forecast, res_serial$dm_forecast)
  expect_equal(res_parallel1$dat, res_serial$dat)

  # --- 5) input validation ---
  expect_error(
    dm_test(mod, insample_methods = c("ets", "auto"), insample_main_comparison = "bad", quiet = TRUE),
    "insample_main_comparison"
  )

  expect_error(
    dm_test(mod, comparison_methods = c("NOPE"), quiet = TRUE),
    "comparison_methods"
  )

  expect_error(
    dm_test(mod, dm.horizons = c(1, 2), quiet = TRUE),
    "dm\\.horizons"
  )

  expect_error(
    dm_test(mod, dm.horizons = 0, quiet = TRUE),
    "dm\\.horizons"
  )

  expect_error(
    dm_test(mod, dm.alternative = "invalid", quiet = TRUE),
    "dm\\.alternative"
  )

  expect_error(
    dm_test(mod, lags = -1, quiet = TRUE),
    "lags"
  )

  # dm.power: warns (per your implementation)
  expect_error(
    dm_test(mod, insample_model = ins_mod, dm.power = 0, dm.horizons = 2, comparison_methods = "RW", quiet = TRUE),
    "dm\\.power"
  )
})
