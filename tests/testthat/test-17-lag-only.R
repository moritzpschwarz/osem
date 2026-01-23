test_that("run_model() works with cvar input", {
  dictionary <- dplyr::tibble(
    model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
    full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
    database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local", "local"),
    geo = "DE",
    dataset_id = NA,
    freq = ""
  )
  # no lags
  specification <- dplyr::tibble(
    type = "n",
    dependent = "N",
    independent = "R + U",
    lag = "",
    cvar = ""
  )
  a <- expect_no_error(run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))

  expect_no_error(a_fcst <- forecast_model(a, quiet = TRUE))
  expect_equal(round(a_fcst$forecast$central.estimate[[1]]$ln.N, 5),
               c(0.87678, -0.00039, -0.70809, -0.62825, -0.56928, -0.5588, -0.57479,
                 -0.58751, -0.51246, -0.52348))
  expect_no_error(print(a_fcst))


  # both lag only
  specification <- dplyr::tibble(
    type = "n",
    dependent = "N",
    independent = "R + U",
    lag = "R, U",
    cvar = ""
  )
  b <- expect_no_error(run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))

  expect_no_error(b_fcst <- forecast_model(a, quiet = TRUE))
  expect_equal(round(b_fcst$forecast$central.estimate[[1]]$ln.N, 5),
               c(0.87678, -0.00039, -0.70809, -0.62825, -0.56928, -0.5588, -0.57479,
                 -0.58751, -0.51246, -0.52348))
  expect_no_error(print(b_fcst))

  # Y lag only but U contemporaneous
  specification <- dplyr::tibble(
    type = "n",
    dependent = "N",
    independent = "R + U",
    lag = "U",
    cvar = ""
  )
  c <- expect_no_error(run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))



  expect_no_error(c_fcst <- forecast_model(a, quiet = TRUE))
  expect_equal(round(c_fcst$forecast$central.estimate[[1]]$ln.N, 5),
               c(0.87678, -0.00039, -0.70809, -0.62825, -0.56928, -0.5588, -0.57479,
                 -0.58751, -0.51246, -0.52348))
  expect_no_error(print(c_fcst))

})
