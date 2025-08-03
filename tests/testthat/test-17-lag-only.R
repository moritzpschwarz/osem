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
    inputdata_directory = test_path("testdata", "cvar"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))
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
    inputdata_directory = test_path("testdata", "cvar"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))
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
    inputdata_directory = test_path("testdata", "cvar"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))
})
