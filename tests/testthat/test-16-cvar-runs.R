dictionary <- dplyr::tibble(
  model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
  full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
  database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local"),
  geo = "DE",
  dataset_id = NA,
  freq = ""
)

test_that("run_model() works with cvar input", {
  specification <- dplyr::tibble(
    type = c("n", "n", "n", "n", "n", "n", "d", "n", "n"),
    dependent = c("Y", "Z", "U", "V", "W", "M", "T", "Q", "S"),
    independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R"),
    lag = c("", "", "", "W", "", "U, Y", "", "", ""),
    cvar = c("system1", "system1", "", "", "", "", "", "", "")
  )
  expect_no_error(a <- run_model(
    specification = specification,
    dictionary = dictionary,
    inputdata_directory = test_path("testdata", "cvar"),
    primary_source = "local",
    use_logs = "none",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))
})

test_that("run_model() works with cvar input", {
  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  expect_no_error(a <- run_model(
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

test_that("run_model() works with cvar input", {
  specification <- dplyr::tibble(
    type = c("n", "n", "n"),
    dependent = c("U", "Y", "Z"),
    independent = c("", "U", "U"),
    lag = c("", "", ""),
    cvar = c("", "system1", "system1")
  )
  expect_no_error(a <- run_model(
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
