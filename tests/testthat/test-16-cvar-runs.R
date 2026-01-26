dictionary <- dplyr::tibble(
  model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
  full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
  database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local", "local"),
  geo = "DE",
  dataset_id = NA,
  freq = ""
)

test_that("run_model() works with cvar input", {
  specification <- dplyr::tibble(
    type = c("n", "n", "n", "n", "n", "n", "d", "n", "n", "n"),
    dependent = c("Y", "Z", "U", "V", "W", "M", "T", "Q", "S", "N"),
    independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R", "R + U"),
    lag = c("", "", "", "W", "", "U, Y", "", "", "", "U"),
    cvar = c("system1", "system1", "", "", "", "", "", "", "", "")
  )
  expect_no_error(a <- run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "none",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))

  expect_no_error(a_fcst <- forecast_model(a, quiet = TRUE, plot = FALSE))
  fcst_table <- dplyr::bind_rows(a_fcst$forecast$central.estimate) %>%
    tidyr::pivot_longer(-time) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = "time") %>%
    dplyr::mutate(dplyr::across(-"time", ~ round(. , 5)))

  expect_equal(
    fcst_table,
    structure(list(time = structure(c(-16436, -16346, -16255, -16163,-16071, -15981, -15890, -15798, -15706, -15616), class = "Date"),
                   U = c(0.14002, -0.08364, 0.00855, 0.07772, 0.04921, 0.02782, 0.03664, 0.04325, 0.04053, 0.03848),
                   Q = c(4.11847, 3.74663, 3.52416, 3.39105, 3.31142, 3.26377, 3.23526, 3.21821, 3.208, 3.2019),
                   Y = c(5.27462, 5.40493, 5.49805, 5.55569, 5.59438, 5.62059, 5.63709, 5.64653, 5.65141, 5.6532),
                   Z = c(1.21938, 1.19239, 1.16316, 1.13728, 1.11888, 1.10665, 1.09741, 1.09003, 1.08435, 1.07992),
                   V = c(1.5849, 1.85623, 1.62282, 2.08275, 2.23139, 2.29355, 2.02872, 2.3561, 2.38142, 2.39486),
                   W = c(2.21001, 2.64515, 2.80238, 2.96458, 3.1161, 3.20093, 3.24511, 3.28027, 3.30802, 3.32507),
                   S = c(1.2837, -0.62646, -3.13316, -1.54726, 0.04445, -1.31482, -2.84327, -1.31996, -0.02021, -1.38578),
                   N = c(2.07912, -0.013, -2.2282, -0.98281, 0.1932, -0.71435, -1.70109, -0.75724, 0.04521, -0.78451),
                   M = c(0.49721, 0.49721, 0.49721, 0.49721, 0.49721, 0.49721, 0.49721, 0.49721, 0.49721, 0.49721),
                   T = c(3.93494, 4.41774, 4.43375, 5.12505, 5.3967, 5.52229, 5.31047, 5.67962, 5.72997, 5.75841)),
              row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"))
  )


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
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
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
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))
})

test_that("run_model() works with only a cvar model", {

  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  expect_no_error(d <- run_model(
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

  expect_no_error(d_fcst <- forecast_model(d, quiet = TRUE, plot = FALSE))
  fcst_table <- dplyr::bind_rows(d_fcst$forecast$central.estimate) %>%
    tidyr::pivot_longer(-time) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = "time") %>%
    dplyr::mutate(dplyr::across(-"time", ~ round(. , 5)))

  expect_equal(
    fcst_table,
    structure(list(time = structure(c(-16436, -16346, -16255, -16163, -16071, -15981, -15890, -15798, -15706, -15616), class = "Date"),
                   ln.Y = c(1.69509, 1.75508, 1.79805, 1.83847, 1.86813, 1.88651, 1.89985, 1.9176, 1.92999, 1.93657),
                   ln.Z = c(1.00993, 0.99992, 0.99271, 1.00891, 1.01148, 1.00589, 1.00463, 1.02105, 1.02305, 1.01939)),
              row.names = c(NA, -10L), class = c("tbl_df", "tbl",
                                                 "data.frame"))
  )
})
