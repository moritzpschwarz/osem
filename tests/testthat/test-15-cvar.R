#### Step 1: generate data
# setup
nobs <- 101 # lose 1 obs
Sigma <- matrix(c(0.2, 0.1, 0.1, 0.1), nrow = 2, ncol = 2) # error matrix
A <- matrix(c(0.8, 0, 0.6, 1), nrow = 2, ncol = 2) # AR matrix
mu <- matrix(c(0.5, 0), nrow = 2, ncol = 1) # intercept
X0 <- matrix(c(5.5, 1), nrow = 2, ncol = 1) # initial values

# CVAR data
data_cvar <- matrix(NA, nrow = 2, ncol = nobs)
rownames(data_cvar) <- c("Y", "Z")
# store initial value
data_cvar[, 1] <- X0
# use loop to generate data
set.seed(123)
for (t in 2:nobs) {
  # generate errors
  epsilont <- matrix(MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = Sigma), nrow = 2, ncol = 1)
  # update observations of Y, Z
  data_cvar[, t] <- A %*% data_cvar[, t - 1] + mu + epsilont
}
data_cvar <- as.data.frame(t(data_cvar)) %>%
  dplyr::mutate(time = 1:nobs)

# generate pure AR process
data_ar <- matrix(NA, nrow = 1, ncol = nobs)
rownames(data_ar) <- "Q"
data_ar[, 1] <- 1.5
for (t in 2:nobs) {
  # update observation Q
  data_ar[, t] <- 1 + 0.3 * data_ar[, t - 1] + rnorm(1)
}

# combine
data <- cbind(data_cvar, t(data_ar)) %>%
  dplyr::mutate(time = seq.Date(from = "1900-01-01", by = "quarter", length.out = nobs))

#### Step 2: Unit tests
test_that("estimate_cvar() raises input errors", {
  expect_error(
    estimate_cvar(data, "sys1", "Y, Z", character(0),
      use_logs = "none", ar = 4, NULL,
      coint_deterministic = "Seasonal", coint_significance = "5pct"
    ),
    "none.*const.*trend"
  )
  expect_error(
    estimate_cvar(data, "sys1", "Y, Z", character(0),
      use_logs = "none", ar = 4, NULL,
      coint_deterministic = "none", coint_significance = "15pct"
    ),
    "1pct.*5pct.*10pct"
  )
})
test_that("estimate_cvar() raises error if series not suitably integrated/stationary", {
  # not all variables in the system are I(1)
  expect_error(
    estimate_cvar(
      data, "sys1", "Y,Q", character(0), "none", 2, NULL, "const", "10pct"
    ),
    "Not all CVAR variables in CVAR sub-system sys1 are I(1). For the following variables, we reject the unit root hypothesis: Q.",
    fixed = TRUE
  )
  # an I(1) variable is included as exogenous regressor
  expect_error(
    estimate_cvar(
      data, "sys1", "Y,Z", "Z", "none", 2, NULL, "const", "10pct"
    ),
    "Not all exogenous regressors in CVAR sub-system sys1 are stationary. For the following variables, we do not reject the unit root hypothesis: Z.",
    fixed = TRUE
  )
})
test_that("estimate_cvar() returns expected output", {
  # general structure
  expect_message(a <- estimate_cvar(data, "sys1", "Y,Z", "Q", "none", 2, NULL, "const", "5pct"))
  expect_type(a, "list")
  expect_length(a, 6L)
  expect_named(a, c("cointtest", "vecm", "varm", "rank", "urtest", "args"))
  expect_s4_class(a$cointtest, "ca.jo")
  expect_type(a$vecm, "list")
  expect_s3_class(a$varm, "vec2var")
  expect_type(a$rank, "integer")
  expect_type(a$urtest, "list")
  expect_type(a$args, "list")
  expect_length(a$urtest, 2L)
  expect_named(a$urtest, c("y_urtest", "x_urtest"))
  expect_length(a$args, 8L)
  expect_named(a$args, c(
    "clean_data", "cvar", "dep_vars_basename",
    "x_vars_basename", "use_logs", "ar",
    "coint_deterministic", "coint_significance"
  ))
  # specific structure for this command - has everything been recorded correctly?
  expect_identical(a$args$cvar, "sys1")
  expect_identical(a$args$dep_vars_basename, "Y,Z")
  expect_identical(a$args$x_vars_basename, "Q")
  expect_identical(a$args$use_logs, "none")
  expect_identical(a$args$ar, 2)
  expect_identical(a$args$coint_deterministic, "const")
  expect_identical(a$args$coint_significance, "5pct")
  expect_identical(a$rank, 1L)
  expect_identical(a$urtest$y_urtest$Z$decision$reject_ur, FALSE)
  expect_identical(a$urtest$y_urtest$Y$decision$reject_ur, FALSE)
  expect_identical(a$urtest$x_urtest$Q$decision$reject_ur, TRUE)
  # we were unable to estimate the DGP (had only one lag in level VAR but ca.jo
  # requires us to set K > 1). We also included Q, which is not in DGP.
  # DGP equation transformed would be:
  # Delta Yt = -0.2*ect + errort = -0.2*(Yt-1 - 3Zt-1 - 2.5) + errort
  # Delta Zt = errort
  # results are still close to the DGP
  ## y results
  results_y <- summary(a$vecm$rlm)$`Response Y.d`$coefficients
  # ect term should be significant; Q, lagged Delta Y, lagged Delta Z insignificant
  expect_true(results_y["ect1", "Pr(>|t|)"] < 0.01)
  expect_true(results_y["Q", "Pr(>|t|)"] > 0.1)
  expect_true(results_y["Y.dl1", "Pr(>|t|)"] > 0.1)
  expect_true(results_y["Z.dl1", "Pr(>|t|)"] > 0.1)
  ## z results
  results_z <- summary(a$vecm$rlm)$`Response Z.d`$coefficients
  # all terms should be insignificant
  expect_true(results_z["ect1", "Pr(>|t|)"] > 0.1)
  expect_true(results_z["Q", "Pr(>|t|)"] > 0.1)
  expect_true(results_z["Y.dl1", "Pr(>|t|)"] > 0.1)
  expect_true(results_z["Z.dl1", "Pr(>|t|)"] > 0.1)
})
test_that("a variety of inputs work for estimate_cvar()", {
  expect_no_error(suppressMessages(estimate_cvar(
    data, "system1",
    "Y,    Z", # unusual spacing
    character(0), "none", 3, NULL,
    "none", # no deterministic terms
    "1pct"
  )))
  expect_no_error(suppressMessages(estimate_cvar(
    data, "system1", "Y,Z", character(0), "none", 3, NULL,
    "const", # constant
    "1pct"
  )))
  expect_no_error(suppressMessages(estimate_cvar(
    data, "system1", "Y,Z", character(0), "none", 3, NULL,
    "trend", # trend
    "1pct"
  )))
  expect_no_error(cvar_seasonal <- suppressMessages(estimate_cvar(
    data, "system1", "Y,Z", character(0), "none", 2,
    4, # quarterly dummies
    "const", "1pct"
  )))
  # seasonal dummies should be insignificant
  coef <- cvar_seasonal$vecm$rlm %>%
    summary() %>%
    purrr::pluck("Response Y.d") %>%
    purrr::pluck("coefficients")
  expect_true(coef["sd1", "Pr(>|t|)"] > 0.1)
  expect_true(coef["sd2", "Pr(>|t|)"] > 0.1)
  expect_true(coef["sd3", "Pr(>|t|)"] > 0.1)
  expect_no_error(suppressMessages(estimate_cvar(
    data, "system1", "Y,Z",
    "Q", # exogenous regressor
    "none", 3, NULL, "const", "5pct"
  )))
  # test log transformations of x and y -> need to exist in dataframe -> rename
  data_log <- data %>%
    dplyr::mutate(ln.Y = Y, ln.Z = Z, ln.Q = Q) # don't actually transform, neg. values
  expect_no_error(suppressMessages(estimate_cvar(
    data_log, "system1", "Y,Z", "Q",
    "y", # dependent variables logged
    2, NULL, "const", "5pct"
  )))
  expect_no_error(suppressMessages(estimate_cvar(
    data_log, "system1", "Y,Z", "Q",
    "x", # regressors logged
    2, NULL, "const", "5pct"
  )))
  expect_no_error(cvar_log <- suppressMessages(estimate_cvar(
    data_log, "system1", "Y,Z", "Q",
    "both", # both logged
    2, NULL, "const", "5pct"
  )))
  # check that output is as expected with the logged variables
  expect_named(cvar_log$urtest$y_urtest, c("ln.Y", "ln.Z"))
  expect_named(cvar_log$urtest$x_urtest, "ln.Q")
  expect_identical(rownames(cvar_log$vecm$beta), c("ln.Y.l1", "ln.Z.l1", "constant"))
  expect_identical(cvar_log$args$dep_vars_basename, "Y,Z")
  expect_identical(cvar_log$args$x_vars_basename, "Q")
  expect_identical(cvar_log$args$use_logs, "both")
})
