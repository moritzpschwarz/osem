# create data
nobs <- 101 # lose 1 obs
Sigma <- matrix(c(0.2, 0.1, 0.1, 0.1), nrow = 2, ncol = 2) # error matrix
A <- matrix(c(0.8, 0, 0.6, 1), nrow = 2, ncol = 2) # AR matrix
mu <- matrix(c(0.5, 0), nrow = 2, ncol = 1) # intercept
X0 <- matrix(c(5.5, 1), nrow = 2, ncol = 1) # initial values

#### Step 1: generate data
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

# generate system without cycles but lagged relationship
# Wt = 0.5Wt-1 + 0.4Vt + Ut + epsilonWt
# Vt = 0.2Vt-1 + 0.5Wt-1 + 2Ut + epsilonVt
# Ut = 0.4Ut-1 + epsilonUt
B <- matrix(c(1, -2, -1, 0, 1, -0.4, 0, 0, 1), nrow = 3, ncol = 3)
A <- matrix(c(0.4, 0, 0, 0, 0.2, 0, 0, 0.5, 0.5), nrow = 3, ncol = 3)
Phi <- solve(B) %*% A # reduced form matrix
data_uni <- matrix(NA, nrow = 3, ncol = nobs)
rownames(data_uni) <- c("U", "V", "W")
data_uni[, 1] <- c(0, 0, 0) # initial values of zero (LR mean)
for (t in 2:nobs) {
  # generate errors
  epsilont <- matrix(MASS::mvrnorm(n = 1, mu = c(0, 0, 0), Sigma = diag(3)), nrow = 3, ncol = 1)
  # update observations of U, V, T
  data_uni[, t] <- Phi %*% data_uni[, t - 1] + epsilont
}
data_uni <- as.data.frame(t(data_uni)) %>%
  dplyr::mutate(time = 1:nobs)

# generate (i) pure AR process (unconnected to rest) and (ii) process with purely exogenous regressor
data_ar <- matrix(NA, nrow = 1, ncol = nobs)
rownames(data_ar) <- "Q"
data_ar[, 1] <- 1.5
for (t in 2:nobs) {
  # update observation Q
  data_ar[, t] <- 1 + 0.3 * data_ar[, t - 1] + rnorm(1)
}
data_other <- data.frame(
  Q = t(data_ar),
  R = rnorm(nobs)
) %>%
  dplyr::mutate(
    S = 5 * R + rnorm(nobs),
    time = 1:nobs
  )

# combine data
data <- data_cvar %>%
  dplyr::select(Y, Z) %>%
  dplyr::bind_cols(data_uni %>% dplyr::select(U, V, W)) %>%
  dplyr::bind_cols(data_other %>% dplyr::select(Q, R, S)) %>%
  # generate a variable based on variables from both CVAR and stationary system: Mt = 0.1Yt-1 + 0.1Ut-1
  dplyr::mutate(error = rnorm(nobs)) %>%
  dplyr::mutate(M = 0.1 * dplyr::lag(Y) + 0.1 * dplyr::lag(U) + error) %>%
  dplyr::select(-error) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(time = seq.Date(from = as.Date("1900-01-01"), by = "quarter", length.out = nobs - 1))

#### Step 2: Unit tests
test_that("test_unit_roots() raises input error", {
  expect_error(test_unit_roots(x = data$U, max.ar = 4, selectlags = "NotAvailable"))
})
test_that("test_unit_roots() returns correct output object", {
  a <- test_unit_roots(x = data$U, max.ar = 4, selectlags = "Fixed")
  expect_type(a, "list")
  expect_length(a, 4)
  expect_named(a, c("args", "none", "drift", "trend"))
  expect_length(a$args, 2)
  expect_named(a$args, c("selectlags", "max.ar"))
  expect_identical(a$args$selectlags, "Fixed")
  expect_identical(a$args$max.ar, 4)
  expect_type(a$none, "S4")
  expect_type(a$drift, "S4")
  expect_type(a$trend, "S4")
  expect_s4_class(a$none, "ur.df")
  expect_s4_class(a$drift, "ur.df")
  expect_s4_class(a$trend, "ur.df")
})
test_that("test_unit_roots() makes sensible decisions for artificial data", {
  #### Y and Z have unit roots
  # Y
  y <- test_unit_roots(x = data$Y, max.ar = 4, selectlags = "BIC")
  # expect not to reject
  expect_false(y$drift@teststat["statistic", "tau2"] < y$drift@cval["tau2", "5pct"])
  expect_false(y$trend@teststat["statistic", "tau3"] < y$trend@cval["tau3", "5pct"])
  # Z
  z <- test_unit_roots(x = data$Z, max.ar = 4, selectlags = "BIC")
  expect_false(z$none@teststat["statistic", "tau1"] < z$none@cval["tau1", "5pct"])
  expect_false(z$drift@teststat["statistic", "tau2"] < z$drift@cval["tau2", "5pct"])
  expect_false(z$trend@teststat["statistic", "tau3"] < z$trend@cval["tau3", "5pct"])
  #### U is stationary AR(1)
  # U
  u <- test_unit_roots(x = data$U, max.ar = 4, selectlags = "BIC")
  # expect to reject
  expect_true(u$none@teststat["statistic", "tau1"] < u$none@cval["tau1", "5pct"])
  expect_true(u$drift@teststat["statistic", "tau2"] < u$drift@cval["tau2", "5pct"])
  expect_true(u$trend@teststat["statistic", "tau3"] < u$trend@cval["tau3", "5pct"])
  #### V and W are stationary ARDL
  # V
  v <- test_unit_roots(x = data$V, max.ar = 4, selectlags = "BIC")
  # expect to reject
  expect_true(v$none@teststat["statistic", "tau1"] < v$none@cval["tau1", "5pct"])
  expect_true(v$drift@teststat["statistic", "tau2"] < v$drift@cval["tau2", "5pct"])
  expect_true(v$trend@teststat["statistic", "tau3"] < v$trend@cval["tau3", "5pct"])
  # W
  w <- test_unit_roots(x = data$W, max.ar = 4, selectlags = "BIC")
  # expect to reject but in fact don't reject all (but without intercept is most appropriate and this rejects, so is good)
  expect_true(w$none@teststat["statistic", "tau1"] < w$none@cval["tau1", "5pct"])
  expect_true(w$drift@teststat["statistic", "tau2"] < w$drift@cval["tau2", "10pct"])
  #### Q, R, and S are stationary
  # Q
  q <- test_unit_roots(x = data$Q, max.ar = 4, selectlags = "BIC")
  # expect to reject
  expect_true(q$none@teststat["statistic", "tau1"] < q$none@cval["tau1", "5pct"])
  expect_true(q$drift@teststat["statistic", "tau2"] < q$drift@cval["tau2", "5pct"])
  expect_true(q$trend@teststat["statistic", "tau3"] < q$trend@cval["tau3", "5pct"])
  # R
  r <- test_unit_roots(x = data$R, max.ar = 4, selectlags = "BIC")
  # expect to reject
  expect_true(r$none@teststat["statistic", "tau1"] < r$none@cval["tau1", "5pct"])
  expect_true(r$drift@teststat["statistic", "tau2"] < r$drift@cval["tau2", "5pct"])
  expect_true(r$trend@teststat["statistic", "tau3"] < r$trend@cval["tau3", "5pct"])
  # S
  s <- test_unit_roots(x = data$S, max.ar = 4, selectlags = "BIC")
  # expect to reject
  expect_true(s$none@teststat["statistic", "tau1"] < s$none@cval["tau1", "5pct"])
  expect_true(s$drift@teststat["statistic", "tau2"] < s$drift@cval["tau2", "5pct"])
  expect_true(s$trend@teststat["statistic", "tau3"] < s$trend@cval["tau3", "5pct"])
})
test_that("decide_unit_roots() raises input error", {
  a <- test_unit_roots(x = data$U, max.ar = 4, selectlags = "Fixed")
  expect_error(decide_unit_roots(urtest = a, alpha = "15pct"))
})
test_that("decide_unit_roots() returns correct output object", {
  a <- test_unit_roots(x = data$U, max.ar = 4, selectlags = "Fixed")
  aa <- decide_unit_roots(urtest = a, alpha = "5pct")
  expect_type(aa, "list")
  expect_length(aa, 5)
  expect_named(aa, c("args", "none", "drift", "trend", "decision"))
  expect_length(aa$args, 2)
  expect_named(aa$args, c("selectlags", "max.ar"))
  expect_identical(aa$args$selectlags, "Fixed")
  expect_identical(aa$args$max.ar, 4)
  expect_type(aa$decision, "list")
  expect_length(aa$decision, 3)
  expect_named(aa$decision, c("alpha_ur", "reject_ur", "when"))
  expect_identical(aa$decision$alpha_ur, "5pct")
  expect_identical(aa[1:4], a) # aa only appends one list
})
test_that("test_unit_roots() makes sensible decisions for artificial data", {
  y <- test_unit_roots(x = data$Y, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(y$decision$reject_ur, FALSE)
  z <- test_unit_roots(x = data$Z, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(z$decision$reject_ur, FALSE)
  u <- test_unit_roots(x = data$U, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(u$decision$reject_ur, TRUE)
  v <- test_unit_roots(x = data$V, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(v$decision$reject_ur, TRUE)
  w <- test_unit_roots(x = data$W, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(w$decision$reject_ur, TRUE)
  q <- test_unit_roots(x = data$Q, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(q$decision$reject_ur, TRUE)
  r <- test_unit_roots(x = data$R, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(r$decision$reject_ur, TRUE)
  s <- test_unit_roots(x = data$S, max.ar = 4, selectlags = "BIC") %>%
    decide_unit_roots("5pct")
  expect_identical(s$decision$reject_ur, TRUE)
})

# test unit root diagnostics
dictionary <- dplyr::tibble(
  model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
  full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "N", "A", "B"),
  database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local", "local"),
  geo = "DE",
  dataset_id = NA,
  freq = ""
)
specification <- dplyr::tibble(
  type = c("n", "n", "n", "n", "n", "n", "d", "n", "n", "n"),
  dependent = c("Y", "Z", "U", "V", "W", "M", "T", "Q", "S", "N"),
  independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R", "R + U"),
  lag = c("", "", "", "W", "", "U, Y", "", "", "", "U"),
  cvar = c("system1", "system1", "", "", "", "", "", "", "", "")
)
test_that("unit root diagnostics works as intended", {
  # no logs
  a <- run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "none",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )
  urdiag_a <- diagnostics_unit_root(a)
  # check output is as expected
  expect_s3_class(urdiag_a, c("tbl_df", "tbl", "data.frame"))
  expect_identical(dim(urdiag_a), c(14L, 6L))
  # T variable is only a dependent variable in identity module, should not be part of diagnostics
  expect_false("T" %in% urdiag_a$basevarname)
  # R variable is only an independent variable
  expect_false("R" %in% (urdiag_a %>% dplyr::filter(.data$type == "dependent") %>% dplyr::pull("basevarname")))
  # dependent endogenous variables: Y, Z, U, V, W, M, Q, S, N
  expect_setequal(c("Y", "Z", "U", "V", "W", "M", "Q", "S", "N"), (urdiag_a %>% dplyr::filter(.data$type == "dependent") %>% dplyr::pull("basevarname")))
  # independent vars in endogenous models: U, V, W, Y, R
  expect_setequal(c("U", "V", "W", "Y", "R"), (urdiag_a %>% dplyr::filter(.data$type == "independent") %>% dplyr::pull("basevarname")))
  # transformation setting should be "level" everywhere
  expect_all_equal(urdiag_a$transformation, "level")
  # ur_test should be lists
  expect_type(urdiag_a$ur_test, "list")
  # ur_decision should be character vector, "ur" or "not ur"
  expect_type(urdiag_a$ur_decision, "character")
  expect_setequal(unique(urdiag_a$ur_decision), c("ur", "not ur"))
  # modules should be lists
  expect_type(urdiag_a$modules, "list")
  # modules should correspond to the index of the module order
  ## check for dependent variables
  urdiag_a_dep <- urdiag_a %>% dplyr::filter(.data$type == "dependent")
  for (i in 1:NROW(urdiag_a_dep)) {
    stored_index <- urdiag_a_dep %>% dplyr::slice(i) %>% dplyr::pull("modules") %>% purrr::pluck(1)
    stored_var <- urdiag_a_dep %>% dplyr::slice(i) %>% dplyr::pull("basevarname")
    # can do simplified search here because each variable is a unique character (partial matching is no problem)
    module_row <- grep(stored_var, a$module_order$dependent)
    module_index <- a$module_order %>% dplyr::slice(module_row) %>% dplyr::pull("index")
    expect_identical(module_index, stored_index)
  }
  ## check for independent variables
  urdiag_a_indep <- urdiag_a %>% dplyr::filter(.data$type == "independent")
  for (i in 1:NROW(urdiag_a_indep)) {
    stored_indices <- urdiag_a_indep %>% dplyr::slice(i) %>% dplyr::pull("modules") %>% purrr::pluck(1)
    stored_var <- urdiag_a_indep %>% dplyr::slice(i) %>% dplyr::pull("basevarname")
    # can do simplified search here because each variable is a unique character (partial matching is no problem); have to filter out definitions
    module_rows <- grep(stored_var, a$module_order %>% dplyr::filter(.data$type == "n") %>% dplyr::pull("independent"))
    module_indices <- a$module_order %>% dplyr::filter(.data$type == "n") %>% dplyr::slice(module_rows) %>% dplyr::pull("index")
    expect_identical(module_indices, stored_indices)
  }

  # run transformation variations
  # run logs in y
  b <- run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "y",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )
  urdiag_b <- diagnostics_unit_root(b)
  # some parts should be identical to object urdiag_a from above
  expect_identical(urdiag_a[, c("basevarname", "type", "modules")], urdiag_b[, c("basevarname", "type", "modules")])
  # transformation should be different
  expect_identical(urdiag_b %>% dplyr::filter(.data$type == "dependent") %>% dplyr::pull("transformation") %>% unique(), "log")
  expect_identical(urdiag_b %>% dplyr::filter(.data$type == "independent") %>% dplyr::pull("transformation") %>% unique(), "level")

  # run logs in x
  c <- run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "x",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )
  urdiag_c <- diagnostics_unit_root(c)
  # some parts should be identical to object urdiag_a from above
  expect_identical(urdiag_a[, c("basevarname", "type", "modules")], urdiag_c[, c("basevarname", "type", "modules")])
  # transformation should be different
  expect_identical(urdiag_c %>% dplyr::filter(.data$type == "dependent") %>% dplyr::pull("transformation") %>% unique(), "level")
  expect_identical(urdiag_c %>% dplyr::filter(.data$type == "independent") %>% dplyr::pull("transformation") %>% unique(), "log")

  # run logs in both
  d <- run_model(
    specification = specification,
    dictionary = dictionary,
    input = test_path("testdata", "cvar", "artificial_cvar_data.rds"),
    primary_source = "local",
    use_logs = "both",
    trend = FALSE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )
  urdiag_d <- diagnostics_unit_root(d)
  # some parts should be identical to object urdiag_a from above
  expect_identical(urdiag_a[, c("basevarname", "type", "modules")], urdiag_d[, c("basevarname", "type", "modules")])
  # transformation should be different
  expect_identical(urdiag_d %>% dplyr::filter(.data$type == "dependent") %>% dplyr::pull("transformation") %>% unique(), "log")
  expect_identical(urdiag_d %>% dplyr::filter(.data$type == "independent") %>% dplyr::pull("transformation") %>% unique(), "log")

})




