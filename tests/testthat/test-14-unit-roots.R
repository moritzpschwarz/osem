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
  dplyr::mutate(time = seq.Date(from = "1900-01-01", by = "quarter", length.out = nobs - 1))

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
