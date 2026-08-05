# test-19-ecm-auto.R

# This file contains simple simulation-based checks for the automatic
# single-equation ECM decision rule.
#
# The tests are designed to map directly onto the intended ECM workflow:
#
#   1. If all variables look I(0), do not force an ECM; estimate a levels ARDL.
#   2. If variables are I(1) and cointegrated, keep the ECM.
#   3. If variables are I(1) but not cointegrated, estimate the differenced model.
#   4. If a variable is I(2) or uncertain, do not keep the ECM.
#   5. In diagnostic mode, report diagnostics but do not change the requested ECM.
#   6. In none mode, preserve the old ECM behaviour and do not run diagnostics.
#   7. Store the final decision in opts_df for each module.
#
# The simulated variables are generated in logs and then exponentiated. This makes
# it safe to run OSEM with use_logs = "both", while still keeping the data-generating
# process transparent.


# Helper functions -----------------------------------------------------------

# Create a minimal local dictionary for the simulated variables.
# The exact dictionary columns may need to be adjusted if the package dictionary
# schema changes, but this mirrors the current local-data test style.
make_ecm_auto_dictionary <- function(vars) {
  dplyr::tibble(
    model_varname = vars,
    full_name = vars,
    database = "local",
    geo = "DE",
    dataset_id = NA_character_,
    freq = ""
  )
}


# Convert a wide simulated data frame into the long local-input format expected
# by run_model(). The input data must contain a column called time and one column
# per simulated variable.
make_ecm_auto_input <- function(df) {
  df %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")
}


# Extract the stored ECM decision object for one dependent variable.
# This is the main object we want to inspect because it stores the requested
# model type, selected model type, reason for the decision, integration diagnostics,
# and the single-equation ECM diagnostic.
extract_ecm_decision <- function(model, dep_var) {
  model$opts_df %>%
    dplyr::filter(.data$dependent == dep_var) %>%
    dplyr::pull(.data$ecm_decision) %>%
    purrr::pluck(1)
}


# Extract only the final selected specification for one dependent variable.
# Expected values in these tests are:
#
#   "ardl"                -> levels ARDL
#   "ecm"                 -> unrestricted ECM retained
#   "fully_differenced"   -> ECM rejected and differenced model used
extract_ecm_selected <- function(model, dep_var) {
  model$opts_df %>%
    dplyr::filter(.data$dependent == dep_var) %>%
    dplyr::pull(.data$ardl_or_ecm_selected)
}


# Simulation functions -------------------------------------------------------

# Case 1: all variables are stationary.
#
# Data-generating process:
#
#   x_t = 0.5 x_{t-1} + u_t
#   y_t = 0.4 y_{t-1} + 0.3 x_t + e_t
#
# Both log variables are I(0). Therefore, if the user requests ECM with
# ecm_pretest = "auto", the automatic rule should not force an ECM. It should
# select a levels ARDL instead.
simulate_i0_ardl_case <- function(nobs = 220, seed = 123) {
  set.seed(seed)

  lx <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx[t] <- 0.5 * lx[t - 1] + rnorm(1, sd = 0.2)
    ly[t] <- 0.4 * ly[t - 1] + 0.3 * lx[t] + rnorm(1, sd = 0.2)
  }

  dplyr::tibble(
    time = seq.Date(from = as.Date("1970-01-01"), by = "quarter", length.out = nobs),
    X = exp(5 + lx),
    Y = exp(5 + ly)
  )
}


# Case 2: variables are I(1) and cointegrated.
#
# Data-generating process:
#
#   x_t = x_{t-1} + u_t
#   z_t = 0.6 z_{t-1} + e_t
#   y_t = 1.2 x_t + z_t
#
# Since z_t is stationary, the relation
#
#   y_t - 1.2 x_t = z_t
#
# is stationary. Therefore y_t and x_t are cointegrated. The automatic ECM rule
# should retain the ECM, and the estimated adjustment coefficient should be
# negative when y is normalised on the left-hand side.
simulate_cointegrated_case <- function(nobs = 240, seed = 123) {
  set.seed(seed)

  lx <- numeric(nobs)
  z <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx[t] <- lx[t - 1] + rnorm(1, sd = 0.12)
    z[t] <- 0.6 * z[t - 1] + rnorm(1, sd = 0.12)
    ly[t] <- 1.2 * lx[t] + z[t]
  }

  dplyr::tibble(
    time = seq.Date(from = as.Date("1970-01-01"), by = "quarter", length.out = nobs),
    X = exp(5 + lx),
    Y = exp(5 + ly)
  )
}


# Case 3: variables are I(1) but not cointegrated.
#
# Data-generating process:
#
#   x_t = x_{t-1} + u_t
#   y_t = y_{t-1} + e_t
#
# with independent shocks. There is no stable long-run relation linking y_t and
# x_t. Therefore the automatic ECM rule should reject the ECM interpretation and
# estimate the corresponding dynamic model in first differences.
simulate_non_cointegrated_case <- function(nobs = 240, seed = 872) {
  set.seed(seed)

  lx <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx[t] <- lx[t - 1] + rnorm(1, sd = 0.12)
    ly[t] <- ly[t - 1] + rnorm(1, sd = 0.12)
  }

  dplyr::tibble(
    time = seq.Date(from = as.Date("1970-01-01"), by = "quarter", length.out = nobs),
    X = exp(5 + lx),
    Y = exp(5 + ly)
  )
}


# Case 4: one variable is I(2) or otherwise too persistent/uncertain.
#
# Data-generating process:
#
#   v_t = v_{t-1} + u_t
#   x_t = x_{t-1} + v_t
#   y_t = y_{t-1} + e_t
#
# x_t is I(2). A standard ECM is not appropriate in this case. The automatic rule
# should not retain the ECM. Depending on the exact unit-root tests, the variable
# may be classified as "I2_or_uncertain" or "uncertain"; the important testable
# implication is that the selected model form is not "ecm".
simulate_i2_or_uncertain_case <- function(nobs = 260, seed = 123) {
  set.seed(seed)

  v <- numeric(nobs)
  lx <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    v[t] <- v[t - 1] + rnorm(1, sd = 0.04)
    lx[t] <- lx[t - 1] + v[t]
    ly[t] <- ly[t - 1] + rnorm(1, sd = 0.10)
  }

  # Rescale before exponentiating to avoid numerical issues.
  lx <- lx / stats::sd(lx)
  ly <- ly / stats::sd(ly)

  dplyr::tibble(
    time = seq.Date(from = as.Date("1970-01-01"), by = "quarter", length.out = nobs),
    X = exp(5 + lx),
    Y = exp(5 + ly)
  )
}


# Case 7: mixed modules.
#
# The first module is cointegrated:
#
#   Yc ~ Xc
#
# The second module is not cointegrated:
#
#   Yn ~ Xn
#
# This checks that the ECM decision is stored module-by-module in opts_df and
# that different modules can receive different automatic decisions within the
# same model run.
simulate_mixed_module_case <- function(nobs = 240, seed = 123) {
  set.seed(seed)

  lxc <- numeric(nobs)
  lyc <- numeric(nobs)
  zc <- numeric(nobs)

  lxn <- numeric(nobs)
  lyn <- numeric(nobs)

  for (t in 2:nobs) {
    lxc[t] <- lxc[t - 1] + rnorm(1, sd = 0.12)
    zc[t] <- 0.6 * zc[t - 1] + rnorm(1, sd = 0.12)
    lyc[t] <- 1.1 * lxc[t] + zc[t]

    lxn[t] <- lxn[t - 1] + rnorm(1, sd = 0.12)
    lyn[t] <- lyn[t - 1] + rnorm(1, sd = 0.12)
  }

  dplyr::tibble(
    time = seq.Date(from = as.Date("1970-01-01"), by = "quarter", length.out = nobs),
    Xc = exp(5 + lxc),
    Yc = exp(5 + lyc),
    Xn = exp(5 + lxn),
    Yn = exp(5 + lyn)
  )
}


# Tests ----------------------------------------------------------------------

test_that("ECM auto chooses ARDL when all variables are I(0)", {
  sim_data <- simulate_i0_ardl_case()

  # sim_data %>% ggplot2::ggplot(ggplot2::aes(x = time)) +
  #   ggplot2::geom_line(ggplot2::aes(y = Y, color = "Y")) +
  #   ggplot2::geom_line(ggplot2::aes(y = X, color = "X")) +
  #   ggplot2::labs(title = "Simulated I(0) data") +
  #   ggplot2::scale_color_manual(values = c("blue", "red")) +
  #   ggplot2::theme_minimal()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "auto",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # Since both variables are stationary, auto should select the levels ARDL.
  expect_identical(extract_ecm_selected(model, "Y"), "ardl")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$requested, "ecm")
  expect_identical(decision$selected, "ardl")
  expect_identical(decision$model_form, "ardl")

  # This is the branch condition that should have triggered the ARDL choice.
  expect_true(all(decision$integration$order == "I0"))
})


test_that("ECM auto keeps ECM when variables are I(1) and cointegrated", {
  sim_data <- simulate_cointegrated_case()

  # sim_data %>% ggplot2::ggplot(ggplot2::aes(x = time)) +
  #   ggplot2::geom_line(ggplot2::aes(y = Y, color = "Y")) +
  #   ggplot2::geom_line(ggplot2::aes(y = X, color = "X")) +
  #   ggplot2::labs(title = "Simulated cointegrated I(1) data") +
  #   ggplot2::scale_color_manual(values = c("blue", "red")) +
  #   ggplot2::theme_minimal()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "auto",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # Since the variables are cointegrated, auto should keep the ECM.
  expect_identical(extract_ecm_selected(model, "Y"), "ecm")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$selected, "ecm")
  expect_identical(decision$model_form, "ecm")

  # The level-block diagnostic should support the ECM interpretation.
  expect_true(isTRUE(decision$coint_test$decision))
  expect_true(decision$coint_test$alpha_hat < 0)
  expect_true(decision$coint_test$p.value < 0.05)
})


test_that("ECM auto uses differenced model when I(1) variables are not cointegrated", {
  sim_data <- simulate_non_cointegrated_case()

  # sim_data %>% ggplot2::ggplot(ggplot2::aes(x = time)) +
  #   ggplot2::geom_line(ggplot2::aes(y = Y, color = "Y")) +
  #   ggplot2::geom_line(ggplot2::aes(y = X, color = "X")) +
  #   ggplot2::labs(title = "Simulated non-cointegrated data") +
  #   ggplot2::scale_color_manual(values = c("blue", "red")) +
  #   ggplot2::theme_minimal()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "auto",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # Since there is no stable long-run relation, auto should estimate the
  # corresponding first-differenced model.
  expect_identical(extract_ecm_selected(model, "Y"), "fully_differenced")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$selected, "fully_differenced")
  expect_identical(decision$model_form, "diff")
  expect_false(isTRUE(decision$coint_test$decision))
})


test_that("ECM auto does not keep ECM when a variable is I(2) or uncertain", {
  sim_data <- simulate_i2_or_uncertain_case()

  sim_data %>% ggplot2::ggplot(ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = Y, color = "Y")) +
    ggplot2::geom_line(ggplot2::aes(y = X, color = "X")) +
    ggplot2::labs(title = "Simulated I(2) data") +
    ggplot2::scale_color_manual(values = c("blue", "red")) +
    ggplot2::theme_minimal()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "auto",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # Standard single-equation ECM should not be retained when a variable is I(2)
  # or the integration order is too uncertain.
  expect_identical(extract_ecm_selected(model, "Y"), "fully_differenced")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$model_form, "diff")
  expect_true(any(decision$integration$order %in% c("I2_or_uncertain", "uncertain")))
})


test_that("ECM diagnostic mode stores diagnostics but does not change the requested ECM", {
  sim_data <- simulate_non_cointegrated_case()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "diagnostic",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # Diagnostic mode should not alter the requested model form.
  # The ECM is estimated even though the diagnostic should say that there is no
  # cointegration evidence.
  expect_identical(extract_ecm_selected(model, "Y"), "ecm")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$pretest, "diagnostic")
  expect_identical(decision$selected, "ecm")
  expect_false(isTRUE(decision$coint_test$decision))
})


test_that("ECM none mode preserves old ECM behaviour without diagnostics", {
  sim_data <- simulate_non_cointegrated_case()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  dictionary <- make_ecm_auto_dictionary(c("Y", "X"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "none",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # None mode should be backwards-compatible: estimate the ECM and do not run
  # unit-root or cointegration diagnostics.
  expect_identical(extract_ecm_selected(model, "Y"), "ecm")

  decision <- extract_ecm_decision(model, "Y")

  expect_identical(decision$pretest, "none")
  expect_identical(decision$selected, "ecm")
  expect_null(decision$integration)
  expect_null(decision$coint_test)
})


test_that("ECM auto decisions are stored in opts_df for multiple modules", {
  sim_data <- simulate_mixed_module_case()

  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Yc", "Yn"),
    independent = c("Xc", "Xn"),
    lag = c("", ""),
    cvar = c("", "")
  )

  dictionary <- make_ecm_auto_dictionary(c("Yc", "Xc", "Yn", "Xn"))

  expect_silent(
    model <- run_model(
      specification = specification,
      dictionary = dictionary,
      input = make_ecm_auto_input(sim_data),
      primary_source = "local",
      use_logs = "both",
      ardl_or_ecm = "ecm",
      ecm_pretest = "auto",
      trend = FALSE,
      max.ar = 4,
      max.dl = 2,
      saturation = NULL,
      gets_selection = FALSE,
      present = FALSE,
      plot = FALSE,
      quiet = TRUE
    )
  )

  # The decision metadata should be stored directly in opts_df.
  expect_true("ecm_decision" %in% names(model$opts_df))
  expect_true("ardl_or_ecm_requested" %in% names(model$opts_df))
  expect_true("ardl_or_ecm_selected" %in% names(model$opts_df))

  # The two modules should receive different decisions.
  expect_identical(extract_ecm_selected(model, "Yc"), "ecm")
  expect_identical(extract_ecm_selected(model, "Yn"), "fully_differenced")

  decision_c <- extract_ecm_decision(model, "Yc")
  decision_n <- extract_ecm_decision(model, "Yn")

  expect_true(isTRUE(decision_c$coint_test$decision))
  expect_false(isTRUE(decision_n$coint_test$decision))
})
