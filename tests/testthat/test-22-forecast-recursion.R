test_that("ARDL innovations enter once and propagate only through AR terms", {
  object <- list(aux = list(y = 2))
  recipe <- list(
    response_scale = "level",
    selected_terms = dplyr::tibble(
      term = "ar1",
      role = "response_lag",
      lag = 1L,
      coefficient = 0.5
    )
  )

  result <- osem:::forecast_recursive_isat(
    isat_obj = object,
    recipe = recipe,
    central_terms = dplyr::tibble(.rows = 3),
    level_history = 2,
    residual_draws = matrix(c(1, 0, 0), ncol = 1)
  )

  expect_equal(result$central_level, c(1, 0.5, 0.25))
  expect_equal(as.numeric(result$draw_level), c(2, 1, 0.5))
})



# -----------------------------------------------------------------------------
# Tiny illustration of uncertainty propagation in dynamic forecasts
# -----------------------------------------------------------------------------

n.ahead <- 5

# Module A: A[t] = 40 + 0.6 * A[t - 1] + error_A[t]
# The long-run value is 100, so the central forecast remains easy to read.
A_initial <- 100
A_intercept <- 40
A_ar <- 0.6
error_A <- c(2, -1, 1.5, 0, -0.5)

# Module B depends contemporaneously on A and dynamically on its own history:
# B[t] = 10 + 0.5 * A[t] + 0.3 * B[t - 1] + error_B[t]
B_initial <- 80
B_intercept <- 10
B_A <- 0.5
B_ar <- 0.3
error_B <- c(-1, 2, 0, -1.5, 1)


# Small recursive helpers -----------------------------------------------------

forecast_A <- function(errors) {
  out <- numeric(n.ahead)

  for (h in seq_len(n.ahead)) {
    previous <- if (h == 1) A_initial else out[h - 1]
    out[h] <- A_intercept + A_ar * previous + errors[h]
  }

  return(out)
}

forecast_B <- function(A_path, errors) {
  out <- numeric(n.ahead)

  for (h in seq_len(n.ahead)) {
    previous <- if (h == 1) B_initial else out[h - 1]
    out[h] <- B_intercept + B_A * A_path[h] + B_ar * previous + errors[h]
  }

  return(out)
}

#
# # 1. Central dynamic forecast: no future innovations -------------------------
#
# A_central <- forecast_A(rep(0, n.ahead))
# B_central <- forecast_B(A_central, rep(0, n.ahead))
#
#
# # 2. Non-cumulative post-processing ------------------------------------------
# # Innovations are added to each predicted level after prediction. They do not
# # feed into the next period's AR term, so dynamic propagation is incomplete.
#
# A_non_cumulative <- A_central + error_A
# B_given_A_non_cumulative <- forecast_B(
#   A_non_cumulative,
#   rep(0, n.ahead)
# )
# B_non_cumulative <- B_given_A_non_cumulative + error_B
#
#
# # 3. Current OSEM approximation ----------------------------------------------
# # Innovations are cumulatively added after prediction. Earlier innovations
# # therefore persist with coefficient 1, rather than through the estimated AR
# # coefficient. The same run is nevertheless passed from A into B.
#
# A_current_osem <- A_central + cumsum(error_A)
# B_given_A_current_osem <- forecast_B(
#   A_current_osem,
#   rep(0, n.ahead)
# )
# B_current_osem <- B_given_A_current_osem + cumsum(error_B)
#
#
# # 4. Ideal recursive simulation ----------------------------------------------
# # Innovations enter inside the recursion. Their later effects are propagated
# # through the estimated AR coefficients. The same simulated path from A is
# # used by B, so uncertainty is also propagated coherently across modules.
#
# A_ideal <- forecast_A(error_A)
# B_ideal <- forecast_B(A_ideal, error_B)
#
#
# # Compare the alternatives ---------------------------------------------------
#
# comparison <- data.frame(
#   horizon = seq_len(n.ahead),
#   A_central = A_central,
#   A_non_cumulative = A_non_cumulative,
#   A_current_osem = A_current_osem,
#   A_ideal = A_ideal,
#   B_central = B_central,
#   B_non_cumulative = B_non_cumulative,
#   B_current_osem = B_current_osem,
#   B_ideal = B_ideal
# )
#
# print(round(comparison, 3))
#
# # The approaches coincide only in special cases. In particular, cumulative
# # residuals are exact for a random walk (AR coefficient = 1), but not for a
# # general dynamic ARDL equation.





test_that("ECM changes update the level state without adding innovations twice", {
  object <- list(aux = list(y = -2))
  recipe <- list(
    response_scale = "difference",
    selected_terms = dplyr::tibble(
      term = "L1.y",
      role = "dependent_level",
      lag = 1L,
      coefficient = -0.2
    )
  )

  result <- osem:::forecast_recursive_isat(
    isat_obj = object,
    recipe = recipe,
    central_terms = dplyr::tibble(.rows = 2),
    level_history = 10,
    residual_draws = matrix(c(1, 0), ncol = 1)
  )

  expect_equal(result$central_response, c(-2, -1.6))
  expect_equal(result$central_level, c(8, 6.4))
  expect_equal(as.numeric(result$draw_level), c(9, 7.2))
})

test_that("stored transformations are applied consistently to forecast terms", {
  recipe <- list(
    dependent = "y",
    transformations = c(y = "none", x = "asinh"),
    selected_terms = dplyr::tibble(
      term = c("D.ln.x", "L1.D.ln.x"),
      role = c("regressor_difference", "regressor_difference"),
      source = c("x", "x"),
      lag = c(0L, 1L)
    )
  )
  state <- dplyr::tibble(
    time = 1:4,
    y = 1:4,
    ln.x = asinh(c(1, 2, -1, -2))
  )

  terms <- osem:::forecast_build_term_data(
    state_data = state,
    deterministic_data = dplyr::tibble(time = 1:4),
    recipe = recipe
  )

  expected_difference <- c(NA, diff(state$ln.x))
  expect_equal(terms$D.ln.x, expected_difference)
  expect_equal(terms$L1.D.ln.x, dplyr::lag(expected_difference))
})

test_that("OSEM transformations round trip", {
  values <- c(-2, 0, 3)
  expect_equal(
    osem:::inverse_transform_osem_values(
      osem:::transform_osem_values(values, "asinh"),
      "asinh"
    ),
    values
  )

  positive <- c(0.5, 2, 8)
  expect_equal(
    osem:::inverse_transform_osem_values(
      osem:::transform_osem_values(positive, "log"),
      "log"
    ),
    positive
  )
})
