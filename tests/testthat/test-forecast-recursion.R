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
