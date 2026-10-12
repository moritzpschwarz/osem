#' Recursively forecast a single-equation ISAT module
#'
#' The horizon loop updates the dependent state only. Forecast draws are
#' vectorised across simulation runs, so recursion does not require fitting or
#' predicting the model once per run.
#'
#' @keywords internal
forecast_recursive_isat <- function(isat_obj,
                                    recipe,
                                    central_terms,
                                    draw_terms = NULL,
                                    level_history,
                                    residual_draws) {
  n.ahead <- NROW(central_terms)
  uncertainty_sample <- NCOL(residual_draws)
  specs <- recipe$selected_terms
  coefficients <- stats::setNames(specs$coefficient, specs$term)

  response_history <- as.numeric(isat_obj$aux$y)
  response_history <- response_history[!is.na(response_history)]
  level_history <- as.numeric(level_history)
  level_history <- level_history[!is.na(level_history)]

  if (length(level_history) == 0L) {
    stop("No observed dependent-variable level is available to initialise the forecast.")
  }

  central_response <- numeric(n.ahead)
  central_level <- numeric(n.ahead)
  draw_response <- matrix(NA_real_, nrow = n.ahead, ncol = uncertainty_sample)
  draw_level <- matrix(NA_real_, nrow = n.ahead, ncol = uncertainty_sample)

  lagged_value <- function(history, forecast, horizon, lag) {
    combined <- c(history, forecast[seq_len(max(0L, horizon - 1L))])
    position <- length(combined) - lag + 1L

    if (position < 1L) {
      return(NA_real_)
    } else {
      return(combined[[position]])
    }
  }

  lagged_draw <- function(history, forecast, horizon, lag) {
    history_matrix <- matrix(
      rep(history, uncertainty_sample),
      nrow = length(history),
      ncol = uncertainty_sample
    )
    if (horizon > 1L) {
      prior <- forecast[seq_len(horizon - 1L), , drop = FALSE]
    } else {
      prior <- NULL
    }

    combined <- rbind(history_matrix, prior)
    position <- NROW(combined) - lag + 1L

    if (position < 1L) {
      return(rep(NA_real_, uncertainty_sample))
    } else {
      return(combined[position, ])
    }
  }

  for (horizon in seq_len(n.ahead)) {
    central_eta <- 0
    draw_eta <- rep(0, uncertainty_sample)

    for (row in seq_len(NROW(specs))) {
      spec <- specs[row, ]
      term <- spec$term[[1]]
      role <- spec$role[[1]]
      lag <- spec$lag[[1]]
      coefficient <- coefficients[[term]]

      if (role == "constant") {
        central_value <- 1
        draw_value <- rep(1, uncertainty_sample)
      } else if (role == "response_lag") {
        central_value <- lagged_value(
          response_history, central_response, horizon, lag
        )
        draw_value <- lagged_draw(
          response_history, draw_response, horizon, lag
        )
      } else if (role == "dependent_level") {
        central_value <- lagged_value(
          level_history, central_level, horizon, lag
        )
        draw_value <- lagged_draw(
          level_history, draw_level, horizon, lag
        )
      } else {
        if (!term %in% names(central_terms)) {
          stop("Forecast term '", term, "' is missing from the prepared data.")
        }
        central_value <- central_terms[[term]][[horizon]]
        draw_value <- if (is.null(draw_terms)) {
          rep(central_value, uncertainty_sample)
        } else {
          vapply(
            draw_terms,
            function(path) path[[term]][[horizon]],
            numeric(1)
          )
        }
      }

      central_eta <- central_eta + coefficient * central_value
      draw_eta <- draw_eta + coefficient * draw_value
    }

    central_response[[horizon]] <- central_eta
    draw_response[horizon, ] <- draw_eta + residual_draws[horizon, ]

    if (identical(recipe$response_scale, "difference")) {
      previous_central_level <- if (horizon == 1L) {
        utils::tail(level_history, 1L)
      } else {
        central_level[[horizon - 1L]]
      }
      previous_draw_level <- if (horizon == 1L) {
        rep(utils::tail(level_history, 1L), uncertainty_sample)
      } else {
        draw_level[horizon - 1L, ]
      }
      central_level[[horizon]] <- previous_central_level + central_response[[horizon]]
      draw_level[horizon, ] <- previous_draw_level + draw_response[horizon, ]
    } else {
      central_level[[horizon]] <- central_response[[horizon]]
      draw_level[horizon, ] <- draw_response[horizon, ]
    }
  }

  list(
    central_response = central_response,
    central_level = central_level,
    draw_response = draw_response,
    draw_level = draw_level
  )
}
