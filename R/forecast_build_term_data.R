#' Build forecast regressors for every upstream uncertainty path
#'
#' @keywords internal
forecast_build_draw_term_data <- function(state_data,
                                          deterministic_data,
                                          deterministic_draw_data,
                                          recipe,
                                          n.ahead,
                                          uncertainty_sample) {
  draw_columns <- names(deterministic_draw_data)[
    grepl("\\.all$", names(deterministic_draw_data))
  ]

  if (length(draw_columns) == 0L) {
    return(NULL)
  }

  future_times <- utils::tail(deterministic_data$time, n.ahead)

  extract_run <- function(value, run) {
    if (is.data.frame(value) || is.matrix(value)) {
      values <- as.numeric(value[1, , drop = TRUE])
    } else if (is.list(value)) {
      values <- unlist(value, recursive = TRUE, use.names = FALSE)
    } else {
      values <- as.numeric(value)
    }

    if (length(values) == 0L) {
      return(NA_real_)
    }
    values[[min(run, length(values))]]
  }

  lapply(seq_len(uncertainty_sample), function(run) {
    run_state <- state_data
    run_deterministic <- deterministic_data

    for (draw_column in draw_columns) {
      term <- sub("\\.all$", "", draw_column)
      values <- vapply(
        deterministic_draw_data[[draw_column]],
        extract_run,
        numeric(1),
        run = run
      )

      draw_path <- dplyr::tibble(
        time = deterministic_draw_data$time,
        value = values
      )

      if (!term %in% names(run_state)) {
        run_state[[term]] <- NA_real_
      }
      positions <- match(draw_path$time, run_state$time)
      valid <- !is.na(positions)
      run_state[[term]][positions[valid]] <- draw_path$value[valid]

      if (!term %in% names(run_deterministic)) {
        run_deterministic[[term]] <- NA_real_
      }
      positions <- match(draw_path$time, run_deterministic$time)
      valid <- !is.na(positions)
      run_deterministic[[term]][positions[valid]] <- draw_path$value[valid]
    }

    forecast_build_term_data(
      state_data = run_state,
      deterministic_data = run_deterministic,
      recipe = recipe
    ) %>%
      dplyr::filter(.data$time %in% future_times) %>%
      dplyr::slice_tail(n = n.ahead) %>%
      dplyr::select(-"time")
  })
}
