#' Build forecast regressors from a stored module recipe
#'
#' @keywords internal
forecast_build_term_data <- function(state_data,
                                     deterministic_data,
                                     recipe) {
  transformed_name <- function(variable) {
    if (recipe$transformations[[variable]] %in% c("log", "asinh")) {
      prefix <- "ln."
    } else {
      prefix <- ""
    }

    return(paste0(prefix, variable))
  }

  out <- state_data %>%
    dplyr::select("time")

  term_specs <- recipe$selected_terms %>%
    dplyr::filter(!.data$role %in% c("constant", "response_lag"))

  for (row in seq_len(NROW(term_specs))) {
    spec <- term_specs[row, ]
    term <- spec$term[[1]]
    role <- spec$role[[1]]

    if (role %in% c("regressor_level", "dependent_level")) {
      source_name <- transformed_name(spec$source[[1]])
      values <- state_data[[source_name]]
      if (spec$lag[[1]] > 0L) {
        values <- dplyr::lag(values, n = spec$lag[[1]])
      }
      out[[term]] <- values
    } else if (role == "regressor_difference") {
      source_name <- transformed_name(spec$source[[1]])
      values <- c(NA_real_, diff(state_data[[source_name]]))
      if (spec$lag[[1]] > 0L) {
        values <- dplyr::lag(values, n = spec$lag[[1]])
      }
      out[[term]] <- values
    } else if (
      term %in% names(deterministic_data)
    ) {
      out <- out %>%
        dplyr::left_join(
          deterministic_data %>%
            dplyr::select("time", dplyr::all_of(term)),
          by = "time"
        )
    } else if (term %in% names(state_data)) {
      out[[term]] <- state_data[[term]]
    } else {
      stop(
        "Could not construct forecast term '",
        term,
        "' for module '",
        recipe$dependent,
        "'."
      )
    }
  }

  out
}
