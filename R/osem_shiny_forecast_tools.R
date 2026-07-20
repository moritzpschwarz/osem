# OSEM Shiny forecast and scenario helpers ----------------------------------

osem_shiny_parse_ci_levels <- function(value) {
  validation <- osem_shiny_validate_ci_levels_text(value)
  if (isTRUE(validation$valid)) validation$levels else numeric()
}

osem_shiny_validate_ci_levels_text <- function(value) {
  text <- paste(as.character(value %||% ""), collapse = ",")
  tokens <- trimws(unlist(strsplit(text, "[,;[:space:]]+", perl = TRUE)))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0L) {
    return(list(
      valid = FALSE,
      levels = numeric(),
      message = "Enter at least one confidence level between zero and one."
    ))
  }

  levels <- suppressWarnings(as.numeric(tokens))
  invalid_numeric <- !is.finite(levels)
  invalid_range <- !invalid_numeric & (levels <= 0 | levels >= 1)
  if (any(invalid_numeric)) {
    return(list(
      valid = FALSE,
      levels = numeric(),
      message = paste0(
        "Confidence levels must be numeric. Invalid value(s): ",
        paste(unique(tokens[invalid_numeric]), collapse = ", "), "."
      )
    ))
  }
  if (any(invalid_range)) {
    return(list(
      valid = FALSE,
      levels = numeric(),
      message = paste0(
        "Every confidence level must be strictly between zero and one. Invalid value(s): ",
        paste(unique(tokens[invalid_range]), collapse = ", "), "."
      )
    ))
  }

  list(
    valid = TRUE,
    levels = sort(unique(levels)),
    message = NULL
  )
}

osem_shiny_regex_escape <- function(value) {
  value <- as.character(value)
  special <- c("\\", ".", "^", "$", "|", "(", ")", "[", "]", "{", "}", "*", "+", "?")
  vapply(value, function(one) {
    characters <- strsplit(one, "", fixed = TRUE)[[1L]]
    paste0(ifelse(characters %in% special, "\\", ""), characters, collapse = "")
  }, character(1L), USE.NAMES = FALSE)
}

osem_shiny_default_forecast_args <- function() {
  list(
    n.ahead = 10L,
    ci.levels = c(0.50, 0.66, 0.95),
    exog_fill_method = "AR",
    ar.fill.max = 4L,
    uncertainty_sample = 100L,
    assumption_mode = "automatic",
    quiet = FALSE,
    seed = 123L
  )
}

osem_shiny_normalise_forecast_args <- function(args = NULL) {
  defaults <- osem_shiny_default_forecast_args()
  if (is.null(args) || !is.list(args)) args <- list()
  out <- defaults
  common <- intersect(names(args), names(defaults))
  out[common] <- args[common]

  out$n.ahead <- osem_shiny_scalar_integer(out$n.ahead, defaults$n.ahead, 1L, 500L)
  levels <- suppressWarnings(as.numeric(out$ci.levels %||% defaults$ci.levels))
  levels <- sort(unique(levels[is.finite(levels) & levels > 0 & levels < 1]))
  if (length(levels) == 0L) levels <- defaults$ci.levels
  out$ci.levels <- levels

  out$exog_fill_method <- osem_shiny_scalar_character(
    out$exog_fill_method, defaults$exog_fill_method
  )
  if (!out$exog_fill_method %in% c("AR", "auto", "ets", "last")) {
    out$exog_fill_method <- defaults$exog_fill_method
  }
  out$ar.fill.max <- osem_shiny_scalar_integer(
    out$ar.fill.max, defaults$ar.fill.max, 1L, 40L
  )
  out$uncertainty_sample <- osem_shiny_scalar_integer(
    out$uncertainty_sample, defaults$uncertainty_sample, 1L, 100000L
  )
  out$assumption_mode <- osem_shiny_scalar_character(
    out$assumption_mode, defaults$assumption_mode
  )
  if (!out$assumption_mode %in% c("automatic", "manual")) {
    out$assumption_mode <- defaults$assumption_mode
  }
  out$quiet <- osem_shiny_scalar_logical(out$quiet, defaults$quiet)
  out$seed <- osem_shiny_scalar_integer(out$seed, defaults$seed, 1L, .Machine$integer.max)
  out
}

osem_shiny_default_forecast_scenario <- function(id,
                                                 name = "Baseline",
                                                 description = "") {
  list(
    id = as.character(id),
    name = as.character(name),
    description = as.character(description),
    args = osem_shiny_default_forecast_args(),
    exog_predictions = NULL,
    result = NULL,
    status = "unavailable",
    stale_reason = NULL,
    log = character(),
    messages = character(),
    warnings = character(),
    error = NULL,
    metadata = list(),
    created_at = Sys.time(),
    updated_at = Sys.time()
  )
}

osem_shiny_as_posixct_scalar <- function(value, default = Sys.time()) {
  fallback <- tryCatch(
    as.POSIXct(default, tz = "UTC"),
    error = function(e) as.POSIXct(Sys.time(), tz = "UTC")
  )
  if (is.null(value) || length(value) == 0L) return(fallback)
  parsed <- suppressWarnings(tryCatch({
    if (inherits(value, "POSIXt")) {
      as.POSIXct(value[[1L]], tz = "UTC")
    } else if (inherits(value, "Date")) {
      as.POSIXct(value[[1L]], tz = "UTC")
    } else if (is.numeric(value)) {
      as.POSIXct(value[[1L]], origin = "1970-01-01", tz = "UTC")
    } else {
      as.POSIXct(as.character(value[[1L]]), tz = "UTC")
    }
  }, error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")))
  if (length(parsed) != 1L || is.na(parsed)) fallback else parsed
}

osem_shiny_normalise_forecast_scenario <- function(scenario) {
  if (!is.list(scenario)) scenario <- list()
  id <- trimws(osem_shiny_scalar_character(scenario$id, "scenario-0001"))
  if (!nzchar(id)) id <- "scenario-0001"
  name <- trimws(osem_shiny_scalar_character(scenario$name, "Scenario"))
  if (!nzchar(name)) name <- "Scenario"
  status <- osem_shiny_scalar_character(scenario$status, "unavailable")
  if (!status %in% c("unavailable", "running", "current", "stale", "failed")) {
    status <- if (is.null(scenario$result)) "unavailable" else "current"
  }
  list(
    id = id,
    name = name,
    description = paste(as.character(scenario$description %||% ""), collapse = "\n"),
    args = osem_shiny_normalise_forecast_args(scenario$args),
    exog_predictions = scenario$exog_predictions %||% NULL,
    result = scenario$result %||% NULL,
    status = status,
    stale_reason = scenario$stale_reason %||% NULL,
    log = as.character(scenario$log %||% character()),
    messages = as.character(scenario$messages %||% character()),
    warnings = as.character(scenario$warnings %||% character()),
    error = scenario$error %||% NULL,
    metadata = if (is.list(scenario$metadata)) scenario$metadata else list(),
    created_at = osem_shiny_as_posixct_scalar(scenario$created_at, Sys.time()),
    updated_at = osem_shiny_as_posixct_scalar(scenario$updated_at, Sys.time())
  )
}

osem_shiny_get_active_scenario <- function(state) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) == 0L) return(NULL)
  ids <- vapply(scenarios, function(x) osem_shiny_normalise_forecast_scenario(x)$id, character(1L))
  index <- match(state$active_scenario_id, ids)
  if (is.na(index)) index <- 1L
  osem_shiny_normalise_forecast_scenario(scenarios[[index]])
}

osem_shiny_set_scenario <- function(state, scenario) {
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  scenarios <- state$forecast_scenarios
  ids <- if (length(scenarios) == 0L) character() else {
    vapply(scenarios, function(x) osem_shiny_normalise_forecast_scenario(x)$id, character(1L))
  }
  index <- match(scenario$id, ids)
  if (is.na(index)) scenarios <- c(scenarios, list(scenario)) else scenarios[[index]] <- scenario
  state$forecast_scenarios <- scenarios
  invisible(scenario)
}

osem_shiny_remove_scenario <- function(state, scenario_id) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) <= 1L) return(FALSE)
  ids <- vapply(scenarios, function(x) osem_shiny_normalise_forecast_scenario(x)$id, character(1L))
  keep <- ids != scenario_id
  if (all(keep)) return(FALSE)
  state$forecast_scenarios <- scenarios[keep]
  if (identical(state$active_scenario_id, scenario_id)) {
    state$active_scenario_id <- osem_shiny_normalise_forecast_scenario(state$forecast_scenarios[[1L]])$id
  }
  osem_shiny_sync_active_forecast(state)
  TRUE
}

osem_shiny_sync_active_forecast <- function(state) {
  active <- osem_shiny_get_active_scenario(state)
  if (is.null(active)) {
    state$forecast <- NULL
    state$forecast_status <- "unavailable"
  } else {
    state$forecast <- active$result
    state$forecast_status <- active$status
  }
  invisible(state)
}

osem_shiny_scenario_choices <- function(state) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) == 0L) return(character())
  ids <- vapply(scenarios, function(x) osem_shiny_normalise_forecast_scenario(x)$id, character(1L))
  labels <- vapply(scenarios, function(x) {
    x <- osem_shiny_normalise_forecast_scenario(x)
    paste0(x$name, " [", x$status, "]")
  }, character(1L))
  stats::setNames(ids, labels)
}

osem_shiny_scenario_table <- function(state) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) == 0L) return(data.frame())
  rows <- lapply(scenarios, function(x) {
    x <- osem_shiny_normalise_forecast_scenario(x)
    data.frame(
      id = x$id,
      Scenario = x$name,
      Status = tools::toTitleCase(x$status),
      Horizon = x$args$n.ahead,
      Assumptions = if (identical(x$args$assumption_mode, "manual")) {
        "User-specified path"
      } else {
        paste0("Automatic: ", x$args$exog_fill_method)
      },
      `Uncertainty draws` = x$args$uncertainty_sample,
      Updated = format(x$updated_at, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

osem_shiny_forecast_exogenous_variables <- function(model) {
  if (is.null(model) || !inherits(model, "osem")) return(character())
  specification <- osem_shiny_safe_data_frame(model$module_order)
  if (nrow(specification) == 0L) return(character())
  classification <- tryCatch(
    classify_variables(specification = specification),
    error = function(e) NULL
  )
  if (!is.data.frame(classification) || !all(c("class", "var") %in% names(classification))) {
    return(character())
  }
  variables <- trimws(as.character(classification$var))
  keep <- !is.na(classification$class) & classification$class == "x" &
    !is.na(variables) & nzchar(variables)
  sort(unique(variables[keep]))
}

osem_shiny_model_frequency <- function(model) {
  if (is.null(model) || !inherits(model, "osem")) {
    return(list(label = "Unknown", by = NA_character_, supported = FALSE, periods_per_year = NA_integer_))
  }
  full_data <- osem_shiny_safe_data_frame(model$full_data)
  if (!"time" %in% names(full_data)) {
    return(list(label = "Unknown", by = NA_character_, supported = FALSE, periods_per_year = NA_integer_))
  }
  dates <- suppressWarnings(tryCatch(
    sort(unique(osem_shiny_parse_time(full_data$time))),
    error = function(e) as.Date(character())
  ))
  dates <- dates[!is.na(dates)]
  if (length(dates) < 2L) {
    return(list(label = "Unknown", by = NA_character_, supported = FALSE, periods_per_year = NA_integer_))
  }
  differences <- as.numeric(diff(dates))
  differences <- differences[is.finite(differences) & differences > 0]
  median_difference <- if (length(differences) == 0L) NA_real_ else stats::median(differences)
  if (is.finite(median_difference) && median_difference >= 350 && median_difference <= 380) {
    list(label = "Annual", by = "year", supported = TRUE, periods_per_year = 1L)
  } else if (is.finite(median_difference) && median_difference >= 80 && median_difference <= 100) {
    list(label = "Quarterly", by = "3 months", supported = TRUE, periods_per_year = 4L)
  } else if (is.finite(median_difference) && median_difference >= 25 && median_difference <= 32) {
    list(label = "Monthly", by = "month", supported = FALSE, periods_per_year = 12L)
  } else if (is.finite(median_difference) && median_difference <= 2) {
    list(label = "Daily", by = "day", supported = FALSE, periods_per_year = 365L)
  } else {
    list(
      label = if (is.finite(median_difference)) paste0("Approximately every ", round(median_difference), " days") else "Unknown",
      by = NA_character_,
      supported = FALSE,
      periods_per_year = NA_integer_
    )
  }
}

osem_shiny_as_date_scalar <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    stop("A non-missing date is required.", call. = FALSE)
  }
  parsed <- suppressWarnings(tryCatch({
    if (inherits(value, "Date")) {
      as.Date(value[[1L]])
    } else if (inherits(value, "POSIXt")) {
      as.Date(value[[1L]])
    } else if (is.numeric(value)) {
      as.Date(value[[1L]], origin = "1970-01-01")
    } else {
      as.Date(as.character(value[[1L]]))
    }
  }, error = function(e) as.Date(NA_character_)))
  if (length(parsed) != 1L || is.na(parsed)) {
    stop("The supplied date could not be interpreted.", call. = FALSE)
  }
  parsed
}

osem_shiny_next_date <- function(date, by) {
  seq.Date(osem_shiny_as_date_scalar(date), by = by, length.out = 2L)[[2L]]
}

osem_shiny_forecast_date_requirements <- function(model, n.ahead = 10L) {
  if (is.null(model) || !inherits(model, "osem")) stop("A fitted OSEM model is required.", call. = FALSE)
  args <- osem_shiny_normalise_forecast_args(list(n.ahead = n.ahead))
  frequency <- osem_shiny_model_frequency(model)
  if (!isTRUE(frequency$supported) || is.na(frequency$by)) {
    stop(
      paste0(
        "The model frequency is ", frequency$label,
        ". The existing forecast_model() implementation supports annual and quarterly forecasts only."
      ),
      call. = FALSE
    )
  }
  data <- osem_shiny_safe_data_frame(model$full_data)
  if (!all(c("time", "na_item", "values") %in% names(data))) {
    stop("The fitted model does not contain the expected long-format data columns.", call. = FALSE)
  }
  data$time <- tryCatch(
    osem_shiny_parse_time(data$time),
    error = function(e) as.Date(rep(NA_character_, nrow(data)))
  )
  data$values <- suppressWarnings(as.numeric(as.character(data$values)))
  data <- data[!is.na(data$time) & is.finite(data$values), , drop = FALSE]
  if (nrow(data) == 0L) stop("The fitted model contains no usable dated observations.", call. = FALSE)
  overall_max <- max(data$time)
  forecast_dates <- seq.Date(overall_max, by = frequency$by, length.out = args$n.ahead + 1L)[-1L]
  variables <- osem_shiny_forecast_exogenous_variables(model)
  last_dates <- stats::setNames(rep(overall_max, length(variables)), variables)
  for (variable in variables) {
    rows <- data[as.character(data$na_item) == variable, , drop = FALSE]
    if (nrow(rows) > 0L) last_dates[[variable]] <- max(rows$time)
  }
  start_date <- if (length(last_dates) > 0L) {
    next_dates <- as.Date(vapply(
      last_dates,
      function(date) as.character(osem_shiny_next_date(date, frequency$by)),
      character(1L)
    ))
    min(next_dates)
  } else {
    forecast_dates[[1L]]
  }
  required_dates <- seq.Date(start_date, max(forecast_dates), by = frequency$by)
  list(
    frequency = frequency,
    overall_max = overall_max,
    forecast_dates = forecast_dates,
    required_dates = required_dates,
    exogenous_variables = variables,
    last_dates = last_dates
  )
}

osem_shiny_last_observation <- function(model, variable, date = NULL) {
  if (is.null(model) || !inherits(model, "osem")) return(NA_real_)
  data <- osem_shiny_safe_data_frame(model$full_data)
  if (!all(c("time", "na_item", "values") %in% names(data))) return(NA_real_)
  data$time <- tryCatch(
    osem_shiny_parse_time(data$time),
    error = function(e) as.Date(rep(NA_character_, nrow(data)))
  )
  data$values <- suppressWarnings(as.numeric(as.character(data$values)))
  keep <- as.character(data$na_item) == variable & is.finite(data$values) & !is.na(data$time)
  if (!is.null(date)) keep <- keep & data$time <= osem_shiny_as_date_scalar(date)
  data <- data[keep, c("time", "values"), drop = FALSE]
  if (nrow(data) == 0L) return(NA_real_)
  data <- data[order(data$time), , drop = FALSE]
  as.numeric(utils::tail(data$values, 1L))
}

osem_shiny_forecast_template <- function(model,
                                         n.ahead = 10L,
                                         fill = c("last", "blank")) {
  fill <- match.arg(fill)
  requirements <- osem_shiny_forecast_date_requirements(model, n.ahead)
  dates <- requirements$required_dates
  variables <- requirements$exogenous_variables
  out <- data.frame(time = dates, stringsAsFactors = FALSE, check.names = FALSE)
  for (variable in variables) {
    values <- vapply(dates, function(date) {
      if (identical(fill, "blank")) NA_real_ else osem_shiny_last_observation(model, variable, date)
    }, numeric(1L))
    out[[variable]] <- values
  }
  osem_shiny_add_quarter_dummies(out)
}

osem_shiny_add_quarter_dummies <- function(data) {
  if (!is.data.frame(data) || !"time" %in% names(data)) return(data)
  dates <- as.Date(data$time)
  month <- suppressWarnings(as.integer(format(dates, "%m")))
  quarter <- ((month - 1L) %/% 3L) + 1L
  for (q in 1:4) data[[paste0("q_", q)]] <- as.integer(quarter == q)
  data
}

osem_shiny_normalise_exog_predictions <- function(data, model = NULL) {
  if (is.null(data)) return(NULL)
  if (!is.data.frame(data)) stop("Forecast assumptions must be a data frame or tibble.", call. = FALSE)
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  if (all(c("na_item", "time", "values") %in% names(data))) {
    data$time <- osem_shiny_parse_time(data$time)
    duplicate <- duplicated(data[, c("na_item", "time"), drop = FALSE])
    if (any(duplicate)) stop("The long-format assumptions contain duplicate variable/date rows.", call. = FALSE)
    data$values <- suppressWarnings(as.numeric(data$values))
    data <- tidyr::pivot_wider(data, id_cols = "time", names_from = "na_item", values_from = "values")
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!"time" %in% names(data)) stop("Forecast assumptions require a 'time' column.", call. = FALSE)
  data$time <- osem_shiny_parse_time(data$time)
  if (any(is.na(data$time))) stop("One or more forecast dates could not be interpreted.", call. = FALSE)
  if (anyDuplicated(data$time)) stop("Forecast assumption dates must be unique.", call. = FALSE)
  data <- data[order(data$time), , drop = FALSE]

  value_columns <- setdiff(names(data), c("time", "q_1", "q_2", "q_3", "q_4"))
  for (name in value_columns) {
    original <- data[[name]]
    character_values <- as.character(original)
    numeric <- suppressWarnings(as.numeric(character_values))
    invalid <- !is.na(original) & trimws(character_values) != "" & is.na(numeric)
    if (any(invalid)) stop(paste0("Forecast values for '", name, "' must be numeric."), call. = FALSE)
    data[[name]] <- numeric
  }
  data <- osem_shiny_add_quarter_dummies(data)
  variables <- if (!is.null(model)) osem_shiny_forecast_exogenous_variables(model) else value_columns
  keep <- unique(c("time", variables, "q_1", "q_2", "q_3", "q_4"))
  keep <- keep[keep %in% names(data)]
  data[, keep, drop = FALSE]
}

osem_shiny_validate_forecast_inputs <- function(model, scenario) {
  issues <- osem_shiny_empty_issues()
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  args <- scenario$args
  if (is.null(model) || !inherits(model, "osem")) {
    issues <- osem_shiny_add_issue(issues, "error", "Forecast", "A fitted OSEM model is required.")
    return(list(valid = FALSE, issues = issues, scenario = scenario, exog_predictions = NULL))
  }

  requirements <- tryCatch(
    osem_shiny_forecast_date_requirements(model, args$n.ahead),
    error = function(e) e
  )
  if (inherits(requirements, "error")) {
    issues <- osem_shiny_add_issue(issues, "error", "Forecast", conditionMessage(requirements))
    requirements <- NULL
  }
  if (identical(model$args$ardl_or_ecm %||% "ardl", "ecm")) {
    issues <- osem_shiny_add_issue(
      issues, "warning", "Forecast",
      "This model was requested as an ECM. The app calls the current forecast_model() implementation unchanged, so its present ECM limitations also apply here."
    )
  }
  if (identical(args$assumption_mode, "automatic") && args$exog_fill_method %in% c("auto", "ets") &&
      !requireNamespace("forecast", quietly = TRUE)) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Environment",
      paste0("The 'forecast' package is required for the ", args$exog_fill_method, " fill method.")
    )
  }

  predictions <- NULL
  if (identical(args$assumption_mode, "manual")) {
    predictions <- tryCatch(
      osem_shiny_normalise_exog_predictions(scenario$exog_predictions, model = model),
      error = function(e) e
    )
    if (inherits(predictions, "error")) {
      issues <- osem_shiny_add_issue(issues, "error", "Forecast assumptions", conditionMessage(predictions))
      predictions <- NULL
    } else if (is.null(predictions)) {
      issues <- osem_shiny_add_issue(
        issues, "error", "Forecast assumptions",
        "Manual assumption mode is selected, but no assumption table is available."
      )
    } else if (!is.null(requirements)) {
      variables <- requirements$exogenous_variables
      missing_variables <- setdiff(variables, names(predictions))
      if (length(missing_variables) > 0L) {
        issues <- osem_shiny_add_issue(
          issues, "error", "Forecast assumptions",
          paste0("Missing exogenous variable column(s): ", paste(missing_variables, collapse = ", "), ".")
        )
      }
      missing_dates <- requirements$required_dates[
        !requirements$required_dates %in% predictions$time
      ]
      if (length(missing_dates) > 0L) {
        issues <- osem_shiny_add_issue(
          issues, "error", "Forecast assumptions",
          paste0(
            "The table does not cover ", length(missing_dates), " required date(s), from ",
            format(min(missing_dates)), " to ", format(max(missing_dates)), "."
          )
        )
      }
      if (length(variables) > 0L && all(variables %in% names(predictions))) {
        required_rows <- predictions$time %in% requirements$required_dates
        missing_values <- sum(is.na(predictions[required_rows, variables, drop = FALSE]))
        if (missing_values > 0L) {
          issues <- osem_shiny_add_issue(
            issues, "error", "Forecast assumptions",
            paste0("The required assumption range contains ", missing_values, " missing value(s).")
          )
        }
      }
      if (nrow(predictions) > length(requirements$required_dates)) {
        issues <- osem_shiny_add_issue(
          issues, "info", "Forecast assumptions",
          "Dates outside the required nowcast/forecast range will be retained but are not needed by this run."
        )
      }
    }
  } else {
    issues <- osem_shiny_add_issue(
      issues, "info", "Forecast assumptions",
      paste0("Exogenous paths will be generated with the '", args$exog_fill_method, "' method.")
    )
  }

  if (!any(issues$level == "error")) {
    issues <- osem_shiny_add_issue(
      issues, "success", "Forecast",
      paste0("The scenario is ready for a ", args$n.ahead, "-period forecast.")
    )
  }
  list(
    valid = !any(issues$level == "error"),
    issues = issues,
    scenario = scenario,
    exog_predictions = predictions,
    requirements = requirements
  )
}

osem_shiny_build_forecast_call_args <- function(model, scenario, validation = NULL) {
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  if (is.null(validation)) validation <- osem_shiny_validate_forecast_inputs(model, scenario)
  args <- scenario$args
  manual <- identical(args$assumption_mode, "manual")
  list(
    model = model,
    exog_predictions = if (manual) validation$exog_predictions else NULL,
    n.ahead = args$n.ahead,
    ci.levels = args$ci.levels,
    exog_fill_method = if (manual) NULL else args$exog_fill_method,
    ar.fill.max = args$ar.fill.max,
    plot = FALSE,
    uncertainty_sample = args$uncertainty_sample,
    quiet = args$quiet
  )
}

osem_shiny_execute_forecast <- function(model, scenario) {
  validation <- osem_shiny_validate_forecast_inputs(model, scenario)
  if (!isTRUE(validation$valid)) {
    errors <- validation$issues$message[validation$issues$level == "error"]
    return(list(
      ok = FALSE,
      value = NULL,
      error = paste(errors, collapse = "\n"),
      warnings = character(),
      messages = character(),
      log = character(),
      started = Sys.time(),
      finished = Sys.time(),
      duration_seconds = 0,
      call_args = NULL,
      validation = validation
    ))
  }
  call_args <- osem_shiny_build_forecast_call_args(model, validation$scenario, validation)
  captured <- osem_shiny_capture_conditions(
    function() do.call(forecast_model, call_args),
    seed = validation$scenario$args$seed
  )
  captured$call_args <- call_args
  captured$validation <- validation
  captured
}

osem_shiny_forecast_plot_data <- function(forecast) {
  if (is.null(forecast) || !inherits(forecast, "osem.forecast")) return(data.frame())
  out <- tryCatch(
    plot(forecast, exclude.exogenous = FALSE, return.data = TRUE),
    error = function(e) NULL
  )
  if (!is.data.frame(out)) return(data.frame())
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  if ("time" %in% names(out)) out$time <- as.Date(out$time)
  out
}

osem_shiny_forecast_central_data <- function(forecast) {
  plot_data <- osem_shiny_forecast_plot_data(forecast)
  if (nrow(plot_data) > 0L && all(c("time", "na_item", "values", "type") %in% names(plot_data))) {
    type <- as.character(plot_data$type)
    keep <- type %in% c("Forecast", "Endogenous Forecast", "forecast")
    out <- plot_data[keep & !is.na(plot_data$values), c("time", "na_item", "values"), drop = FALSE]
    if (nrow(out) > 0L) {
      out$time <- as.Date(out$time)
      out <- unique(out)
      rownames(out) <- NULL
      return(out)
    }
  }
  if (!is.data.frame(forecast$forecast)) return(data.frame())
  pieces <- vector("list", nrow(forecast$forecast))
  for (i in seq_len(nrow(forecast$forecast))) {
    dep <- as.character(forecast$forecast$dep_var[[i]])
    central <- forecast$forecast$central.estimate[[i]]
    if (!is.data.frame(central) || !"time" %in% names(central)) next
    value_columns <- setdiff(names(central), "time")
    if (length(value_columns) == 0L) next
    selected <- value_columns[[1L]]
    stripped <- gsub("^(ln\\.|D\\.|D\\.ln\\.|ln\\.D\\.)", "", value_columns)
    matching <- which(stripped == dep)
    if (length(matching) > 0L) selected <- value_columns[matching[[1L]]]
    pieces[[i]] <- data.frame(
      time = as.Date(central$time),
      na_item = dep,
      values = as.numeric(central[[selected]]),
      stringsAsFactors = FALSE
    )
  }
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0L) data.frame() else {
    out <- do.call(rbind, pieces)
    rownames(out) <- NULL
    out
  }
}

osem_shiny_forecast_table <- function(forecast, variables = NULL) {
  data <- osem_shiny_forecast_plot_data(forecast)
  if (nrow(data) == 0L) {
    central <- osem_shiny_forecast_central_data(forecast)
    if (nrow(central) == 0L) return(data.frame())
    central$type <- "Forecast"
    return(central)
  }
  if (!is.null(variables)) data <- data[as.character(data$na_item) %in% variables, , drop = FALSE]
  type <- as.character(data$type)
  keep <- type %in% c("Forecast", "Endogenous Forecast", "Nowcast", "Exogenous Forecast")
  data <- data[keep, , drop = FALSE]
  data <- data[order(data$na_item, data$time, data$type), , drop = FALSE]
  rownames(data) <- NULL
  data
}

osem_shiny_forecast_variable_choices <- function(forecast, include_exogenous = TRUE) {
  data <- osem_shiny_forecast_plot_data(forecast)
  if (nrow(data) == 0L || !"na_item" %in% names(data)) return(character())
  if (!isTRUE(include_exogenous) && "type" %in% names(data)) {
    data <- data[as.character(data$type) != "Exogenous Forecast", , drop = FALSE]
  }
  variables <- sort(unique(as.character(data$na_item)))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  stats::setNames(variables, variables)
}

osem_shiny_forecast_assumptions <- function(forecast) {
  if (is.null(forecast) || !inherits(forecast, "osem.forecast")) return(data.frame())
  data <- forecast$exog_data_nowcast %||% forecast$exog_data
  if (!is.data.frame(data)) return(data.frame())
  as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
}

osem_shiny_scenario_results_long <- function(state, include_stale = TRUE) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) == 0L) return(data.frame())
  pieces <- lapply(scenarios, function(scenario) {
    scenario <- osem_shiny_normalise_forecast_scenario(scenario)
    if (is.null(scenario$result)) return(NULL)
    if (!include_stale && !identical(scenario$status, "current")) return(NULL)
    data <- osem_shiny_forecast_central_data(scenario$result)
    if (nrow(data) == 0L) return(NULL)
    data$scenario_id <- scenario$id
    data$scenario <- scenario$name
    data$status <- scenario$status
    data
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0L) return(data.frame())
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

osem_shiny_scenario_comparison <- function(state, baseline_id = NULL) {
  data <- osem_shiny_scenario_results_long(state, include_stale = TRUE)
  if (nrow(data) == 0L) return(data.frame())
  ids <- unique(data$scenario_id)
  if (is.null(baseline_id) || !baseline_id %in% ids) baseline_id <- ids[[1L]]
  baseline <- data[data$scenario_id == baseline_id, c("time", "na_item", "values"), drop = FALSE]
  names(baseline)[names(baseline) == "values"] <- "baseline_value"
  out <- merge(data, baseline, by = c("time", "na_item"), all.x = TRUE, sort = FALSE)
  out$difference <- out$values - out$baseline_value
  out$percent_difference <- ifelse(
    is.na(out$baseline_value) | out$baseline_value == 0,
    NA_real_,
    100 * out$difference / out$baseline_value
  )
  out <- out[order(out$na_item, out$time, out$scenario), , drop = FALSE]
  rownames(out) <- NULL
  out
}
