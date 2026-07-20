# OSEM Shiny state ----------------------------------------------------------

osem_shiny_default_specification <- function() {
  data.frame(
    type = c("d", "n", "n", "n"),
    dependent = c(
      "TOTS",
      "Import",
      "EmiCO2Combustion",
      "EmiCO2Industry"
    ),
    independent = c(
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "HDD + HICP_Gas + HICP_Electricity + GValueAdd",
      "HICP_Gas + HICP_Electricity + GValueAddIndus"
    ),
    lag = rep("", 4L),
    cvar = rep("", 4L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_default_dictionary <- function() {
  osem_shiny_normalise_dictionary(get("dict", envir = asNamespace("osem")))
}

osem_shiny_state_new <- function(session,
                                 initial_model = NULL,
                                 initial_project = NULL) {
  token <- session$token
  if (is.null(token) || !nzchar(token)) {
    token <- paste0(Sys.getpid(), "-", as.integer(stats::runif(1L, 1, 1e9)))
  }
  session_dir <- file.path(tempdir(), paste0("osem-shiny-", token))
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

  default_scenario <- osem_shiny_default_forecast_scenario(
    id = "scenario-0001",
    name = "Baseline"
  )

  state <- shiny::reactiveValues(
    project_name = "Untitled OSEM project",
    project_description = "",
    project_revision = 0L,
    specification = osem_shiny_default_specification(),
    dictionary = osem_shiny_default_dictionary(),
    primary_source = "local",
    input_sources = list(),
    source_sequence = 0L,
    run_args = osem_shiny_default_run_args(),
    model = NULL,
    model_status = "unavailable",
    model_log = character(),
    model_messages = character(),
    model_warnings = character(),
    model_error = NULL,
    model_run_metadata = list(),
    processed_snapshot_path = NULL,
    forecast_scenarios = list(default_scenario),
    active_scenario_id = default_scenario$id,
    scenario_sequence = 1L,
    forecast = NULL,
    forecast_status = "unavailable",
    data_revision = 0L,
    specification_revision = 0L,
    dictionary_revision = 0L,
    settings_revision = 0L,
    forecast_revision = 0L,
    last_change = Sys.time(),
    last_change_reason = "Workspace created",
    activity = osem_shiny_activity_add(
      osem_shiny_empty_activity(),
      area = "Project",
      action = "Created workspace",
      detail = "Started a new OSEM project."
    ),
    session_dir = session_dir
  )

  shiny::isolate({
    if (!is.null(initial_model)) {
      imported <- tryCatch(
        osem_shiny_state_import_model(
          state,
          initial_model,
          project_name = "Imported OSEM model"
        ),
        error = function(e) e
      )
      if (inherits(imported, "error")) {
        warning(
          paste0(
            "The object passed to run_shiny() was ignored: ",
            conditionMessage(imported)
          ),
          call. = FALSE
        )
      }
    }

    if (!is.null(initial_project)) {
      osem_shiny_project_restore(state, initial_project)
    }
  })

  session$onSessionEnded(function() {
    unlink(session_dir, recursive = TRUE, force = TRUE)
  })

  state
}

osem_shiny_state_derived <- function(state) {
  effective_data <- shiny::reactive({
    osem_shiny_effective_data(state$input_sources)
  })

  workspace <- shiny::reactive({
    base <- osem_shiny_validate_workspace(
      specification = state$specification,
      dictionary = state$dictionary,
      input_sources = state$input_sources,
      primary_source = state$primary_source,
      effective = effective_data()
    )
    settings <- osem_shiny_validate_run_args(
      state$run_args,
      specification = base$specification
    )
    base$issues <- osem_shiny_bind_issues(base$issues, settings$issues)
    if (nrow(base$issues) > 0L) {
      base$issues <- unique(base$issues)
      priority <- match(base$issues$level, c("error", "warning", "info", "success"))
      base$issues <- base$issues[order(priority, base$issues$area, base$issues$message), , drop = FALSE]
      rownames(base$issues) <- NULL
    }
    base$ready <- isTRUE(base$ready) && isTRUE(settings$valid)
    base$run_settings <- settings$settings
    base
  })

  variable_profile <- shiny::reactive({
    osem_shiny_variable_profile(
      effective = effective_data(),
      specification = state$specification,
      dictionary = state$dictionary,
      primary_source = state$primary_source,
      workspace = workspace()
    )
  })

  active_scenario <- shiny::reactive({
    osem_shiny_get_active_scenario(state)
  })

  list(
    effective_data = effective_data,
    workspace = workspace,
    variable_profile = variable_profile,
    run_input = shiny::reactive(osem_shiny_build_run_input(state$input_sources)),
    source_table = shiny::reactive(osem_shiny_source_table(state$input_sources)),
    active_scenario = active_scenario,
    exogenous_variables = shiny::reactive({
      if (is.null(state$model) || !inherits(state$model, "osem")) character() else {
        osem_shiny_forecast_exogenous_variables(state$model)
      }
    })
  )
}

osem_shiny_state_next_source_id <- function(state) {
  state$source_sequence <- as.integer(state$source_sequence) + 1L
  sprintf("source-%04d", state$source_sequence)
}

osem_shiny_state_next_scenario_id <- function(state) {
  state$scenario_sequence <- as.integer(state$scenario_sequence) + 1L
  sprintf("scenario-%04d", state$scenario_sequence)
}

osem_shiny_state_invalidate_forecasts <- function(state, reason = NULL) {
  scenarios <- state$forecast_scenarios
  if (length(scenarios) > 0L) {
    scenarios <- lapply(scenarios, function(scenario) {
      scenario <- osem_shiny_normalise_forecast_scenario(scenario)
      scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
      if (!is.null(reason)) scenario$stale_reason <- reason
      scenario
    })
    state$forecast_scenarios <- scenarios
  }
  osem_shiny_sync_active_forecast(state)
  invisible(state)
}

osem_shiny_state_mark_changed <- function(state,
                                          area = c("data", "specification", "dictionary", "settings", "forecast"),
                                          reason = "Inputs changed") {
  area <- match.arg(area)

  if (identical(area, "data")) {
    state$data_revision <- state$data_revision + 1L
  } else if (identical(area, "specification")) {
    state$specification_revision <- state$specification_revision + 1L
  } else if (identical(area, "dictionary")) {
    state$dictionary_revision <- state$dictionary_revision + 1L
  } else if (identical(area, "settings")) {
    state$settings_revision <- state$settings_revision + 1L
  } else if (identical(area, "forecast")) {
    state$forecast_revision <- state$forecast_revision + 1L
  }

  if (area %in% c("data", "specification", "dictionary", "settings")) {
    state$model_status <- if (is.null(state$model)) "unavailable" else "stale"
    osem_shiny_state_invalidate_forecasts(state, reason)
  } else if (identical(area, "forecast")) {
    active <- osem_shiny_get_active_scenario(state)
    if (!is.null(active)) {
      active$status <- if (is.null(active$result)) "unavailable" else "stale"
      active$stale_reason <- reason
      osem_shiny_set_scenario(state, active)
    }
    osem_shiny_sync_active_forecast(state)
  }

  state$last_change <- Sys.time()
  state$last_change_reason <- reason
  state$activity <- osem_shiny_activity_add(
    state$activity,
    area = tools::toTitleCase(area),
    action = "Changed",
    detail = reason
  )
  invisible(state)
}

osem_shiny_empty_activity <- function() {
  data.frame(
    timestamp = as.POSIXct(character()),
    area = character(),
    action = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}

osem_shiny_activity_add <- function(activity, area, action, detail) {
  new_row <- data.frame(
    timestamp = Sys.time(),
    area = as.character(area),
    action = as.character(action),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
  out <- rbind(activity, new_row)
  if (nrow(out) > 250L) {
    out <- utils::tail(out, 250L)
  }
  rownames(out) <- NULL
  out
}

osem_shiny_project_snapshot <- function(state, include_model = TRUE) {
  package_version <- tryCatch(
    as.character(utils::packageVersion("osem")),
    error = function(e) NA_character_
  )

  source_snapshots <- lapply(state$input_sources, osem_shiny_source_for_project)
  scenarios <- lapply(state$forecast_scenarios, function(scenario) {
    scenario <- osem_shiny_normalise_forecast_scenario(scenario)
    if (!isTRUE(include_model)) {
      scenario$result <- NULL
      scenario$status <- "unavailable"
      scenario$log <- character()
      scenario$messages <- character()
      scenario$warnings <- character()
      scenario$error <- NULL
    }
    scenario
  })

  out <- list(
    schema_version = 3L,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    package_version = package_version,
    project = list(
      name = state$project_name,
      description = state$project_description
    ),
    specification = state$specification,
    dictionary = state$dictionary,
    primary_source = state$primary_source,
    input_sources = source_snapshots,
    run_args = state$run_args,
    model = if (isTRUE(include_model)) state$model else NULL,
    forecast = if (isTRUE(include_model)) state$forecast else NULL,
    forecast_scenarios = scenarios,
    active_scenario_id = state$active_scenario_id,
    model_run = if (isTRUE(include_model)) list(
      log = state$model_log,
      messages = state$model_messages,
      warnings = state$model_warnings,
      error = state$model_error,
      metadata = state$model_run_metadata
    ) else list(),
    status = list(
      model = if (isTRUE(include_model)) state$model_status else "unavailable",
      forecast = if (isTRUE(include_model)) state$forecast_status else "unavailable"
    ),
    revisions = list(
      data = state$data_revision,
      specification = state$specification_revision,
      dictionary = state$dictionary_revision,
      settings = state$settings_revision,
      forecast = state$forecast_revision
    ),
    activity = state$activity
  )
  class(out) <- c("osem_shiny_project", "list")
  out
}

osem_shiny_project_validate <- function(project) {
  if (!is.list(project)) {
    stop("The selected file does not contain an OSEM project.", call. = FALSE)
  }
  required <- c(
    "schema_version", "project", "specification", "dictionary",
    "primary_source", "input_sources"
  )
  missing <- setdiff(required, names(project))
  if (length(missing) > 0L) {
    stop(
      paste0("The project file is missing: ", paste(missing, collapse = ", "), "."),
      call. = FALSE
    )
  }
  version <- suppressWarnings(as.integer(project$schema_version))
  if (length(version) != 1L || is.na(version) || !version %in% c(1L, 2L, 3L)) {
    stop("This OSEM project schema is not supported by the current app.", call. = FALSE)
  }
  if (!is.list(project$project)) {
    stop("The project metadata section is invalid.", call. = FALSE)
  }
  if (!is.data.frame(project$specification) && !is.matrix(project$specification)) {
    stop("The project specification must be a data frame or matrix.", call. = FALSE)
  }
  if (!is.data.frame(project$dictionary)) {
    stop("The project dictionary must be a data frame.", call. = FALSE)
  }
  if (!is.list(project$input_sources)) {
    stop("The project's input_sources section must be a list.", call. = FALSE)
  }
  optional_lists <- c(
    "run_args", "model_run", "status", "revisions",
    "forecast_scenarios"
  )
  invalid_list <- optional_lists[
    vapply(optional_lists, function(name) {
      !is.null(project[[name]]) && !is.list(project[[name]])
    }, logical(1L))
  ]
  if (length(invalid_list) > 0L) {
    stop(
      paste0(
        "The project contains an invalid list section: ",
        paste(invalid_list, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  if (!is.null(project$activity) && !is.data.frame(project$activity)) {
    stop("The project activity log must be a data frame.", call. = FALSE)
  }
  invisible(TRUE)
}

osem_shiny_restore_integer <- function(value, default = 0L, minimum = 0L) {
  if (is.null(value) || length(value) == 0L) return(as.integer(default))
  out <- suppressWarnings(as.integer(value[[1L]]))
  if (length(out) == 0L || is.na(out) || out < minimum) as.integer(default) else out
}

osem_shiny_project_prepare <- function(project) {
  osem_shiny_project_validate(project)
  schema_version <- as.integer(project$schema_version)

  project_name <- osem_shiny_scalar_character(
    project$project$name, "Imported OSEM project"
  )
  project_name <- trimws(project_name)
  if (!nzchar(project_name)) project_name <- "Imported OSEM project"
  project_description <- paste(
    as.character(project$project$description %||% ""),
    collapse = "\n"
  )

  specification <- osem_shiny_normalise_specification(project$specification)
  dictionary <- osem_shiny_normalise_dictionary(project$dictionary)
  primary_source <- osem_shiny_scalar_character(project$primary_source, "local")
  if (!primary_source %in% c("local", "download")) primary_source <- "local"

  input_sources <- lapply(project$input_sources, osem_shiny_source_from_project)
  if (length(input_sources) > 0L) {
    source_ids <- vapply(input_sources, function(x) x$id, character(1L))
    unique_source_ids <- make.unique(source_ids, sep = "-")
    if (!identical(source_ids, unique_source_ids)) {
      for (i in seq_along(input_sources)) input_sources[[i]]$id <- unique_source_ids[[i]]
      source_ids <- unique_source_ids
    }
  } else {
    source_ids <- character()
  }
  source_numbers <- suppressWarnings(as.integer(sub(
    "^source-([0-9]+).*$", "\\1", source_ids
  )))
  source_sequence <- if (length(source_ids) == 0L) {
    0L
  } else {
    as.integer(max(c(length(source_ids), source_numbers), na.rm = TRUE))
  }

  run_args <- osem_shiny_normalise_run_args(project$run_args %||% list())
  model <- project$model %||% NULL
  if (!is.null(model)) {
    if (!inherits(model, "osem")) {
      stop("The project contains a model object that is not of class 'osem'.", call. = FALSE)
    }
    model_check <- tryCatch(
      osem_shiny_prepare_imported_model(model, project_name = project_name),
      error = function(e) e
    )
    if (inherits(model_check, "error")) {
      stop(
        paste0("The project contains an unusable fitted model: ", conditionMessage(model_check)),
        call. = FALSE
      )
    }
    model <- model_check$model
  }

  status <- project$status %||% list()
  model_status <- osem_shiny_scalar_character(
    status$model,
    if (is.null(model)) "unavailable" else "current"
  )
  if (is.null(model)) {
    model_status <- "unavailable"
  } else if (!model_status %in% c("current", "stale", "failed")) {
    model_status <- "current"
  }

  model_run <- project$model_run %||% list()
  model_log <- as.character(model_run$log %||% character())
  model_messages <- as.character(model_run$messages %||% character())
  model_warnings <- as.character(model_run$warnings %||% character())
  model_error <- model_run$error %||% NULL
  model_run_metadata <- if (is.list(model_run$metadata)) model_run$metadata else list()

  if (schema_version >= 2L && length(project$forecast_scenarios %||% list()) > 0L) {
    scenarios <- lapply(project$forecast_scenarios, osem_shiny_normalise_forecast_scenario)
  } else {
    legacy <- osem_shiny_default_forecast_scenario("scenario-0001", "Baseline")
    legacy$result <- project$forecast %||% NULL
    legacy$status <- status$forecast %||%
      if (is.null(legacy$result)) "unavailable" else "current"
    scenarios <- list(legacy)
  }
  if (length(scenarios) == 0L) {
    scenarios <- list(osem_shiny_default_forecast_scenario("scenario-0001", "Baseline"))
  }
  scenarios <- lapply(scenarios, function(scenario) {
    scenario <- osem_shiny_normalise_forecast_scenario(scenario)
    if (!is.null(scenario$result) && !inherits(scenario$result, "osem.forecast")) {
      scenario$result <- NULL
      scenario$status <- "unavailable"
      scenario$error <- paste0(
        "The stored forecast object was not of class 'osem.forecast' and was discarded."
      )
    }
    if (identical(scenario$status, "running")) {
      scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
      scenario$stale_reason <- paste0(
        "The project was saved while this scenario was marked as running. ",
        "Run it again to confirm the result."
      )
    }
    if (is.null(scenario$result) && scenario$status %in% c("current", "stale")) {
      scenario$status <- "unavailable"
    }
    scenario
  })

  scenario_ids <- vapply(scenarios, function(x) x$id, character(1L))
  unique_scenario_ids <- make.unique(scenario_ids, sep = "-")
  if (!identical(unique_scenario_ids, scenario_ids)) {
    for (i in seq_along(scenarios)) scenarios[[i]]$id <- unique_scenario_ids[[i]]
    scenario_ids <- unique_scenario_ids
  }
  active_scenario_id <- osem_shiny_scalar_character(
    project$active_scenario_id, scenarios[[1L]]$id
  )
  if (!active_scenario_id %in% scenario_ids) active_scenario_id <- scenario_ids[[1L]]
  scenario_numbers <- suppressWarnings(as.integer(sub(
    "^scenario-([0-9]+).*$", "\\1", scenario_ids
  )))
  scenario_sequence <- as.integer(max(c(length(scenarios), scenario_numbers), na.rm = TRUE))

  revisions <- project$revisions %||% list()
  revision_values <- list(
    data = osem_shiny_scalar_integer(revisions$data, 0L, 0L, .Machine$integer.max),
    specification = osem_shiny_scalar_integer(
      revisions$specification, 0L, 0L, .Machine$integer.max
    ),
    dictionary = osem_shiny_scalar_integer(
      revisions$dictionary, 0L, 0L, .Machine$integer.max
    ),
    settings = osem_shiny_scalar_integer(
      revisions$settings, 0L, 0L, .Machine$integer.max
    ),
    forecast = osem_shiny_scalar_integer(
      revisions$forecast, 0L, 0L, .Machine$integer.max
    )
  )

  restored_activity <- project$activity %||% osem_shiny_empty_activity()
  activity_columns <- c("timestamp", "area", "action", "detail")
  if (!is.data.frame(restored_activity) || !all(activity_columns %in% names(restored_activity))) {
    restored_activity <- osem_shiny_empty_activity()
  } else {
    restored_activity <- restored_activity[, activity_columns, drop = FALSE]
    if (nrow(restored_activity) > 0L) {
      restored_activity$timestamp <- as.POSIXct(vapply(
        seq_len(nrow(restored_activity)),
        function(i) {
          format(
            osem_shiny_as_posixct_scalar(restored_activity$timestamp[[i]], Sys.time()),
            tz = "UTC",
            usetz = TRUE
          )
        },
        character(1L)
      ), tz = "UTC")
    } else {
      restored_activity$timestamp <- as.POSIXct(character(), tz = "UTC")
    }
    restored_activity$area <- as.character(restored_activity$area)
    restored_activity$action <- as.character(restored_activity$action)
    restored_activity$detail <- as.character(restored_activity$detail)
  }

  list(
    project_name = project_name,
    project_description = project_description,
    specification = specification,
    dictionary = dictionary,
    primary_source = primary_source,
    input_sources = input_sources,
    source_sequence = source_sequence,
    run_args = run_args,
    model = model,
    model_status = model_status,
    model_log = model_log,
    model_messages = model_messages,
    model_warnings = model_warnings,
    model_error = model_error,
    model_run_metadata = model_run_metadata,
    forecast_scenarios = scenarios,
    active_scenario_id = active_scenario_id,
    scenario_sequence = scenario_sequence,
    revisions = revision_values,
    activity = restored_activity
  )
}

osem_shiny_project_restore <- function(state, project) {
  # Prepare and validate every component before mutating reactive state. A
  # malformed project therefore cannot leave the current workspace half-open.
  restored <- osem_shiny_project_prepare(project)

  state$project_name <- restored$project_name
  state$project_description <- restored$project_description
  state$specification <- restored$specification
  state$dictionary <- restored$dictionary
  state$primary_source <- restored$primary_source
  state$input_sources <- restored$input_sources
  state$source_sequence <- restored$source_sequence
  state$run_args <- restored$run_args
  state$model <- restored$model
  state$model_status <- restored$model_status
  state$model_log <- restored$model_log
  state$model_messages <- restored$model_messages
  state$model_warnings <- restored$model_warnings
  state$model_error <- restored$model_error
  state$model_run_metadata <- restored$model_run_metadata
  state$processed_snapshot_path <- NULL
  state$forecast_scenarios <- restored$forecast_scenarios
  state$active_scenario_id <- restored$active_scenario_id
  state$scenario_sequence <- restored$scenario_sequence
  osem_shiny_sync_active_forecast(state)

  state$data_revision <- restored$revisions$data
  state$specification_revision <- restored$revisions$specification
  state$dictionary_revision <- restored$revisions$dictionary
  state$settings_revision <- restored$revisions$settings
  state$forecast_revision <- restored$revisions$forecast
  state$project_revision <- osem_shiny_scalar_integer(
    state$project_revision, 0L, 0L, .Machine$integer.max - 1L
  ) + 1L
  state$last_change <- Sys.time()
  state$last_change_reason <- "Project opened"
  state$activity <- osem_shiny_activity_add(
    restored$activity,
    area = "Project",
    action = "Opened project",
    detail = "Restored an OSEM project file."
  )
  invisible(state)
}

osem_shiny_prepare_imported_model <- function(model, project_name = NULL) {
  if (!inherits(model, "osem")) {
    stop("The selected file does not contain an object of class 'osem'.", call. = FALSE)
  }
  specification_raw <- model$args$specification %||% model$module_order
  if (!is.data.frame(specification_raw) && !is.matrix(specification_raw)) {
    stop("The fitted model does not contain a usable specification.", call. = FALSE)
  }
  specification <- osem_shiny_normalise_specification(specification_raw)

  dictionary_raw <- model$dictionary %||% osem_shiny_default_dictionary()
  dictionary <- osem_shiny_normalise_dictionary(dictionary_raw)
  dictionary_validation <- osem_shiny_validate_dictionary(dictionary)
  if (!isTRUE(dictionary_validation$valid)) {
    messages <- dictionary_validation$issues$message[
      dictionary_validation$issues$level == "error"
    ]
    stop(
      paste0("The fitted model dictionary is not usable: ", paste(messages, collapse = " ")),
      call. = FALSE
    )
  }

  processed <- model$processed_input_data %||% model$full_data
  if (!is.data.frame(processed)) {
    stop("The fitted model does not contain a usable input-data snapshot.", call. = FALSE)
  }
  source <- osem_shiny_source_from_data(
    data = processed,
    display_name = "Data snapshot from imported model",
    source_id = "source-0001",
    kind = "model_snapshot"
  )
  if (!isTRUE(source$valid)) {
    messages <- source$issues$message[source$issues$level == "error"]
    stop(
      paste0("The fitted model input-data snapshot is not usable: ", paste(messages, collapse = " ")),
      call. = FALSE
    )
  }

  run_args <- osem_shiny_normalise_run_args(model$args %||% list())
  primary_source <- osem_shiny_scalar_character(
    model$args$primary_source, "local"
  )
  if (!primary_source %in% c("local", "download")) primary_source <- "local"
  name <- trimws(osem_shiny_scalar_character(project_name, "Imported OSEM model"))
  if (!nzchar(name)) name <- "Imported OSEM model"

  list(
    model = model,
    specification = specification,
    dictionary = dictionary_validation$data,
    source = source,
    run_args = run_args,
    primary_source = primary_source,
    project_name = name
  )
}

osem_shiny_state_import_model <- function(state, model, project_name = NULL) {
  # As with project restoration, complete validation before replacing any
  # element of the active workspace.
  imported <- osem_shiny_prepare_imported_model(model, project_name)
  baseline <- osem_shiny_default_forecast_scenario("scenario-0001", "Baseline")

  state$model <- imported$model
  state$model_status <- "current"
  state$model_log <- character()
  state$model_messages <- character()
  state$model_warnings <- character()
  state$model_error <- NULL
  state$model_run_metadata <- list(
    imported = TRUE,
    finished = Sys.time(),
    duration_seconds = NA_real_,
    run_args = imported$run_args,
    revisions = osem_shiny_revision_stamp(state)
  )
  state$specification <- imported$specification
  state$dictionary <- imported$dictionary
  state$primary_source <- imported$primary_source
  state$run_args <- imported$run_args
  state$input_sources <- list(imported$source)
  state$source_sequence <- 1L

  state$forecast_scenarios <- list(baseline)
  state$active_scenario_id <- baseline$id
  state$scenario_sequence <- 1L
  state$forecast <- NULL
  state$forecast_status <- "unavailable"
  state$processed_snapshot_path <- NULL

  state$project_name <- imported$project_name
  state$data_revision <- 0L
  state$specification_revision <- 0L
  state$dictionary_revision <- 0L
  state$settings_revision <- 0L
  state$forecast_revision <- 0L
  state$project_revision <- osem_shiny_scalar_integer(
    state$project_revision, 0L, 0L, .Machine$integer.max - 1L
  ) + 1L
  state$last_change <- Sys.time()
  state$last_change_reason <- "Fitted model imported"
  state$activity <- osem_shiny_activity_add(
    state$activity,
    area = "Project",
    action = "Imported fitted model",
    detail = "Loaded an OSEM object and its processed input-data snapshot."
  )
  invisible(state)
}


osem_shiny_project_reset <- function(state) {
  default_scenario <- osem_shiny_default_forecast_scenario(
    id = "scenario-0001",
    name = "Baseline"
  )
  state$project_name <- "Untitled OSEM project"
  state$project_description <- ""
  state$specification <- osem_shiny_default_specification()
  state$dictionary <- osem_shiny_default_dictionary()
  state$primary_source <- "local"
  state$input_sources <- list()
  state$source_sequence <- 0L
  state$run_args <- osem_shiny_default_run_args()
  state$model <- NULL
  state$model_status <- "unavailable"
  state$model_log <- character()
  state$model_messages <- character()
  state$model_warnings <- character()
  state$model_error <- NULL
  state$model_run_metadata <- list()
  state$processed_snapshot_path <- NULL
  state$forecast_scenarios <- list(default_scenario)
  state$active_scenario_id <- default_scenario$id
  state$scenario_sequence <- 1L
  state$forecast <- NULL
  state$forecast_status <- "unavailable"
  state$data_revision <- 0L
  state$specification_revision <- 0L
  state$dictionary_revision <- 0L
  state$settings_revision <- 0L
  state$forecast_revision <- 0L
  state$project_revision <- osem_shiny_scalar_integer(
    state$project_revision, 0L, 0L, .Machine$integer.max - 1L
  ) + 1L
  state$last_change <- Sys.time()
  state$last_change_reason <- "Workspace reset"
  state$activity <- osem_shiny_activity_add(
    osem_shiny_empty_activity(),
    area = "Project",
    action = "Reset workspace",
    detail = "Returned to the default specification and dictionary."
  )
  invisible(state)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}
