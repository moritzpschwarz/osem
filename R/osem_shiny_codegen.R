# OSEM Shiny reproducibility and code generation ----------------------------
#
# Calls are constructed as R language objects and then deparsed. This keeps
# execution settings, code previews, downloaded scripts, and replication
# bundles aligned with the public run_model() and forecast_model() APIs.

osem_shiny_namespaced_function <- function(package, fun) {
  as.call(list(as.name("::"), as.name(package), as.name(fun)))
}

osem_shiny_language_call <- function(package, fun, arguments) {
  if (!is.list(arguments)) stop("'arguments' must be a list.", call. = FALSE)
  as.call(c(list(osem_shiny_namespaced_function(package, fun)), arguments))
}

osem_shiny_deparse_code <- function(expression, width = 100L) {
  paste(deparse(expression, width.cutoff = as.integer(width)), collapse = "\n")
}

osem_shiny_symbol <- function(name) {
  as.name(as.character(name)[[1L]])
}

osem_shiny_scenario_slug <- function(scenario) {
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  id <- gsub("[^A-Za-z0-9]+", "-", scenario$id)
  id <- gsub("(^-+|-+$)", "", id)
  paste0(osem_shiny_project_slug(scenario$name), "-", id)
}

osem_shiny_reproduction_context <- function(state, exact = TRUE) {
  has_model <- inherits(state$model, "osem")

  if (isTRUE(exact) && has_model) {
    model_args <- state$model$args %||% list()
    run_args <- state$model_run_metadata$run_args %||% model_args
    specification <- model_args$specification %||% state$specification
    dictionary <- state$model$dictionary %||% model_args$dictionary %||% state$dictionary
    data <- state$model$processed_input_data %||% state$model$full_data

    return(list(
      exact = TRUE,
      label = "Stored fitted-model snapshot",
      specification = osem_shiny_normalise_specification(specification),
      dictionary = osem_shiny_normalise_dictionary(dictionary),
      data = if (is.data.frame(data)) {
        as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        NULL
      },
      run_args = osem_shiny_normalise_run_args(run_args),
      primary_source = "local",
      model = state$model
    ))
  }

  list(
    exact = FALSE,
    label = "Current workspace inputs",
    specification = osem_shiny_normalise_specification(state$specification),
    dictionary = osem_shiny_normalise_dictionary(state$dictionary),
    data = osem_shiny_build_run_input(state$input_sources),
    run_args = osem_shiny_normalise_run_args(state$run_args),
    primary_source = state$primary_source,
    model = state$model
  )
}

osem_shiny_portable_save_expression <- function(run_args, exact = FALSE) {
  settings <- osem_shiny_normalise_run_args(run_args)
  if (isTRUE(exact) || is.null(settings$save_to_disk)) return(NULL)

  extension <- tolower(tools::file_ext(settings$save_to_disk))
  if (!nzchar(extension)) extension <- "rds"
  as.call(list(
    as.name("file.path"),
    "output",
    paste0("processed-input.", extension)
  ))
}

osem_shiny_run_call_expression <- function(run_args,
                                            primary_source = "local",
                                            exact = FALSE) {
  settings <- osem_shiny_normalise_run_args(run_args)
  arguments <- list(
    specification = osem_shiny_symbol("specification"),
    dictionary = osem_shiny_symbol("dictionary"),
    input = osem_shiny_symbol("input_data"),
    primary_source = if (isTRUE(exact)) "local" else primary_source,
    save_to_disk = osem_shiny_portable_save_expression(settings, exact = exact),
    use_logs = settings$use_logs,
    trend = settings$trend,
    ardl_or_ecm = settings$ardl_or_ecm,
    ecm_pretest = settings$ecm_pretest,
    max.ar = settings$max.ar,
    max.dl = settings$max.dl,
    saturation = settings$saturation,
    saturation.tpval = settings$saturation.tpval,
    max.block.size = settings$max.block.size,
    gets_selection = settings$gets_selection,
    selection.tpval = settings$selection.tpval,
    constrain.to.minimum.sample = settings$constrain.to.minimum.sample,
    keep = settings$keep,
    pretest_steps = settings$pretest_steps,
    present = FALSE,
    quiet = settings$quiet,
    plot = FALSE,
    cvar.ar = settings$cvar.ar,
    coint_seasonal = settings$coint_seasonal,
    coint_deterministic = settings$coint_deterministic,
    coint_significance = settings$coint_significance
  )
  osem_shiny_language_call("osem", "run_model", arguments)
}

osem_shiny_forecast_call_expression <- function(scenario,
                                                 model_symbol = "model",
                                                 assumptions_symbol = NULL) {
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  args <- scenario$args
  has_assumptions <- !is.null(assumptions_symbol)
  arguments <- list(
    model = osem_shiny_symbol(model_symbol),
    exog_predictions = if (has_assumptions) {
      osem_shiny_symbol(assumptions_symbol)
    } else {
      NULL
    },
    n.ahead = args$n.ahead,
    ci.levels = args$ci.levels,
    exog_fill_method = if (has_assumptions) NULL else args$exog_fill_method,
    ar.fill.max = args$ar.fill.max,
    plot = FALSE,
    uncertainty_sample = args$uncertainty_sample,
    quiet = args$quiet
  )
  osem_shiny_language_call("osem", "forecast_model", arguments)
}

osem_shiny_run_code <- function(state, exact = FALSE) {
  context <- osem_shiny_reproduction_context(state, exact = exact)
  call <- osem_shiny_run_call_expression(
    context$run_args,
    primary_source = context$primary_source,
    exact = context$exact
  )
  paste0(
    "# Context: ", context$label, "\n",
    "# Objects expected: specification, dictionary, input_data\n",
    "set.seed(", context$run_args$seed, ")\n",
    "model <- ", osem_shiny_deparse_code(call)
  )
}

osem_shiny_scenario_assumptions_for_export <- function(scenario, exact = TRUE) {
  scenario <- osem_shiny_normalise_forecast_scenario(scenario)
  if (isTRUE(exact) && identical(scenario$status, "current") &&
      inherits(scenario$result, "osem.forecast")) {
    assumptions <- scenario$result$exog_data_nowcast %||% scenario$result$exog_data
    if (is.data.frame(assumptions)) {
      assumptions <- as.data.frame(
        assumptions,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if ("time" %in% names(assumptions)) assumptions$time <- as.Date(assumptions$time)
      return(assumptions)
    }
  }

  if (is.data.frame(scenario$exog_predictions)) {
    assumptions <- as.data.frame(
      scenario$exog_predictions,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if ("time" %in% names(assumptions)) assumptions$time <- as.Date(assumptions$time)
    return(assumptions)
  }
  NULL
}

osem_shiny_active_forecast_code <- function(state, exact = FALSE) {
  scenario <- osem_shiny_get_active_scenario(state)
  if (is.null(scenario)) return("# No forecast scenario is configured.")

  assumptions <- osem_shiny_scenario_assumptions_for_export(scenario, exact = exact)
  assumptions_symbol <- if (is.data.frame(assumptions)) "exog_predictions" else NULL
  call <- osem_shiny_forecast_call_expression(
    scenario,
    model_symbol = "model",
    assumptions_symbol = assumptions_symbol
  )
  assumptions_note <- if (is.null(assumptions_symbol)) {
    paste0(
      "# Exogenous assumptions will be generated with method '",
      scenario$args$exog_fill_method,
      "'.\n"
    )
  } else {
    "# Object expected: exog_predictions\n"
  }
  paste0(
    "# Scenario: ", scenario$name, "\n",
    assumptions_note,
    "set.seed(", scenario$args$seed, ")\n",
    "forecast <- ", osem_shiny_deparse_code(call)
  )
}

osem_shiny_reproduction_data <- function(state, exact = TRUE) {
  osem_shiny_reproduction_context(state, exact = exact)$data
}

osem_shiny_selected_scenarios <- function(state, include_all_scenarios = FALSE) {
  if (isTRUE(include_all_scenarios)) return(state$forecast_scenarios)
  active <- osem_shiny_get_active_scenario(state)
  if (is.null(active)) list() else list(active)
}

osem_shiny_complete_script <- function(state,
                                       exact = TRUE,
                                       include_all_scenarios = FALSE) {
  context <- osem_shiny_reproduction_context(state, exact = exact)
  run_call <- osem_shiny_run_call_expression(
    context$run_args,
    primary_source = context$primary_source,
    exact = context$exact
  )
  package_version <- tryCatch(
    as.character(utils::packageVersion("osem")),
    error = function(e) "unknown"
  )

  lines <- c(
    "# Reproducible OSEM analysis generated by the OSEM Shiny workspace",
    paste0("# Project: ", state$project_name),
    paste0("# OSEM package version at generation: ", package_version),
    paste0("# Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0(
      "# Mode: ",
      if (isTRUE(context$exact)) {
        "exact fitted-model snapshot"
      } else {
        "current workspace / source refresh"
      }
    ),
    "",
    "if (!requireNamespace(\"osem\", quietly = TRUE)) {",
    "  stop(\"Install the 'osem' package before running this script.\")",
    "}",
    paste0("generated_osem_version <- ", deparse(package_version)),
    "installed_osem_version <- as.character(utils::packageVersion(\"osem\"))",
    "if (!identical(generated_osem_version, \"unknown\") &&",
    "    !identical(installed_osem_version, generated_osem_version)) {",
    "  warning(paste0(",
    "    \"This script was generated with osem \",",
    "    generated_osem_version,",
    "    \" but is running with osem \",",
    "    installed_osem_version,",
    "    \".\"",
    "  ))",
    "}",
    "",
    "dir.create(\"output\", recursive = TRUE, showWarnings = FALSE)",
    "",
    "specification <- utils::read.csv(",
    "  file.path(\"data\", \"specification.csv\"),",
    "  stringsAsFactors = FALSE,",
    "  check.names = FALSE,",
    "  na.strings = \"NA\"",
    ")",
    "dictionary <- utils::read.csv(",
    "  file.path(\"data\", \"dictionary.csv\"),",
    "  stringsAsFactors = FALSE,",
    "  check.names = FALSE,",
    "  na.strings = \"NA\"",
    ")",
    "input_data <- readRDS(file.path(\"data\", \"input-data.rds\"))",
    "",
    paste0("set.seed(", context$run_args$seed, ")"),
    paste0("model <- ", osem_shiny_deparse_code(run_call)),
    "saveRDS(model, file.path(\"output\", \"model.rds\"))"
  )

  scenarios <- osem_shiny_selected_scenarios(
    state,
    include_all_scenarios = include_all_scenarios
  )
  if (length(scenarios) > 0L) {
    lines <- c(lines, "", "forecasts <- list()")
    used_symbols <- character()

    for (scenario in scenarios) {
      scenario <- osem_shiny_normalise_forecast_scenario(scenario)
      slug <- osem_shiny_scenario_slug(scenario)
      symbol_slug <- gsub("[^A-Za-z0-9_]", "_", slug)
      if (!grepl("^[A-Za-z]", symbol_slug)) {
        symbol_slug <- paste0("scenario_", symbol_slug)
      }
      while (symbol_slug %in% used_symbols) {
        symbol_slug <- paste0(symbol_slug, "_x")
      }
      used_symbols <- c(used_symbols, symbol_slug)

      assumptions <- osem_shiny_scenario_assumptions_for_export(
        scenario,
        exact = context$exact
      )
      assumptions_name <- paste0("exog_", symbol_slug)
      result_name <- paste0("forecast_", symbol_slug)
      call <- osem_shiny_forecast_call_expression(
        scenario,
        model_symbol = "model",
        assumptions_symbol = if (is.data.frame(assumptions)) assumptions_name else NULL
      )

      lines <- c(
        lines,
        "",
        paste0("# Scenario: ", scenario$name),
        if (is.data.frame(assumptions)) {
          paste0(
            assumptions_name,
            " <- utils::read.csv(file.path(\"data\", \"forecast-assumptions-",
            slug,
            ".csv\"), stringsAsFactors = FALSE, check.names = FALSE, na.strings = \"NA\")"
          )
        } else {
          NULL
        },
        if (is.data.frame(assumptions)) {
          paste0(assumptions_name, "$time <- as.Date(", assumptions_name, "$time)")
        } else {
          NULL
        },
        paste0("set.seed(", scenario$args$seed, ")"),
        paste0(result_name, " <- ", osem_shiny_deparse_code(call)),
        paste0("forecasts[[", deparse(scenario$name), "]] <- ", result_name),
        paste0(
          "saveRDS(", result_name, ", file.path(\"output\", \"forecast-",
          slug, ".rds\"))"
        )
      )
    }
    lines <- c(
      lines,
      "",
      "saveRDS(forecasts, file.path(\"output\", \"forecasts.rds\"))"
    )
  }

  paste(lines, collapse = "\n")
}

osem_shiny_script_parse_status <- function(script) {
  parsed <- tryCatch(parse(text = script), error = function(e) e)
  if (inherits(parsed, "error")) {
    list(valid = FALSE, error = conditionMessage(parsed))
  } else {
    list(valid = TRUE, error = NULL)
  }
}

osem_shiny_reproduction_readiness <- function(state,
                                               exact = TRUE,
                                               include_all_scenarios = FALSE) {
  issues <- osem_shiny_empty_issues()
  context <- osem_shiny_reproduction_context(state, exact = exact)

  spec_validation <- osem_shiny_validate_specification(context$specification)
  dictionary_validation <- osem_shiny_validate_dictionary(context$dictionary)
  issues <- osem_shiny_bind_issues(
    issues,
    spec_validation$issues,
    dictionary_validation$issues
  )

  if (isTRUE(exact)) {
    if (!inherits(state$model, "osem")) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Exact reproduction",
        "Run or open a fitted OSEM model before creating an exact-result bundle."
      )
    } else if (!is.data.frame(context$data)) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Exact reproduction",
        "The fitted model does not contain a processed input-data snapshot."
      )
    }
    if (inherits(state$model, "osem") && !identical(state$model_status, "current")) {
      issues <- osem_shiny_add_issue(
        issues,
        "warning",
        "Exact reproduction",
        paste0(
          "The fitted model is marked ", state$model_status,
          " relative to the current workspace. The exact bundle uses the specification, ",
          "dictionary, processed data, and recorded settings associated with the stored model."
        )
      )
    }
    if (inherits(state$model, "osem") && is.null(state$model_run_metadata$run_args)) {
      issues <- osem_shiny_add_issue(
        issues,
        "warning",
        "Exact reproduction",
        paste0(
          "This model was imported without an app-side run-settings snapshot. ",
          "The script reconstructs available arguments from model$args; arguments not ",
          "stored in the modelling object use current package defaults."
        )
      )
    }
  } else if (identical(context$primary_source, "local") && is.null(context$data)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Current-input reproduction",
      "Local data are the primary source, but no local input snapshot is available."
    )
  }

  scenarios <- osem_shiny_selected_scenarios(
    state,
    include_all_scenarios = include_all_scenarios
  )
  if (length(scenarios) > 0L) {
    for (scenario in scenarios) {
      scenario <- osem_shiny_normalise_forecast_scenario(scenario)
      if (isTRUE(exact) && !is.null(scenario$result) &&
          !identical(scenario$status, "current")) {
        issues <- osem_shiny_add_issue(
          issues,
          "warning",
          "Forecast reproduction",
          paste0(
            "Scenario '", scenario$name,
            "' has a stored but non-current result. Its current scenario settings will be rerun."
          )
        )
      }

      if (identical(scenario$args$assumption_mode, "manual")) {
        if (inherits(context$model, "osem")) {
          validation <- tryCatch(
            osem_shiny_validate_forecast_inputs(context$model, scenario),
            error = function(e) e
          )
          if (inherits(validation, "error")) {
            issues <- osem_shiny_add_issue(
              issues,
              "error",
              "Forecast reproduction",
              paste0("Scenario '", scenario$name, "': ", conditionMessage(validation))
            )
          } else if (!isTRUE(validation$valid)) {
            messages <- validation$issues$message[validation$issues$level == "error"]
            issues <- osem_shiny_add_issue(
              issues,
              "error",
              "Forecast reproduction",
              paste0(
                "Scenario '", scenario$name,
                "' has incomplete manual assumptions: ",
                paste(messages, collapse = " ")
              )
            )
          }
        } else if (!is.data.frame(scenario$exog_predictions)) {
          issues <- osem_shiny_add_issue(
            issues,
            "error",
            "Forecast reproduction",
            paste0(
              "Scenario '", scenario$name,
              "' uses manual assumptions but no assumption table is available."
            )
          )
        } else {
          issues <- osem_shiny_add_issue(
            issues,
            "warning",
            "Forecast reproduction",
            paste0(
              "Scenario '", scenario$name,
              "' assumptions cannot be checked against exogenous variables until the model is fitted."
            )
          )
        }
      }
    }
  }

  script <- osem_shiny_complete_script(
    state,
    exact = exact,
    include_all_scenarios = include_all_scenarios
  )
  parse_status <- osem_shiny_script_parse_status(script)
  if (!isTRUE(parse_status$valid)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Generated code",
      paste0("The generated script does not parse: ", parse_status$error)
    )
  } else {
    issues <- osem_shiny_add_issue(
      issues,
      "success",
      "Generated code",
      "The generated R script parses successfully."
    )
  }

  list(
    valid = !any(issues$level == "error"),
    issues = issues,
    script = script,
    parse_status = parse_status,
    data = context$data,
    context = context
  )
}

osem_shiny_write_csv <- function(data, path) {
  utils::write.csv(
    as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE),
    file = path,
    row.names = FALSE,
    na = "NA",
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

osem_shiny_replication_manifest <- function(root) {
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(files, nchar(root) + 2L)
  hashes <- unname(tools::md5sum(files))
  info <- file.info(files)
  data.frame(
    file = relative,
    bytes = as.numeric(info$size),
    md5 = hashes,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_write_bundle_metadata <- function(state, path, exact) {
  context <- osem_shiny_reproduction_context(state, exact = exact)
  package_version <- tryCatch(
    as.character(utils::packageVersion("osem")),
    error = function(e) NA_character_
  )
  metadata <- data.frame(
    field = c(
      "project_name", "project_description", "created_utc", "mode",
      "context", "osem_version", "R_version", "model_status",
      "forecast_status", "primary_source", "data_revision",
      "specification_revision", "dictionary_revision", "settings_revision",
      "forecast_revision"
    ),
    value = c(
      state$project_name,
      state$project_description,
      format(Sys.time(), tz = "UTC", usetz = TRUE),
      if (isTRUE(context$exact)) {
        "exact fitted-model snapshot"
      } else {
        "current workspace / source refresh"
      },
      context$label,
      package_version,
      R.version.string,
      state$model_status,
      state$forecast_status,
      context$primary_source,
      state$data_revision,
      state$specification_revision,
      state$dictionary_revision,
      state$settings_revision,
      state$forecast_revision
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  osem_shiny_write_csv(metadata, path)
}

osem_shiny_zip_directory <- function(root, target_zip) {
  target_zip <- normalizePath(target_zip, winslash = "/", mustWork = FALSE)
  if (file.exists(target_zip)) unlink(target_zip, force = TRUE)
  files <- list.files(root, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  info <- file.info(file.path(root, files))
  files <- files[!is.na(info$isdir) & !info$isdir]
  if (length(files) == 0L) {
    stop("There are no files to place in the replication bundle.", call. = FALSE)
  }

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(
      zipfile = target_zip,
      files = files,
      recurse = FALSE,
      include_directories = FALSE,
      root = root
    )
  } else {
    old <- setwd(root)
    on.exit(setwd(old), add = TRUE)
    suppressWarnings(utils::zip(zipfile = target_zip, files = files, flags = "-r9X"))
  }
  if (!file.exists(target_zip)) {
    stop(
      paste0(
        "The replication ZIP could not be created. Install the suggested 'zip' ",
        "package or configure an external ZIP utility for R."
      ),
      call. = FALSE
    )
  }
  invisible(target_zip)
}

osem_shiny_write_replication_bundle <- function(state,
                                                target_zip,
                                                exact = TRUE,
                                                include_objects = TRUE,
                                                include_all_scenarios = TRUE) {
  readiness <- osem_shiny_reproduction_readiness(
    state,
    exact = exact,
    include_all_scenarios = include_all_scenarios
  )
  if (!isTRUE(readiness$valid)) {
    errors <- readiness$issues$message[readiness$issues$level == "error"]
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  root <- tempfile("osem-replication-", tmpdir = state$session_dir)
  data_dir <- file.path(root, "data")
  output_dir <- file.path(root, "output")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  context <- readiness$context
  osem_shiny_write_csv(
    context$specification,
    file.path(data_dir, "specification.csv")
  )
  osem_shiny_write_csv(
    context$dictionary,
    file.path(data_dir, "dictionary.csv")
  )
  saveRDS(readiness$data, file.path(data_dir, "input-data.rds"), version = 3)

  scenarios <- osem_shiny_selected_scenarios(
    state,
    include_all_scenarios = include_all_scenarios
  )
  for (scenario in scenarios) {
    scenario <- osem_shiny_normalise_forecast_scenario(scenario)
    assumptions <- osem_shiny_scenario_assumptions_for_export(
      scenario,
      exact = context$exact
    )
    if (is.data.frame(assumptions)) {
      osem_shiny_write_csv(
        assumptions,
        file.path(
          data_dir,
          paste0(
            "forecast-assumptions-",
            osem_shiny_scenario_slug(scenario),
            ".csv"
          )
        )
      )
    }
  }

  writeLines(readiness$script, file.path(root, "analysis.R"), useBytes = TRUE)

  project <- osem_shiny_project_snapshot(state, include_model = include_objects)
  saveRDS(project, file.path(root, "project.osem-project.rds"), version = 3)
  if (isTRUE(include_objects) && inherits(state$model, "osem")) {
    saveRDS(state$model, file.path(output_dir, "model-from-app.rds"), version = 3)
  }
  if (isTRUE(include_objects)) {
    for (scenario in scenarios) {
      scenario <- osem_shiny_normalise_forecast_scenario(scenario)
      if (inherits(scenario$result, "osem.forecast")) {
        saveRDS(
          scenario$result,
          file.path(
            output_dir,
            paste0(
              "forecast-from-app-",
              osem_shiny_scenario_slug(scenario),
              ".rds"
            )
          ),
          version = 3
        )
      }
    }
  }

  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(root, "session-info.txt"),
    useBytes = TRUE
  )
  osem_shiny_write_bundle_metadata(
    state,
    file.path(root, "project-metadata.csv"),
    exact = exact
  )

  readme <- c(
    "OSEM replication bundle",
    "=======================",
    "",
    paste0("Project: ", state$project_name),
    paste0("Created: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0("Context: ", context$label),
    "",
    if (isTRUE(context$exact)) {
      paste0(
        "The bundle uses the specification, dictionary, processed data, and ",
        "recorded run settings associated with the fitted model stored in the app."
      )
    } else {
      paste0(
        "The bundle uses the current prepared local-input snapshot and current ",
        "source priority. Downloaded source data may be revised when the script runs."
      )
    },
    "",
    "To reproduce:",
    "1. Extract the complete ZIP file.",
    "2. Start R with the extracted directory as the working directory.",
    "3. Install a compatible version of the osem package.",
    "4. Run source(\"analysis.R\").",
    "",
    "The script reads only relative paths. Generated objects are written to output/.",
    "The project.osem-project.rds file can be reopened in the OSEM Shiny app.",
    "project-metadata.csv and session-info.txt record the generating environment.",
    "manifest.csv records file sizes and MD5 checksums for files created before it."
  )
  writeLines(readme, file.path(root, "README.txt"), useBytes = TRUE)

  manifest <- osem_shiny_replication_manifest(root)
  osem_shiny_write_csv(manifest, file.path(root, "manifest.csv"))
  osem_shiny_zip_directory(root, target_zip)
  invisible(target_zip)
}

osem_shiny_bundle_contents <- function(state,
                                       exact = TRUE,
                                       include_all_scenarios = TRUE,
                                       include_objects = TRUE) {
  context <- osem_shiny_reproduction_context(state, exact = exact)
  scenarios <- osem_shiny_selected_scenarios(
    state,
    include_all_scenarios = include_all_scenarios
  )
  assumptions <- if (length(scenarios) == 0L) {
    logical()
  } else {
    vapply(
      scenarios,
      function(scenario) {
        is.data.frame(
          osem_shiny_scenario_assumptions_for_export(
            scenario,
            exact = context$exact
          )
        )
      },
      logical(1L)
    )
  }
  result_count <- if (length(scenarios) == 0L) {
    0L
  } else {
    sum(vapply(
      scenarios,
      function(scenario) {
        inherits(
          osem_shiny_normalise_forecast_scenario(scenario)$result,
          "osem.forecast"
        )
      },
      logical(1L)
    ))
  }

  data.frame(
    Item = c(
      "analysis.R",
      "data/specification.csv",
      "data/dictionary.csv",
      "data/input-data.rds",
      "data/forecast-assumptions-<scenario>.csv",
      "project.osem-project.rds",
      "output/model-from-app.rds",
      "output/forecast-from-app-<scenario>.rds",
      "README.txt / metadata / session information / manifest"
    ),
    Included = c(
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      as.character(sum(assumptions)),
      "Yes",
      if (isTRUE(include_objects) && inherits(state$model, "osem")) "Yes" else "No",
      if (isTRUE(include_objects)) as.character(result_count) else "0",
      "Yes"
    ),
    Purpose = c(
      "Executable analysis script using relative paths",
      if (isTRUE(context$exact)) "Specification associated with the fitted model" else "Current five-column model specification",
      if (isTRUE(context$exact)) "Dictionary associated with the fitted model" else "Current variable dictionary",
      if (isTRUE(context$exact)) "Processed data snapshot stored in the fitted model" else "Current prepared local-input snapshot",
      "Manual or realised automatic exogenous paths",
      "Reopen the complete workspace in the app",
      "Optional fitted object for audit and comparison",
      "Optional forecast objects for audit and comparison",
      "Instructions, environment metadata, and checksums"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
