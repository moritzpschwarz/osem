# OSEM Shiny estimation helpers ---------------------------------------------

osem_shiny_default_run_args <- function() {
  list(
    use_logs = "both",
    trend = TRUE,
    ardl_or_ecm = "ardl",
    ecm_pretest = "auto",
    max.ar = 4L,
    max.dl = 4L,
    saturation = c("IIS", "SIS"),
    saturation.tpval = 0.01,
    max.block.size = 20L,
    gets_selection = TRUE,
    selection.tpval = 0.01,
    constrain.to.minimum.sample = TRUE,
    keep = NULL,
    pretest_steps = FALSE,
    quiet = FALSE,
    save_to_disk = NULL,
    cvar.ar = 2L,
    coint_seasonal = FALSE,
    coint_deterministic = "const",
    coint_significance = "5pct",
    seed = 123L
  )
}

osem_shiny_scalar_character <- function(x, default = "") {
  x <- as.character(x %||% default)
  x <- x[!is.na(x)]
  if (length(x) == 0L) default else x[[1L]]
}

osem_shiny_scalar_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L) return(isTRUE(default))
  value <- suppressWarnings(as.logical(x[[1L]]))
  if (length(value) == 0L || is.na(value)) isTRUE(default) else isTRUE(value)
}

osem_shiny_scalar_integer <- function(x, default, minimum = -Inf, maximum = Inf) {
  if (is.null(x) || length(x) == 0L) return(as.integer(default))
  value <- suppressWarnings(as.integer(x[[1L]]))
  if (length(value) == 0L || is.na(value) || value < minimum || value > maximum) {
    as.integer(default)
  } else {
    value
  }
}

osem_shiny_scalar_numeric <- function(x, default, minimum = -Inf, maximum = Inf) {
  if (is.null(x) || length(x) == 0L) return(as.numeric(default))
  value <- suppressWarnings(as.numeric(x[[1L]]))
  if (length(value) == 0L || !is.finite(value) || value < minimum || value > maximum) {
    as.numeric(default)
  } else {
    value
  }
}

osem_shiny_normalise_run_args <- function(args = NULL) {
  defaults <- osem_shiny_default_run_args()
  if (is.null(args) || !is.list(args)) args <- list()
  out <- defaults
  common <- intersect(names(args), names(defaults))
  out[common] <- args[common]

  out$use_logs <- osem_shiny_scalar_character(out$use_logs, defaults$use_logs)
  if (!out$use_logs %in% c("both", "y", "x", "none")) out$use_logs <- defaults$use_logs

  out$trend <- osem_shiny_scalar_logical(out$trend, defaults$trend)
  out$ardl_or_ecm <- osem_shiny_scalar_character(out$ardl_or_ecm, defaults$ardl_or_ecm)
  if (!out$ardl_or_ecm %in% c("ardl", "ecm")) out$ardl_or_ecm <- defaults$ardl_or_ecm

  out$ecm_pretest <- osem_shiny_scalar_character(out$ecm_pretest, defaults$ecm_pretest)
  if (!out$ecm_pretest %in% c("auto", "diagnostic", "none")) {
    out$ecm_pretest <- defaults$ecm_pretest
  }

  out$max.ar <- osem_shiny_scalar_integer(out$max.ar, defaults$max.ar, 0L, 40L)
  out$max.dl <- osem_shiny_scalar_integer(out$max.dl, defaults$max.dl, 0L, 40L)

  saturation <- toupper(as.character(out$saturation %||% character()))
  saturation <- unique(saturation[!is.na(saturation) & saturation %in% c("IIS", "SIS", "TIS")])
  out$saturation <- if (length(saturation) == 0L) NULL else saturation

  out$saturation.tpval <- osem_shiny_scalar_numeric(
    out$saturation.tpval, defaults$saturation.tpval, 0, 1
  )
  out$max.block.size <- osem_shiny_scalar_integer(
    out$max.block.size, defaults$max.block.size, 1L, 10000L
  )
  out$gets_selection <- osem_shiny_scalar_logical(
    out$gets_selection, defaults$gets_selection
  )
  out$selection.tpval <- osem_shiny_scalar_numeric(
    out$selection.tpval, defaults$selection.tpval, 0, 1
  )
  out$constrain.to.minimum.sample <- osem_shiny_scalar_logical(
    out$constrain.to.minimum.sample, defaults$constrain.to.minimum.sample
  )

  keep <- trimws(osem_shiny_scalar_character(out$keep, ""))
  out$keep <- if (nzchar(keep)) keep else NULL
  out$pretest_steps <- osem_shiny_scalar_logical(out$pretest_steps, defaults$pretest_steps)
  out$quiet <- osem_shiny_scalar_logical(out$quiet, defaults$quiet)

  save_to_disk <- trimws(osem_shiny_scalar_character(out$save_to_disk, ""))
  out$save_to_disk <- if (nzchar(save_to_disk)) save_to_disk else NULL

  out$cvar.ar <- osem_shiny_scalar_integer(out$cvar.ar, defaults$cvar.ar, 1L, 40L)
  out$coint_seasonal <- osem_shiny_scalar_logical(
    out$coint_seasonal, defaults$coint_seasonal
  )
  out$coint_deterministic <- osem_shiny_scalar_character(
    out$coint_deterministic, defaults$coint_deterministic
  )
  if (!out$coint_deterministic %in% c("none", "const", "trend")) {
    out$coint_deterministic <- defaults$coint_deterministic
  }
  out$coint_significance <- osem_shiny_scalar_character(
    out$coint_significance, defaults$coint_significance
  )
  if (!out$coint_significance %in% c("1pct", "5pct", "10pct")) {
    out$coint_significance <- defaults$coint_significance
  }
  out$seed <- osem_shiny_scalar_integer(out$seed, defaults$seed, 1L, .Machine$integer.max)
  out
}

osem_shiny_validate_run_args <- function(args, specification = NULL) {
  raw <- if (is.list(args)) args else list()
  settings <- osem_shiny_normalise_run_args(raw)
  issues <- osem_shiny_empty_issues()

  raw_max_ar <- if (is.null(raw$max.ar) || length(raw$max.ar) == 0L) NA_real_ else suppressWarnings(as.numeric(raw$max.ar[[1L]]))
  raw_max_dl <- if (is.null(raw$max.dl) || length(raw$max.dl) == 0L) NA_real_ else suppressWarnings(as.numeric(raw$max.dl[[1L]]))
  if (is.finite(raw_max_ar) && raw_max_ar < 0) {
    issues <- osem_shiny_add_issue(issues, "error", "Estimation settings", "Maximum autoregressive lags cannot be negative.")
  }
  if (is.finite(raw_max_dl) && raw_max_dl < 0) {
    issues <- osem_shiny_add_issue(issues, "error", "Estimation settings", "Maximum distributed lags cannot be negative.")
  }
  if (!is.null(raw$saturation.tpval) && length(raw$saturation.tpval) > 0L) {
    p <- suppressWarnings(as.numeric(raw$saturation.tpval[[1L]]))
    if (!is.finite(p) || p < 0 || p > 1) {
      issues <- osem_shiny_add_issue(issues, "error", "Estimation settings", "The indicator-saturation p-value must be between zero and one.")
    }
  }
  if (!is.null(raw$selection.tpval) && length(raw$selection.tpval) > 0L) {
    p <- suppressWarnings(as.numeric(raw$selection.tpval[[1L]]))
    if (!is.finite(p) || p < 0 || p > 1) {
      issues <- osem_shiny_add_issue(issues, "error", "Estimation settings", "The GETS selection p-value must be between zero and one.")
    }
  }
  if (!isTRUE(settings$gets_selection) && !is.null(settings$keep)) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Estimation settings",
      "A keep expression can only be used when GETS selection is enabled."
    )
  }
  if (isTRUE(settings$pretest_steps) && !"SIS" %in% (settings$saturation %||% character())) {
    issues <- osem_shiny_add_issue(
      issues, "warning", "Estimation settings",
      "Staged saturation pretesting has no effect unless SIS is selected."
    )
  }
  if (identical(settings$ardl_or_ecm, "ardl") && !identical(settings$ecm_pretest, "auto")) {
    issues <- osem_shiny_add_issue(
      issues, "info", "Estimation settings",
      "The ECM pretest setting is ignored for an ARDL run."
    )
  }
  if (!is.null(settings$save_to_disk)) {
    extension <- tolower(tools::file_ext(settings$save_to_disk))
    if (!extension %in% c("rds", "csv", "xls", "xlsx")) {
      issues <- osem_shiny_add_issue(
        issues, "error", "Estimation settings",
        "The optional server-side processed-data path must end in .rds, .csv, .xls, or .xlsx."
      )
    }
  }

  has_cvar <- FALSE
  if (is.data.frame(specification) && "cvar" %in% names(specification)) {
    cvar <- trimws(as.character(specification$cvar))
    has_cvar <- any(!is.na(cvar) & nzchar(cvar))
  }
  if (has_cvar) {
    issues <- osem_shiny_add_issue(
      issues, "info", "CVAR settings",
      paste0(
        "CVAR systems will use lag order ", settings$cvar.ar,
        ", deterministic term '", settings$coint_deterministic,
        "', and the ", settings$coint_significance, " rank-test threshold."
      )
    )
  } else if (isTRUE(settings$coint_seasonal)) {
    issues <- osem_shiny_add_issue(
      issues, "warning", "CVAR settings",
      "Seasonal cointegration dummies are enabled, but the specification contains no CVAR system."
    )
  }

  if (!any(issues$level == "error")) {
    issues <- osem_shiny_add_issue(
      issues, "success", "Estimation settings",
      "The estimation settings are internally consistent."
    )
  }

  list(
    valid = !any(issues$level == "error"),
    settings = settings,
    issues = issues
  )
}

osem_shiny_build_run_call_args <- function(specification,
                                           dictionary,
                                           input,
                                           primary_source,
                                           run_args) {
  settings <- osem_shiny_normalise_run_args(run_args)
  settings$seed <- NULL
  c(
    list(
      specification = osem_shiny_normalise_specification(specification),
      dictionary = osem_shiny_normalise_dictionary(dictionary),
      input = input,
      primary_source = primary_source
    ),
    settings,
    list(
      present = FALSE,
      plot = FALSE
    )
  )
}

osem_shiny_capture_conditions <- function(fun, seed = NULL) {
  if (!is.function(fun)) stop("'fun' must be a function.", call. = FALSE)
  warnings <- character()
  messages <- character()
  value <- NULL
  error <- NULL
  started <- Sys.time()

  console <- capture.output({
    value <- tryCatch(
      withCallingHandlers(
        {
          if (!is.null(seed)) set.seed(as.integer(seed))
          fun()
        },
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      ),
      error = function(e) {
        error <<- conditionMessage(e)
        NULL
      }
    )
  }, type = "output")

  finished <- Sys.time()
  list(
    ok = is.null(error),
    value = value,
    error = error,
    warnings = unique(warnings[nzchar(warnings)]),
    messages = unique(messages[nzchar(messages)]),
    log = console,
    started = started,
    finished = finished,
    duration_seconds = as.numeric(difftime(finished, started, units = "secs"))
  )
}

osem_shiny_execute_model <- function(specification,
                                     dictionary,
                                     input,
                                     primary_source,
                                     run_args) {
  settings_validation <- osem_shiny_validate_run_args(
    run_args,
    specification = specification
  )
  if (!isTRUE(settings_validation$valid)) {
    errors <- settings_validation$issues$message[
      settings_validation$issues$level == "error"
    ]
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
      settings = settings_validation$settings
    ))
  }

  call_args <- osem_shiny_build_run_call_args(
    specification = specification,
    dictionary = dictionary,
    input = input,
    primary_source = primary_source,
    run_args = settings_validation$settings
  )
  seed <- settings_validation$settings$seed
  captured <- osem_shiny_capture_conditions(
    function() do.call(run_model, call_args),
    seed = seed
  )
  captured$call_args <- call_args
  captured$settings <- settings_validation$settings
  captured
}

osem_shiny_run_argument_table <- function(run_args,
                                          primary_source = "local",
                                          input_count = 0L) {
  args <- osem_shiny_normalise_run_args(run_args)
  values <- list(
    primary_source = primary_source,
    input = if (input_count == 0L) "NULL" else paste0(input_count, " local data object(s)"),
    use_logs = args$use_logs,
    trend = args$trend,
    ardl_or_ecm = args$ardl_or_ecm,
    ecm_pretest = args$ecm_pretest,
    max.ar = args$max.ar,
    max.dl = args$max.dl,
    saturation = args$saturation %||% "NULL",
    saturation.tpval = args$saturation.tpval,
    max.block.size = args$max.block.size,
    gets_selection = args$gets_selection,
    selection.tpval = args$selection.tpval,
    constrain.to.minimum.sample = args$constrain.to.minimum.sample,
    keep = args$keep %||% "NULL",
    pretest_steps = args$pretest_steps,
    save_to_disk = args$save_to_disk %||% "NULL",
    quiet = args$quiet,
    cvar.ar = args$cvar.ar,
    coint_seasonal = args$coint_seasonal,
    coint_deterministic = args$coint_deterministic,
    coint_significance = args$coint_significance,
    seed = args$seed,
    present = FALSE,
    plot = FALSE
  )
  data.frame(
    Argument = names(values),
    Value = vapply(values, function(x) paste(as.character(x), collapse = ", "), character(1L)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_run_summary_text <- function(run_args, specification = NULL) {
  args <- osem_shiny_normalise_run_args(run_args)
  model_form <- if (identical(args$ardl_or_ecm, "ecm")) {
    paste0("an ECM workflow with '", args$ecm_pretest, "' pretesting")
  } else {
    "a levels ARDL workflow"
  }
  logs <- switch(
    args$use_logs,
    both = "dependent and conditioning variables may be transformed",
    y = "dependent variables may be transformed",
    x = "conditioning variables may be transformed",
    none = "log transformations are disabled"
  )
  saturation <- if (is.null(args$saturation)) {
    "indicator saturation is disabled"
  } else {
    paste0(
      paste(args$saturation, collapse = ", "),
      " saturation is enabled at p = ", format(args$saturation.tpval)
    )
  }
  selection <- if (isTRUE(args$gets_selection)) {
    paste0("GETS selection is enabled at p = ", format(args$selection.tpval))
  } else {
    "GETS selection is disabled"
  }
  cvar_count <- 0L
  if (is.data.frame(specification) && "cvar" %in% names(specification)) {
    cvar <- trimws(as.character(specification$cvar))
    cvar_count <- length(unique(cvar[!is.na(cvar) & nzchar(cvar)]))
  }

  paste0(
    "OSEM will estimate ", model_form, ". Up to ", args$max.ar,
    " autoregressive and ", args$max.dl, " distributed lags will be considered; ",
    logs, ". A deterministic trend is ", if (isTRUE(args$trend)) "included" else "not included",
    ", ", saturation, ", and ", selection, ". ",
    if (isTRUE(args$constrain.to.minimum.sample)) {
      "All series will be constrained to their common minimum sample."
    } else {
      "Each module may use its available sample."
    },
    if (cvar_count > 0L) {
      paste0(
        " The specification contains ", cvar_count,
        " CVAR system(s), using lag order ", args$cvar.ar,
        " and deterministic term '", args$coint_deterministic, "'."
      )
    } else ""
  )
}

osem_shiny_revision_stamp <- function(state) {
  list(
    data = as.integer(state$data_revision),
    specification = as.integer(state$specification_revision),
    dictionary = as.integer(state$dictionary_revision),
    settings = as.integer(state$settings_revision),
    generated_at = Sys.time()
  )
}
