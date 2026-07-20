# OSEM Shiny model-result helpers -------------------------------------------

osem_shiny_safe_data_frame <- function(value) {
  if (is.null(value)) return(data.frame())
  tryCatch(
    as.data.frame(value, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
}

osem_shiny_model_summary <- function(model) {
  if (is.null(model) || !inherits(model, "osem")) return(list())
  module_order <- osem_shiny_safe_data_frame(model$module_order)
  full_data <- osem_shiny_safe_data_frame(model$full_data)
  processed_data <- osem_shiny_safe_data_frame(model$processed_input_data)
  dates <- if ("time" %in% names(full_data)) {
    tryCatch(osem_shiny_parse_time(full_data$time), error = function(e) as.Date(character()))
  } else {
    as.Date(character())
  }
  dates <- dates[!is.na(dates)]
  cvar <- if ("cvar" %in% names(module_order)) {
    trimws(as.character(module_order$cvar))
  } else {
    character()
  }
  module_type <- if ("type" %in% names(module_order)) {
    as.character(module_order$type)
  } else {
    character()
  }
  frequency <- osem_shiny_model_frequency(model)
  list(
    modules = nrow(module_order),
    estimated = sum(module_type == "n", na.rm = TRUE),
    identities = sum(module_type == "d", na.rm = TRUE),
    cvar_systems = length(unique(cvar[!is.na(cvar) & nzchar(cvar)])),
    input_rows = nrow(processed_data),
    full_rows = nrow(full_data),
    variables = if ("na_item" %in% names(full_data)) {
      values <- trimws(as.character(full_data$na_item))
      length(unique(values[!is.na(values) & nzchar(values)]))
    } else {
      0L
    },
    start = if (length(dates) > 0L) min(dates) else as.Date(NA),
    end = if (length(dates) > 0L) max(dates) else as.Date(NA),
    frequency = frequency$label %||% "Unknown"
  )
}

osem_shiny_model_diagnostics <- function(model) {
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  out <- tryCatch(diagnostics_model(model), error = function(e) e)
  if (inherits(out, "error")) {
    return(data.frame(
      module = NA_character_,
      diagnostic_error = conditionMessage(out),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  if (!is.data.frame(out)) {
    return(data.frame(
      module = NA_character_,
      diagnostic_error = "diagnostics_model() did not return a data frame.",
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

osem_shiny_module_object <- function(model, row_index) {
  if (is.null(model) || !inherits(model, "osem")) return(NULL)
  collection <- model$module_collection
  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.data.frame(collection) || is.na(row_index) || row_index < 1L ||
      row_index > nrow(collection) || !"model" %in% names(collection)) {
    return(NULL)
  }
  tryCatch(collection$model[[row_index]], error = function(e) NULL)
}

osem_shiny_module_args <- function(model, row_index) {
  if (is.null(model) || !inherits(model, "osem")) return(NULL)
  collection <- model$module_collection
  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.data.frame(collection) || is.na(row_index) || row_index < 1L ||
      row_index > nrow(collection) || !"model.args" %in% names(collection)) {
    return(NULL)
  }
  tryCatch(collection$model.args[[row_index]], error = function(e) NULL)
}

osem_shiny_module_dataset <- function(model, row_index) {
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  collection <- model$module_collection
  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.data.frame(collection) || is.na(row_index) || row_index < 1L ||
      row_index > nrow(collection) || !"dataset" %in% names(collection)) {
    return(data.frame())
  }
  out <- tryCatch(collection$dataset[[row_index]], error = function(e) NULL)
  osem_shiny_safe_data_frame(out)
}

osem_shiny_module_kind <- function(module_row, object = NULL) {
  if (is.data.frame(module_row) && nrow(module_row) > 0L && identical(as.character(module_row$type[[1L]]), "d")) {
    return("Identity")
  }
  if (inherits(object, "osem.cvar")) return("CVAR system")
  "Estimated equation"
}

osem_shiny_module_selected_form <- function(module_args, object = NULL) {
  if (inherits(object, "osem.cvar")) return("CVAR")
  if (is.null(module_args) || !is.list(module_args)) return(NA_character_)
  selected <- module_args$ardl_or_ecm_selected %||%
    module_args$model_form %||%
    module_args$ardl_or_ecm %||%
    NA_character_
  selected <- as.character(selected)
  if (length(selected) == 0L) NA_character_ else toupper(selected[[1L]])
}

osem_shiny_module_observations <- function(object, dataset = NULL) {
  valid_n <- function(value) {
    length(value) == 1L && !is.na(value) && is.finite(value) && value >= 0L
  }
  if (inherits(object, "osem.cvar")) {
    n <- tryCatch(as.integer(object$varm$obs)[[1L]], error = function(e) NA_integer_)
    if (valid_n(n)) return(n)
  }
  n <- tryCatch(as.integer(object$n)[[1L]], error = function(e) NA_integer_)
  if (valid_n(n)) return(n)
  n <- tryCatch(as.integer(stats::nobs(object))[[1L]], error = function(e) NA_integer_)
  if (valid_n(n)) return(n)
  if (is.data.frame(dataset)) return(nrow(dataset))
  NA_integer_
}

osem_shiny_module_overview <- function(model) {
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  collection <- model$module_collection
  required <- c("type", "dependent")
  if (!is.data.frame(collection) || nrow(collection) == 0L ||
      !all(required %in% names(collection))) {
    return(data.frame())
  }
  diagnostics <- osem_shiny_model_diagnostics(model)
  rows <- vector("list", nrow(collection))

  for (i in seq_len(nrow(collection))) {
    row <- collection[i, , drop = FALSE]
    object <- osem_shiny_module_object(model, i)
    args <- osem_shiny_module_args(model, i)
    dataset <- osem_shiny_module_dataset(model, i)
    dep <- trimws(osem_shiny_scalar_character(row$dependent, paste0("Module ", i)))
    if (!nzchar(dep)) dep <- paste0("Module ", i)
    diag_row <- if (nrow(diagnostics) > 0L && "module" %in% names(diagnostics)) {
      diagnostics[!is.na(diagnostics$module) & as.character(diagnostics$module) == dep, , drop = FALSE]
    } else {
      data.frame()
    }
    ar <- if (nrow(diag_row) > 0L && "AR" %in% names(diag_row)) {
      suppressWarnings(as.numeric(diag_row$AR[[1L]]))
    } else {
      NA_real_
    }
    arch <- if (nrow(diag_row) > 0L && "ARCH" %in% names(diag_row)) {
      suppressWarnings(as.numeric(diag_row$ARCH[[1L]]))
    } else {
      NA_real_
    }
    type <- osem_shiny_scalar_character(row$type, "")
    diagnostic_status <- if (identical(type, "d")) {
      "Not applicable"
    } else if ((!is.na(ar) && ar < 0.05) || (!is.na(arch) && arch < 0.05)) {
      "Review"
    } else if (is.na(ar) && is.na(arch)) {
      "Unavailable"
    } else {
      "No 5% rejection"
    }
    rows[[i]] <- data.frame(
      Row = i,
      Order = if ("order" %in% names(row)) {
        osem_shiny_scalar_integer(row$order, i, 1L, .Machine$integer.max)
      } else {
        i
      },
      Module = dep,
      Kind = osem_shiny_module_kind(row, object),
      Form = osem_shiny_module_selected_form(args, object),
      Observations = osem_shiny_module_observations(object, dataset),
      `AR p-value` = ar,
      `ARCH p-value` = arch,
      `Diagnostic status` = diagnostic_status,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (length(rows) == 0L) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


osem_shiny_module_choices <- function(model) {
  overview <- osem_shiny_module_overview(model)
  if (nrow(overview) == 0L) return(character())
  labels <- paste0(overview$Order, ". ", overview$Module, " — ", overview$Kind)
  stats::setNames(as.character(overview$Row), labels)
}

osem_shiny_module_equation <- function(model, row_index) {
  if (is.null(model) || !inherits(model, "osem")) return("")
  collection <- model$module_collection
  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.data.frame(collection) || is.na(row_index) || row_index < 1L ||
      row_index > nrow(collection) || !all(c("dependent", "independent") %in% names(collection))) {
    return("")
  }
  row <- collection[row_index, , drop = FALSE]
  dep <- osem_shiny_scalar_character(row$dependent, paste0("Module ", row_index))
  rhs <- trimws(osem_shiny_scalar_character(row$independent, ""))
  if (!nzchar(rhs)) rhs <- "own lags only"
  paste0(dep, " = ", rhs)
}

osem_shiny_lm_coefficient_table <- function(object, equation = NULL) {
  if (is.null(object)) return(data.frame())
  summary_object <- if (is.list(object) && !is.null(object$coefficients)) {
    object
  } else {
    tryCatch(summary(object), error = function(e) NULL)
  }
  matrix <- if (!is.null(summary_object) && !is.null(summary_object$coefficients)) {
    summary_object$coefficients
  } else NULL
  if (is.null(matrix)) {
    coefficients <- tryCatch(stats::coef(object), error = function(e) NULL)
    if (is.null(coefficients)) return(data.frame())
    matrix <- cbind(Estimate = as.numeric(coefficients))
    rownames(matrix) <- names(coefficients)
  }
  matrix <- as.matrix(matrix)
  if (nrow(matrix) == 0L) return(data.frame())
  term_names <- rownames(matrix)
  if (is.null(term_names) || length(term_names) != nrow(matrix)) {
    term_names <- paste0("Term ", seq_len(nrow(matrix)))
  }
  output <- data.frame(
    term = term_names,
    estimate = as.numeric(matrix[, 1L]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  output$std.error <- if (ncol(matrix) >= 2L) as.numeric(matrix[, 2L]) else NA_real_
  output$statistic <- if (ncol(matrix) >= 3L) as.numeric(matrix[, 3L]) else NA_real_
  output$p.value <- if (ncol(matrix) >= 4L) as.numeric(matrix[, 4L]) else NA_real_
  if (!is.null(equation)) output$equation <- equation
  output
}

osem_shiny_safe_column_name <- function(x, index, fallback) {
  names <- colnames(x)
  if (is.null(names) || length(names) < index || is.na(names[[index]]) || !nzchar(names[[index]])) {
    fallback
  } else {
    names[[index]]
  }
}

osem_shiny_cvar_coefficient_table <- function(object) {
  if (!inherits(object, "osem.cvar")) return(data.frame())
  rlm <- tryCatch(object$vecm$rlm, error = function(e) NULL)
  if (is.null(rlm)) return(data.frame())

  summaries <- tryCatch(summary(rlm), error = function(e) NULL)
  if (is.list(summaries) && !is.null(summaries[[1L]]) &&
      is.list(summaries[[1L]]) && !is.null(summaries[[1L]]$coefficients)) {
    equation_names <- names(summaries)
    if (is.null(equation_names) || any(!nzchar(equation_names))) {
      equation_names <- paste0("Equation ", seq_along(summaries))
    }
    pieces <- lapply(seq_along(summaries), function(i) {
      osem_shiny_lm_coefficient_table(summaries[[i]], equation = equation_names[[i]])
    })
    pieces <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, pieces)
    if (length(pieces) > 0L) {
      out <- do.call(rbind, pieces)
      rownames(out) <- NULL
      return(out)
    }
  }

  coefficients <- tryCatch(stats::coef(rlm), error = function(e) NULL)
  if (is.matrix(coefficients)) {
    term_names <- rownames(coefficients)
    if (is.null(term_names) || length(term_names) != nrow(coefficients)) {
      term_names <- paste0("Term ", seq_len(nrow(coefficients)))
    }
    rows <- vector("list", ncol(coefficients))
    for (i in seq_len(ncol(coefficients))) {
      rows[[i]] <- data.frame(
        term = term_names,
        estimate = as.numeric(coefficients[, i]),
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        equation = osem_shiny_safe_column_name(coefficients, i, paste0("Equation ", i)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  data.frame()
}

osem_shiny_module_coefficients <- function(model, row_index) {
  object <- osem_shiny_module_object(model, as.integer(row_index))
  if (is.null(object)) return(data.frame())
  if (inherits(object, "osem.cvar")) return(osem_shiny_cvar_coefficient_table(object))

  lm_object <- if (inherits(object, "lm")) {
    object
  } else {
    tryCatch(gets::as.lm(object), error = function(e) NULL)
  }
  if (!is.null(lm_object)) return(osem_shiny_lm_coefficient_table(lm_object))

  mean_results <- tryCatch(object$mean.results, error = function(e) NULL)
  if (is.matrix(mean_results) || is.data.frame(mean_results)) {
    matrix <- as.matrix(mean_results)
    term_names <- rownames(matrix)
    if (is.null(term_names) || length(term_names) != nrow(matrix)) {
      term_names <- paste0("Term ", seq_len(nrow(matrix)))
    }
    out <- data.frame(
      term = term_names,
      estimate = as.numeric(matrix[, 1L]),
      std.error = if (ncol(matrix) >= 2L) as.numeric(matrix[, 2L]) else NA_real_,
      statistic = if (ncol(matrix) >= 3L) as.numeric(matrix[, 3L]) else NA_real_,
      p.value = if (ncol(matrix) >= 4L) as.numeric(matrix[, 4L]) else NA_real_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(out)
  }
  data.frame()
}

osem_shiny_module_terms <- function(model, row_index) {
  object <- osem_shiny_module_object(model, as.integer(row_index))
  if (is.null(object) || inherits(object, "osem.cvar")) {
    return(data.frame(Category = character(), Term = character(), stringsAsFactors = FALSE))
  }
  candidate <- tryCatch(as.character(object$aux$mXnames), error = function(e) character())
  retained <- osem_shiny_module_coefficients(model, row_index)$term %||% character()
  retained <- unique(as.character(retained))
  candidate <- unique(candidate[!is.na(candidate) & nzchar(candidate)])
  dropped <- setdiff(candidate, retained)
  indicator_pattern <- "^(iis|sis|tis)[[:digit:]]+"
  category <- function(term, retained_flag) {
    if (grepl(indicator_pattern, term, ignore.case = TRUE)) {
      if (retained_flag) "Retained indicator" else "Dropped indicator"
    } else if (retained_flag) {
      "Retained regressor"
    } else {
      "Dropped regressor"
    }
  }
  rows <- c(
    lapply(retained, function(term) data.frame(Category = category(term, TRUE), Term = term, stringsAsFactors = FALSE)),
    lapply(dropped, function(term) data.frame(Category = category(term, FALSE), Term = term, stringsAsFactors = FALSE))
  )
  if (length(rows) == 0L) return(data.frame(Category = character(), Term = character(), stringsAsFactors = FALSE))
  out <- do.call(rbind, rows)
  out <- out[order(out$Category, out$Term), , drop = FALSE]
  rownames(out) <- NULL
  out
}

osem_shiny_module_ecm_decision <- function(model, row_index) {
  args <- osem_shiny_module_args(model, as.integer(row_index))
  if (is.null(args) || !is.list(args) || is.null(args$ecm_decision)) return(data.frame())
  decision <- args$ecm_decision
  if (!is.list(decision)) decision <- list(reason = paste(as.character(decision), collapse = ", "))
  data.frame(
    Field = c("Requested form", "Pretest", "Selected form", "Model form", "Reason"),
    Value = c(
      osem_shiny_scalar_character(args$ardl_or_ecm_requested, NA_character_),
      osem_shiny_scalar_character(decision$pretest %||% args$ecm_pretest, NA_character_),
      osem_shiny_scalar_character(decision$selected %||% args$ardl_or_ecm_selected, NA_character_),
      osem_shiny_scalar_character(decision$model_form %||% args$model_form, NA_character_),
      osem_shiny_scalar_character(decision$reason, NA_character_)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_cvar_summary <- function(object) {
  if (!inherits(object, "osem.cvar")) return(data.frame())
  rank <- tryCatch(
    suppressWarnings(as.integer(object$rank)[[1L]]),
    error = function(e) NA_integer_
  )
  teststat_object <- tryCatch(object$cointtest@teststat, error = function(e) NULL)
  teststat <- if (is.null(teststat_object)) numeric() else suppressWarnings(as.numeric(teststat_object))
  critical <- tryCatch(
    as.matrix(object$cointtest@cval),
    error = function(e) matrix(numeric(), 0L, 0L)
  )
  labels <- names(teststat_object)
  if (is.null(labels) || length(labels) != length(teststat)) {
    labels <- paste0("Trace test ", seq_along(teststat))
  }
  if (length(teststat) == 0L) {
    return(data.frame(
      Field = "Selected rank",
      Value = rank,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  rows <- data.frame(
    Test = labels,
    Statistic = teststat,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (nrow(critical) == nrow(rows)) {
    for (column in intersect(c("1pct", "5pct", "10pct"), colnames(critical))) {
      rows[[paste0("Critical ", column)]] <- as.numeric(critical[, column])
    }
  }
  rows$`Selected rank` <- rep(rank, nrow(rows))
  rows
}

osem_shiny_model_plot_data <- function(model, include_exogenous = FALSE) {
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  out <- tryCatch(
    plot(model, exclude.exogenous = !isTRUE(include_exogenous), return.data = TRUE),
    error = function(e) NULL
  )
  if (is.data.frame(out)) {
    out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
    if ("time" %in% names(out)) {
      out$time <- tryCatch(osem_shiny_parse_time(out$time), error = function(e) as.Date(rep(NA_character_, nrow(out))))
    }
    return(out)
  }
  data <- osem_shiny_safe_data_frame(model$full_data)
  if (!all(c("na_item", "time", "values") %in% names(data))) return(data.frame())
  original_names <- as.character(data$na_item)
  parsed_time <- tryCatch(
    osem_shiny_parse_time(data$time),
    error = function(e) as.Date(rep(NA_character_, nrow(data)))
  )
  data.frame(
    time = parsed_time,
    na_item = sub("\\.hat$", "", original_names),
    values = suppressWarnings(as.numeric(as.character(data$values))),
    type = ifelse(grepl("\\.hat$", original_names), "Insample Fit", "Observation"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_module_plot_data <- function(model, row_index) {
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  collection <- model$module_collection
  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.data.frame(collection) || is.na(row_index) || row_index < 1L ||
      row_index > nrow(collection) || !"dependent" %in% names(collection)) {
    return(data.frame())
  }
  dep <- osem_shiny_scalar_character(collection$dependent[[row_index]], "")
  variables <- trimws(unlist(strsplit(dep, ",", fixed = TRUE)))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  data <- osem_shiny_model_plot_data(model, include_exogenous = TRUE)
  if (nrow(data) == 0L || !"na_item" %in% names(data)) return(data.frame())
  data[as.character(data$na_item) %in% variables, , drop = FALSE]
}

osem_shiny_module_residual_data <- function(model, row_index) {
  object <- osem_shiny_module_object(model, as.integer(row_index))
  if (is.null(object)) return(data.frame())
  if (inherits(object, "osem.cvar")) {
    residuals <- tryCatch(as.matrix(object$varm$resid), error = function(e) NULL)
    if (is.null(residuals)) return(data.frame())
    time <- tryCatch(as.Date(model$module_collection$dataset[[as.integer(row_index)]]$time), error = function(e) as.Date(character()))
    if (length(time) >= nrow(residuals)) time <- utils::tail(time, nrow(residuals)) else time <- seq_len(nrow(residuals))
    rows <- lapply(seq_len(ncol(residuals)), function(i) {
      data.frame(
        time = time,
        equation = osem_shiny_safe_column_name(residuals, i, paste0("Equation ", i)),
        residual = as.numeric(residuals[, i]),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  residuals <- tryCatch(as.numeric(stats::residuals(object)), error = function(e) numeric())
  if (length(residuals) == 0L) {
    lm_object <- tryCatch(gets::as.lm(object), error = function(e) NULL)
    residuals <- if (is.null(lm_object)) numeric() else tryCatch(as.numeric(stats::residuals(lm_object)), error = function(e) numeric())
  }
  if (length(residuals) == 0L) return(data.frame())
  time <- tryCatch(as.Date(object$aux$y.index), error = function(e) as.Date(character()))
  if (length(time) != length(residuals)) time <- seq_along(residuals)
  data.frame(time = time, equation = "Residual", residual = residuals, stringsAsFactors = FALSE)
}

osem_shiny_module_argument_table <- function(model, row_index) {
  args <- osem_shiny_module_args(model, as.integer(row_index))
  if (is.null(args) || !is.list(args)) return(data.frame())
  flatten <- function(value) {
    if (is.null(value)) return("NULL")
    if (is.atomic(value) && length(value) <= 20L) return(paste(as.character(value), collapse = ", "))
    if (is.data.frame(value)) return(paste0(nrow(value), " x ", ncol(value), " data frame"))
    if (is.list(value)) return(paste0("list (", length(value), " element(s))"))
    paste0("object of class ", paste(class(value), collapse = "/"))
  }
  argument_names <- names(args)
  if (is.null(argument_names) || length(argument_names) != length(args)) {
    argument_names <- paste0("Argument ", seq_along(args))
  }
  data.frame(
    Argument = argument_names,
    Value = vapply(args, flatten, character(1L)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

osem_shiny_model_data_export <- function(model, which = c("processed", "full")) {
  which <- match.arg(which)
  if (is.null(model) || !inherits(model, "osem")) return(data.frame())
  data <- if (identical(which, "processed")) model$processed_input_data else model$full_data
  osem_shiny_safe_data_frame(data)
}

osem_shiny_model_network_plot <- function(model, layout = "kk") {
  if (is.null(model) || !inherits(model, "osem")) return(NULL)
  if (requireNamespace("ggraph", quietly = TRUE) && requireNamespace("tidygraph", quietly = TRUE)) {
    out <- tryCatch(network(model, layout = layout), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  module_order <- osem_shiny_safe_data_frame(model$module_order)
  if (nrow(module_order) == 0L) return(NULL)
  tryCatch(
    osem_shiny_plot_specification_graph(module_order),
    error = function(e) NULL
  )
}
