# OSEM Shiny validation -----------------------------------------------------

osem_shiny_empty_issues <- function() {
  data.frame(
    level = character(),
    area = character(),
    message = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
}

osem_shiny_add_issue <- function(issues,
                                 level = c("error", "warning", "info", "success"),
                                 area,
                                 message,
                                 source = NA_character_) {
  level <- match.arg(level)
  row <- data.frame(
    level = level,
    area = as.character(area),
    message = as.character(message),
    source = as.character(source %||% NA_character_),
    stringsAsFactors = FALSE
  )
  out <- rbind(issues, row)
  rownames(out) <- NULL
  out
}

osem_shiny_bind_issues <- function(...) {
  items <- list(...)
  items <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, items)
  if (length(items) == 0L) {
    return(osem_shiny_empty_issues())
  }
  out <- do.call(rbind, items)
  rownames(out) <- NULL
  out
}

osem_shiny_issue_counts <- function(issues) {
  if (!is.data.frame(issues) || nrow(issues) == 0L) {
    return(list(error = 0L, warning = 0L, info = 0L, success = 0L))
  }
  list(
    error = sum(issues$level == "error"),
    warning = sum(issues$level == "warning"),
    info = sum(issues$level == "info"),
    success = sum(issues$level == "success")
  )
}

osem_shiny_normalise_specification <- function(specification) {
  if (!is.data.frame(specification) && !is.matrix(specification)) {
    stop("Specification must be a data frame, tibble, or matrix.", call. = FALSE)
  }
  specification <- as.data.frame(
    specification,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required_base <- c("type", "dependent", "independent")
  missing <- setdiff(required_base, names(specification))
  if (length(missing) > 0L) {
    stop(
      paste0("Specification is missing: ", paste(missing, collapse = ", "), "."),
      call. = FALSE
    )
  }
  if (!"lag" %in% names(specification)) specification$lag <- ""
  if (!"cvar" %in% names(specification)) specification$cvar <- ""
  specification <- specification[, c("type", "dependent", "independent", "lag", "cvar"), drop = FALSE]

  for (name in names(specification)) {
    values <- as.character(specification[[name]])
    values[is.na(values)] <- ""
    specification[[name]] <- trimws(values)
  }
  specification$type <- tolower(specification$type)

  # Spreadsheet uploads often contain empty formatted rows at the end.
  fully_empty <- apply(specification, 1L, function(row) all(row == ""))
  if (any(fully_empty)) {
    specification <- specification[!fully_empty, , drop = FALSE]
  }
  rownames(specification) <- NULL
  specification
}

osem_shiny_validate_specification <- function(specification) {
  issues <- osem_shiny_empty_issues()
  normalised <- tryCatch(
    osem_shiny_normalise_specification(specification),
    error = function(e) e
  )
  if (inherits(normalised, "error")) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Specification", conditionMessage(normalised)
    )
    return(list(data = NULL, issues = issues, valid = FALSE, ordered = NULL))
  }

  if (nrow(normalised) == 0L) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Specification", "The specification contains no modules."
    )
  }
  invalid_type <- !(normalised$type %in% c("n", "d"))
  if (any(invalid_type)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      paste0(sum(invalid_type), " module(s) have a type other than 'n' or 'd'.")
    )
  }
  blank_dependent <- normalised$dependent == ""
  if (any(blank_dependent)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      paste0(sum(blank_dependent), " module(s) have no dependent variable.")
    )
  }
  duplicated_dependent <- duplicated(normalised$dependent) & normalised$dependent != ""
  if (any(duplicated_dependent)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      paste0(
        "Dependent variables must be unique. Duplicates: ",
        paste(unique(normalised$dependent[duplicated_dependent]), collapse = ", "), "."
      )
    )
  }
  illegal_dependent <- grepl("[-+*/^]", normalised$dependent)
  if (any(illegal_dependent)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      "Dependent variable names cannot contain +, -, /, *, or ^."
    )
  }
  estimated <- normalised$type == "n"
  illegal_estimated_rhs <- estimated & grepl("[-*/^]", normalised$independent)
  if (any(illegal_estimated_rhs)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      "Estimated equations may separate regressors with '+', but cannot contain -, /, *, or ^."
    )
  }
  blank_identity_rhs <- normalised$type == "d" & normalised$independent == ""
  if (any(blank_identity_rhs)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      paste0(sum(blank_identity_rhs), " identity module(s) have no right-hand side.")
    )
  }
  identity_with_lag <- normalised$type == "d" & nzchar(normalised$lag)
  if (any(identity_with_lag)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      "Lag-only regressors can only be assigned to estimated equations."
    )
  }
  identity_with_cvar <- normalised$type == "d" & nzchar(normalised$cvar)
  if (any(identity_with_cvar)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      "Accounting identities cannot be assigned to a CVAR system."
    )
  }

  estimated_rows <- which(normalised$type == "n" & nzchar(normalised$lag))
  invalid_lag_rows <- integer()
  invalid_lag_variables <- character()
  for (row in estimated_rows) {
    regressors <- trimws(unlist(strsplit(normalised$independent[[row]], "+", fixed = TRUE)))
    regressors <- regressors[nzchar(regressors)]
    lag_only <- trimws(unlist(strsplit(normalised$lag[[row]], "[+,;[:space:]]+", perl = TRUE)))
    lag_only <- lag_only[nzchar(lag_only)]
    missing_lag <- setdiff(lag_only, regressors)
    if (length(missing_lag) > 0L) {
      invalid_lag_rows <- c(invalid_lag_rows, row)
      invalid_lag_variables <- c(invalid_lag_variables, missing_lag)
    }
  }
  if (length(invalid_lag_rows) > 0L) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Specification",
      paste0(
        "Every lag-only variable must also appear on that equation's right-hand side. ",
        "Problem variable(s): ",
        paste(unique(invalid_lag_variables), collapse = ", "), "."
      )
    )
  }

  cvar_labels <- normalised$cvar[nzchar(normalised$cvar)]
  if (length(cvar_labels) > 0L) {
    cvar_counts <- table(cvar_labels)
    singleton_cvar <- names(cvar_counts)[cvar_counts < 2L]
    if (length(singleton_cvar) > 0L) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Specification",
        paste0(
          "Each CVAR system must contain at least two dependent variables. ",
          "Single-member system(s): ", paste(singleton_cvar, collapse = ", "), "."
        )
      )
    }
  }

  ordered <- NULL
  if (!any(issues$level == "error")) {
    if (!requireNamespace("purrr", quietly = TRUE)) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Environment",
        "Package 'purrr' is required by OSEM's specification-ordering code."
      )
    } else {
      ordered <- tryCatch(
        check_config_table(normalised, quiet = TRUE),
        error = function(e) e
      )
      if (inherits(ordered, "error")) {
        issues <- osem_shiny_add_issue(
          issues,
          "error",
          "Specification",
          conditionMessage(ordered)
        )
        ordered <- NULL
      }
    }
  }

  if (!any(issues$level == "error")) {
    issues <- osem_shiny_add_issue(
      issues,
      "success",
      "Specification",
      paste0("Specification is valid and contains ", nrow(normalised), " module(s).")
    )
  }

  list(
    data = normalised,
    issues = issues,
    valid = !any(issues$level == "error"),
    ordered = ordered
  )
}

osem_shiny_normalise_dictionary <- function(dictionary) {
  if (!is.data.frame(dictionary)) {
    stop("Dictionary must be a data frame or tibble.", call. = FALSE)
  }
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE, check.names = FALSE)
  if ("model_varname" %in% names(dictionary)) {
    dictionary$model_varname <- trimws(as.character(dictionary$model_varname))
  }
  if ("database" %in% names(dictionary)) {
    database <- tolower(trimws(as.character(dictionary$database)))
    database[database == ""] <- NA_character_
    dictionary$database <- database
  }
  dictionary
}

osem_shiny_validate_dictionary <- function(dictionary) {
  issues <- osem_shiny_empty_issues()
  if (!is.data.frame(dictionary)) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Dictionary", "Dictionary must be a data frame or tibble."
    )
    return(list(data = NULL, issues = issues, valid = FALSE))
  }
  dictionary <- osem_shiny_normalise_dictionary(dictionary)
  required <- c("model_varname", "full_name", "database", "dataset_id", "freq")
  missing <- setdiff(required, names(dictionary))
  if (length(missing) > 0L) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Dictionary",
      paste0("Dictionary is missing: ", paste(missing, collapse = ", "), ".")
    )
    return(list(data = dictionary, issues = issues, valid = FALSE))
  }

  model_names <- trimws(as.character(dictionary$model_varname))
  blank <- is.na(model_names) | model_names == ""
  if (any(blank)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Dictionary",
      paste0(sum(blank), " dictionary row(s) have no model_varname.")
    )
  }
  duplicates <- duplicated(model_names) & !blank
  if (any(duplicates)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Dictionary",
      paste0(
        "model_varname must be unique. Duplicates: ",
        paste(unique(model_names[duplicates]), collapse = ", "), "."
      )
    )
  }
  illegal <- grepl("[-+*/^]", model_names)
  if (any(illegal, na.rm = TRUE)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Dictionary",
      "model_varname cannot contain +, -, /, *, or ^."
    )
  }

  databases <- tolower(trimws(as.character(dictionary$database)))
  databases[databases == ""] <- NA_character_
  unsupported <- !is.na(databases) &
    !(databases %in% c("eurostat", "edgar", "local", "statcan", "imf"))
  if (any(unsupported)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Dictionary",
      paste0(
        "Unsupported database value(s): ",
        paste(unique(databases[unsupported]), collapse = ", "), "."
      )
    )
  }

  if (!any(issues$level == "error")) {
    issues <- osem_shiny_add_issue(
      issues,
      "success",
      "Dictionary",
      paste0("Dictionary is valid and contains ", nrow(dictionary), " variables.")
    )
  }

  list(
    data = dictionary,
    issues = issues,
    valid = !any(issues$level == "error")
  )
}

osem_shiny_variable_coverage <- function(specification,
                                         dictionary,
                                         effective,
                                         primary_source) {
  local_variables <- if (is.data.frame(effective$data) && "na_item" %in% names(effective$data)) {
    unique(as.character(effective$data$na_item))
  } else {
    character()
  }

  required <- tryCatch(
    determine_variables(specification = specification, dictionary = dictionary),
    error = function(e) e
  )
  if (inherits(required, "error")) {
    return(list(data = data.frame(), error = conditionMessage(required)))
  }

  if (nrow(required) == 0L) {
    return(list(data = data.frame(), error = NULL))
  }

  coverage <- as.data.frame(required, stringsAsFactors = FALSE)
  coverage$required <- TRUE
  coverage$available_local <- coverage$model_varname %in% local_variables
  database <- tolower(trimws(as.character(coverage$database)))
  dataset_id <- if ("dataset_id" %in% names(coverage)) {
    trimws(as.character(coverage$dataset_id))
  } else {
    rep(NA_character_, nrow(coverage))
  }
  missing_dataset_id <- is.na(dataset_id) | dataset_id == ""

  coverage$planned_source <- vapply(seq_len(nrow(coverage)), function(i) {
    db <- database[[i]]
    local <- coverage$available_local[[i]]
    needs_download <- !is.na(db) && nzchar(db) && !identical(db, "local") &&
      !(identical(primary_source, "local") && local)
    if (is.na(db) || !nzchar(db)) {
      "No data source in dictionary"
    } else if (identical(db, "local")) {
      if (local) "Local input" else "Missing local input"
    } else if (needs_download && missing_dataset_id[[i]]) {
      paste0("Missing dataset_id for ", db)
    } else if (identical(primary_source, "local") && local) {
      "Local input (preferred)"
    } else if (identical(primary_source, "local")) {
      paste0(db, " download (fallback)")
    } else {
      paste0(db, " download (preferred)")
    }
  }, character(1L))

  coverage$status <- vapply(seq_len(nrow(coverage)), function(i) {
    db <- database[[i]]
    local <- coverage$available_local[[i]]
    needs_download <- !is.na(db) && nzchar(db) && !identical(db, "local") &&
      !(identical(primary_source, "local") && local)
    if (is.na(db) || !nzchar(db)) {
      "Missing"
    } else if (identical(db, "local") && !local) {
      "Missing"
    } else if (needs_download && missing_dataset_id[[i]]) {
      "Missing"
    } else if (local && (identical(primary_source, "local") || identical(db, "local"))) {
      "Ready locally"
    } else {
      "Available by download"
    }
  }, character(1L))

  keep <- intersect(
    c(
      "model_varname", "full_name", "database", "dataset_id", "freq",
      "required", "available_local", "planned_source", "status"
    ),
    names(coverage)
  )
  coverage <- coverage[, keep, drop = FALSE]
  rownames(coverage) <- NULL
  list(data = coverage, error = NULL)
}

osem_shiny_validate_workspace <- function(specification,
                                          dictionary,
                                          input_sources,
                                          primary_source = c("local", "download"),
                                          effective = NULL) {
  primary_source <- match.arg(primary_source)
  if (is.null(effective)) {
    effective <- osem_shiny_effective_data(input_sources)
  }

  spec_check <- osem_shiny_validate_specification(specification)
  dictionary_check <- osem_shiny_validate_dictionary(dictionary)
  issues <- osem_shiny_bind_issues(
    spec_check$issues,
    dictionary_check$issues,
    effective$issues
  )

  if (length(input_sources) == 0L) {
    issues <- osem_shiny_add_issue(
      issues,
      "info",
      "Data",
      "No local input source has been added. Variables assigned to online databases can still be downloaded."
    )
  }

  if (is.data.frame(effective$overlaps) && nrow(effective$overlaps) > 0L) {
    overlap_variables <- unique(effective$overlaps$model_varname)
    issues <- osem_shiny_add_issue(
      issues,
      "warning",
      "Data",
      paste0(
        length(overlap_variables),
        " variable(s) occur in more than one local source. The first source in the list takes precedence: ",
        paste(utils::head(overlap_variables, 10L), collapse = ", "),
        if (length(overlap_variables) > 10L) ", ..." else "."
      )
    )
  }

  data <- effective$data
  if (is.data.frame(data) && nrow(data) > 0L &&
      all(c("na_item", "time", "values") %in% names(data))) {
    duplicate_key <- duplicated(data[, c("na_item", "time"), drop = FALSE])
    if (any(duplicate_key)) {
      variables <- unique(as.character(data$na_item[duplicate_key]))
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Data",
        paste0(
          "Duplicate variable/date observations were found for: ",
          paste(utils::head(variables, 15L), collapse = ", "),
          if (length(variables) > 15L) ", ..." else "."
        )
      )
    }
    missing_values <- sum(is.na(data$values))
    if (missing_values > 0L) {
      issues <- osem_shiny_add_issue(
        issues,
        "warning",
        "Data",
        paste0("The effective local input contains ", missing_values, " missing value(s).")
      )
    }
  }

  coverage <- data.frame()
  if (isTRUE(spec_check$valid) && isTRUE(dictionary_check$valid)) {
    coverage_result <- osem_shiny_variable_coverage(
      specification = spec_check$data,
      dictionary = dictionary_check$data,
      effective = effective,
      primary_source = primary_source
    )
    if (!is.null(coverage_result$error)) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Specification",
        coverage_result$error
      )
    } else {
      coverage <- coverage_result$data
      if (nrow(coverage) > 0L) {
        missing_local <- coverage$status == "Missing"
        if (any(missing_local)) {
          issues <- osem_shiny_add_issue(
            issues,
            "error",
            "Data",
            paste0(
              "Required variables have no usable local or downloadable source: ",
              paste(coverage$model_varname[missing_local], collapse = ", "), "."
            )
          )
        }
      }
    }
  }

  if (is.data.frame(data) && nrow(data) > 0L &&
      isTRUE(dictionary_check$valid) &&
      isTRUE(spec_check$valid) &&
      "na_item" %in% names(data)) {

    local_variables <- unique(as.character(data$na_item))
    dictionary_variables <- as.character(dictionary_check$data$model_varname)

    # Variables produced by accounting identities are internal model variables
    # and therefore do not need a dictionary entry.
    identity_variables <- unique(
      spec_check$data$dependent[
        spec_check$data$type == "d" &
          nzchar(spec_check$data$dependent)
      ]
    )

    known_variables <- union(dictionary_variables, identity_variables)
    unknown <- setdiff(local_variables, known_variables)

    if (length(unknown) > 0L) {
      issues <- osem_shiny_add_issue(
        issues,
        "warning",
        "Data",
        paste0(
          length(unknown),
          " local variable(s) are neither in the dictionary nor generated by an identity ",
          "and will be ignored unless added: ",
          paste(utils::head(unknown, 10L), collapse = ", "),
          if (length(unknown) > 10L) ", ..." else "."
        )
      )
    }
  }

  # Remove exact duplicate messages generated by a source and the workspace.
  if (nrow(issues) > 0L) {
    issues <- unique(issues)
    priority <- match(issues$level, c("error", "warning", "info", "success"))
    issues <- issues[order(priority, issues$area, issues$message), , drop = FALSE]
    rownames(issues) <- NULL
  }

  list(
    ready = !any(issues$level == "error"),
    issues = issues,
    coverage = coverage,
    specification = spec_check$data,
    ordered_specification = spec_check$ordered,
    dictionary = dictionary_check$data,
    effective_data = effective$data,
    attribution = effective$attribution,
    overlaps = effective$overlaps
  )
}
