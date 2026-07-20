# OSEM Shiny data input/output ----------------------------------------------

osem_shiny_safe_filename <- function(name, fallback = "upload") {
  name <- basename(as.character(name %||% ""))
  name <- gsub("[^A-Za-z0-9._-]", "_", name)
  name <- gsub("_+", "_", name)
  if (!nzchar(name) || name %in% c(".", "..")) {
    name <- fallback
  }
  name
}

osem_shiny_stage_upload <- function(datapath, original_name, session_dir, source_id) {
  safe_name <- osem_shiny_safe_filename(original_name, fallback = source_id)
  destination <- file.path(session_dir, paste0(source_id, "-", safe_name))
  ok <- file.copy(datapath, destination, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop(paste0("Could not copy uploaded file '", original_name, "'."), call. = FALSE)
  }
  destination
}

osem_shiny_read_file <- function(path, display_name = basename(path), allow_list = FALSE) {
  extension <- tolower(tools::file_ext(display_name))
  issues <- osem_shiny_empty_issues()

  object <- tryCatch({
    if (extension == "csv") {
      if (requireNamespace("readr", quietly = TRUE)) {
        readr::read_csv(
          path,
          show_col_types = FALSE,
          progress = FALSE,
          guess_max = 1000000
        )
      } else {
        issues <- osem_shiny_add_issue(
          issues,
          "warning",
          "Data",
          "Package 'readr' is not installed; the CSV was read with utils::read.csv().",
          display_name
        )
        utils::read.csv(
          path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    } else if (extension %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("Package 'readxl' is required for Excel files.", call. = FALSE)
      }
      sheets <- readxl::excel_sheets(path)
      if (length(sheets) == 0L) {
        stop("The workbook does not contain a worksheet.", call. = FALSE)
      }
      if (length(sheets) > 1L) {
        issues <- osem_shiny_add_issue(
          issues,
          "info",
          "Data",
          paste0("The workbook has ", length(sheets), " worksheets; OSEM uses the first worksheet ('", sheets[[1L]], "')."),
          display_name
        )
      }
      readxl::read_excel(path, sheet = sheets[[1L]], guess_max = 1000000)
    } else if (extension %in% c("rds", "rda")) {
      if (extension == "rda") {
        stop(".rda files are not supported. Save the object as .rds instead.", call. = FALSE)
      }
      readRDS(path)
    } else {
      stop(
        paste0("Unsupported file extension '.", extension, "'. Use CSV, RDS, XLS, or XLSX."),
        call. = FALSE
      )
    }
  }, error = function(e) e)

  if (inherits(object, "error")) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      conditionMessage(object),
      display_name
    )
    return(list(data = list(), names = character(), issues = issues))
  }

  if (is.data.frame(object)) {
    data_list <- list(as.data.frame(object, stringsAsFactors = FALSE, check.names = FALSE))
    data_names <- display_name
  } else if (isTRUE(allow_list) && is.list(object) &&
             length(object) > 0L && all(vapply(object, is.data.frame, logical(1L)))) {
    data_list <- lapply(object, as.data.frame, stringsAsFactors = FALSE, check.names = FALSE)
    data_names <- names(object)
    if (is.null(data_names)) {
      data_names <- paste0(display_name, " [", seq_along(data_list), "]")
    } else {
      blank <- is.na(data_names) | !nzchar(data_names)
      data_names[blank] <- paste0(display_name, " [", which(blank), "]")
    }
  } else {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      if (isTRUE(allow_list)) {
        "The file must contain a data frame or a list consisting only of data frames."
      } else {
        "The file must contain one data frame."
      },
      display_name
    )
    data_list <- list()
    data_names <- character()
  }

  list(data = data_list, names = data_names, issues = issues)
}

osem_shiny_import_table <- function(path, display_name = basename(path)) {
  imported <- osem_shiny_read_file(path, display_name = display_name, allow_list = FALSE)
  if (nrow(imported$issues) > 0L && any(imported$issues$level == "error")) {
    stop(paste(imported$issues$message[imported$issues$level == "error"], collapse = " "), call. = FALSE)
  }
  if (length(imported$data) != 1L) {
    stop("The selected file did not contain one table.", call. = FALSE)
  }
  imported$data[[1L]]
}

osem_shiny_parse_time <- function(x) {
  n <- length(x)
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    return(as.Date(x))
  }

  out <- as.Date(rep(NA_character_, n))
  if (n == 0L) {
    return(out)
  }

  if (is.numeric(x)) {
    year_like <- !is.na(x) & x == floor(x) & x >= 1800 & x <= 2200
    out[year_like] <- as.Date(paste0(as.integer(x[year_like]), "-01-01"))
    excel_like <- !is.na(x) & !year_like & x > 1000 & x < 100000
    out[excel_like] <- as.Date(x[excel_like], origin = "1899-12-30")
    return(out)
  }

  values <- trimws(as.character(x))
  missing_value <- is.na(x) | values == ""
  out <- suppressWarnings(tryCatch(
    as.Date(values),
    error = function(e) as.Date(rep(NA_character_, n))
  ))

  # Common quarterly notation, e.g. 2024-Q3 or 2024 Q3.
  quarter_match <- grepl("^[0-9]{4}[- ]?Q[1-4]$", toupper(values))
  if (any(quarter_match & is.na(out))) {
    q_values <- toupper(values[quarter_match & is.na(out)])
    year <- as.integer(substr(q_values, 1L, 4L))
    quarter <- as.integer(sub(".*Q", "", q_values))
    month <- (quarter - 1L) * 3L + 1L
    out[quarter_match & is.na(out)] <- as.Date(sprintf("%04d-%02d-01", year, month))
  }

  unresolved <- !missing_value & is.na(out)
  if (any(unresolved) && requireNamespace("lubridate", quietly = TRUE)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      values[unresolved],
      orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS", "mdY HMS", "Ym", "Y"),
      quiet = TRUE,
      truncated = 3
    ))
    out[unresolved] <- as.Date(parsed)
  }
  out
}

osem_shiny_prepare_model_data <- function(data, source_name = "Input data") {
  issues <- osem_shiny_empty_issues()
  if (!is.data.frame(data)) {
    issues <- osem_shiny_add_issue(
      issues, "error", "Data", "Input is not a data frame.", source_name
    )
    return(list(data = data, issues = issues, valid = FALSE))
  }

  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("na_item", "time", "values")
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      paste0("Missing required column(s): ", paste(missing_columns, collapse = ", "), "."),
      source_name
    )
    return(list(data = data, issues = issues, valid = FALSE))
  }

  data$na_item <- trimws(as.character(data$na_item))
  blank_variable <- is.na(data$na_item) | data$na_item == ""
  if (any(blank_variable)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      paste0(sum(blank_variable), " row(s) have no value in 'na_item'."),
      source_name
    )
  }

  parsed_time <- osem_shiny_parse_time(data$time)
  nonmissing_original <- !(is.na(data$time) | trimws(as.character(data$time)) == "")
  unparsed <- nonmissing_original & is.na(parsed_time)
  missing_time <- is.na(data$time) | trimws(as.character(data$time)) == ""
  if (any(missing_time)) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      paste0(sum(missing_time), " row(s) have no value in 'time'."),
      source_name
    )
  }
  if (any(unparsed)) {
    examples <- unique(as.character(data$time[unparsed]))
    examples <- utils::head(examples, 3L)
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      paste0(
        sum(unparsed), " date value(s) could not be parsed. Examples: ",
        paste(examples, collapse = ", "), "."
      ),
      source_name
    )
  } else {
    if (!inherits(data$time, "Date")) {
      issues <- osem_shiny_add_issue(
        issues,
        "info",
        "Data",
        "The 'time' column was converted to Date format for modelling.",
        source_name
      )
    }
    data$time <- parsed_time
  }

  if (!is.numeric(data$values)) {
    original_values <- data$values
    converted <- suppressWarnings(as.numeric(as.character(original_values)))
    failed <- !is.na(original_values) & trimws(as.character(original_values)) != "" & is.na(converted)
    if (any(failed)) {
      issues <- osem_shiny_add_issue(
        issues,
        "error",
        "Data",
        paste0(sum(failed), " value(s) in 'values' are not numeric."),
        source_name
      )
    } else {
      data$values <- converted
      issues <- osem_shiny_add_issue(
        issues,
        "info",
        "Data",
        "The 'values' column was converted to numeric format for modelling.",
        source_name
      )
    }
  }

  if (is.numeric(data$values) && any(!is.finite(data$values) & !is.na(data$values))) {
    issues <- osem_shiny_add_issue(
      issues,
      "error",
      "Data",
      "The 'values' column contains infinite values.",
      source_name
    )
  }

  if (nrow(data) == 0L) {
    issues <- osem_shiny_add_issue(
      issues, "warning", "Data", "The data frame has no rows.", source_name
    )
  }

  valid <- !any(issues$level == "error")
  list(data = data, issues = issues, valid = valid)
}

osem_shiny_source_from_upload <- function(upload_row, source_id, session_dir) {
  display_name <- as.character(upload_row$name[[1L]])
  staged_path <- tryCatch(
    osem_shiny_stage_upload(
      datapath = upload_row$datapath[[1L]],
      original_name = display_name,
      session_dir = session_dir,
      source_id = source_id
    ),
    error = function(e) e
  )

  if (inherits(staged_path, "error")) {
    return(list(
      id = source_id,
      display_name = display_name,
      kind = "upload",
      format = tolower(tools::file_ext(display_name)),
      staged_path = NULL,
      size_bytes = as.numeric(upload_row$size[[1L]] %||% NA_real_),
      md5 = NA_character_,
      imported_at = Sys.time(),
      datasets = list(),
      dataset_names = character(),
      issues = osem_shiny_add_issue(
        osem_shiny_empty_issues(), "error", "Data", conditionMessage(staged_path), display_name
      ),
      valid = FALSE
    ))
  }

  imported <- osem_shiny_read_file(staged_path, display_name, allow_list = TRUE)
  prepared <- vector("list", length(imported$data))
  all_issues <- imported$issues
  for (i in seq_along(imported$data)) {
    item_name <- imported$names[[i]] %||% paste0(display_name, " [", i, "]")
    prepared[[i]] <- osem_shiny_prepare_model_data(imported$data[[i]], item_name)
    all_issues <- osem_shiny_bind_issues(all_issues, prepared[[i]]$issues)
  }

  datasets <- lapply(prepared, `[[`, "data")
  valid <- length(datasets) > 0L && !any(all_issues$level == "error")
  md5 <- tryCatch(unname(tools::md5sum(staged_path)), error = function(e) NA_character_)

  list(
    id = source_id,
    display_name = display_name,
    kind = "upload",
    format = tolower(tools::file_ext(display_name)),
    staged_path = staged_path,
    size_bytes = as.numeric(upload_row$size[[1L]] %||% file.info(staged_path)$size),
    md5 = md5,
    imported_at = Sys.time(),
    datasets = datasets,
    dataset_names = imported$names,
    issues = all_issues,
    valid = valid
  )
}

osem_shiny_source_from_data <- function(data,
                                        display_name,
                                        source_id,
                                        kind = "memory") {
  if (is.data.frame(data)) {
    data <- list(data)
  }
  if (!is.list(data) || length(data) == 0L || !all(vapply(data, is.data.frame, logical(1L)))) {
    stop("In-memory OSEM input must be a data frame or a list of data frames.", call. = FALSE)
  }

  prepared <- lapply(seq_along(data), function(i) {
    osem_shiny_prepare_model_data(data[[i]], paste0(display_name, " [", i, "]"))
  })
  issues <- do.call(osem_shiny_bind_issues, lapply(prepared, `[[`, "issues"))

  list(
    id = source_id,
    display_name = display_name,
    kind = kind,
    format = "in-memory",
    staged_path = NULL,
    size_bytes = as.numeric(object.size(data)),
    md5 = NA_character_,
    imported_at = Sys.time(),
    datasets = lapply(prepared, `[[`, "data"),
    dataset_names = paste0(display_name, " [", seq_along(prepared), "]"),
    issues = issues,
    valid = !any(issues$level == "error")
  )
}

osem_shiny_source_for_project <- function(source) {
  list(
    id = source$id,
    display_name = source$display_name,
    kind = "project_snapshot",
    format = source$format,
    size_bytes = source$size_bytes,
    md5 = source$md5,
    imported_at = source$imported_at,
    datasets = source$datasets,
    dataset_names = source$dataset_names,
    issues = source$issues,
    valid = source$valid
  )
}

osem_shiny_source_from_project <- function(source) {
  required <- c("id", "display_name", "datasets")
  if (!is.list(source) || length(setdiff(required, names(source))) > 0L) {
    stop("An input source in the project file is invalid.", call. = FALSE)
  }
  if (!is.list(source$datasets) || length(source$datasets) == 0L ||
      !all(vapply(source$datasets, is.data.frame, logical(1L)))) {
    stop("A project input source must contain one or more data frames.", call. = FALSE)
  }

  source$id <- trimws(osem_shiny_scalar_character(source$id, "source-0001"))
  if (!nzchar(source$id)) source$id <- "source-0001"
  source$display_name <- trimws(osem_shiny_scalar_character(
    source$display_name, "Project input"
  ))
  if (!nzchar(source$display_name)) source$display_name <- "Project input"

  dataset_names <- as.character(source$dataset_names %||% character())
  if (length(dataset_names) != length(source$datasets)) {
    dataset_names <- paste0(source$display_name, " [", seq_along(source$datasets), "]")
  }
  blank_names <- is.na(dataset_names) | !nzchar(trimws(dataset_names))
  dataset_names[blank_names] <- paste0(
    source$display_name, " [", which(blank_names), "]"
  )

  prepared <- lapply(seq_along(source$datasets), function(i) {
    osem_shiny_prepare_model_data(source$datasets[[i]], dataset_names[[i]])
  })
  source$datasets <- lapply(prepared, `[[`, "data")
  source$dataset_names <- dataset_names
  source$issues <- do.call(osem_shiny_bind_issues, lapply(prepared, `[[`, "issues"))
  source$valid <- !any(source$issues$level == "error")
  source$kind <- "project_snapshot"
  source$staged_path <- NULL
  source$format <- osem_shiny_scalar_character(source$format, "project")
  source$size_bytes <- osem_shiny_scalar_numeric(
    source$size_bytes,
    as.numeric(object.size(source$datasets)),
    minimum = 0
  )
  source$md5 <- osem_shiny_scalar_character(source$md5, NA_character_)
  source$imported_at <- osem_shiny_as_posixct_scalar(source$imported_at, Sys.time())
  source
}


osem_shiny_dataset_name <- function(source, dataset_index) {
  names <- source$dataset_names %||% character()
  if (length(names) >= dataset_index && !is.na(names[[dataset_index]]) && nzchar(names[[dataset_index]])) {
    return(names[[dataset_index]])
  }
  paste0(source$display_name %||% "Input source", " [", dataset_index, "]")
}

osem_shiny_effective_data <- function(input_sources) {
  output <- list()
  attribution <- list()
  overlaps <- list()
  issues <- osem_shiny_empty_issues()
  claimed <- character()
  item_index <- 0L

  if (length(input_sources) == 0L) {
    return(list(
      data = data.frame(),
      attribution = data.frame(),
      overlaps = data.frame(),
      issues = issues
    ))
  }

  for (source_order in seq_along(input_sources)) {
    source <- input_sources[[source_order]]
    issues <- osem_shiny_bind_issues(issues, source$issues %||% osem_shiny_empty_issues())
    if (!isTRUE(source$valid) || length(source$datasets) == 0L) {
      next
    }

    for (dataset_index in seq_along(source$datasets)) {
      data <- source$datasets[[dataset_index]]
      if (!is.data.frame(data) || !all(c("na_item", "time", "values") %in% names(data))) {
        next
      }
      variables <- unique(as.character(data$na_item))
      variables <- variables[!is.na(variables) & nzchar(variables)]
      shadowed <- intersect(variables, claimed)
      selected <- setdiff(variables, claimed)

      if (length(shadowed) > 0L) {
        overlaps[[length(overlaps) + 1L]] <- data.frame(
          model_varname = shadowed,
          ignored_source = source$display_name,
          source_order = source_order,
          stringsAsFactors = FALSE
        )
      }
      if (length(selected) == 0L) {
        next
      }

      item_index <- item_index + 1L
      selected_data <- data[data$na_item %in% selected, , drop = FALSE]
      selected_data$.osem_source_id <- source$id
      selected_data$.osem_source_name <- source$display_name
      selected_data$.osem_source_order <- source_order
      dataset_name <- osem_shiny_dataset_name(source, dataset_index)
      selected_data$.osem_dataset_name <- dataset_name
      output[[item_index]] <- selected_data
      attribution[[item_index]] <- data.frame(
        model_varname = selected,
        source_id = source$id,
        source_name = source$display_name,
        source_order = source_order,
        dataset_name = dataset_name,
        stringsAsFactors = FALSE
      )
      claimed <- union(claimed, selected)
    }
  }

  combined <- if (length(output) == 0L) data.frame() else {
    dplyr::bind_rows(output)
  }
  attribution_df <- if (length(attribution) == 0L) data.frame() else {
    do.call(rbind, attribution)
  }
  overlaps_df <- if (length(overlaps) == 0L) data.frame() else {
    do.call(rbind, overlaps)
  }
  rownames(attribution_df) <- NULL
  rownames(overlaps_df) <- NULL

  list(
    data = combined,
    attribution = attribution_df,
    overlaps = overlaps_df,
    issues = issues
  )
}

osem_shiny_build_run_input <- function(input_sources) {
  input <- list()
  for (source in input_sources) {
    if (!isTRUE(source$valid) || length(source$datasets) == 0L) {
      next
    }
    for (data in source$datasets) {
      if (is.data.frame(data)) {
        input[[length(input) + 1L]] <- data
      }
    }
  }
  if (length(input) == 0L) {
    return(NULL)
  }
  if (length(input) == 1L) {
    return(input[[1L]])
  }
  input
}

osem_shiny_source_table <- function(input_sources) {
  if (length(input_sources) == 0L) {
    return(data.frame(
      Order = integer(),
      Source = character(),
      Kind = character(),
      Format = character(),
      Size = character(),
      Datasets = integer(),
      Variables = integer(),
      Rows = integer(),
      Start = as.Date(character()),
      End = as.Date(character()),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_along(input_sources), function(i) {
    source <- input_sources[[i]]
    datasets <- source$datasets %||% list()
    valid_data <- datasets[vapply(datasets, is.data.frame, logical(1L))]
    all_data <- if (length(valid_data) == 0L) data.frame() else dplyr::bind_rows(valid_data)
    variables <- if ("na_item" %in% names(all_data)) unique(all_data$na_item) else character()
    dates <- if ("time" %in% names(all_data)) osem_shiny_parse_time(all_data$time) else as.Date(character())
    dates <- dates[!is.na(dates)]
    data.frame(
      Order = i,
      Source = source$display_name,
      Kind = source$kind,
      Format = source$format,
      Size = osem_shiny_format_bytes(source$size_bytes),
      Datasets = length(datasets),
      Variables = length(unique(variables[!is.na(variables)])),
      Rows = nrow(all_data),
      Start = if (length(dates) == 0L) as.Date(NA) else min(dates),
      End = if (length(dates) == 0L) as.Date(NA) else max(dates),
      Status = if (isTRUE(source$valid)) "Ready" else "Needs attention",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

osem_shiny_variable_profile <- function(effective,
                                        specification,
                                        dictionary,
                                        primary_source,
                                        workspace = NULL) {
  data <- effective$data
  local <- data.frame()

  if (is.data.frame(data) && nrow(data) > 0L && "na_item" %in% names(data)) {
    split_data <- split(data, as.character(data$na_item), drop = TRUE)
    rows <- lapply(names(split_data), function(variable) {
      item <- split_data[[variable]]
      dates <- as.Date(item$time)
      dates_nonmissing <- dates[!is.na(dates)]
      duplicate_dates <- sum(duplicated(dates[!is.na(dates)]))
      source_names <- if (".osem_source_name" %in% names(item)) {
        paste(unique(item$.osem_source_name), collapse = ", ")
      } else {
        NA_character_
      }
      data.frame(
        model_varname = variable,
        observations = nrow(item),
        missing_values = sum(is.na(item$values)),
        duplicate_dates = duplicate_dates,
        start = if (length(dates_nonmissing) == 0L) as.Date(NA) else min(dates_nonmissing),
        end = if (length(dates_nonmissing) == 0L) as.Date(NA) else max(dates_nonmissing),
        local_source = source_names,
        stringsAsFactors = FALSE
      )
    })
    local <- do.call(rbind, rows)
    rownames(local) <- NULL
  }

  coverage <- if (!is.null(workspace) && is.data.frame(workspace$coverage)) {
    workspace$coverage
  } else {
    data.frame()
  }

  if (nrow(local) == 0L && nrow(coverage) == 0L) {
    return(data.frame())
  }
  if (nrow(local) == 0L) {
    out <- coverage
  } else if (nrow(coverage) == 0L) {
    out <- local
    out$required <- FALSE
  } else {
    out <- merge(coverage, local, by = "model_varname", all = TRUE, sort = FALSE)
  }

  dictionary_subset <- data.frame()
  if (is.data.frame(dictionary) && "model_varname" %in% names(dictionary)) {
    keep <- intersect(c("model_varname", "full_name", "database", "freq"), names(dictionary))
    dictionary_subset <- unique(dictionary[, keep, drop = FALSE])
  }
  if (nrow(dictionary_subset) > 0L) {
    new_columns <- setdiff(names(dictionary_subset), names(out))
    if (length(new_columns) > 0L) {
      out <- merge(out, dictionary_subset[, c("model_varname", new_columns), drop = FALSE],
                   by = "model_varname", all.x = TRUE, sort = FALSE)
    }
  }

  if (!"required" %in% names(out)) out$required <- FALSE
  if (!"available_local" %in% names(out)) out$available_local <- !is.na(out$observations)
  out$required[is.na(out$required)] <- FALSE
  out$available_local[is.na(out$available_local)] <- !is.na(out$observations[is.na(out$available_local)])

  preferred <- c(
    "model_varname", "full_name", "required", "database", "freq",
    "planned_source", "status", "available_local", "observations",
    "start", "end", "missing_values", "duplicate_dates", "local_source"
  )
  out <- out[, c(intersect(preferred, names(out)), setdiff(names(out), preferred)), drop = FALSE]
  rownames(out) <- NULL
  out
}

osem_shiny_format_bytes <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes))
  if (length(bytes) == 0L || is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB")
  index <- 1L
  while (bytes >= 1024 && index < length(units)) {
    bytes <- bytes / 1024
    index <- index + 1L
  }
  paste0(format(round(bytes, 1L), trim = TRUE, nsmall = if (index == 1L) 0L else 1L), " ", units[[index]])
}

osem_shiny_strip_source_columns <- function(data) {
  if (!is.data.frame(data)) return(data)
  data[, !grepl("^\\.osem_", names(data)), drop = FALSE]
}
