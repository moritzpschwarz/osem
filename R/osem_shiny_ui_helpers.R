# OSEM Shiny UI helpers -----------------------------------------------------

osem_shiny_status_class <- function(status) {
  status <- tolower(as.character(status %||% "unavailable"))
  if (status %in% c("current", "ready", "success", "valid")) return("success")
  if (status %in% c("stale", "warning", "needs attention")) return("warning")
  if (status %in% c("running", "info", "available by download")) return("info")
  if (status %in% c("failed", "error", "missing")) return("danger")
  "muted"
}

osem_shiny_badge <- function(label, status = "muted") {
  shiny::span(
    class = paste0("osem-badge osem-badge-", status),
    as.character(label)
  )
}

osem_shiny_page_header <- function(title, subtitle = NULL, right = NULL) {
  shiny::div(
    class = "osem-page-header",
    shiny::div(
      shiny::tags$h2(title),
      if (!is.null(subtitle)) shiny::tags$p(class = "osem-lead", subtitle)
    ),
    if (!is.null(right)) shiny::div(class = "osem-page-header-actions", right)
  )
}

osem_shiny_metric_card <- function(title, value, subtitle = NULL, status = "muted") {
  shiny::div(
    class = paste0("osem-metric-card osem-metric-", status),
    shiny::div(class = "osem-metric-title", title),
    shiny::div(class = "osem-metric-value", value),
    if (!is.null(subtitle)) shiny::div(class = "osem-metric-subtitle", subtitle)
  )
}

osem_shiny_panel <- function(title = NULL, ..., class = NULL) {
  shiny::div(
    class = paste("osem-panel", class %||% ""),
    if (!is.null(title)) shiny::tags$h3(class = "osem-panel-title", title),
    ...
  )
}

osem_shiny_empty_state <- function(title, text) {
  shiny::div(
    class = "osem-empty-state",
    shiny::tags$h4(title),
    shiny::tags$p(text)
  )
}

osem_shiny_issue_list <- function(issues, area = NULL, max_items = 30L) {
  if (!is.data.frame(issues)) {
    return(osem_shiny_empty_state("No validation result", "Validation has not run yet."))
  }
  if (!is.null(area)) {
    issues <- issues[issues$area %in% area, , drop = FALSE]
  }
  if (nrow(issues) == 0L) {
    return(shiny::div(
      class = "osem-validation-item osem-validation-success",
      shiny::tags$strong("No issues found."),
      shiny::tags$span("This section passes the current validation checks.")
    ))
  }

  shown <- utils::head(issues, max_items)
  items <- lapply(seq_len(nrow(shown)), function(i) {
    row <- shown[i, , drop = FALSE]
    source_text <- if (!is.na(row$source) && nzchar(row$source)) {
      paste0(" · ", row$source)
    } else {
      ""
    }
    shiny::div(
      class = paste0("osem-validation-item osem-validation-", row$level),
      shiny::div(
        class = "osem-validation-heading",
        shiny::tags$strong(paste0(tools::toTitleCase(row$level), ": ", row$area)),
        shiny::span(class = "osem-validation-source", source_text)
      ),
      shiny::div(row$message)
    )
  })
  if (nrow(issues) > nrow(shown)) {
    items[[length(items) + 1L]] <- shiny::div(
      class = "osem-validation-more",
      paste0(nrow(issues) - nrow(shown), " additional message(s) are not shown here.")
    )
  }
  do.call(shiny::tagList, items)
}

osem_shiny_project_slug <- function(project_name) {
  slug <- tolower(trimws(as.character(project_name %||% "osem-project")))
  slug <- gsub("[^a-z0-9]+", "-", slug)
  slug <- gsub("(^-+|-+$)", "", slug)
  if (!nzchar(slug)) slug <- "osem-project"
  slug
}

osem_shiny_dt_options <- function(page_length = 15L, scroll_x = TRUE) {
  list(
    pageLength = as.integer(page_length),
    scrollX = isTRUE(scroll_x),
    autoWidth = TRUE,
    lengthMenu = c(10, 15, 25, 50, 100)
  )
}

osem_shiny_notify_error <- function(error, session = shiny::getDefaultReactiveDomain()) {
  shiny::showNotification(
    conditionMessage(error),
    type = "error",
    duration = 10,
    session = session
  )
}
