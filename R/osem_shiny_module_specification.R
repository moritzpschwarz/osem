# OSEM Shiny module: specification -----------------------------------------

osem_shiny_specification_ui <- function(id) {
  ns <- shiny::NS(id)
  export_formats <- c("CSV" = "csv", "RDS" = "rds")
  if (requireNamespace("writexl", quietly = TRUE)) {
    export_formats <- c(export_formats, "Excel" = "xlsx")
  }

  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Model specification",
      "Build equations with guided controls, inspect the underlying five-column specification, and review the implied execution order before estimation."
    ),
    shiny::tabsetPanel(
      id = ns("specification_tabs"),
      shiny::tabPanel(
        title = "Guided builder",
        value = "builder",
        shiny::fluidRow(
          shiny::column(
            width = 4,
            osem_shiny_panel(
              "Modules",
              shiny::uiOutput(ns("module_selector")),
              shiny::div(
                class = "osem-button-grid osem-spec-module-actions",
                shiny::actionButton(ns("add_module"), "Add", class = "btn-primary"),
                shiny::actionButton(ns("duplicate_module"), "Duplicate"),
                shiny::actionButton(ns("move_module_up"), "Move up"),
                shiny::actionButton(ns("move_module_down"), "Move down"),
                shiny::actionButton(ns("delete_module"), "Delete")
              ),
              shiny::tags$hr(),
              shiny::uiOutput(ns("builder_summary"))
            )
          ),
          shiny::column(
            width = 8,
            shiny::uiOutput(ns("module_editor"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            osem_shiny_panel(
              "Draft validation",
              shiny::uiOutput(ns("draft_validation"))
            )
          ),
          shiny::column(
            width = 6,
            osem_shiny_panel(
              "Equation preview",
              shiny::uiOutput(ns("equation_preview"))
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Advanced table",
        value = "advanced",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::fileInput(
              ns("specification_file"),
              "Replace specification from CSV, RDS, XLS, or XLSX",
              accept = c(".csv", ".rds", ".RDS", ".xls", ".xlsx")
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "osem-action-row osem-action-row-top",
              shiny::actionButton(ns("advanced_add_row"), "Add module", class = "btn-primary"),
              shiny::actionButton(ns("advanced_delete_rows"), "Delete selected"),
              shiny::actionButton(ns("reset"), "Reset default")
            )
          )
        ),
        osem_shiny_panel(
          "Five-column specification",
          shiny::tags$p(
            class = "help-block",
            "type: n = estimated module, d = identity; lag: comma-separated regressors entering only with lags; cvar: common system label. Double-click a cell to edit it."
          ),
          DT::DTOutput(ns("specification_table"))
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns("specification_format"),
              "Export format",
              choices = export_formats
            )
          ),
          shiny::column(
            3,
            shiny::downloadButton(
              ns("download_specification"),
              "Download specification",
              class = "btn-primary osem-download-align"
            )
          )
        ),
        osem_shiny_panel(
          "Whole-specification validation",
          shiny::uiOutput(ns("validation"))
        )
      ),
      shiny::tabPanel(
        title = "Order and dependencies",
        value = "dependencies",
        shiny::uiOutput(ns("summary_cards")),
        shiny::fluidRow(
          shiny::column(
            7,
            osem_shiny_panel(
              "Dependency graph",
              shiny::tags$p(
                class = "help-block",
                "Solid arrows denote contemporaneous or identity relationships. Dashed arrows denote regressors that enter only through lags."
              ),
              shiny::plotOutput(ns("dependency_plot"), height = "560px")
            )
          ),
          shiny::column(
            5,
            osem_shiny_panel(
              "Execution order preview",
              DT::DTOutput(ns("ordered_specification"))
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Required variables",
        value = "variables",
        shiny::tags$p(
          class = "osem-tab-intro",
          "These variables are inferred from the specification and matched to the current dictionary and data-source plan."
        ),
        DT::DTOutput(ns("required_variables"))
      )
    )
  )
}

osem_shiny_specification_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_module <- shiny::reactiveVal(1L)

    current_index <- shiny::reactive({
      specification <- state$specification
      if (!is.data.frame(specification) || nrow(specification) == 0L) {
        return(NULL)
      }
      index <- suppressWarnings(as.integer(selected_module()))
      if (length(index) != 1L || is.na(index)) index <- 1L
      max(1L, min(index, nrow(specification)))
    })

    shiny::observeEvent(input$module_index, {
      index <- suppressWarnings(as.integer(input$module_index))
      if (length(index) == 1L && !is.na(index)) selected_module(index)
    }, ignoreInit = TRUE)

    shiny::observe({
      specification <- state$specification
      if (!is.data.frame(specification) || nrow(specification) == 0L) {
        selected_module(NULL)
      } else {
        index <- suppressWarnings(as.integer(selected_module()))
        if (length(index) != 1L || is.na(index) || index < 1L) index <- 1L
        if (index > nrow(specification)) index <- nrow(specification)
        if (!identical(index, selected_module())) selected_module(index)
      }
    })

    output$module_selector <- shiny::renderUI({
      choices <- osem_shiny_spec_module_labels(state$specification)
      if (length(choices) == 0L) {
        return(osem_shiny_empty_state(
          "No modules",
          "Add a module to begin building the specification."
        ))
      }
      shiny::selectInput(
        ns("module_index"),
        "Current module",
        choices = choices,
        selected = as.character(current_index()),
        width = "100%"
      )
    })

    output$module_editor <- shiny::renderUI({
      index <- current_index()
      if (is.null(index)) {
        return(osem_shiny_empty_state(
          "No module selected",
          "Use Add in the Modules panel to create the first equation."
        ))
      }

      specification <- state$specification
      row <- specification[index, , drop = FALSE]
      variables <- osem_shiny_spec_variable_choices(
        dictionary = state$dictionary,
        specification = specification,
        include = c(
          row$dependent,
          osem_shiny_spec_split_plus(row$independent),
          osem_shiny_spec_formula_tokens(row$independent)
        )
      )
      independent_variables <- osem_shiny_spec_split_plus(row$independent)
      lag_variables <- osem_shiny_spec_split_lag(row$lag)
      cvar_systems <- sort(unique(c("", specification$cvar[nzchar(specification$cvar)])))
      dependent_label <- if (nzchar(row$dependent)) row$dependent else "unnamed module"

      osem_shiny_panel(
        paste0("Edit module ", index, ": ", dependent_label),
        class = "osem-spec-editor",
        shiny::selectInput(
          ns("module_type"),
          "Module type",
          choices = c(
            "Estimated equation" = "n",
            "Identity / accounting definition" = "d"
          ),
          selected = row$type
        ),
        shiny::selectizeInput(
          ns("dependent_variable"),
          "Dependent variable",
          choices = variables,
          selected = row$dependent,
          multiple = FALSE,
          options = list(
            create = TRUE,
            persist = FALSE,
            placeholder = "Choose or type a model variable"
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'n'", ns("module_type")),
          shiny::selectizeInput(
            ns("independent_variables"),
            "Regressors",
            choices = variables,
            selected = independent_variables,
            multiple = TRUE,
            options = list(
              create = TRUE,
              persist = FALSE,
              plugins = c("remove_button"),
              placeholder = "Leave empty for an autoregressive-only equation"
            )
          ),
          shiny::selectizeInput(
            ns("lag_variables"),
            "Regressors entering only through lags",
            choices = independent_variables,
            selected = intersect(lag_variables, independent_variables),
            multiple = TRUE,
            options = list(
              create = FALSE,
              plugins = c("remove_button"),
              placeholder = "Optional"
            )
          ),
          shiny::selectizeInput(
            ns("cvar_system"),
            "CVAR system",
            choices = cvar_systems,
            selected = row$cvar,
            multiple = FALSE,
            options = list(
              create = TRUE,
              persist = FALSE,
              allowEmptyOption = TRUE,
              placeholder = "Leave blank for a single equation"
            )
          ),
          shiny::checkboxInput(
            ns("synchronise_cvar"),
            "Apply these regressors to every member of this CVAR system",
            value = TRUE
          ),
          shiny::tags$p(
            class = "help-block",
            "OSEM requires every member of a CVAR system to use the same regressors. CVAR modules cannot use the lag-only field."
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'd'", ns("module_type")),
          shiny::textAreaInput(
            ns("identity_expression"),
            "Right-hand-side identity",
            value = row$independent,
            rows = 4,
            resize = "vertical",
            placeholder = "For example: GValueAdd + Import"
          ),
          shiny::fluidRow(
            shiny::column(
              7,
              shiny::selectizeInput(
                ns("identity_variable"),
                "Insert a variable",
                choices = variables,
                selected = character(),
                multiple = FALSE,
                options = list(placeholder = "Choose a variable")
              )
            ),
            shiny::column(
              5,
              shiny::div(
                class = "osem-action-row osem-identity-insert-actions",
                shiny::actionButton(ns("append_identity_plus"), "+ Add"),
                shiny::actionButton(ns("append_identity_minus"), "- Subtract")
              )
            )
          )
        ),
        shiny::div(
          class = "osem-spec-apply-row",
          shiny::actionButton(
            ns("apply_module"),
            "Apply module changes",
            class = "btn-primary"
          ),
          shiny::tags$span(
            class = "help-block osem-inline-help",
            "The fitted model and forecast become stale when the specification changes."
          )
        )
      )
    })

    draft_row <- shiny::reactive({
      index <- current_index()
      if (is.null(index)) return(NULL)
      osem_shiny_spec_editor_row(
        type = input$module_type %||% state$specification$type[[index]],
        dependent = input$dependent_variable %||% state$specification$dependent[[index]],
        independent_variables = input$independent_variables %||% character(),
        identity_expression = input$identity_expression %||% "",
        lag_variables = input$lag_variables %||% character(),
        cvar_system = input$cvar_system %||% ""
      )
    })

    shiny::observeEvent(input$independent_variables, {
      independent <- input$independent_variables %||% character()
      selected <- intersect(input$lag_variables %||% character(), independent)
      shiny::updateSelectizeInput(
        session,
        "lag_variables",
        choices = independent,
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = TRUE)

    append_identity_variable <- function(operator) {
      variable <- trimws(as.character(input$identity_variable %||% ""))
      if (!nzchar(variable)) {
        shiny::showNotification(
          "Choose a variable to insert.",
          type = "warning",
          session = session
        )
        return()
      }
      expression <- trimws(as.character(input$identity_expression %||% ""))
      updated <- if (!nzchar(expression)) {
        if (identical(operator, "-")) paste0("-", variable) else variable
      } else {
        paste(expression, operator, variable)
      }
      shiny::updateTextAreaInput(session, "identity_expression", value = updated)
    }

    shiny::observeEvent(input$append_identity_plus, {
      append_identity_variable("+")
    })
    shiny::observeEvent(input$append_identity_minus, {
      append_identity_variable("-")
    })

    shiny::observeEvent(input$apply_module, {
      index <- current_index()
      row <- draft_row()
      shiny::req(!is.null(index), !is.null(row))

      updated <- tryCatch(
        osem_shiny_spec_apply_editor_row(
          specification = state$specification,
          index = index,
          row = row,
          synchronise_cvar = isTRUE(input$synchronise_cvar)
        ),
        error = function(e) e
      )
      if (inherits(updated, "error")) {
        osem_shiny_notify_error(updated, session)
        return()
      }

      state$specification <- updated
      osem_shiny_state_mark_changed(
        state,
        "specification",
        paste0("Updated specification module ", index, ".")
      )
      shiny::showNotification("Module changes applied.", type = "message", session = session)
    })

    add_module <- function() {
      specification <- state$specification
      new_row <- data.frame(
        type = "n",
        dependent = "",
        independent = "",
        lag = "",
        cvar = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      state$specification <- rbind(specification, new_row)
      rownames(state$specification) <- NULL
      selected_module(nrow(state$specification))
      osem_shiny_state_mark_changed(state, "specification", "Added a specification module.")
    }

    shiny::observeEvent(input$add_module, add_module())
    shiny::observeEvent(input$advanced_add_row, add_module())

    shiny::observeEvent(input$duplicate_module, {
      index <- current_index()
      if (is.null(index)) return()
      specification <- state$specification
      before <- specification[seq_len(index), , drop = FALSE]
      after <- if (index < nrow(specification)) {
        specification[seq.int(index + 1L, nrow(specification)), , drop = FALSE]
      } else {
        specification[FALSE, , drop = FALSE]
      }
      duplicate <- specification[index, , drop = FALSE]
      if (nzchar(duplicate$dependent)) {
        duplicate$dependent <- paste0(duplicate$dependent, "_copy")
      }
      state$specification <- rbind(before, duplicate, after)
      rownames(state$specification) <- NULL
      selected_module(index + 1L)
      osem_shiny_state_mark_changed(state, "specification", "Duplicated a specification module.")
    })

    move_module <- function(direction) {
      index <- current_index()
      specification <- state$specification
      if (is.null(index) || nrow(specification) < 2L) return()
      target <- index + direction
      if (target < 1L || target > nrow(specification)) return()
      order <- seq_len(nrow(specification))
      order[c(index, target)] <- order[c(target, index)]
      state$specification <- specification[order, , drop = FALSE]
      rownames(state$specification) <- NULL
      selected_module(target)
      osem_shiny_state_mark_changed(state, "specification", "Reordered specification modules.")
    }

    shiny::observeEvent(input$move_module_up, move_module(-1L))
    shiny::observeEvent(input$move_module_down, move_module(1L))

    shiny::observeEvent(input$delete_module, {
      index <- current_index()
      if (is.null(index)) return()
      state$specification <- state$specification[-index, , drop = FALSE]
      rownames(state$specification) <- NULL
      selected_module(if (nrow(state$specification) == 0L) NULL else min(index, nrow(state$specification)))
      osem_shiny_state_mark_changed(state, "specification", "Deleted a specification module.")
    })

    shiny::observeEvent(input$advanced_delete_rows, {
      selected <- sort(unique(input$specification_table_rows_selected %||% integer()))
      if (length(selected) == 0L) {
        shiny::showNotification(
          "Select specification rows to delete.",
          type = "warning",
          session = session
        )
        return()
      }
      state$specification <- state$specification[-selected, , drop = FALSE]
      rownames(state$specification) <- NULL
      selected_module(if (nrow(state$specification) == 0L) NULL else 1L)
      osem_shiny_state_mark_changed(state, "specification", "Deleted specification module(s).")
    })

    shiny::observeEvent(input$reset, {
      state$specification <- osem_shiny_default_specification()
      selected_module(1L)
      osem_shiny_state_mark_changed(state, "specification", "Restored the default specification.")
    })

    output$builder_summary <- shiny::renderUI({
      summary <- osem_shiny_specification_summary(state$specification)
      shiny::tagList(
        shiny::div(
          class = "osem-spec-summary-grid",
          shiny::div(shiny::tags$strong(summary$modules), shiny::tags$span("Modules")),
          shiny::div(shiny::tags$strong(summary$estimated), shiny::tags$span("Estimated")),
          shiny::div(shiny::tags$strong(summary$identities), shiny::tags$span("Identities")),
          shiny::div(shiny::tags$strong(summary$cvar_systems), shiny::tags$span("CVAR systems"))
        ),
        shiny::tags$p(
          class = "help-block",
          paste0(summary$lag_only_variables, " distinct lag-only variable(s).")
        )
      )
    })

    output$draft_validation <- shiny::renderUI({
      index <- current_index()
      row <- draft_row()
      if (is.null(index) || is.null(row)) {
        return(osem_shiny_empty_state(
          "No draft",
          "Select or add a module to see validation feedback."
        ))
      }
      issues <- osem_shiny_spec_draft_issues(
        state$specification,
        index,
        row,
        synchronise_cvar = isTRUE(input$synchronise_cvar)
      )
      osem_shiny_issue_list(issues, max_items = 15L)
    })

    output$equation_preview <- shiny::renderUI({
      row <- draft_row()
      if (is.null(row)) {
        return(osem_shiny_empty_state(
          "No equation",
          "Select or add a module to see its equation."
        ))
      }

      dependent <- if (nzchar(row$dependent)) row$dependent else "(dependent variable)"
      if (identical(row$type, "d")) {
        expression <- if (nzchar(row$independent)) row$independent else "(identity expression)"
        return(shiny::tagList(
          shiny::tags$div(class = "osem-equation-code", paste0(dependent, " = ", expression)),
          shiny::tags$p(
            "This module is evaluated as an accounting identity rather than estimated."
          )
        ))
      }

      regressors <- osem_shiny_spec_split_plus(row$independent)
      lag_only <- osem_shiny_spec_split_lag(row$lag)
      formula <- if (length(regressors) == 0L) {
        paste0(dependent, " = autoregressive process")
      } else {
        paste0(dependent, " ~ ", paste(regressors, collapse = " + "))
      }
      notes <- character()
      if (length(lag_only) > 0L) {
        notes <- c(notes, paste0("Lag-only: ", paste(lag_only, collapse = ", "), "."))
      }
      if (nzchar(row$cvar)) {
        notes <- c(notes, paste0("Jointly estimated in CVAR system '", row$cvar, "'."))
      }
      if (length(notes) == 0L) {
        notes <- "OSEM will add autoregressive and distributed lags according to the estimation settings."
      }
      shiny::tagList(
        shiny::tags$div(class = "osem-equation-code", formula),
        lapply(notes, shiny::tags$p)
      )
    })

    shiny::observeEvent(input$specification_file, {
      shiny::req(input$specification_file$datapath)
      imported <- tryCatch(
        osem_shiny_import_table(
          input$specification_file$datapath,
          input$specification_file$name
        ),
        error = function(e) e
      )
      if (inherits(imported, "error")) {
        osem_shiny_notify_error(imported, session)
        return()
      }
      normalised <- tryCatch(osem_shiny_normalise_specification(imported), error = function(e) e)
      if (inherits(normalised, "error")) {
        osem_shiny_notify_error(normalised, session)
        return()
      }
      state$specification <- normalised
      selected_module(if (nrow(normalised) > 0L) 1L else NULL)
      osem_shiny_state_mark_changed(
        state,
        "specification",
        paste0("Replaced the specification with '", input$specification_file$name, "'.")
      )
    })

    output$specification_table <- DT::renderDT({
      DT::datatable(
        state$specification,
        rownames = FALSE,
        editable = list(target = "cell"),
        selection = list(mode = "multiple", target = "row"),
        filter = "top",
        options = c(
          osem_shiny_dt_options(20L),
          list(columnDefs = list(list(className = "dt-left", targets = "_all")))
        )
      )
    }, server = TRUE)

    shiny::observeEvent(input$specification_table_cell_edit, {
      edited <- tryCatch(
        DT::editData(
          state$specification,
          input$specification_table_cell_edit,
          rownames = FALSE
        ),
        error = function(e) e
      )
      if (inherits(edited, "error")) {
        osem_shiny_notify_error(edited, session)
        return()
      }
      normalised <- tryCatch(osem_shiny_normalise_specification(edited), error = function(e) e)
      if (inherits(normalised, "error")) {
        osem_shiny_notify_error(normalised, session)
        return()
      }
      state$specification <- normalised
      osem_shiny_state_mark_changed(state, "specification", "Edited the advanced specification table.")
    })

    output$validation <- shiny::renderUI({
      osem_shiny_issue_list(
        derived$workspace()$issues,
        area = c("Specification", "Environment"),
        max_items = 30L
      )
    })

    output$summary_cards <- shiny::renderUI({
      summary <- osem_shiny_specification_summary(state$specification)
      shiny::fluidRow(
        shiny::column(3, osem_shiny_metric_card("Modules", summary$modules, NULL, "info")),
        shiny::column(3, osem_shiny_metric_card("Estimated", summary$estimated, NULL, "info")),
        shiny::column(3, osem_shiny_metric_card("Identities", summary$identities, NULL, "info")),
        shiny::column(3, osem_shiny_metric_card("CVAR systems", summary$cvar_systems, NULL, "info"))
      )
    })

    output$dependency_plot <- shiny::renderPlot({
      osem_shiny_plot_specification_graph(state$specification)
    }, res = 96)

    output$ordered_specification <- DT::renderDT({
      ordered <- derived$workspace()$ordered_specification
      if (is.null(ordered) || !is.data.frame(ordered)) {
        return(DT::datatable(
          data.frame(Message = "Execution order is unavailable until the specification is valid."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      DT::datatable(
        ordered,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
    })

    output$required_variables <- DT::renderDT({
      coverage <- derived$workspace()$coverage
      if (!is.data.frame(coverage) || nrow(coverage) == 0L) {
        return(DT::datatable(
          data.frame(Message = "Required variables will appear after specification and dictionary validation."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      logical_columns <- names(coverage)[vapply(coverage, is.logical, logical(1L))]
      for (name in logical_columns) coverage[[name]] <- ifelse(coverage[[name]], "Yes", "No")
      names(coverage) <- gsub("_", " ", tools::toTitleCase(names(coverage)), fixed = TRUE)
      DT::datatable(
        coverage,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
    })

    output$download_specification <- shiny::downloadHandler(
      filename = function() {
        paste0(
          osem_shiny_project_slug(state$project_name),
          "-specification.",
          input$specification_format %||% "csv"
        )
      },
      content = function(file) {
        specification <- state$specification
        format <- input$specification_format %||% "csv"
        if (identical(format, "rds")) {
          saveRDS(specification, file, version = 3)
        } else if (identical(format, "xlsx")) {
          if (!requireNamespace("writexl", quietly = TRUE)) {
            stop("Package 'writexl' is required for Excel export.", call. = FALSE)
          }
          writexl::write_xlsx(specification, path = file)
        } else {
          utils::write.csv(specification, file, row.names = FALSE, na = "")
        }
      }
    )
  })
}
