# OSEM Shiny module: project ------------------------------------------------

osem_shiny_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Project workspace",
      "Name the analysis, review readiness, and save or reopen a portable OSEM project file."
    ),
    shiny::uiOutput(ns("metrics")),
    shiny::fluidRow(
      shiny::column(
        width = 7,
        osem_shiny_panel(
          "Project details",
          shiny::textInput(ns("project_name"), "Project name"),
          shiny::textAreaInput(
            ns("project_description"),
            "Description",
            rows = 5,
            placeholder = "Purpose, policy question, country, vintage, or institutional notes"
          )
        ),
        osem_shiny_panel(
          "Workspace readiness",
          shiny::uiOutput(ns("validation"))
        )
      ),
      shiny::column(
        width = 5,
        osem_shiny_panel(
          "Start or reset",
          shiny::tags$p(
            "The bundled example loads the package's sample input without changing any modelling function."
          ),
          shiny::actionButton(
            ns("load_example"),
            "Load bundled sample data",
            class = "btn-primary"
          ),
          shiny::actionButton(
            ns("reset_workspace"),
            "Reset workspace",
            class = "btn-default"
          )
        ),
        osem_shiny_panel(
          "Save or open a project",
          shiny::fileInput(
            ns("project_file"),
            "Open OSEM project (typically a .rds file)",
            accept = c(".rds", ".osem-project.rds", "application/octet-stream")
          ),
          shiny::fileInput(
            ns("model_file"),
            "Or open a fitted OSEM model (typically a .rds file)",
            accept = c(".rds", "application/octet-stream")
          ),
          shiny::checkboxInput(
            ns("include_model"),
            "Include fitted model and forecast objects when available",
            value = TRUE
          ),
          shiny::downloadButton(
            ns("download_project"),
            "Download project (.rds)",
            class = "btn-primary"
          ),
          shiny::tags$p(
            class = "help-block",
            "The project file stores specifications, dictionary, prepared data snapshots, settings, and optionally results. The Reproduce page creates a standalone replication bundle."
          )
        )
      )
    ),
    osem_shiny_panel(
      "Activity",
      DT::DTOutput(ns("activity"))
    )
  )
}

osem_shiny_project_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      state$project_revision
      shiny::updateTextInput(session, "project_name", value = state$project_name)
      shiny::updateTextAreaInput(session, "project_description", value = state$project_description)
    })

    shiny::observeEvent(input$project_name, {
      value <- trimws(input$project_name %||% "")
      if (!nzchar(value)) value <- "Untitled OSEM project"
      state$project_name <- value
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$project_description, {
      state$project_description <- input$project_description %||% ""
    }, ignoreInit = TRUE)

    output$metrics <- shiny::renderUI({
      workspace <- derived$workspace()
      effective <- derived$effective_data()$data
      variables <- if (is.data.frame(effective) && "na_item" %in% names(effective)) {
        length(unique(effective$na_item))
      } else {
        0L
      }
      source_count <- length(state$input_sources)
      module_count <- if (is.data.frame(state$specification)) nrow(state$specification) else 0L
      counts <- osem_shiny_issue_counts(workspace$issues)

      shiny::fluidRow(
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Readiness",
            if (workspace$ready) "Ready" else "Attention",
            if (workspace$ready) "Inputs can be passed to run_model()." else paste0(counts$error, " blocking error(s)"),
            if (workspace$ready) "success" else "warning"
          )
        ),
        shiny::column(3, osem_shiny_metric_card("Modules", module_count, "Specification rows", "info")),
        shiny::column(3, osem_shiny_metric_card("Local variables", variables, paste0(source_count, " source(s)"), "info")),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Model",
            tools::toTitleCase(state$model_status),
            state$last_change_reason,
            osem_shiny_status_class(state$model_status)
          )
        )
      )
    })

    output$validation <- shiny::renderUI({
      osem_shiny_issue_list(derived$workspace()$issues, max_items = 12L)
    })

    shiny::observeEvent(input$load_example, {
      already_loaded <- any(vapply(
        state$input_sources,
        function(x) identical(x$kind, "bundled_example"),
        logical(1L)
      ))
      if (already_loaded) {
        shiny::showNotification(
          "The bundled sample data are already in the source list.",
          type = "message",
          session = session
        )
        return()
      }

      example_data <- get("sample_input", envir = asNamespace("osem"))
      source <- osem_shiny_source_from_data(
        data = example_data,
        display_name = "OSEM bundled sample input",
        source_id = osem_shiny_state_next_source_id(state),
        kind = "bundled_example"
      )
      state$input_sources <- c(state$input_sources, list(source))
      osem_shiny_state_mark_changed(
        state,
        "data",
        "Loaded the bundled OSEM sample input."
      )
      shiny::showNotification(
        "Bundled sample input added.",
        type = "message",
        session = session
      )
    })

    shiny::observeEvent(input$reset_workspace, {
      shiny::showModal(shiny::modalDialog(
        title = "Reset the OSEM workspace?",
        "This removes current input sources, settings, model, and forecast objects from this session.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            session$ns("confirm_reset"),
            "Reset workspace",
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_reset, {
      shiny::removeModal()
      osem_shiny_project_reset(state)
      shiny::showNotification("Workspace reset.", type = "message", session = session)
    })

    shiny::observeEvent(input$model_file, {
      shiny::req(input$model_file$datapath)
      model <- tryCatch(readRDS(input$model_file$datapath), error = function(e) e)
      if (inherits(model, "error")) {
        osem_shiny_notify_error(model, session)
        return()
      }
      display_name <- tools::file_path_sans_ext(basename(input$model_file$name %||% "Imported OSEM model"))
      imported <- tryCatch(
        osem_shiny_state_import_model(state, model, project_name = display_name),
        error = function(e) e
      )
      if (inherits(imported, "error")) {
        osem_shiny_notify_error(imported, session)
        return()
      }
      shiny::showNotification(
        paste0("Opened fitted model: ", state$project_name),
        type = "message",
        duration = 6,
        session = session
      )
    })

    shiny::observeEvent(input$project_file, {
      shiny::req(input$project_file$datapath)
      project <- tryCatch(readRDS(input$project_file$datapath), error = function(e) e)
      if (inherits(project, "error")) {
        osem_shiny_notify_error(project, session)
        return()
      }
      restored <- tryCatch(osem_shiny_project_restore(state, project), error = function(e) e)
      if (inherits(restored, "error")) {
        osem_shiny_notify_error(restored, session)
        return()
      }
      shiny::showNotification(
        paste0("Opened project: ", state$project_name),
        type = "message",
        duration = 6,
        session = session
      )
    })

    output$download_project <- shiny::downloadHandler(
      filename = function() {
        paste0(osem_shiny_project_slug(state$project_name), ".osem-project.rds")
      },
      content = function(file) {
        project <- osem_shiny_project_snapshot(
          state,
          include_model = isTRUE(input$include_model)
        )
        saveRDS(project, file = file, version = 3)
        state$activity <- osem_shiny_activity_add(
          state$activity,
          area = "Project",
          action = "Downloaded project",
          detail = basename(file)
        )
      }
    )

    output$activity <- DT::renderDT({
      activity <- state$activity
      if (nrow(activity) == 0L) return(DT::datatable(data.frame()))
      activity$timestamp <- format(activity$timestamp, "%Y-%m-%d %H:%M:%S")
      activity <- activity[nrow(activity):1L, , drop = FALSE]
      names(activity) <- c("Time", "Area", "Action", "Detail")
      DT::datatable(
        activity,
        rownames = FALSE,
        filter = "top",
        options = c(osem_shiny_dt_options(10L), list(order = list(list(0, "desc"))))
      )
    })
  })
}
