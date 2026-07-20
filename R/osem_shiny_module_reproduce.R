# OSEM Shiny module: reproduction ------------------------------------------

osem_shiny_reproduce_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Reproduce",
      paste0(
        "Review the R calls represented by the workspace, copy them into an ",
        "analysis script, or download a self-contained replication bundle."
      ),
      shiny::div(
        class = "osem-action-row",
        shiny::downloadButton(ns("download_script"), "Download analysis.R"),
        shiny::downloadButton(
          ns("download_bundle"),
          "Download replication ZIP",
          class = "btn-primary"
        )
      )
    ),
    shiny::uiOutput(ns("metrics")),
    shiny::fluidRow(
      shiny::column(
        4,
        osem_shiny_panel(
          "Reproduction settings",
          shiny::radioButtons(
            ns("reproduction_mode"),
            "Data and settings context",
            choices = c(
              "Reproduce the stored fitted model" = "exact",
              "Use the current workspace and source priority" = "current"
            ),
            selected = "exact"
          ),
          shiny::checkboxInput(
            ns("include_all_scenarios"),
            "Include every configured forecast scenario",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("include_objects"),
            "Include fitted model and forecast RDS objects",
            value = TRUE
          ),
          shiny::tags$p(
            class = "help-block",
            paste0(
              "Exact mode uses the specification, dictionary, processed data, ",
              "and recorded settings associated with the stored fitted model. ",
              "Current-workspace mode retains the selected local/download priority."
            )
          )
        ),
        osem_shiny_panel(
          "Portable inputs",
          shiny::div(
            class = "osem-button-grid",
            shiny::downloadButton(ns("download_snapshot"), "Input snapshot (RDS)"),
            shiny::downloadButton(ns("download_specification"), "Specification (CSV)"),
            shiny::downloadButton(ns("download_dictionary"), "Dictionary (CSV)"),
            shiny::downloadButton(ns("download_project"), "Project (RDS)")
          )
        )
      ),
      shiny::column(
        8,
        osem_shiny_panel(
          "Readiness",
          shiny::uiOutput(ns("readiness"))
        ),
        osem_shiny_panel(
          "What the ZIP contains",
          DT::DTOutput(ns("bundle_contents"))
        )
      )
    ),
    shiny::div(
      class = "osem-callout osem-callout-info",
      shiny::tags$strong("Portable execution: "),
      paste0(
        "The standalone analysis.R file expects its companion data/ files. ",
        "Use the replication ZIP when transferring the analysis to another ",
        "machine or analyst."
      )
    ),
    shiny::tabsetPanel(
      id = ns("reproduction_tabs"),
      shiny::tabPanel(
        "Complete script",
        shiny::div(
          class = "osem-code-toolbar",
          shiny::actionButton(ns("copy_complete"), "Copy complete script"),
          shiny::downloadButton(ns("download_script_inline"), "Download script")
        ),
        shiny::verbatimTextOutput(ns("complete_code"), placeholder = TRUE)
      ),
      shiny::tabPanel(
        "Model call",
        shiny::div(
          class = "osem-code-toolbar",
          shiny::actionButton(ns("copy_model"), "Copy model code")
        ),
        shiny::verbatimTextOutput(ns("model_code"), placeholder = TRUE),
        osem_shiny_panel(
          "Resolved run_model() arguments",
          DT::DTOutput(ns("run_arguments"))
        )
      ),
      shiny::tabPanel(
        "Active forecast call",
        shiny::div(
          class = "osem-code-toolbar",
          shiny::actionButton(ns("copy_forecast"), "Copy forecast code")
        ),
        shiny::verbatimTextOutput(ns("forecast_code"), placeholder = TRUE),
        osem_shiny_panel(
          "Forecast assumptions included in the bundle",
          DT::DTOutput(ns("forecast_assumptions"))
        )
      ),
      shiny::tabPanel(
        "Provenance",
        shiny::fluidRow(
          shiny::column(
            6,
            osem_shiny_panel("Workspace record", DT::DTOutput(ns("provenance_summary")))
          ),
          shiny::column(
            6,
            osem_shiny_panel("Input sources", DT::DTOutput(ns("source_provenance")))
          )
        ),
        osem_shiny_panel("Variable attribution", DT::DTOutput(ns("variable_provenance")))
      )
    )
  )
}

osem_shiny_reproduce_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    exact_mode <- shiny::reactive({
      identical(input$reproduction_mode %||% "exact", "exact")
    })
    include_all <- shiny::reactive(isTRUE(input$include_all_scenarios))

    readiness <- shiny::reactive({
      osem_shiny_reproduction_readiness(
        state,
        exact = exact_mode(),
        include_all_scenarios = include_all()
      )
    })

    complete_code <- shiny::reactive(readiness()$script)
    model_code <- shiny::reactive(osem_shiny_run_code(state, exact = exact_mode()))
    forecast_code <- shiny::reactive(
      osem_shiny_active_forecast_code(state, exact = exact_mode())
    )

    output$metrics <- shiny::renderUI({
      ready <- readiness()
      context <- ready$context
      data <- context$data
      data_objects <- if (is.null(data)) {
        0L
      } else if (is.data.frame(data)) {
        1L
      } else if (is.list(data)) {
        length(data)
      } else {
        1L
      }
      scenario_count <- length(osem_shiny_selected_scenarios(
        state,
        include_all_scenarios = include_all()
      ))
      issue_counts <- osem_shiny_issue_counts(ready$issues)

      shiny::fluidRow(
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Context",
            if (isTRUE(context$exact)) "Exact snapshot" else "Current workspace",
            context$label,
            if (isTRUE(context$exact)) "success" else "info"
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Generated code",
            if (isTRUE(ready$parse_status$valid)) "Parses" else "Invalid",
            if (isTRUE(ready$parse_status$valid)) {
              "Language-object generation passed"
            } else {
              ready$parse_status$error
            },
            if (isTRUE(ready$parse_status$valid)) "success" else "danger"
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Input objects",
            data_objects,
            paste0(context$primary_source, " source priority"),
            if (data_objects > 0L || identical(context$primary_source, "download")) "info" else "warning"
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Bundle readiness",
            if (isTRUE(ready$valid)) "Ready" else "Blocked",
            paste0(issue_counts$error, " error(s); ", scenario_count, " scenario(s)"),
            if (isTRUE(ready$valid)) "success" else "danger"
          )
        )
      )
    })

    output$readiness <- shiny::renderUI({
      osem_shiny_issue_list(readiness()$issues, max_items = 40L)
    })

    output$complete_code <- shiny::renderText(complete_code())
    output$model_code <- shiny::renderText(model_code())
    output$forecast_code <- shiny::renderText(forecast_code())

    copy_text <- function(text, label) {
      session$sendCustomMessage(
        "osem-copy-text",
        list(text = as.character(text), label = label)
      )
      invisible(TRUE)
    }

    shiny::observeEvent(input$copy_complete, {
      copy_text(complete_code(), "Complete OSEM script")
    })
    shiny::observeEvent(input$copy_model, {
      copy_text(model_code(), "Model code")
    })
    shiny::observeEvent(input$copy_forecast, {
      copy_text(forecast_code(), "Forecast code")
    })

    output$run_arguments <- DT::renderDT({
      context <- readiness()$context
      input_count <- if (is.null(context$data)) {
        0L
      } else if (is.data.frame(context$data)) {
        1L
      } else if (is.list(context$data)) {
        length(context$data)
      } else {
        1L
      }
      DT::datatable(
        osem_shiny_run_argument_table(
          context$run_args,
          primary_source = context$primary_source,
          input_count = input_count
        ),
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    output$bundle_contents <- DT::renderDT({
      DT::datatable(
        osem_shiny_bundle_contents(
          state,
          exact = exact_mode(),
          include_all_scenarios = include_all(),
          include_objects = isTRUE(input$include_objects)
        ),
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    output$forecast_assumptions <- DT::renderDT({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        return(DT::datatable(
          data.frame(Message = "No forecast scenario is configured."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      assumptions <- osem_shiny_scenario_assumptions_for_export(
        scenario,
        exact = exact_mode()
      )
      if (!is.data.frame(assumptions) || nrow(assumptions) == 0L) {
        return(DT::datatable(
          data.frame(
            Message = paste0(
              "No explicit path is exported. forecast_model() will generate exogenous values with '",
              scenario$args$exog_fill_method,
              "'."
            )
          ),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      display <- assumptions
      if ("time" %in% names(display)) display$time <- format(as.Date(display$time))
      DT::datatable(
        display,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
    })

    output$provenance_summary <- DT::renderDT({
      context <- readiness()$context
      summary <- data.frame(
        Item = c(
          "Project",
          "Reproduction context",
          "OSEM package version",
          "R version",
          "Model status",
          "Forecast status",
          "Primary source",
          "Data revision",
          "Specification revision",
          "Dictionary revision",
          "Settings revision",
          "Forecast revision",
          "Last workspace change"
        ),
        Value = c(
          state$project_name,
          context$label,
          tryCatch(as.character(utils::packageVersion("osem")), error = function(e) "Unknown"),
          R.version.string,
          state$model_status,
          state$forecast_status,
          context$primary_source,
          state$data_revision,
          state$specification_revision,
          state$dictionary_revision,
          state$settings_revision,
          state$forecast_revision,
          paste0(format(state$last_change, "%Y-%m-%d %H:%M:%S"), " — ", state$last_change_reason)
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      DT::datatable(summary, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$source_provenance <- DT::renderDT({
      table <- derived$source_table()
      if (!is.data.frame(table) || nrow(table) == 0L) {
        table <- data.frame(Message = "No local input source is registered.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        filter = if (ncol(table) > 1L) "top" else "none",
        options = osem_shiny_dt_options(15L)
      )
    })

    output$variable_provenance <- DT::renderDT({
      effective <- derived$effective_data()
      table <- effective$attribution
      if (!is.data.frame(table) || nrow(table) == 0L) {
        table <- data.frame(Message = "No effective local-variable attribution is available.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        filter = if (ncol(table) > 1L) "top" else "none",
        options = osem_shiny_dt_options(20L)
      )
    })

    script_filename <- function() {
      paste0(osem_shiny_project_slug(state$project_name), "-analysis.R")
    }
    write_script <- function(file) {
      writeLines(complete_code(), file, useBytes = TRUE)
    }

    output$download_script <- shiny::downloadHandler(
      filename = script_filename,
      content = write_script
    )
    output$download_script_inline <- shiny::downloadHandler(
      filename = script_filename,
      content = write_script
    )

    output$download_bundle <- shiny::downloadHandler(
      filename = function() {
        paste0(
          osem_shiny_project_slug(state$project_name),
          if (exact_mode()) "-exact-replication.zip" else "-current-workspace.zip"
        )
      },
      content = function(file) {
        ready <- readiness()
        if (!isTRUE(ready$valid)) {
          errors <- ready$issues$message[ready$issues$level == "error"]
          stop(paste(errors, collapse = "\n"), call. = FALSE)
        }
        osem_shiny_write_replication_bundle(
          state,
          target_zip = file,
          exact = exact_mode(),
          include_objects = isTRUE(input$include_objects),
          include_all_scenarios = include_all()
        )
        state$activity <- osem_shiny_activity_add(
          state$activity,
          area = "Reproduce",
          action = "Downloaded bundle",
          detail = if (exact_mode()) "Exact fitted-model snapshot" else "Current workspace"
        )
      }
    )

    output$download_snapshot <- shiny::downloadHandler(
      filename = function() {
        paste0(
          osem_shiny_project_slug(state$project_name),
          if (exact_mode()) "-model-input-snapshot.rds" else "-current-input-snapshot.rds"
        )
      },
      content = function(file) {
        saveRDS(readiness()$context$data, file, version = 3)
      }
    )

    output$download_specification <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-specification.csv"),
      content = function(file) {
        osem_shiny_write_csv(readiness()$context$specification, file)
      }
    )

    output$download_dictionary <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-dictionary.csv"),
      content = function(file) {
        osem_shiny_write_csv(readiness()$context$dictionary, file)
      }
    )

    output$download_project <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), ".osem-project.rds"),
      content = function(file) {
        saveRDS(
          osem_shiny_project_snapshot(state, include_model = isTRUE(input$include_objects)),
          file,
          version = 3
        )
      }
    )
  })
}
