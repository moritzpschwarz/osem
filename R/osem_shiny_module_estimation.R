# OSEM Shiny module: estimation --------------------------------------------

osem_shiny_estimation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Estimation",
      "Configure and run the complete OSEM estimation workflow. The interface calls run_model() directly and leaves all modelling functions unchanged.",
      shiny::div(
        class = "osem-action-row",
        shiny::downloadButton(ns("download_model"), "Model RDS"),
        shiny::downloadButton(ns("download_processed"), "Processed data")
      )
    ),
    shiny::uiOutput(ns("metrics")),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        osem_shiny_panel(
          "Estimation settings",
          shiny::tabsetPanel(
            id = ns("settings_tabs"),
            shiny::tabPanel(
              "Core model",
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::selectInput(
                    ns("ardl_or_ecm"),
                    "Model workflow",
                    choices = c("ARDL" = "ardl", "ECM" = "ecm"),
                    selected = "ardl"
                  ),
                  shiny::conditionalPanel(
                    condition = "input.ardl_or_ecm == 'ecm'",
                    ns = ns,
                    shiny::selectInput(
                      ns("ecm_pretest"),
                      "ECM pretesting",
                      choices = c(
                        "Automatic model-form decision" = "auto",
                        "Diagnostics only; estimate ECM" = "diagnostic",
                        "No pretest; unrestricted ECM" = "none"
                      ),
                      selected = "auto"
                    )
                  ),
                  shiny::selectInput(
                    ns("use_logs"),
                    "Transformation scope",
                    choices = c(
                      "Dependent and conditioning variables" = "both",
                      "Dependent variables only" = "y",
                      "Conditioning variables only" = "x",
                      "No log/asinh transformations" = "none"
                    ),
                    selected = "both"
                  ),
                  shiny::checkboxInput(ns("trend"), "Include a deterministic trend", TRUE)
                ),
                shiny::column(
                  6,
                  shiny::numericInput(ns("max_ar"), "Maximum autoregressive lags", 4, min = 0, max = 40, step = 1),
                  shiny::numericInput(ns("max_dl"), "Maximum distributed lags", 4, min = 0, max = 40, step = 1),
                  shiny::checkboxInput(
                    ns("minimum_sample"),
                    "Constrain all series to the common minimum sample",
                    TRUE
                  ),
                  shiny::tags$p(
                    class = "help-block",
                    "Lag-only regressors and CVAR membership are configured on the Specification page."
                  )
                )
              )
            ),
            shiny::tabPanel(
              "Selection and saturation",
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::checkboxInput(ns("gets_selection"), "Run GETS model selection", TRUE),
                  shiny::numericInput(
                    ns("selection_tpval"),
                    "GETS target p-value",
                    0.01,
                    min = 0,
                    max = 1,
                    step = 0.001
                  ),
                  shiny::textInput(
                    ns("keep"),
                    "Keep expression (regular expression)",
                    value = "",
                    placeholder = "Example: policy_rate|mconst"
                  ),
                  shiny::tags$p(
                    class = "help-block",
                    "Terms matching this expression are protected from selection. Leave blank for no protection."
                  )
                ),
                shiny::column(
                  6,
                  shiny::checkboxGroupInput(
                    ns("saturation"),
                    "Indicator saturation",
                    choices = c(
                      "Impulse indicators (IIS)" = "IIS",
                      "Step indicators (SIS)" = "SIS",
                      "Trend indicators (TIS)" = "TIS"
                    ),
                    selected = c("IIS", "SIS")
                  ),
                  shiny::numericInput(
                    ns("saturation_tpval"),
                    "Saturation target p-value",
                    0.01,
                    min = 0,
                    max = 1,
                    step = 0.001
                  ),
                  shiny::numericInput(
                    ns("max_block_size"),
                    "Maximum saturation block size",
                    20,
                    min = 1,
                    max = 10000,
                    step = 1
                  ),
                  shiny::checkboxInput(
                    ns("pretest_steps"),
                    "Run staged SIS pretesting before other saturation methods",
                    FALSE
                  )
                )
              )
            ),
            shiny::tabPanel(
              "CVAR systems",
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::numericInput(
                    ns("cvar_ar"),
                    "VAR lag order in levels",
                    2,
                    min = 1,
                    max = 40,
                    step = 1
                  ),
                  shiny::checkboxInput(
                    ns("coint_seasonal"),
                    "Include seasonal dummies in cointegration analysis",
                    FALSE
                  )
                ),
                shiny::column(
                  6,
                  shiny::selectInput(
                    ns("coint_deterministic"),
                    "Deterministic term in the cointegrating relation",
                    choices = c("None" = "none", "Constant" = "const", "Trend" = "trend"),
                    selected = "const"
                  ),
                  shiny::selectInput(
                    ns("coint_significance"),
                    "Trace-test significance threshold",
                    choices = c("1 percent" = "1pct", "5 percent" = "5pct", "10 percent" = "10pct"),
                    selected = "5pct"
                  )
                )
              ),
              shiny::tags$p(
                class = "help-block",
                "These controls are used only for modules assigned to a CVAR system."
              )
            ),
            shiny::tabPanel(
              "Execution",
              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::numericInput(
                    ns("seed"),
                    "Random seed",
                    123,
                    min = 1,
                    max = .Machine$integer.max,
                    step = 1
                  ),
                  shiny::checkboxInput(
                    ns("quiet"),
                    "Suppress routine model messages",
                    FALSE
                  )
                ),
                shiny::column(
                  6,
                  shiny::textInput(
                    ns("save_to_disk"),
                    "Optional server-side processed-data path",
                    value = "",
                    placeholder = "Leave blank for no server-side write"
                  ),
                  shiny::tags$p(
                    class = "help-block",
                    "For normal app use, leave this blank and use the download button. A path here is interpreted on the machine hosting Shiny."
                  )
                )
              )
            ),
            shiny::tabPanel(
              "Review",
              shiny::uiOutput(ns("settings_summary")),
              DT::DTOutput(ns("argument_table"))
            )
          ),
          shiny::div(
            class = "osem-estimation-actions",
            shiny::actionButton(ns("apply_settings"), "Apply settings", class = "btn-default"),
            shiny::uiOutput(ns("run_button"))
          )
        )
      ),
      shiny::column(
        width = 4,
        osem_shiny_panel("Readiness", shiny::uiOutput(ns("validation"))),
        osem_shiny_panel(
          "Run status",
          shiny::uiOutput(ns("run_status")),
          shiny::downloadButton(ns("download_log"), "Download execution log")
        )
      )
    ),
    osem_shiny_panel(
      "Execution log",
      shiny::tags$pre(class = "osem-log-output", shiny::textOutput(ns("log")))
    )
  )
}

osem_shiny_estimation_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    collect_settings <- shiny::reactive({
      osem_shiny_normalise_run_args(list(
        use_logs = input$use_logs,
        trend = input$trend,
        ardl_or_ecm = input$ardl_or_ecm,
        ecm_pretest = input$ecm_pretest %||% "auto",
        max.ar = input$max_ar,
        max.dl = input$max_dl,
        saturation = input$saturation,
        saturation.tpval = input$saturation_tpval,
        max.block.size = input$max_block_size,
        gets_selection = input$gets_selection,
        selection.tpval = input$selection_tpval,
        constrain.to.minimum.sample = input$minimum_sample,
        keep = input$keep,
        pretest_steps = input$pretest_steps,
        quiet = input$quiet,
        save_to_disk = input$save_to_disk,
        cvar.ar = input$cvar_ar,
        coint_seasonal = input$coint_seasonal,
        coint_deterministic = input$coint_deterministic,
        coint_significance = input$coint_significance,
        seed = input$seed
      ))
    })

    sync_inputs <- function() {
      args <- osem_shiny_normalise_run_args(state$run_args)
      shiny::updateSelectInput(session, "ardl_or_ecm", selected = args$ardl_or_ecm)
      shiny::updateSelectInput(session, "ecm_pretest", selected = args$ecm_pretest)
      shiny::updateSelectInput(session, "use_logs", selected = args$use_logs)
      shiny::updateCheckboxInput(session, "trend", value = args$trend)
      shiny::updateNumericInput(session, "max_ar", value = args$max.ar)
      shiny::updateNumericInput(session, "max_dl", value = args$max.dl)
      shiny::updateCheckboxInput(session, "minimum_sample", value = args$constrain.to.minimum.sample)
      shiny::updateCheckboxInput(session, "gets_selection", value = args$gets_selection)
      shiny::updateNumericInput(session, "selection_tpval", value = args$selection.tpval)
      shiny::updateTextInput(session, "keep", value = args$keep %||% "")
      shiny::updateCheckboxGroupInput(session, "saturation", selected = args$saturation %||% character())
      shiny::updateNumericInput(session, "saturation_tpval", value = args$saturation.tpval)
      shiny::updateNumericInput(session, "max_block_size", value = args$max.block.size)
      shiny::updateCheckboxInput(session, "pretest_steps", value = args$pretest_steps)
      shiny::updateNumericInput(session, "cvar_ar", value = args$cvar.ar)
      shiny::updateCheckboxInput(session, "coint_seasonal", value = args$coint_seasonal)
      shiny::updateSelectInput(session, "coint_deterministic", selected = args$coint_deterministic)
      shiny::updateSelectInput(session, "coint_significance", selected = args$coint_significance)
      shiny::updateNumericInput(session, "seed", value = args$seed)
      shiny::updateCheckboxInput(session, "quiet", value = args$quiet)
      shiny::updateTextInput(session, "save_to_disk", value = args$save_to_disk %||% "")
    }

    shiny::observeEvent(state$project_revision, sync_inputs(), ignoreInit = FALSE)

    live_validation <- shiny::reactive({
      osem_shiny_validate_run_args(collect_settings(), specification = state$specification)
    })

    output$metrics <- shiny::renderUI({
      workspace <- derived$workspace()
      summary <- if (inherits(state$model, "osem")) osem_shiny_model_summary(state$model) else list()
      input <- derived$run_input()
      input_count <- if (is.null(input)) 0L else if (is.data.frame(input)) 1L else length(input)
      shiny::fluidRow(
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Input readiness",
            if (workspace$ready) "Ready" else "Blocked",
            if (workspace$ready) "Specification, dictionary, data and settings pass validation." else "Resolve blocking validation errors before estimation.",
            if (workspace$ready) "success" else "warning"
          )
        ),
        shiny::column(3, osem_shiny_metric_card("Modules", nrow(state$specification), "Specification rows", "info")),
        shiny::column(3, osem_shiny_metric_card("Local input", input_count, "Prepared data object(s)", "info")),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Model status",
            tools::toTitleCase(state$model_status),
            if (length(summary) > 0L) paste0(summary$modules, " fitted module(s)") else "No fitted model",
            osem_shiny_status_class(state$model_status)
          )
        )
      )
    })

    output$settings_summary <- shiny::renderUI({
      validation <- live_validation()
      shiny::tagList(
        shiny::div(
          class = "osem-callout osem-callout-info",
          osem_shiny_run_summary_text(validation$settings, state$specification)
        ),
        osem_shiny_issue_list(validation$issues, max_items = 12L)
      )
    })

    output$argument_table <- DT::renderDT({
      run_input <- derived$run_input()
      input_count <- if (is.null(run_input)) 0L else if (is.data.frame(run_input)) 1L else length(run_input)
      DT::datatable(
        osem_shiny_run_argument_table(collect_settings(), state$primary_source, input_count),
        rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    output$validation <- shiny::renderUI({
      workspace <- derived$workspace()
      settings <- live_validation()
      issues <- osem_shiny_bind_issues(workspace$issues, settings$issues)
      osem_shiny_issue_list(issues, max_items = 24L)
    })

    output$run_button <- shiny::renderUI({
      workspace <- derived$workspace()
      settings <- live_validation()
      ready <- isTRUE(workspace$ready) && isTRUE(settings$valid) && !identical(state$model_status, "running")
      shiny::actionButton(
        session$ns("run_model"),
        if (identical(state$model_status, "running")) "Running model..." else "Run model",
        class = "btn-primary btn-lg",
        disabled = if (ready) NULL else "disabled"
      )
    })

    shiny::observeEvent(input$apply_settings, {
      new_args <- collect_settings()
      if (!identical(new_args, osem_shiny_normalise_run_args(state$run_args))) {
        state$run_args <- new_args
        osem_shiny_state_mark_changed(
          state,
          "settings",
          "Applied revised estimation settings."
        )
      }
      shiny::showNotification("Estimation settings applied.", type = "message", session = session)
    })

    shiny::observeEvent(input$run_model, {
      new_args <- collect_settings()
      settings_validation <- osem_shiny_validate_run_args(new_args, state$specification)
      workspace <- osem_shiny_validate_workspace(
        specification = state$specification,
        dictionary = state$dictionary,
        input_sources = state$input_sources,
        primary_source = state$primary_source,
        effective = derived$effective_data()
      )
      if (!isTRUE(workspace$ready) || !isTRUE(settings_validation$valid)) {
        shiny::showNotification(
          "The model cannot run until the blocking validation errors are resolved.",
          type = "error",
          duration = 10,
          session = session
        )
        return()
      }

      if (!identical(new_args, osem_shiny_normalise_run_args(state$run_args))) {
        state$run_args <- new_args
        osem_shiny_state_mark_changed(state, "settings", "Applied settings before estimation.")
      }
      state$model_status <- "running"
      state$model_error <- NULL
      state$model_log <- character()
      state$model_messages <- character()
      state$model_warnings <- character()

      result <- shiny::withProgress(
        message = "Running OSEM model",
        detail = "Preparing data and estimating modules...",
        value = 0,
        {
          osem_shiny_execute_model(
            specification = state$specification,
            dictionary = state$dictionary,
            input = derived$run_input(),
            primary_source = state$primary_source,
            run_args = state$run_args
          )
        }
      )

      state$model_log <- result$log %||% character()
      state$model_messages <- result$messages %||% character()
      state$model_warnings <- result$warnings %||% character()
      state$model_error <- result$error
      state$model_run_metadata <- list(
        started = result$started,
        finished = result$finished,
        duration_seconds = result$duration_seconds,
        seed = state$run_args$seed,
        run_args = osem_shiny_normalise_run_args(state$run_args),
        revisions = osem_shiny_revision_stamp(state),
        imported = FALSE
      )

      if (isTRUE(result$ok) && inherits(result$value, "osem")) {
        state$model <- result$value
        state$model_status <- "current"
        snapshot <- file.path(state$session_dir, "processed-input-from-latest-model.rds")
        processed <- state$model$processed_input_data %||% state$model$full_data
        if (is.data.frame(processed)) {
          try(saveRDS(processed, snapshot, version = 3), silent = TRUE)
        }
        state$processed_snapshot_path <- if (file.exists(snapshot)) snapshot else NULL
        osem_shiny_state_invalidate_forecasts(state, "A new model was estimated.")
        state$forecast_revision <- state$forecast_revision + 1L
        state$last_change <- Sys.time()
        state$last_change_reason <- "Model estimation completed"
        state$activity <- osem_shiny_activity_add(
          state$activity,
          area = "Estimation",
          action = "Model estimated",
          detail = paste0(
            nrow(state$model$module_order), " module(s) in ",
            round(result$duration_seconds, 2), " second(s)."
          )
        )
        shiny::showNotification(
          "Model estimation completed successfully.",
          type = "message",
          duration = 8,
          session = session
        )
        shiny::updateNavbarPage(session$rootScope(), "osem_main_navigation", selected = "Results")
      } else {
        state$model_status <- "failed"
        state$last_change <- Sys.time()
        state$last_change_reason <- "The latest model estimation attempt failed"
        state$activity <- osem_shiny_activity_add(
          state$activity,
          area = "Estimation",
          action = "Model failed",
          detail = result$error %||% "Unknown estimation error"
        )
        shiny::showNotification(
          paste0("Estimation failed: ", result$error %||% "Unknown error"),
          type = "error",
          duration = NULL,
          session = session
        )
      }
    }, ignoreInit = TRUE)

    output$run_status <- shiny::renderUI({
      metadata <- state$model_run_metadata
      duration <- suppressWarnings(as.numeric(metadata$duration_seconds %||% NA_real_))
      shiny::tagList(
        shiny::div(
          class = "osem-status-line",
          osem_shiny_badge(tools::toTitleCase(state$model_status), osem_shiny_status_class(state$model_status)),
          if (is.finite(duration)) shiny::span(paste0("Duration: ", round(duration, 2), " seconds"))
        ),
        if (!is.null(state$model_error)) {
          shiny::div(class = "osem-callout osem-callout-danger", state$model_error)
        },
        if (length(state$model_warnings) > 0L) {
          shiny::div(
            class = "osem-callout osem-callout-warning",
            shiny::tags$strong(paste0(length(state$model_warnings), " warning(s): ")),
            paste(state$model_warnings, collapse = " | ")
          )
        },
        if (length(metadata$finished %||% NULL) > 0L) {
          shiny::tags$p(
            class = "help-block",
            paste0("Last completed attempt: ", format(as.POSIXct(metadata$finished), "%Y-%m-%d %H:%M:%S"))
          )
        }
      )
    })

    output$log <- shiny::renderText({
      parts <- c(
        if (length(state$model_log) > 0L) c("Console output", state$model_log) else NULL,
        if (length(state$model_messages) > 0L) c("", "Messages", state$model_messages) else NULL,
        if (length(state$model_warnings) > 0L) c("", "Warnings", state$model_warnings) else NULL,
        if (!is.null(state$model_error)) c("", "Error", state$model_error) else NULL
      )
      if (length(parts) == 0L) "No model execution has been recorded in this session." else paste(parts, collapse = "\n")
    })

    output$download_model <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-model.rds"),
      content = function(file) {
        if (!inherits(state$model, "osem")) stop("No fitted OSEM model is available.", call. = FALSE)
        saveRDS(state$model, file, version = 3)
      }
    )

    output$download_processed <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-processed-input.csv"),
      content = function(file) {
        if (!inherits(state$model, "osem")) stop("No fitted OSEM model is available.", call. = FALSE)
        processed <- state$model$processed_input_data %||% state$model$full_data
        if (!is.data.frame(processed)) {
          stop("The model does not contain a processed-data table.", call. = FALSE)
        }
        osem_shiny_write_csv(processed, file)
      }
    )

    output$download_log <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-estimation-log.txt"),
      content = function(file) {
        lines <- c(
          paste0("Project: ", state$project_name),
          paste0("Model status: ", state$model_status),
          "",
          "Console output:", state$model_log,
          "",
          "Messages:", state$model_messages,
          "",
          "Warnings:", state$model_warnings,
          "",
          "Error:", state$model_error %||% "None"
        )
        writeLines(lines, file, useBytes = TRUE)
      }
    )
  })
}
