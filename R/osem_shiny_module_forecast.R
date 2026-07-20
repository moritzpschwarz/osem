# OSEM Shiny module: forecast -----------------------------------------------

osem_shiny_forecast_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Forecast and scenarios",
      "Configure the complete forecast_model() interface, enter policy assumptions, run multiple scenarios, and compare their implications.",
      shiny::tagList(
        shiny::actionButton(ns("run_forecast"), "Run active scenario", class = "btn-primary"),
        shiny::downloadButton(ns("download_forecast"), "Download active forecast", class = "btn-default")
      )
    ),
    shiny::uiOutput(ns("content"))
  )
}

osem_shiny_forecast_workspace_ui <- function(ns, model) {
  module_order <- osem_shiny_safe_data_frame(model$module_order)
  variables <- if ("dependent" %in% names(module_order)) {
    unique(trimws(unlist(strsplit(as.character(module_order$dependent), ",", fixed = TRUE))))
  } else {
    character()
  }
  variables <- variables[!is.na(variables) & nzchar(variables)]
  exog_variables <- as.character(unlist(model$module_collection$indep))
  exog_variables <- exog_variables[!is.na(exog_variables) & nzchar(exog_variables)]
  exog_variables <- setdiff(exog_variables, variables)
  shiny::tagList(
    shiny::uiOutput(ns("metrics")),
    shiny::uiOutput(ns("status_banner")),
    shiny::tabsetPanel(
      id = ns("forecast_tabs"),
      shiny::tabPanel(
        "Scenarios",
        shiny::fluidRow(
          shiny::column(
            8,
            osem_shiny_panel(
              "Scenario portfolio",
              DT::DTOutput(ns("scenario_table"))
            )
          ),
          shiny::column(
            4,
            osem_shiny_panel(
              "Manage scenarios",
              shiny::selectInput(ns("scenario_selected"), "Active scenario", choices = character()),
              shiny::div(
                class = "osem-action-row",
                shiny::actionButton(ns("add_scenario"), "New"),
                shiny::actionButton(ns("duplicate_scenario"), "Duplicate"),
                shiny::actionButton(ns("delete_scenario"), "Delete")
              ),
              shiny::textInput(ns("scenario_name"), "Scenario name"),
              shiny::textAreaInput(ns("scenario_description"), "Description", rows = 4)
            )
          )
        )
      ),
      shiny::tabPanel(
        "Setup and assumptions",
        shiny::fluidRow(
          shiny::column(
            5,
            osem_shiny_panel(
              "Forecast configuration",
              shiny::numericInput(ns("n_ahead"), "Forecast horizon", 10, min = 1, max = 500, step = 1),
              shiny::textInput(
                ns("ci_levels"),
                "Confidence levels",
                value = "0.50, 0.66, 0.95",
                placeholder = "Comma-separated values between 0 and 1"
              ),
              shiny::numericInput(ns("uncertainty_sample"), "Uncertainty draws", 100, min = 1, max = 100000, step = 10),
              shiny::numericInput(ns("random_seed"), "Random seed", 123, min = 1, max = .Machine$integer.max, step = 1),
              shiny::checkboxInput(ns("quiet"), "Suppress routine forecast messages", FALSE),
              shiny::tags$p(
                class = "help-block",
                "The random seed is stored with the scenario and inserted into the replication script."
              )
            ),
            osem_shiny_panel(
              "Exogenous assumptions",
              shiny::radioButtons(
                ns("assumption_mode"),
                "How should future exogenous values be supplied?",
                choices = c(
                  "Generate automatically" = "automatic",
                  "Use a complete user-specified path" = "manual"
                ),
                selected = "automatic"
              ),
              shiny::conditionalPanel(
                condition = "input.assumption_mode == 'automatic'",
                ns = ns,
                shiny::selectInput(
                  ns("exog_fill_method"),
                  "Automatic fill method",
                  choices = c(
                    "Autoregressive model" = "AR",
                    "Automatic ARIMA" = "auto",
                    "Exponential smoothing (ETS)" = "ets",
                    "Last available value" = "last"
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input.exog_fill_method == 'AR'",
                  ns = ns,
                  shiny::numericInput(ns("ar_fill_max"), "Maximum AR lags", 4, min = 1, max = 40, step = 1)
                )
              )
            )
          ),
          shiny::column(
            7,
            shiny::conditionalPanel(
              condition = "input.assumption_mode == 'manual'",
              ns = ns,
              osem_shiny_panel(
                "Manual assumption path",
                shiny::fluidRow(
                  shiny::column(
                    6,
                    shiny::fileInput(
                      ns("assumption_file"),
                      "Import CSV, RDS, XLS, or XLSX",
                      accept = c(".csv", ".rds", ".RDS", ".xls", ".xlsx")
                    )
                  ),
                  shiny::column(
                    6,
                    shiny::div(
                      class = "osem-action-row osem-action-row-top",
                      shiny::actionButton(ns("template_last"), "Template: repeat last values"),
                      shiny::actionButton(ns("template_blank"), "Blank template")
                    )
                  )
                ),
                shiny::tags$p(
                  class = "help-block",
                  "The table must contain one row per forecast period, a 'time' column, and one column for every exogenous variable. Cells are editable."
                ),
                DT::DTOutput(ns("assumptions_table")),
                shiny::downloadButton(ns("download_assumptions"), "Download assumptions (CSV)", class = "btn-default")
              )
            ),
            osem_shiny_panel(
              "Scenario validation",
              shiny::uiOutput(ns("forecast_validation"))
            ),
            osem_shiny_panel(
              "Required exogenous variables",
              DT::DTOutput(ns("exogenous_variables"))
            )
          )
        )
      ),
      shiny::tabPanel(
        "Active result",
        shiny::fluidRow(
          shiny::column(
            8,
            osem_shiny_panel(
              "Forecast chart",
              shiny::radioButtons(
                ns("forecast_selection_mode"),
                "Variable selection",
                choices = c(
                  "Select variables" = "names",
                  "Use a pattern (grepl)" = "grepl"
                ),
                selected = "names",
                inline = TRUE
              ),
              shiny::fluidRow(
                shiny::column(
                  8,
                  shiny::conditionalPanel(
                    condition = "input.forecast_selection_mode == 'names'",
                    ns = ns,
                    shiny::selectizeInput(
                      ns("forecast_variables"),
                      "Variables",
                      choices = variables,
                      selected = variables,
                      multiple = TRUE,
                      options = list(
                        plugins = list("remove_button")
                      )
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = "input.forecast_selection_mode == 'grepl'",
                    ns = ns,
                    shiny::textInput(
                      ns("forecast_grepl_variables"),
                      "Variable selection pattern",
                      value = "",
                      placeholder = "For example: Emi|Energy|GDP"
                    )
                  )
                ),
                shiny::column(
                  4,
                  shiny::dateInput(
                    ns("forecast_first_date"),
                    "First date shown",
                    value = "2000-01-01",
                    format = "yyyy-mm-dd"
                  ),
                  shiny::checkboxInput(
                    ns("include_exogenous"),
                    "Include exogenous assumptions",
                    value = FALSE
                  )
                )
              ),
              shiny::plotOutput(ns("forecast_plot"), height = "650px")
            )
          ),
          shiny::column(
            4,
            osem_shiny_panel(
              "Active scenario",
              shiny::uiOutput(ns("active_result_summary")),
              shiny::downloadButton(ns("download_forecast_table"), "Download forecast table (CSV)", class = "btn-default")
            )
          )
        ),
        osem_shiny_panel(
          "Forecast data",
          DT::DTOutput(ns("forecast_table"))
        )
      ),
      shiny::tabPanel(
        "Compare scenarios",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(ns("comparison_baseline"), "Baseline scenario", choices = character())
          ),
          shiny::column(
            8,
            shiny::selectizeInput(
              ns("comparison_variables"),
              "Variables",
              choices = variables,
              selected = utils::head(variables, min(4L, length(variables))),
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            osem_shiny_panel(
              "Scenario levels",
              shiny::plotOutput(ns("comparison_level_plot"), height = "500px")
            )
          ),
          shiny::column(
            6,
            osem_shiny_panel(
              "Difference from baseline",
              shiny::plotOutput(ns("comparison_difference_plot"), height = "500px")
            )
          )
        ),
        osem_shiny_panel(
          "Comparison table",
          DT::DTOutput(ns("comparison_table")),
          shiny::downloadButton(ns("download_comparison"), "Download comparison (CSV)", class = "btn-default")
        )
      ),
      shiny::tabPanel(
        "Execution log",
        shiny::uiOutput(ns("forecast_run_status")),
        shiny::fluidRow(
          shiny::column(
            6,
            osem_shiny_panel("Console output", shiny::verbatimTextOutput(ns("forecast_log"), placeholder = TRUE))
          ),
          shiny::column(
            6,
            osem_shiny_panel("Messages, warnings, and errors", shiny::uiOutput(ns("forecast_messages")))
          )
        )
      )
    )
  )
}

osem_shiny_forecast_server <- function(id, state, derived) {

  shiny::moduleServer(id, function(input, output, session) {
    updating_controls <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$include_exogenous, {
      model <- state$model
      shiny::req(!is.null(model), inherits(model, "osem"))

      module_order <- osem_shiny_safe_data_frame(model$module_order)
      endogenous <- if ("dependent" %in% names(module_order)) {
        unique(trimws(unlist(strsplit(
          as.character(module_order$dependent),
          ",",
          fixed = TRUE
        ))))
      } else {
        character()
      }
      endogenous <- endogenous[!is.na(endogenous) & nzchar(endogenous)]

      exogenous <- as.character(unlist(model$module_collection$indep))
      exogenous <- exogenous[!is.na(exogenous) & nzchar(exogenous)]
      exogenous <- setdiff(exogenous, endogenous)

      choices <- if (isTRUE(input$include_exogenous)) {
        unique(c(endogenous, exogenous))
      } else {
        endogenous
      }

      current <- shiny::isolate(
        input$forecast_variables %||% character()
      )
      selected <- if (isTRUE(input$include_exogenous)) {
        unique(c(intersect(current, choices), exogenous))
      } else {
        intersect(current, endogenous)
      }
      if (length(selected) == 0L) selected <- endogenous

      shiny::updateSelectizeInput(
        session,
        "forecast_variables",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = TRUE)

    output$content <- shiny::renderUI({
      model <- state$model
      if (is.null(model) || !inherits(model, "osem")) {
        return(osem_shiny_empty_state(
          "A fitted model is required",
          "Run a model on the Estimation page or open a project containing a fitted OSEM model before configuring forecasts."
        ))
      }
      osem_shiny_forecast_workspace_ui(session$ns, model)
    })

    update_scenario_controls <- function() {
      scenario <- osem_shiny_get_active_scenario(state)
      choices <- osem_shiny_scenario_choices(state)
      if (is.null(scenario)) return(invisible(NULL))
      updating_controls(TRUE)
      shiny::updateSelectInput(
        session,
        "scenario_selected",
        choices = choices,
        selected = scenario$id
      )
      available_ids <- unname(choices)
      selected_baseline <- input$comparison_baseline
      if (is.null(selected_baseline) || !selected_baseline %in% available_ids) {
        selected_baseline <- scenario$id
      }
      shiny::updateSelectInput(
        session,
        "comparison_baseline",
        choices = choices,
        selected = selected_baseline
      )
      shiny::updateTextInput(session, "scenario_name", value = scenario$name)
      shiny::updateTextAreaInput(session, "scenario_description", value = scenario$description)
      shiny::updateNumericInput(session, "n_ahead", value = scenario$args$n.ahead)
      shiny::updateTextInput(
        session,
        "ci_levels",
        value = paste(format(scenario$args$ci.levels, trim = TRUE, scientific = FALSE), collapse = ", ")
      )
      shiny::updateNumericInput(session, "uncertainty_sample", value = scenario$args$uncertainty_sample)
      shiny::updateNumericInput(session, "random_seed", value = scenario$args$seed)
      shiny::updateCheckboxInput(session, "quiet", value = scenario$args$quiet)
      shiny::updateRadioButtons(session, "assumption_mode", selected = scenario$args$assumption_mode)
      shiny::updateSelectInput(session, "exog_fill_method", selected = scenario$args$exog_fill_method)
      shiny::updateNumericInput(session, "ar_fill_max", value = scenario$args$ar.fill.max)
      session$onFlushed(function() updating_controls(FALSE), once = TRUE)
      invisible(NULL)
    }

    shiny::observe({
      state$project_revision
      state$forecast_scenarios
      state$active_scenario_id
      update_scenario_controls()
    })

    shiny::observeEvent(input$scenario_selected, {
      if (isTRUE(updating_controls())) return()
      choices <- vapply(
        state$forecast_scenarios,
        function(x) osem_shiny_normalise_forecast_scenario(x)$id,
        character(1L)
      )
      if (!is.null(input$scenario_selected) && input$scenario_selected %in% choices) {
        state$active_scenario_id <- input$scenario_selected
        osem_shiny_sync_active_forecast(state)
        update_scenario_controls()
      }
    }, ignoreInit = TRUE)

    ci_input_validation <- shiny::reactive({
      value <- input$ci_levels
      if (is.null(value)) {
        scenario <- osem_shiny_get_active_scenario(state)
        value <- if (is.null(scenario)) "" else paste(scenario$args$ci.levels, collapse = ", ")
      }
      osem_shiny_validate_ci_levels_text(value)
    })

    scenario_input_values <- shiny::reactive({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) return(NULL)
      ci <- ci_input_validation()
      list(
        scenario_id = scenario$id,
        name = trimws(input$scenario_name %||% scenario$name),
        description = input$scenario_description %||% scenario$description,
        ci_validation = ci,
        args = osem_shiny_normalise_forecast_args(list(
          n.ahead = input$n_ahead %||% scenario$args$n.ahead,
          ci.levels = if (isTRUE(ci$valid)) ci$levels else scenario$args$ci.levels,
          exog_fill_method = input$exog_fill_method %||% scenario$args$exog_fill_method,
          ar.fill.max = input$ar_fill_max %||% scenario$args$ar.fill.max,
          uncertainty_sample = input$uncertainty_sample %||% scenario$args$uncertainty_sample,
          quiet = input$quiet %||% scenario$args$quiet,
          seed = input$random_seed %||% scenario$args$seed,
          assumption_mode = input$assumption_mode %||% scenario$args$assumption_mode
        ))
      )
    })

    scenario_inputs <- shiny::debounce(scenario_input_values, millis = 500)

    shiny::observe({
      values <- scenario_inputs()
      if (isTRUE(updating_controls()) || is.null(values)) return()
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario) || !identical(values$scenario_id, scenario$id)) return()
      if (!nzchar(values$name)) values$name <- "Scenario"

      metadata_changed <- !identical(scenario$name, values$name) ||
        !identical(scenario$description, values$description)
      args_changed <- !identical(scenario$args, values$args)
      if (!metadata_changed && !args_changed) return()

      scenario$name <- values$name
      scenario$description <- values$description
      if (args_changed) {
        scenario$args <- values$args
        scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
        scenario$stale_reason <- "Scenario settings changed after the last forecast run."
      }
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- if (args_changed) {
        "Forecast scenario settings changed"
      } else {
        "Forecast scenario metadata changed"
      }
      osem_shiny_sync_active_forecast(state)
    })

    shiny::observeEvent(input$add_scenario, {
      id <- osem_shiny_state_next_scenario_id(state)
      number <- length(state$forecast_scenarios) + 1L
      scenario <- osem_shiny_default_forecast_scenario(id, paste0("Scenario ", number))
      state$forecast_scenarios <- c(state$forecast_scenarios, list(scenario))
      state$active_scenario_id <- id
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Forecast scenario added"
      osem_shiny_sync_active_forecast(state)
      state$activity <- osem_shiny_activity_add(
        state$activity,
        area = "Forecast",
        action = "Added scenario",
        detail = scenario$name
      )
      update_scenario_controls()
    })

    shiny::observeEvent(input$duplicate_scenario, {
      source <- osem_shiny_get_active_scenario(state)
      if (is.null(source)) return()
      source$id <- osem_shiny_state_next_scenario_id(state)
      source$name <- paste0(source$name, " copy")
      source$result <- NULL
      source$status <- "unavailable"
      source$error <- NULL
      source$log <- character()
      source$messages <- character()
      source$warnings <- character()
      source$metadata <- list()
      source$created_at <- Sys.time()
      source$updated_at <- Sys.time()
      state$forecast_scenarios <- c(state$forecast_scenarios, list(source))
      state$active_scenario_id <- source$id
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Forecast scenario duplicated"
      osem_shiny_sync_active_forecast(state)
      state$activity <- osem_shiny_activity_add(
        state$activity,
        area = "Forecast",
        action = "Duplicated scenario",
        detail = source$name
      )
      update_scenario_controls()
    })

    shiny::observeEvent(input$delete_scenario, {
      if (length(state$forecast_scenarios) <= 1L) {
        shiny::showNotification("At least one scenario must remain.", type = "warning", session = session)
        return()
      }
      active <- osem_shiny_get_active_scenario(state)
      if (is.null(active)) return()
      shiny::showModal(shiny::modalDialog(
        title = paste0("Delete scenario '", active$name, "'?"),
        "The scenario settings and any stored forecast result will be removed from this workspace.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_delete_scenario"), "Delete scenario", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_delete_scenario, {
      shiny::removeModal()
      active_id <- state$active_scenario_id
      keep <- vapply(
        state$forecast_scenarios,
        function(x) osem_shiny_normalise_forecast_scenario(x)$id != active_id,
        logical(1L)
      )
      state$forecast_scenarios <- state$forecast_scenarios[keep]
      state$active_scenario_id <- osem_shiny_normalise_forecast_scenario(state$forecast_scenarios[[1L]])$id
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Forecast scenario deleted"
      osem_shiny_sync_active_forecast(state)
      state$activity <- osem_shiny_activity_add(
        state$activity,
        area = "Forecast",
        action = "Deleted scenario",
        detail = active_id %||% "Unknown scenario"
      )
      update_scenario_controls()
    })

    output$scenario_table <- DT::renderDT({
      table <- osem_shiny_scenario_table(state)
      if (nrow(table) == 0L) return(DT::datatable(data.frame()))
      display <- table[, setdiff(names(table), "id"), drop = FALSE]
      DT::datatable(
        display,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(10L)
      )
    })

    output$metrics <- shiny::renderUI({
      active <- osem_shiny_get_active_scenario(state)
      if (is.null(active)) {
        return(osem_shiny_empty_state(
          "No forecast scenario",
          "Create or restore a scenario before configuring a forecast."
        ))
      }
      frequency <- osem_shiny_model_frequency(state$model)
      current_count <- sum(vapply(
        state$forecast_scenarios,
        function(x) identical(osem_shiny_normalise_forecast_scenario(x)$status, "current"),
        logical(1L)
      ))
      shiny::fluidRow(
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Model",
            tools::toTitleCase(state$model_status),
            if (identical(state$model_status, "current")) "Ready to forecast" else "Re-estimation recommended",
            osem_shiny_status_class(state$model_status)
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Active scenario",
            active$name,
            tools::toTitleCase(active$status),
            osem_shiny_status_class(active$status)
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Frequency",
            frequency$label,
            if (frequency$supported) "Supported by forecast_model()" else "Not currently supported",
            if (frequency$supported) "success" else "warning"
          )
        ),
        shiny::column(
          3,
          osem_shiny_metric_card(
            "Scenario results",
            current_count,
            paste0(length(state$forecast_scenarios), " scenario(s) configured"),
            "info"
          )
        )
      )
    })

    output$status_banner <- shiny::renderUI({
      active <- osem_shiny_get_active_scenario(state)
      if (is.null(active)) return(NULL)
      if (identical(state$model_status, "current") && active$status %in% c("current", "unavailable")) return(NULL)
      message <- if (!identical(state$model_status, "current")) {
        "The fitted model does not match the current workspace inputs. Re-estimate before producing a policy forecast."
      } else if (identical(active$status, "stale")) {
        active$stale_reason %||% "The active forecast predates changes to its settings or assumptions."
      } else if (identical(active$status, "failed")) {
        paste0("The last forecast attempt failed: ", active$error %||% "error not recorded")
      } else {
        paste0("Active forecast status: ", active$status, ".")
      }
      shiny::div(
        class = paste0("osem-status-banner osem-status-", if (!identical(state$model_status, "current")) "warning" else osem_shiny_status_class(active$status)),
        shiny::tags$strong("Forecast status: "),
        message
      )
    })

    output$exogenous_variables <- DT::renderDT({
      variables <- derived$exogenous_variables()
      dictionary <- state$dictionary
      table <- data.frame(model_varname = variables, stringsAsFactors = FALSE)
      if (length(variables) > 0L && is.data.frame(dictionary) && "model_varname" %in% names(dictionary)) {
        fields <- intersect(c("model_varname", "full_name", "database", "freq"), names(dictionary))
        table <- merge(table, dictionary[, fields, drop = FALSE], by = "model_varname", all.x = TRUE, sort = FALSE)
      }
      if (nrow(table) == 0L) table <- data.frame(Message = "The model contains no exogenous variables.")
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$forecast_validation <- shiny::renderUI({
      active <- osem_shiny_get_active_scenario(state)
      if (is.null(active)) {
        return(osem_shiny_empty_state(
          "No scenario",
          "Create a forecast scenario before validation."
        ))
      }
      validation <- osem_shiny_validate_forecast_inputs(state$model, active)
      extra <- osem_shiny_empty_issues()
      ci <- ci_input_validation()
      if (!isTRUE(ci$valid)) {
        extra <- osem_shiny_add_issue(extra, "error", "Forecast", ci$message)
      }
      if (!identical(state$model_status, "current")) {
        extra <- osem_shiny_add_issue(
          extra,
          "error",
          "Forecast",
          "The fitted model is stale or failed. Re-estimate it before running a new forecast."
        )
      }
      osem_shiny_issue_list(osem_shiny_bind_issues(extra, validation$issues), max_items = 30L)
    })

    output$assumptions_table <- DT::renderDT({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        return(DT::datatable(
          data.frame(Message = "Create a forecast scenario."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      data <- scenario$exog_predictions
      if (!is.data.frame(data)) {
        return(DT::datatable(
          data.frame(Message = "Create or import a manual assumption table."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      display <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
      if ("time" %in% names(display)) display$time <- format(as.Date(display$time))
      DT::datatable(
        display,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = 0L)),
        options = c(osem_shiny_dt_options(20L), list(scrollY = "420px", scrollCollapse = TRUE))
      )
    })

    shiny::observeEvent(input$assumptions_table_cell_edit, {
      info <- input$assumptions_table_cell_edit
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) return()
      data <- scenario$exog_predictions
      if (!is.data.frame(data)) return()
      row <- info$row
      column <- info$col + 1L
      if (column < 1L || column > ncol(data) || row < 1L || row > nrow(data)) return()
      data[[column]][[row]] <- DT::coerceValue(info$value, data[[column]][[row]])
      scenario$exog_predictions <- data
      scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
      scenario$stale_reason <- "Manual forecast assumptions changed."
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Manual forecast assumptions changed"
      osem_shiny_sync_active_forecast(state)
    })

    load_template <- function(fill) {
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario) || !inherits(state$model, "osem")) return()
      template <- tryCatch(
        osem_shiny_forecast_template(state$model, scenario$args$n.ahead, fill = fill),
        error = function(e) e
      )
      if (inherits(template, "error")) {
        osem_shiny_notify_error(template, session)
        return()
      }
      scenario$args$assumption_mode <- "manual"
      scenario$exog_predictions <- template
      scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
      scenario$stale_reason <- "Manual forecast assumptions changed."
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Manual forecast assumption template created"
      osem_shiny_sync_active_forecast(state)
      update_scenario_controls()
    }

    shiny::observeEvent(input$template_last, {
      load_template("last")
    })
    shiny::observeEvent(input$template_blank, {
      load_template("blank")
    })

    shiny::observeEvent(input$assumption_file, {
      shiny::req(input$assumption_file$datapath)
      imported <- tryCatch(
        osem_shiny_import_table(input$assumption_file$datapath, input$assumption_file$name),
        error = function(e) e
      )
      if (inherits(imported, "error")) {
        osem_shiny_notify_error(imported, session)
        return()
      }
      normalised <- tryCatch(
        osem_shiny_normalise_exog_predictions(imported, model = state$model),
        error = function(e) e
      )
      if (inherits(normalised, "error")) {
        osem_shiny_notify_error(normalised, session)
        return()
      }
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        shiny::showNotification("Create a scenario before importing assumptions.", type = "error", session = session)
        return()
      }
      scenario$args$assumption_mode <- "manual"
      scenario$exog_predictions <- normalised
      scenario$status <- if (is.null(scenario$result)) "unavailable" else "stale"
      scenario$stale_reason <- "Manual forecast assumptions were imported."
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- "Manual forecast assumptions imported"
      osem_shiny_sync_active_forecast(state)
      update_scenario_controls()
      shiny::showNotification("Forecast assumptions imported.", type = "message", session = session)
    })

    shiny::observeEvent(input$run_forecast, {
      if (!identical(state$model_status, "current")) {
        shiny::showNotification(
          "Re-estimate the model before running a new forecast.",
          type = "error",
          duration = 10,
          session = session
        )
        return()
      }
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        shiny::showNotification(
          "No forecast scenario is configured.",
          type = "error",
          duration = 10,
          session = session
        )
        return()
      }
      values <- scenario_input_values()
      if (is.null(values) || !identical(values$scenario_id, scenario$id)) return()
      ci <- values$ci_validation
      if (!isTRUE(ci$valid)) {
        shiny::showNotification(ci$message, type = "error", duration = 10, session = session)
        shiny::updateTabsetPanel(session, "forecast_tabs", selected = "Setup and assumptions")
        return()
      }
      scenario$name <- if (nzchar(values$name)) values$name else "Scenario"
      scenario$description <- values$description
      scenario$args <- values$args
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      validation <- osem_shiny_validate_forecast_inputs(state$model, scenario)
      if (!isTRUE(validation$valid)) {
        shiny::showNotification(
          "The scenario contains blocking validation errors.",
          type = "error",
          duration = 10,
          session = session
        )
        shiny::updateTabsetPanel(session, "forecast_tabs", selected = "Setup and assumptions")
        return()
      }

      previous_result <- scenario$result
      scenario$status <- "running"
      scenario$error <- NULL
      scenario$log <- character()
      scenario$messages <- character()
      scenario$warnings <- character()
      scenario$updated_at <- Sys.time()
      osem_shiny_set_scenario(state, scenario)
      osem_shiny_sync_active_forecast(state)

      captured <- shiny::withProgress(
        message = paste0("Running scenario: ", scenario$name),
        value = 0,
        {
          shiny::setProgress(0.05, detail = "Preparing exogenous assumptions")
          result <- osem_shiny_execute_forecast(state$model, scenario)
          shiny::setProgress(1, detail = "Finalising forecast")
          result
        }
      )

      scenario$log <- captured$log
      scenario$messages <- captured$messages
      scenario$warnings <- captured$warnings
      scenario$metadata <- list(
        started = captured$started,
        finished = captured$finished,
        duration_seconds = captured$duration_seconds,
        seed = scenario$args$seed,
        model_status = state$model_status,
        model_settings_revision = state$settings_revision,
        model_data_revision = state$data_revision,
        model_specification_revision = state$specification_revision
      )
      scenario$updated_at <- Sys.time()

      if (!is.null(captured$error)) {
        scenario$result <- previous_result
        scenario$status <- "failed"
        scenario$error <- captured$error
        osem_shiny_set_scenario(state, scenario)
        osem_shiny_sync_active_forecast(state)
        state$forecast_revision <- state$forecast_revision + 1L
        state$last_change <- Sys.time()
        state$last_change_reason <- paste0("Forecast scenario failed: ", scenario$name)
        state$activity <- osem_shiny_activity_add(
          state$activity,
          area = "Forecast",
          action = "Scenario failed",
          detail = paste0(scenario$name, ": ", captured$error)
        )
        shiny::showNotification(
          paste0("Forecast failed: ", captured$error),
          type = "error",
          duration = 15,
          session = session
        )
        shiny::updateTabsetPanel(session, "forecast_tabs", selected = "Execution log")
        return()
      }

      scenario$result <- captured$value
      scenario$status <- "current"
      scenario$error <- NULL
      scenario$stale_reason <- NULL
      if (identical(scenario$args$assumption_mode, "manual")) {
        scenario$exog_predictions <- validation$exog_predictions
      }
      osem_shiny_set_scenario(state, scenario)
      osem_shiny_sync_active_forecast(state)
      state$forecast_revision <- state$forecast_revision + 1L
      state$last_change <- Sys.time()
      state$last_change_reason <- paste0("Forecast scenario completed: ", scenario$name)
      state$activity <- osem_shiny_activity_add(
        state$activity,
        area = "Forecast",
        action = "Scenario completed",
        detail = paste0(scenario$name, " in ", round(captured$duration_seconds, 1), " seconds.")
      )
      shiny::showNotification(
        paste0(
          "Scenario completed in ", round(captured$duration_seconds, 1), " seconds.",
          if (length(captured$warnings) > 0L) paste0(" ", length(captured$warnings), " warning(s) were captured.") else ""
        ),
        type = if (length(captured$warnings) > 0L) "warning" else "message",
        duration = 10,
        session = session
      )
      shiny::updateTabsetPanel(session, "forecast_tabs", selected = "Active result")
    })

    active_forecast <- shiny::reactive({
      scenario <- osem_shiny_get_active_scenario(state)
      shiny::req(!is.null(scenario), !is.null(scenario$result), inherits(scenario$result, "osem.forecast"))
      scenario$result
    })

    output$forecast_plot <- shiny::renderPlot({
      forecast <- active_forecast()

      mode <- input$forecast_selection_mode %||% "names"
      regex <- if (identical(mode, "grepl")) {
        pattern <- trimws(input$forecast_grepl_variables %||% "")
        if (nzchar(pattern)) pattern else NULL
      } else {
        variables <- input$forecast_variables %||% character()
        if (length(variables) == 0L) {
          NULL
        } else {
          paste0(
            "^(" ,
            paste(osem_shiny_regex_escape(variables), collapse = "|"),
            ")$"
          )
        }
      }

      first_date <- input$forecast_first_date
      if (is.null(first_date) || length(first_date) == 0L || is.na(first_date)) {
        first_date <- NULL
      } else {
        first_date <- as.Date(first_date)
      }

      tryCatch(
        plot(
          forecast,
          exclude.exogenous = !isTRUE(input$include_exogenous),
          grepl_variables = regex,
          first_date = first_date,
          title = paste0("OSEM forecast: ", osem_shiny_get_active_scenario(state)$name)
        ),
        error = function(e) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste0("Forecast plot unavailable:\n", conditionMessage(e)))
        }
      )
    }, res = 110)

    active_plot_data <- shiny::reactive({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario) || is.null(scenario$result)) return(data.frame())
      data <- osem_shiny_forecast_plot_data(scenario$result)
      variables <- input$forecast_variables %||% character()
      if (length(variables) > 0L && "na_item" %in% names(data)) {
        data <- data[as.character(data$na_item) %in% variables, , drop = FALSE]
      }
      if (!isTRUE(input$include_exogenous) && "type" %in% names(data)) {
        data <- data[!grepl("Exogenous", as.character(data$type), ignore.case = TRUE), , drop = FALSE]
      }
      data
    })

    output$forecast_table <- DT::renderDT({
      data <- active_plot_data()
      if (nrow(data) == 0L) data <- data.frame(Message = "Run the active scenario to create forecast data.")
      data <- dplyr::mutate(data, dplyr::across(c(where(is.numeric)), ~ ifelse(is.na(.x), NA_character_, format(.x, trim = TRUE, scientific = FALSE))))
      DT::datatable(data, rownames = FALSE, filter = "top", options = osem_shiny_dt_options(25L))
    })

    output$active_result_summary <- shiny::renderUI({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario) || is.null(scenario$result)) {
        return(osem_shiny_empty_state("No result", "Run this scenario to populate the forecast workspace."))
      }
      duration <- scenario$metadata$duration_seconds %||% NA_real_
      shiny::tagList(
        osem_shiny_badge(paste0("Status: ", scenario$status), osem_shiny_status_class(scenario$status)),
        shiny::tags$dl(
          shiny::tags$dt("Horizon"), shiny::tags$dd(scenario$args$n.ahead),
          shiny::tags$dt("Assumptions"), shiny::tags$dd(if (scenario$args$assumption_mode == "manual") "Manual path" else scenario$args$exog_fill_method),
          shiny::tags$dt("Uncertainty draws"), shiny::tags$dd(scenario$args$uncertainty_sample),
          shiny::tags$dt("Routine messages"), shiny::tags$dd(if (isTRUE(scenario$args$quiet)) "Suppressed" else "Captured"),
          shiny::tags$dt("Random seed"), shiny::tags$dd(scenario$args$seed),
          shiny::tags$dt("Run duration"), shiny::tags$dd(if (is.finite(duration)) paste0(round(duration, 1), " seconds") else "Not recorded")
        )
      )
    })

    comparison_data <- shiny::reactive({
      osem_shiny_scenario_comparison(state, baseline_id = input$comparison_baseline)
    })

    output$comparison_level_plot <- shiny::renderPlot({
      data <- osem_shiny_scenario_results_long(state, include_stale = TRUE)
      variables <- input$comparison_variables %||% character()
      if (length(variables) > 0L) data <- data[data$na_item %in% variables, , drop = FALSE]
      shiny::validate(shiny::need(nrow(data) > 0L, "At least one completed scenario is required."))
      ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$values, colour = .data$scenario)) +
        ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
        ggplot2::facet_wrap(~.data$na_item, scales = "free_y") +
        ggplot2::labs(x = NULL, y = NULL, colour = "Scenario") +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
    }, res = 110)

    output$comparison_difference_plot <- shiny::renderPlot({
      data <- comparison_data()
      variables <- input$comparison_variables %||% character()
      if (length(variables) > 0L) data <- data[data$na_item %in% variables, , drop = FALSE]
      shiny::validate(shiny::need(nrow(data) > 0L, "At least one completed scenario is required."))
      ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$difference, colour = .data$scenario)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2) +
        ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
        ggplot2::facet_wrap(~.data$na_item, scales = "free_y") +
        ggplot2::labs(x = NULL, y = "Difference from baseline", colour = "Scenario") +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
    }, res = 110)

    output$comparison_table <- DT::renderDT({
      data <- comparison_data()
      variables <- input$comparison_variables %||% character()
      if (length(variables) > 0L && nrow(data) > 0L) data <- data[data$na_item %in% variables, , drop = FALSE]
      if (nrow(data) == 0L) data <- data.frame(Message = "Complete at least one scenario to compare results.")
      data <- dplyr::mutate(data, dplyr::across(c(where(is.numeric)), ~ ifelse(is.na(.x), NA_character_, format(.x, trim = TRUE, scientific = FALSE))))
      DT::datatable(data, rownames = FALSE, filter = "top", options = osem_shiny_dt_options(25L))
    })

    output$forecast_run_status <- shiny::renderUI({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        return(osem_shiny_empty_state(
          "No scenario",
          "Create a scenario to view execution status."
        ))
      }
      duration <- scenario$metadata$duration_seconds %||% NA_real_
      shiny::fluidRow(
        shiny::column(4, osem_shiny_metric_card("Status", tools::toTitleCase(scenario$status), scenario$stale_reason %||% "", osem_shiny_status_class(scenario$status))),
        shiny::column(4, osem_shiny_metric_card("Warnings", length(scenario$warnings), if (length(scenario$warnings) > 0L) "Review before use" else "None captured", if (length(scenario$warnings) > 0L) "warning" else "success")),
        shiny::column(4, osem_shiny_metric_card("Duration", if (is.finite(duration)) paste0(round(duration, 1), " seconds") else "Not available", scenario$error %||% "", if (is.null(scenario$error)) "muted" else "danger"))
      )
    })

    output$forecast_log <- shiny::renderText({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario) || length(scenario$log) == 0L) {
        "No console output has been captured for this scenario."
      } else {
        paste(scenario$log, collapse = "\n")
      }
    })

    output$forecast_messages <- shiny::renderUI({
      scenario <- osem_shiny_get_active_scenario(state)
      if (is.null(scenario)) {
        return(osem_shiny_empty_state("No scenario", "Create a scenario to view messages."))
      }
      items <- list()
      if (!is.null(scenario$error)) {
        items[[length(items) + 1L]] <- shiny::div(class = "osem-validation-item osem-validation-error", shiny::tags$strong("Error"), shiny::tags$div(scenario$error))
      }
      for (message in scenario$warnings) {
        items[[length(items) + 1L]] <- shiny::div(class = "osem-validation-item osem-validation-warning", shiny::tags$strong("Warning"), shiny::tags$div(message))
      }
      for (message in scenario$messages) {
        items[[length(items) + 1L]] <- shiny::div(class = "osem-validation-item osem-validation-info", shiny::tags$strong("Message"), shiny::tags$div(message))
      }
      if (length(items) == 0L) return(osem_shiny_empty_state("No messages", "Forecast messages and warnings will appear here."))
      do.call(shiny::tagList, items)
    })

    output$download_assumptions <- shiny::downloadHandler(
      filename = function() {
        scenario <- osem_shiny_get_active_scenario(state)
        paste0(osem_shiny_project_slug(scenario$name %||% "forecast-scenario"), "-assumptions.csv")
      },
      content = function(file) {
        scenario <- osem_shiny_get_active_scenario(state)
        shiny::req(is.data.frame(scenario$exog_predictions))
        utils::write.csv(scenario$exog_predictions, file, row.names = FALSE, na = "")
      }
    )

    output$download_forecast <- shiny::downloadHandler(
      filename = function() {
        scenario <- osem_shiny_get_active_scenario(state)
        paste0(osem_shiny_project_slug(scenario$name %||% "forecast-scenario"), "-forecast.rds")
      },
      content = function(file) {
        forecast <- active_forecast()
        saveRDS(forecast, file, version = 3)
      }
    )

    output$download_forecast_table <- shiny::downloadHandler(
      filename = function() {
        scenario <- osem_shiny_get_active_scenario(state)
        paste0(osem_shiny_project_slug(scenario$name %||% "forecast-scenario"), "-forecast.csv")
      },
      content = function(file) {
        utils::write.csv(active_plot_data(), file, row.names = FALSE, na = "")
      }
    )

    output$download_comparison <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-scenario-comparison.csv"),
      content = function(file) {
        utils::write.csv(comparison_data(), file, row.names = FALSE, na = "")
      }
    )
  })
}
