# OSEM Shiny module: model results ------------------------------------------

osem_shiny_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Model results",
      "Review the fitted system at model and module level, inspect diagnostics, and export auditable result tables.",
      shiny::div(
        class = "osem-action-row",
        shiny::downloadButton(ns("download_model"), "Model RDS"),
        shiny::downloadButton(ns("download_overview"), "Module overview")
      )
    ),
    shiny::uiOutput(ns("status_banner")),
    shiny::uiOutput(ns("metrics")),
    shiny::uiOutput(ns("no_model")),
    shiny::conditionalPanel(
      condition = "output.has_model == 'true'",
      ns = ns,
      shiny::tabsetPanel(
        id = ns("result_tabs"),
        shiny::tabPanel(
          "Overview",
          shiny::fluidRow(
            shiny::column(
              4,
              osem_shiny_panel(
                "Chart controls",
                shiny::radioButtons(
                  ns("overview_selection_mode"),
                  "Variable selection",
                  choices = c(
                    "Select variables" = "names",
                    "Use a pattern (grepl)" = "grepl"
                  ),
                  selected = "names",
                  inline = TRUE
                ),
                shiny::conditionalPanel(
                  condition = "input.overview_selection_mode == 'names'",
                  ns = ns,
                  shiny::selectizeInput(
                    ns("overview_variables"),
                    "Variables",
                    choices = character(),
                    selected = character(),
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"))
                  )
                ),
                shiny::conditionalPanel(
                  condition = "input.overview_selection_mode == 'grepl'",
                  ns = ns,
                  shiny::textInput(
                    ns("overview_grepl_variables"),
                    "Variable selection pattern",
                    value = "",
                    placeholder = "For example: Emi|Energy|GDP"
                  )
                ),
                shiny::dateInput(
                  ns("overview_first_date"),
                  "First date shown",
                  value = "2000-01-01",
                  format = "yyyy-mm-dd",
                ),
                shiny::checkboxInput(
                  ns("overview_include_exogenous"),
                  "Include exogenous variables",
                  value = FALSE
                )
              )
            ),
            shiny::column(
              8,
              osem_shiny_panel("Observed and fitted values", shiny::plotOutput(ns("overview_plot"), height = 520))
            )
          ),
          osem_shiny_panel("Ordered modules", DT::DTOutput(ns("module_overview")))
        ),
        shiny::tabPanel(
          "Module explorer",
          shiny::fluidRow(
            shiny::column(
              4,
              osem_shiny_panel(
                "Select module",
                shiny::selectInput(ns("selected_module"), "Module", choices = NULL),
                shiny::uiOutput(ns("module_summary"))
              ),
              osem_shiny_panel("Specification", shiny::uiOutput(ns("module_equation"))),
              osem_shiny_panel("Model-form decision", DT::DTOutput(ns("module_form")))
            ),
            shiny::column(
              8,
              osem_shiny_panel("Coefficient estimates", DT::DTOutput(ns("coefficients")))
            )
          ),
          shiny::fluidRow(
            shiny::column(6, osem_shiny_panel("Observed and fitted", shiny::plotOutput(ns("module_fit_plot"), height = 330))),
            shiny::column(6, osem_shiny_panel("Residuals", shiny::plotOutput(ns("module_residual_plot"), height = 330)))
          ),
          shiny::fluidRow(
            shiny::column(6, osem_shiny_panel("Retained and dropped terms", DT::DTOutput(ns("module_terms")))),
            shiny::column(6, osem_shiny_panel("Module diagnostics", DT::DTOutput(ns("module_diagnostics"))))
          ),
          osem_shiny_panel("Module arguments", DT::DTOutput(ns("module_arguments"))),
          shiny::div(
            class = "osem-action-row",
            shiny::downloadButton(ns("download_coefficients"), "Download selected coefficients"),
            shiny::downloadButton(ns("download_module_data"), "Download selected module data")
          )
        ),
        shiny::tabPanel(
          "Diagnostics",
          shiny::fluidRow(
            shiny::column(8, osem_shiny_panel("System diagnostics", DT::DTOutput(ns("diagnostics")))),
            shiny::column(4, osem_shiny_panel("Interpretation", shiny::uiOutput(ns("diagnostic_interpretation"))))
          ),
          shiny::downloadButton(ns("download_diagnostics"), "Download diagnostics")
        ),
        shiny::tabPanel(
          "Dependency network",
          shiny::fluidRow(
            shiny::column(
              3,
              osem_shiny_panel(
                "Network controls",
                shiny::selectInput(
                  ns("network_layout"),
                  "Layout",
                  choices = c(
                    "Kamada-Kawai" = "kk",
                    "Automatic" = "auto",
                    "Fruchterman-Reingold" = "fr",
                    "Davidson-Harel" = "dh",
                    "Circle" = "circle"
                  ),
                  selected = "kk"
                ),
                shiny::tags$p(
                  class = "help-block",
                  "With ggraph and tidygraph installed, solid and dashed edges reflect retained and removed relationships. Otherwise the app displays the specification graph."
                )
              )
            ),
            shiny::column(9, osem_shiny_panel("Model dependencies", shiny::plotOutput(ns("network_plot"), height = 720)))
          )
        ),
        shiny::tabPanel(
          "Data and audit",
          shiny::fluidRow(
            shiny::column(
              4,
              osem_shiny_panel(
                "Data view",
                shiny::radioButtons(
                  ns("data_view"),
                  "Dataset",
                  choices = c("Processed estimation input" = "processed", "Full data including fitted values" = "full"),
                  selected = "processed"
                ),
                shiny::numericInput(ns("data_preview_rows"), "Preview rows", 100, min = 10, max = 5000, step = 10),
                shiny::div(
                  class = "osem-action-row",
                  shiny::downloadButton(ns("download_processed"), "Processed CSV"),
                  shiny::downloadButton(ns("download_full"), "Full data CSV")
                )
              )
            ),
            shiny::column(8, osem_shiny_panel("Data preview", DT::DTOutput(ns("data_preview"))))
          ),
          osem_shiny_panel("Transformation and module options", DT::DTOutput(ns("opts_table"))),
          osem_shiny_panel("Latest estimation log", shiny::tags$pre(class = "osem-log-output", shiny::textOutput(ns("estimation_log"))))
        )
      )
    )
  )
}

osem_shiny_results_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    has_model <- shiny::reactive(inherits(state$model, "osem"))
    output$has_model <- shiny::renderText({
      if (isTRUE(has_model())) "true" else "false"
    })
    shiny::outputOptions(output, "has_model", suspendWhenHidden = FALSE)

    output$no_model <- shiny::renderUI({
      if (has_model()) return(NULL)
      osem_shiny_empty_state(
        "No fitted model is available",
        "Go to Estimation, resolve any validation issues, and run the model. You can also open a project or model RDS from the Project page."
      )
    })

    output$status_banner <- shiny::renderUI({
      if (!has_model()) return(NULL)
      if (identical(state$model_status, "current")) {
        return(shiny::div(
          class = "osem-callout osem-callout-success",
          "These results correspond to the current specification, dictionary, data, and estimation settings."
        ))
      }
      shiny::div(
        class = paste0("osem-callout osem-callout-", if (identical(state$model_status, "failed")) "danger" else "warning"),
        shiny::tags$strong(paste0("Model status: ", tools::toTitleCase(state$model_status), ". ")),
        if (identical(state$model_status, "failed")) {
          paste0("The latest estimation attempt failed. The stored object is the last successful model. ", state$model_error %||% "")
        } else {
          paste0("Inputs changed after this model was fitted. ", state$last_change_reason)
        }
      )
    })

    output$metrics <- shiny::renderUI({
      if (!has_model()) return(NULL)
      summary <- osem_shiny_model_summary(state$model)
      sample <- if (!is.na(summary$start) && !is.na(summary$end)) {
        paste0(format(summary$start), " to ", format(summary$end))
      } else "Unknown"
      shiny::fluidRow(
        shiny::column(3, osem_shiny_metric_card("Modules", summary$modules, paste0(summary$estimated, " estimated; ", summary$identities, " identities"), "info")),
        shiny::column(3, osem_shiny_metric_card("CVAR systems", summary$cvar_systems, summary$frequency, "info")),
        shiny::column(3, osem_shiny_metric_card("Sample", sample, paste0(summary$variables, " variables"), "info")),
        shiny::column(3, osem_shiny_metric_card("Status", tools::toTitleCase(state$model_status), NULL, osem_shiny_status_class(state$model_status)))
      )
    })

    shiny::observe({
      if (!has_model()) return()

      choices <- osem_shiny_module_choices(state$model)
      if (length(choices) > 0L) {
        current <- input$selected_module
        if (is.null(current) || !current %in% unname(choices)) {
          current <- unname(choices)[[1L]]
        }
        shiny::updateSelectInput(
          session,
          "selected_module",
          choices = choices,
          selected = current
        )
      } else {
        shiny::updateSelectInput(
          session,
          "selected_module",
          choices = character(),
          selected = character()
        )
      }
    })

    shiny::observeEvent(input$overview_include_exogenous, {
      shiny::req(has_model())

      module_order <- osem_shiny_safe_data_frame(state$model$module_order)
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

      exogenous <- as.character(unlist(state$model$module_collection$indep))
      exogenous <- exogenous[!is.na(exogenous) & nzchar(exogenous)]
      exogenous <- setdiff(exogenous, endogenous)

      choices <- if (isTRUE(input$overview_include_exogenous)) {
        unique(c(endogenous, exogenous))
      } else {
        endogenous
      }

      current <- shiny::isolate(
        input$overview_variables %||% character()
      )
      selected <- if (isTRUE(input$overview_include_exogenous)) {
        unique(c(intersect(current, choices), exogenous))
      } else {
        intersect(current, endogenous)
      }
      if (length(selected) == 0L) selected <- endogenous

      shiny::updateSelectizeInput(
        session,
        "overview_variables",
        choices = choices,
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    output$overview_plot <- shiny::renderPlot({
      shiny::req(has_model())

      mode <- input$overview_selection_mode %||% "names"
      pattern <- if (identical(mode, "grepl")) {
        trimws(input$overview_grepl_variables %||% "")
      } else {
        variables <- input$overview_variables %||% character()
        if (length(variables) == 0L) {
          ""
        } else {
          paste0(
            "^(" ,
            paste(osem_shiny_regex_escape(variables), collapse = "|"),
            ")$"
          )
        }
      }

      first_date <- input$overview_first_date
      if (is.null(first_date) || length(first_date) == 0L || is.na(first_date)) {
        first_date <- NULL
      } else {
        first_date <- as.Date(first_date)
      }
      plot(
        state$model,
        grepl_variables = if (nzchar(pattern)) pattern else NULL,
        exclude.exogenous = !isTRUE(input$overview_include_exogenous),
        first_date = first_date
      )
    })

    output$module_overview <- DT::renderDT({
      shiny::req(has_model())
      overview <- osem_shiny_module_overview(state$model)
      widget <- DT::datatable(
        overview,
        rownames = FALSE,
        filter = "top",
        selection = "single",
        options = osem_shiny_dt_options(20L)
      )
      numeric_columns <- intersect(c("AR p-value", "ARCH p-value"), names(overview))
      if (length(numeric_columns) > 0L) {
        widget <- DT::formatRound(widget, numeric_columns, digits = 4)
      }
      widget
    })

    shiny::observeEvent(input$module_overview_rows_selected, {
      selected <- input$module_overview_rows_selected
      overview <- osem_shiny_module_overview(state$model)
      if (length(selected) == 1L && selected >= 1L && selected <= nrow(overview)) {
        model_row <- if ("Row" %in% names(overview)) overview$Row[[selected]] else selected
        shiny::updateSelectInput(session, "selected_module", selected = as.character(model_row))
        shiny::updateTabsetPanel(session, "result_tabs", selected = "Module explorer")
      }
    })

    module_overview_data <- shiny::reactive({
      if (!has_model()) return(data.frame())
      osem_shiny_module_overview(state$model)
    })

    selected_row <- shiny::reactive({
      shiny::req(has_model())
      overview <- module_overview_data()
      shiny::validate(shiny::need(nrow(overview) > 0L, "No usable module collection is stored in this model."))
      available <- if ("Row" %in% names(overview)) {
        suppressWarnings(as.integer(overview$Row))
      } else {
        seq_len(nrow(overview))
      }
      available <- available[!is.na(available) & available >= 1L]
      shiny::validate(shiny::need(length(available) > 0L, "No usable module index is stored in this model."))
      index <- suppressWarnings(as.integer(input$selected_module %||% available[[1L]]))
      if (length(index) != 1L || is.na(index) || !index %in% available) index <- available[[1L]]
      index
    })

    selected_module_name <- shiny::reactive({
      overview <- module_overview_data()
      index <- selected_row()
      if (nrow(overview) == 0L || !"Module" %in% names(overview)) return("module")
      row_ids <- if ("Row" %in% names(overview)) suppressWarnings(as.integer(overview$Row)) else seq_len(nrow(overview))
      match_index <- match(index, row_ids)
      if (is.na(match_index)) return("module")
      name <- trimws(osem_shiny_scalar_character(overview$Module[[match_index]], "module"))
      if (nzchar(name)) name else "module"
    })

    output$module_summary <- shiny::renderUI({
      i <- selected_row()
      collection <- osem_shiny_safe_data_frame(state$model$module_collection)
      shiny::validate(shiny::need(i <= nrow(collection), "The selected module is not available."))
      row <- collection[i, , drop = FALSE]
      object <- osem_shiny_module_object(state$model, i)
      args <- osem_shiny_module_args(state$model, i)
      data <- osem_shiny_module_dataset(state$model, i)
      selected_form <- osem_shiny_module_selected_form(args, object)
      if (length(selected_form) == 0L || is.na(selected_form) || !nzchar(selected_form)) {
        selected_form <- "Not applicable"
      }
      order_value <- if ("order" %in% names(row)) {
        osem_shiny_scalar_integer(row$order, i, 1L, .Machine$integer.max)
      } else {
        i
      }
      shiny::div(
        class = "osem-key-value-list",
        shiny::div(shiny::strong("Kind"), shiny::span(osem_shiny_module_kind(row, object))),
        shiny::div(shiny::strong("Selected form"), shiny::span(selected_form)),
        shiny::div(shiny::strong("Observations"), shiny::span(osem_shiny_module_observations(object, data))),
        shiny::div(shiny::strong("Order"), shiny::span(as.character(order_value)))
      )
    })

    output$module_equation <- shiny::renderUI({
      shiny::div(class = "osem-equation-code", osem_shiny_module_equation(state$model, selected_row()))
    })

    output$coefficients <- DT::renderDT({
      coefficients <- osem_shiny_module_coefficients(state$model, selected_row())
      if (nrow(coefficients) == 0L) {
        return(DT::datatable(data.frame(Note = "This module has no coefficient table (for example, an accounting identity)."), rownames = FALSE, options = list(dom = "t")))
      }
      widget <- DT::datatable(
        coefficients,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
      numeric_columns <- intersect(
        c("estimate", "std.error", "statistic", "p.value"),
        names(coefficients)
      )
      if (length(numeric_columns) > 0L) {
        widget <- DT::formatRound(widget, numeric_columns, digits = 5)
      }
      widget
    })

    output$module_form <- DT::renderDT({
      i <- selected_row()
      object <- osem_shiny_module_object(state$model, i)
      table <- if (inherits(object, "osem.cvar")) {
        osem_shiny_cvar_summary(object)
      } else {
        osem_shiny_module_ecm_decision(state$model, i)
      }
      if (nrow(table) == 0L) table <- data.frame(Note = "No separate model-form decision is recorded for this module.")
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$module_fit_plot <- shiny::renderPlot({
      shiny::req(has_model())

      variable <- selected_module_name()
      shiny::validate(
        shiny::need(
          nzchar(variable) && !identical(variable, "module"),
          "No variable is associated with this module."
        )
      )

      plot(
        state$model,
        grepl_variables = paste0("^", variable, "$")
      )
    })

    output$module_residual_plot <- shiny::renderPlot({
      data <- osem_shiny_module_residual_data(state$model, selected_row())
      shiny::validate(shiny::need(nrow(data) > 0L, "Residuals are not available for this module."))
      ggplot2::ggplot(data, ggplot2::aes(x = .data$time, y = .data$residual)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
        ggplot2::facet_wrap(~equation, scales = "free_y") +
        ggplot2::labs(x = NULL, y = "Residual") +
        ggplot2::theme_minimal() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    output$module_terms <- DT::renderDT({
      table <- osem_shiny_module_terms(state$model, selected_row())
      if (nrow(table) == 0L) table <- data.frame(Note = "Candidate/retained term information is not available for this module.")
      DT::datatable(table, rownames = FALSE, filter = "top", options = osem_shiny_dt_options(15L))
    })

    output$module_diagnostics <- DT::renderDT({
      diagnostics <- osem_shiny_model_diagnostics(state$model)
      dep <- selected_module_name()
      if (nrow(diagnostics) > 0L && "module" %in% names(diagnostics)) {
        module_values <- as.character(diagnostics$module)
        keep <- (!is.na(module_values) & module_values == dep) |
          (is.na(module_values) & "diagnostic_error" %in% names(diagnostics))
        diagnostics <- diagnostics[keep, , drop = FALSE]
        # round
        diagnostics <- dplyr::mutate(diagnostics, dplyr::across(c(where(is.numeric),-where(is.integer)), ~ round(.x, 3)))

      }
      if (nrow(diagnostics) == 0L) diagnostics <- data.frame(Note = "Diagnostics are not available for this module.")
      DT::datatable(diagnostics, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })

    output$module_arguments <- DT::renderDT({
      table <- osem_shiny_module_argument_table(state$model, selected_row())
      if (nrow(table) == 0L) table <- data.frame(Note = "No estimation arguments are stored for this module.")
      DT::datatable(table, rownames = FALSE, options = osem_shiny_dt_options(15L))
    })

    output$diagnostics <- DT::renderDT({
      table <- osem_shiny_model_diagnostics(state$model)
      if (nrow(table) == 0L) table <- data.frame(Note = "No system diagnostics could be extracted.")
      widget <- DT::datatable(
        table,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(25L)
      )
      numeric_columns <- intersect(
        c("AR", "ARCH", "Super Exogeneity", "Share of Indicators"),
        names(table)
      )
      if (length(numeric_columns) > 0L) {
        widget <- DT::formatRound(widget, numeric_columns, digits = 4)
      }
      widget
    })

    output$diagnostic_interpretation <- shiny::renderUI({
      table <- osem_shiny_model_diagnostics(state$model)
      if ("diagnostic_error" %in% names(table)) {
        messages <- unique(trimws(as.character(table$diagnostic_error)))
        messages <- messages[!is.na(messages) & nzchar(messages)]
        return(osem_shiny_empty_state(
          "Diagnostics could not be calculated",
          if (length(messages) > 0L) paste(messages, collapse = " ") else "The diagnostics function returned an error."
        ))
      }
      if (nrow(table) == 0L || !all(c("AR", "ARCH") %in% names(table))) {
        return(osem_shiny_empty_state("No diagnostics", "The package did not return a diagnostics table for this model."))
      }
      ar_rejections <- sum(suppressWarnings(as.numeric(table$AR)) < 0.05, na.rm = TRUE)
      arch_rejections <- sum(suppressWarnings(as.numeric(table$ARCH)) < 0.05, na.rm = TRUE)
      shiny::tagList(
        shiny::div(
          class = paste0("osem-callout osem-callout-", if (ar_rejections + arch_rejections > 0L) "warning" else "success"),
          paste0(
            ar_rejections, " module(s) reject the AR null and ",
            arch_rejections, " module(s) reject the ARCH null at 5 percent."
          )
        ),
        shiny::tags$p(
          "These are diagnostic tests rather than automatic model-quality scores. A low p-value warrants substantive review of the equation, sample, and retained dynamics."
        ),
        shiny::tags$p(
          "For CVAR systems, the table also reports rank and trace-test information when available."
        )
      )
    })

    output$network_plot <- shiny::renderPlot({
      shiny::req(has_model())
      plot <- osem_shiny_model_network_plot(state$model, input$network_layout %||% "kk")
      shiny::validate(shiny::need(!is.null(plot), "The dependency graph could not be constructed."))
      plot
    })

    output$data_preview <- DT::renderDT({
      data <- osem_shiny_model_data_export(state$model, input$data_view %||% "processed")
      requested_rows <- suppressWarnings(as.integer(input$data_preview_rows %||% 100L))
      if (is.na(requested_rows) || requested_rows < 1L) requested_rows <- 100L
      rows <- min(nrow(data), requested_rows)
      data <- if (rows > 0L) utils::head(data, rows) else data
      data <- dplyr::mutate(data, dplyr::across(c(where(is.numeric),-where(is.integer)), ~ round(.x, 5)))
      DT::datatable(data, rownames = FALSE, filter = "top", options = osem_shiny_dt_options(20L))
    })

    output$opts_table <- DT::renderDT({
      opts <- osem_shiny_safe_data_frame(state$model$opts_df)
      if (ncol(opts) == 0L) {
        return(DT::datatable(
          data.frame(Note = "No transformation/module options table is stored in this model."),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }
      for (name in names(opts)) {
        if (is.list(opts[[name]])) {
          opts[[name]] <- vapply(opts[[name]], function(value) {
            tryCatch({
              if (is.null(value)) {
                "NULL"
              } else if (is.data.frame(value)) {
                paste0(nrow(value), " x ", ncol(value), " table")
              } else if (is.list(value)) {
                paste0("list (", length(value), ")")
              } else if (methods::isS4(value)) {
                paste0("S4 object: ", paste(class(value), collapse = ", "))
              } else {
                paste(as.character(value), collapse = ", ")
              }
            }, error = function(e) paste0("object: ", paste(class(value), collapse = ", ")))
          }, character(1L))
        }
      }
      DT::datatable(opts, rownames = FALSE, filter = "top", options = osem_shiny_dt_options(20L))
    })

    output$estimation_log <- shiny::renderText({
      parts <- c(state$model_log, state$model_messages, state$model_warnings, state$model_error %||% character())
      if (length(parts) == 0L) "No execution log is stored for this model." else paste(parts, collapse = "\n")
    })

    output$download_model <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-model.rds"),
      content = function(file) {
        if (!has_model()) stop("No model is available.", call. = FALSE)
        saveRDS(state$model, file, version = 3)
      }
    )
    output$download_overview <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-module-overview.csv"),
      content = function(file) osem_shiny_write_csv(osem_shiny_module_overview(state$model), file)
    )
    output$download_coefficients <- shiny::downloadHandler(
      filename = function() {
        dep <- if (has_model()) selected_module_name() else "module"
        paste0(osem_shiny_project_slug(dep), "-coefficients.csv")
      },
      content = function(file) osem_shiny_write_csv(osem_shiny_module_coefficients(state$model, selected_row()), file)
    )
    output$download_module_data <- shiny::downloadHandler(
      filename = function() {
        dep <- if (has_model()) selected_module_name() else "module"
        paste0(osem_shiny_project_slug(dep), "-module-data.csv")
      },
      content = function(file) osem_shiny_write_csv(osem_shiny_module_dataset(state$model, selected_row()), file)
    )
    output$download_diagnostics <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-diagnostics.csv"),
      content = function(file) osem_shiny_write_csv(osem_shiny_model_diagnostics(state$model), file)
    )
    output$download_processed <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-processed-input.csv"),
      content = function(file) osem_shiny_write_csv(osem_shiny_model_data_export(state$model, "processed"), file)
    )
    output$download_full <- shiny::downloadHandler(
      filename = function() paste0(osem_shiny_project_slug(state$project_name), "-full-data.csv"),
      content = function(file) osem_shiny_write_csv(osem_shiny_model_data_export(state$model, "full"), file)
    )
  })
}
