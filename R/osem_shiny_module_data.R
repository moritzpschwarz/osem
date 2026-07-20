# OSEM Shiny module: data ---------------------------------------------------

osem_shiny_data_ui <- function(id) {
  ns <- shiny::NS(id)
  export_formats <- c("CSV" = "csv", "RDS" = "rds")
  if (requireNamespace("writexl", quietly = TRUE)) {
    export_formats <- c(export_formats, "Excel" = "xlsx")
  }

  shiny::div(
    class = "osem-page",
    osem_shiny_page_header(
      "Data and dictionary",
      "Add one or more local files, control source priority, inspect coverage, and verify provenance before estimation."
    ),
    osem_shiny_panel(
      "Data-source strategy",
      shiny::radioButtons(
        ns("primary_source"),
        "Which source should take precedence?",
        choices = c(
          "Use local data first, then download missing variables" = "local",
          "Download first according to the dictionary" = "download"
        ),
        selected = "local"
      ),
      shiny::tags$p(
        class = "help-block",
        "Within local inputs, the first source listed below supplies a variable. Later copies of that variable are retained for provenance but are not used."
      )
    ),
    shiny::tabsetPanel(
      id = ns("data_tabs"),
      shiny::tabPanel(
        "Sources",
        shiny::fluidRow(
          shiny::column(
            7,
            osem_shiny_panel(
              "Add local input",
              shiny::fileInput(
                ns("input_files"),
                "Select CSV, RDS, XLS, or XLSX files",
                multiple = TRUE,
                accept = c(".csv", ".rds", ".RDS", ".xls", ".xlsx")
              ),
              shiny::actionButton(ns("add_files"), "Add selected files", class = "btn-primary"),
              shiny::actionButton(ns("load_example"), "Add bundled sample", class = "btn-default")
            )
          ),
          shiny::column(
            5,
            osem_shiny_panel(
              "Manage source order",
              shiny::actionButton(ns("move_up"), "Move up"),
              shiny::actionButton(ns("move_down"), "Move down"),
              shiny::actionButton(ns("remove_sources"), "Remove selected"),
              shiny::actionButton(ns("clear_sources"), "Clear all", class = "btn-default")
            )
          )
        ),
        osem_shiny_panel(
          "Local sources",
          DT::DTOutput(ns("source_table"))
        ),
        osem_shiny_panel(
          "Data validation",
          shiny::uiOutput(ns("data_validation"))
        )
      ),
      shiny::tabPanel(
        "Variable coverage",
        osem_shiny_panel(
          "Required and available variables",
          DT::DTOutput(ns("variable_profile"))
        )
      ),
      shiny::tabPanel(
        "Preview and export",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectizeInput(
              ns("preview_variable"),
              "Variable",
              choices = c("All variables" = "__all__")
            )
          ),
          shiny::column(
            3,
            shiny::numericInput(ns("preview_rows"), "Maximum preview rows", 500, min = 25, max = 5000, step = 25)
          ),
          shiny::column(
            3,
            shiny::selectInput(ns("prepared_format"), "Download format", choices = export_formats)
          ),
          shiny::column(
            2,
            shiny::downloadButton(ns("download_prepared_data"), "Download", class = "btn-primary osem-download-align")
          )
        ),
        osem_shiny_panel(
          "Prepared local input",
          DT::DTOutput(ns("data_preview"))
        )
      ),
      shiny::tabPanel(
        "Provenance",
        osem_shiny_panel(
          "Variable-level provenance",
          DT::DTOutput(ns("provenance"))
        )
      ),
      shiny::tabPanel(
        "Dictionary",
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::fileInput(
              ns("dictionary_file"),
              "Replace dictionary from CSV, RDS, XLS, or XLSX",
              accept = c(".csv", ".rds", ".RDS", ".xls", ".xlsx")
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "osem-action-row osem-action-row-top",
              shiny::actionButton(ns("dictionary_add_row"), "Add row"),
              shiny::actionButton(ns("dictionary_delete_rows"), "Delete selected"),
              shiny::actionButton(ns("dictionary_reset"), "Reset default")
            )
          )
        ),
        osem_shiny_panel(
          "Dictionary table",
          DT::DTOutput(ns("dictionary_table"))
        ),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(ns("dictionary_format"), "Dictionary export format", choices = export_formats)
          ),
          shiny::column(
            3,
            shiny::downloadButton(ns("download_dictionary"), "Download dictionary", class = "btn-primary osem-download-align")
          )
        ),
        osem_shiny_panel(
          "Dictionary validation",
          shiny::uiOutput(ns("dictionary_validation"))
        )
      )
    )
  )
}

osem_shiny_data_server <- function(id, state, derived) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      state$project_revision
      shiny::updateRadioButtons(session, "primary_source", selected = state$primary_source)
    })

    shiny::observeEvent(input$primary_source, {
      if (!is.null(input$primary_source) && !identical(input$primary_source, state$primary_source)) {
        state$primary_source <- input$primary_source
        osem_shiny_state_mark_changed(
          state,
          "data",
          paste0("Changed data-source priority to '", input$primary_source, "'.")
        )
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_files, {
      shiny::req(input$input_files)
      uploads <- input$input_files
      new_sources <- vector("list", nrow(uploads))

      shiny::withProgress(message = "Reading local input", value = 0, {
        for (i in seq_len(nrow(uploads))) {
          shiny::incProgress(1 / nrow(uploads), detail = uploads$name[[i]])
          source_id <- osem_shiny_state_next_source_id(state)
          new_sources[[i]] <- osem_shiny_source_from_upload(
            upload_row = uploads[i, , drop = FALSE],
            source_id = source_id,
            session_dir = state$session_dir
          )
        }
      })

      state$input_sources <- c(state$input_sources, new_sources)
      osem_shiny_state_mark_changed(
        state,
        "data",
        paste0("Added ", length(new_sources), " local input source(s).")
      )

      invalid <- sum(!vapply(new_sources, function(x) isTRUE(x$valid), logical(1L)))
      shiny::showNotification(
        if (invalid == 0L) {
          paste0("Added ", length(new_sources), " source(s).")
        } else {
          paste0("Added ", length(new_sources), " source(s); ", invalid, " need attention.")
        },
        type = if (invalid == 0L) "message" else "warning",
        duration = 8,
        session = session
      )
    })

    shiny::observeEvent(input$load_example, {
      already_loaded <- any(vapply(
        state$input_sources,
        function(x) identical(x$kind, "bundled_example"),
        logical(1L)
      ))
      if (already_loaded) {
        shiny::showNotification("Bundled sample is already loaded.", type = "message", session = session)
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
      osem_shiny_state_mark_changed(state, "data", "Loaded the bundled OSEM sample input.")
    })

    output$source_table <- DT::renderDT({
      table <- derived$source_table()
      DT::datatable(
        table,
        rownames = FALSE,
        selection = list(mode = "multiple", target = "row"),
        filter = "top",
        options = osem_shiny_dt_options(10L)
      )
    })

    shiny::observeEvent(input$remove_sources, {
      selected <- sort(unique(input$source_table_rows_selected %||% integer()))
      if (length(selected) == 0L) {
        shiny::showNotification("Select one or more sources to remove.", type = "warning", session = session)
        return()
      }
      keep <- setdiff(seq_along(state$input_sources), selected)
      removed_names <- vapply(state$input_sources[selected], `[[`, character(1L), "display_name")
      state$input_sources <- state$input_sources[keep]
      osem_shiny_state_mark_changed(
        state,
        "data",
        paste0("Removed local source(s): ", paste(removed_names, collapse = ", "), ".")
      )
    })

    move_source <- function(direction) {
      selected <- input$source_table_rows_selected %||% integer()
      if (length(selected) != 1L) {
        shiny::showNotification("Select exactly one source to move.", type = "warning", session = session)
        return()
      }
      current <- selected[[1L]]
      target <- current + direction
      if (target < 1L || target > length(state$input_sources)) return()
      order <- seq_along(state$input_sources)
      order[c(current, target)] <- order[c(target, current)]
      state$input_sources <- state$input_sources[order]
      osem_shiny_state_mark_changed(state, "data", "Changed local source precedence.")
    }

    shiny::observeEvent(input$move_up, move_source(-1L))
    shiny::observeEvent(input$move_down, move_source(1L))

    shiny::observeEvent(input$clear_sources, {
      if (length(state$input_sources) == 0L) return()
      shiny::showModal(shiny::modalDialog(
        title = "Clear all local sources?",
        "This removes all uploaded and in-memory data sources from the current workspace.",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("confirm_clear_sources"), "Clear sources", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_clear_sources, {
      shiny::removeModal()
      state$input_sources <- list()
      osem_shiny_state_mark_changed(state, "data", "Cleared all local input sources.")
    })

    output$data_validation <- shiny::renderUI({
      osem_shiny_issue_list(
        derived$workspace()$issues,
        area = c("Data", "Environment"),
        max_items = 25L
      )
    })

    output$variable_profile <- DT::renderDT({
      profile <- derived$variable_profile()
      if (nrow(profile) == 0L) {
        return(DT::datatable(data.frame(Message = "No variable profile is available yet."), rownames = FALSE))
      }
      logical_columns <- names(profile)[vapply(profile, is.logical, logical(1L))]
      for (name in logical_columns) {
        profile[[name]] <- ifelse(profile[[name]], "Yes", "No")
      }
      names(profile) <- gsub("_", " ", tools::toTitleCase(names(profile)), fixed = TRUE)
      DT::datatable(
        profile,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
    })

    shiny::observe({
      data <- derived$effective_data()$data
      variables <- if (is.data.frame(data) && "na_item" %in% names(data)) {
        variables <- unique(as.character(data$na_item))
        sort(variables[!is.na(variables) & nzchar(variables)])
      } else {
        character()
      }
      choices <- c("All variables" = "__all__", stats::setNames(variables, variables))
      current <- shiny::isolate(input$preview_variable)
      selected <- if (!is.null(current) && current %in% unname(choices)) current else "__all__"
      shiny::updateSelectizeInput(session, "preview_variable", choices = choices, selected = selected, server = TRUE)
    })

    output$data_preview <- DT::renderDT({
      data <- derived$effective_data()$data
      if (!is.data.frame(data) || nrow(data) == 0L) {
        return(DT::datatable(data.frame(Message = "No valid local data are available."), rownames = FALSE))
      }
      if (!identical(input$preview_variable, "__all__") && !is.null(input$preview_variable)) {
        keep <- !is.na(data$na_item) & data$na_item == input$preview_variable
        data <- data[keep, , drop = FALSE]
      }
      data <- osem_shiny_strip_source_columns(data)
      data <- utils::head(data, as.integer(input$preview_rows %||% 500L))
      DT::datatable(
        data,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(25L)
      )
    })

    output$provenance <- DT::renderDT({
      effective <- derived$effective_data()
      attribution <- effective$attribution
      if (!is.data.frame(attribution) || nrow(attribution) == 0L) {
        return(DT::datatable(data.frame(Message = "No variable provenance is available."), rownames = FALSE))
      }
      source_meta <- do.call(rbind, lapply(state$input_sources, function(source) {
        data.frame(
          source_id = source$id,
          source_format = source$format,
          imported_at = format(as.POSIXct(source$imported_at), "%Y-%m-%d %H:%M:%S"),
          md5 = source$md5 %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }))
      provenance <- merge(attribution, source_meta, by = "source_id", all.x = TRUE, sort = FALSE)
      names(provenance) <- gsub("_", " ", tools::toTitleCase(names(provenance)), fixed = TRUE)
      DT::datatable(
        provenance,
        rownames = FALSE,
        filter = "top",
        options = osem_shiny_dt_options(20L)
      )
    })

    output$download_prepared_data <- shiny::downloadHandler(
      filename = function() {
        paste0(
          osem_shiny_project_slug(state$project_name),
          "-prepared-input.",
          input$prepared_format %||% "csv"
        )
      },
      content = function(file) {
        data <- derived$effective_data()$data
        shiny::req(is.data.frame(data), nrow(data) > 0L)
        data <- osem_shiny_strip_source_columns(data)
        format <- input$prepared_format %||% "csv"
        if (identical(format, "rds")) {
          saveRDS(data, file, version = 3)
        } else if (identical(format, "xlsx")) {
          if (!requireNamespace("writexl", quietly = TRUE)) {
            stop("Package 'writexl' is required for Excel export.", call. = FALSE)
          }
          writexl::write_xlsx(data, path = file)
        } else {
          utils::write.csv(data, file, row.names = FALSE, na = "")
        }
      }
    )

    shiny::observeEvent(input$dictionary_file, {
      shiny::req(input$dictionary_file$datapath)
      imported <- tryCatch(
        osem_shiny_import_table(
          input$dictionary_file$datapath,
          input$dictionary_file$name
        ),
        error = function(e) e
      )
      if (inherits(imported, "error")) {
        osem_shiny_notify_error(imported, session)
        return()
      }
      state$dictionary <- osem_shiny_normalise_dictionary(imported)
      osem_shiny_state_mark_changed(
        state,
        "dictionary",
        paste0("Replaced the dictionary with '", input$dictionary_file$name, "'.")
      )
    })

    output$dictionary_table <- DT::renderDT({
      DT::datatable(
        state$dictionary,
        rownames = FALSE,
        editable = list(target = "cell"),
        selection = list(mode = "multiple", target = "row"),
        filter = "top",
        options = osem_shiny_dt_options(15L)
      )
    }, server = TRUE)

    shiny::observeEvent(input$dictionary_table_cell_edit, {
      edited_dictionary <- DT::editData(
        state$dictionary,
        input$dictionary_table_cell_edit,
        rownames = FALSE
      )
      state$dictionary <- osem_shiny_normalise_dictionary(edited_dictionary)
      osem_shiny_state_mark_changed(state, "dictionary", "Edited a dictionary value.")
    })

    shiny::observeEvent(input$dictionary_add_row, {
      dictionary <- state$dictionary
      new_row <- as.data.frame(lapply(dictionary, function(column) {
        if (is.numeric(column)) NA_real_ else if (inherits(column, "Date")) as.Date(NA) else ""
      }), stringsAsFactors = FALSE, check.names = FALSE)
      names(new_row) <- names(dictionary)
      state$dictionary <- rbind(dictionary, new_row)
      osem_shiny_state_mark_changed(state, "dictionary", "Added a dictionary row.")
    })

    shiny::observeEvent(input$dictionary_delete_rows, {
      selected <- sort(unique(input$dictionary_table_rows_selected %||% integer()))
      if (length(selected) == 0L) {
        shiny::showNotification("Select dictionary rows to delete.", type = "warning", session = session)
        return()
      }
      state$dictionary <- state$dictionary[-selected, , drop = FALSE]
      osem_shiny_state_mark_changed(state, "dictionary", "Deleted dictionary row(s).")
    })

    shiny::observeEvent(input$dictionary_reset, {
      state$dictionary <- osem_shiny_default_dictionary()
      osem_shiny_state_mark_changed(state, "dictionary", "Restored the bundled OSEM dictionary.")
    })

    output$dictionary_validation <- shiny::renderUI({
      osem_shiny_issue_list(
        derived$workspace()$issues,
        area = c("Dictionary", "Environment"),
        max_items = 20L
      )
    })

    output$download_dictionary <- shiny::downloadHandler(
      filename = function() {
        paste0(
          osem_shiny_project_slug(state$project_name),
          "-dictionary.",
          input$dictionary_format %||% "csv"
        )
      },
      content = function(file) {
        dictionary <- state$dictionary
        format <- input$dictionary_format %||% "csv"
        if (identical(format, "rds")) {
          saveRDS(dictionary, file, version = 3)
        } else if (identical(format, "xlsx")) {
          if (!requireNamespace("writexl", quietly = TRUE)) {
            stop("Package 'writexl' is required for Excel export.", call. = FALSE)
          }
          writexl::write_xlsx(dictionary, path = file)
        } else {
          utils::write.csv(dictionary, file, row.names = FALSE, na = "")
        }
      }
    )
  })
}
