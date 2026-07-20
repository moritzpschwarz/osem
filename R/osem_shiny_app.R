# OSEM Shiny application ----------------------------------------------------
#
# All application-specific R files use the `osem_shiny_` prefix so that the
# user interface remains clearly separated from the econometric model code.

osem_shiny_asset_directory <- function() {
  installed <- system.file(
    "shiny-output",
    "shinyconfigmodel",
    "www",
    package = "osem"
  )
  if (nzchar(installed) && dir.exists(installed)) return(installed)

  # These fallbacks support development loading and direct source-tree testing.
  namespace_path <- tryCatch(
    getNamespaceInfo(asNamespace("osem"), "path"),
    error = function(e) ""
  )
  candidates <- c(
    file.path(namespace_path, "shiny-output", "shinyconfigmodel", "www"),
    file.path(namespace_path, "inst", "shiny-output", "shinyconfigmodel", "www"),
    file.path("inst", "shiny-output", "shinyconfigmodel", "www"),
    file.path(getwd(), "inst", "shiny-output", "shinyconfigmodel", "www")
  )
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  existing <- candidates[dir.exists(candidates)]
  if (length(existing) > 0L) existing[[1L]] else ""
}

osem_shiny_register_assets <- function() {
  directory <- osem_shiny_asset_directory()
  if (!nzchar(directory) || !dir.exists(directory)) {
    stop(
      "Could not find the OSEM Shiny assets. Reinstall the 'osem' package.",
      call. = FALSE
    )
  }

  resource_name <- "osem-shiny-assets"
  registered <- shiny::resourcePaths()
  if (!resource_name %in% names(registered)) {
    shiny::addResourcePath(resource_name, directory)
  }

  invisible(resource_name)
}

osem_shiny_create_app <- function(initial_model = NULL,
                                  initial_project = NULL,
                                  max_upload_mb = 500) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The OSEM app requires the 'shiny' package.", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("The OSEM app requires the 'DT' package.", call. = FALSE)
  }

  upload_limit <- suppressWarnings(as.numeric(max_upload_mb)) * 1024^2
  if (!is.finite(upload_limit) || upload_limit <= 0) {
    upload_limit <- 500 * 1024^2
  }
  current_limit <- getOption("shiny.maxRequestSize")
  if (is.null(current_limit) || !is.finite(current_limit) || current_limit < upload_limit) {
    options(shiny.maxRequestSize = upload_limit)
  }

  osem_shiny_register_assets()

  shiny::shinyApp(
    ui = osem_shiny_app_ui(),
    server = function(input, output, session) {
      osem_shiny_app_server(
        input = input,
        output = output,
        session = session,
        initial_model = initial_model,
        initial_project = initial_project
      )
    }
  )
}

osem_shiny_app_ui <- function() {
  shiny::navbarPage(
    title = shiny::div(
      class = "osem-brand",
      shiny::tags$img(
        src = "osem-shiny-assets/osem-logo.png",
        alt = "OSEM logo"
      ),
      shiny::span("OSEM")
    ),
    id = "osem_main_navigation",
    windowTitle = "OSEM model workspace",
    inverse = TRUE,
    collapsible = TRUE,
    header = shiny::tagList(
      shiny::tags$head(
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "osem-shiny-assets/osem.css"
        ),
        shiny::tags$script(shiny::HTML(
          "(function() {
             function report(ok, label, detail) {
               if (window.Shiny && Shiny.setInputValue) {
                 Shiny.setInputValue('osem_clipboard_status', {
                   ok: ok, label: label || 'Code', detail: detail || '', nonce: Math.random()
                 }, {priority: 'event'});
               }
             }
             function fallbackCopy(text) {
               var area = document.createElement('textarea');
               area.value = text;
               area.setAttribute('readonly', '');
               area.style.position = 'fixed';
               area.style.opacity = '0';
               document.body.appendChild(area);
               area.select();
               var ok = false;
               try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
               document.body.removeChild(area);
               return ok;
             }
             function registerCopyHandler() {
               if (!(window.Shiny && Shiny.addCustomMessageHandler)) {
                 window.setTimeout(registerCopyHandler, 50);
                 return;
               }
               Shiny.addCustomMessageHandler('osem-copy-text', function(message) {
                 var text = String(message.text || '');
                 var label = String(message.label || 'Code');
                 if (navigator.clipboard && window.isSecureContext) {
                   navigator.clipboard.writeText(text).then(
                     function() { report(true, label, ''); },
                     function(err) {
                       var ok = fallbackCopy(text);
                       report(ok, label, ok ? '' : String(err || 'Clipboard access failed.'));
                     }
                   );
                 } else {
                   var ok = fallbackCopy(text);
                   report(ok, label, ok ? '' : 'Clipboard access is not available in this browser.');
                 }
               });
             }
             registerCopyHandler();
           })();"
        ))
      ),
      shiny::div(
        class = "osem-workspace-bar",
        shiny::uiOutput("osem_global_status")
      )
    ),
    shiny::tabPanel("Project", osem_shiny_project_ui("project")),
    shiny::tabPanel("Data", osem_shiny_data_ui("data")),
    shiny::tabPanel("Specification", osem_shiny_specification_ui("specification")),
    shiny::tabPanel("Estimation", osem_shiny_estimation_ui("estimation")),
    shiny::tabPanel("Results", osem_shiny_results_ui("results")),
    shiny::tabPanel("Forecast", osem_shiny_forecast_ui("forecast")),
    shiny::tabPanel("Reproduce", osem_shiny_reproduce_ui("reproduce")),
    footer = shiny::div(
      class = "osem-footer",
      "Open-Source Empirical Macroeconomic Model"
    )
  )
}

osem_shiny_app_server <- function(input,
                                  output,
                                  session,
                                  initial_model = NULL,
                                  initial_project = NULL) {
  state <- osem_shiny_state_new(
    session = session,
    initial_model = initial_model,
    initial_project = initial_project
  )
  derived <- osem_shiny_state_derived(state)

  # Expose the state for testServer()/shinytest2 without creating global state.
  session$userData$osem_state <- state
  session$userData$osem_derived <- derived

  output$osem_global_status <- shiny::renderUI({
    workspace <- derived$workspace()
    issue_counts <- osem_shiny_issue_counts(workspace$issues)

    shiny::div(
      class = "osem-global-status",
      shiny::span(
        class = "osem-global-project",
        state$project_name
      ),
      osem_shiny_badge(
        label = if (workspace$ready) "Inputs ready" else "Inputs need attention",
        status = if (workspace$ready) "success" else "warning"
      ),
      osem_shiny_badge(
        label = paste0(issue_counts$error, " errors"),
        status = if (issue_counts$error > 0L) "danger" else "muted"
      ),
      osem_shiny_badge(
        label = paste0(issue_counts$warning, " warnings"),
        status = if (issue_counts$warning > 0L) "warning" else "muted"
      ),
      osem_shiny_badge(
        label = paste0("Model: ", state$model_status),
        status = osem_shiny_status_class(state$model_status)
      ),
      osem_shiny_badge(
        label = paste0("Forecast: ", state$forecast_status),
        status = osem_shiny_status_class(state$forecast_status)
      )
    )
  })


  shiny::observeEvent(input$osem_clipboard_status, {
    status <- input$osem_clipboard_status
    if (isTRUE(status$ok)) {
      shiny::showNotification(
        paste0(status$label %||% "Code", " copied to the clipboard."),
        type = "message",
        duration = 4,
        session = session
      )
    } else {
      shiny::showNotification(
        paste0(
          "Could not copy ", tolower(status$label %||% "code"), ". ",
          status$detail %||% "Use the script download instead."
        ),
        type = "error",
        duration = 8,
        session = session
      )
    }
  }, ignoreInit = TRUE)

  osem_shiny_project_server("project", state, derived)
  osem_shiny_data_server("data", state, derived)
  osem_shiny_specification_server("specification", state, derived)
  osem_shiny_estimation_server("estimation", state, derived)
  osem_shiny_results_server("results", state, derived)
  osem_shiny_forecast_server("forecast", state, derived)
  osem_shiny_reproduce_server("reproduce", state, derived)

  invisible(state)
}
