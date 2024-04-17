# devtools::load_all()
library(shiny)
library(DT)
library(shinycssloaders) # possibly still a good idea to pass through more specific info along the steps of the estimation

# Define UI for app
ui <- fluidPage(

  tags$head(
    # set smooth scrolling for scroll links
    tags$style(HTML("
      html {
        scroll-behavior: smooth;
      }
      .rightAlign {
        display: flex;
        width: 100%;
        justify-content:right;
      }
      .rightAlign > * {
        margin-right: 2px;
        margin-left: 2px;
      }"))
  ),

  titlePanel("Aggregate Model Configuration App"),

  #HTML("<p>Lorem ipsum dolor sit amet.</p>"),
  HTML('    <section>
        <p>This powerful tool is designed to implement and operationalize the Aggregate Model, a collaborative effort by Jonas Kurle, Andrew Martinez, Felix Pretis, and Moritz Schwarz. This model is an adaptation of the renowned Norwegian Aggregate Model, originally developed by Gunnar Bardsen and Ragnar Nymoen.</p>
        <p>The "Aggregate Model" is a cutting-edge, open-source econometric model builder. Its primary objective is to deliver robust empirical forecasts of sectoral carbon emissions. The model leverages advanced econometric tools from the robust time series modeling literature, incorporating techniques such as diagnostic testing, indicator saturation, and automatic forecast evaluation. For more information see </p>
    </section>

    <section>
        <h2>Key Features:</h2>
        <ul>
            <li><strong>Robust Forecasting:</strong> The Aggregate Model excels in providing reliable and robust forecasts for sectoral carbon emissions.</li>
            <li><strong>Econometric Tools:</strong> Leveraging state-of-the-art econometric tools ensures accurate and data-driven predictions.</li>
            <li><strong>Open-Source:</strong> As an open-source initiative, the Aggregate Model encourages collaboration and transparency in the modeling community.</li>
        </ul>
    </section>

    <section>
        <h2>How to Use this app:</h2>
        <ol>
            <li><strong>Configure the Model Parameters:</strong> Easily customize model parameters to tailor predictions to your specific needs.</li>
            <li><strong>Explore Diagnostic Tests:</strong> Dive into diagnostic testing capabilities to ensure the robustness of your forecasts.</li>
            <li><strong>Evaluate Forecasts Automatically:</strong> The Aggregate Model automates forecast evaluation, streamlining the process for enhanced efficiency.</li>
        </ol>
    </section>

    <section>
        <h2>Get Started:</h2>
        <p>Begin your journey with the Aggregate Model Package today and unlock the potential for precise and reliable sectoral carbon emissions forecasting. Visit <a href="https://moritzschwarz.org/aggregate.model/" target="_blank">the Aggregate Model Package Website</a> for more information.</p>
        <p><em>Disclaimer: The Aggregate Model Package is a collaborative effort and is continuously evolving. User feedback and contributions are welcomed to enhance its capabilities.</em></p>
    </section>
    <a href="#input">Get Started</a>

    <br><br>'),

  tags$div(
    `id` = "input",
    tags$h1("Input"),
  ),

  fluidRow(
    column(12, align="left",
           selectInput("preset", label = NULL, choices = c("Choose preset model..." = "", "Custom", "Placeholder1", "Placeholder2"))
    )
  ),

  tabsetPanel( id = "SettingsTab",
               tabPanel(
                 "Specification",
                 fileInput("spec", "Upload Specification (CSV)", accept = ".csv"),
                 DT::dataTableOutput("specification_table"),
                 fluidRow(actionLink("addRow", "Add Row"), " - ",
                          actionLink("deleteRows", "Delete selected rows"), "(Rows can be selected by clicking)",
                          class = 'rightAlign'
                 )
               ),
               tabPanel(
                 "Input Data",
                 fileInput("data", "Upload Input Data (CSV, from 'Save Processed Input data')", accept = ".csv"),
                 DT::dataTableOutput("input_data_table")
               ),
               tabPanel(

                 "Settings",
                 fluidRow(
                   column(6,
                          "Model settings",
                          numericInput(
                            "max_ar",
                            "Maximum Number of Autoregressive Lags:",
                            min = 0,
                            max = 12,
                            step = 1,
                            value = 4
                          ),
                          numericInput(
                            "max_dl",
                            "Maximum Number of Distributed Lags:",
                            min = 0,
                            max = 12,
                            step = 1,
                            value = 2
                          ),
                          radioButtons(
                            "use.logs",
                            "Use logs?",
                            choices = c("both", "y", "x"),
                            selected = "both"
                          ),
                          checkboxInput("trend", "Should a trend be added?", value = TRUE),
                          radioButtons( # formulation of setting can be done later when the implementation is final, it is fine like this for now (call with moritz in april 2024)
                            "ardl.or.ecm",
                            "ardl or ecm?",
                            choices = c("ardl", "ecm"),
                            selected = "ardl"
                          ),
                          checkboxInput("gets_selection", "Run GETS Selection?", value = TRUE),
                          numericInput(
                            "gets_pvalue",
                            "GETS P-value:",
                            min = 0.01,
                            max = 0.5,
                            step = 0.01,
                            value = 0.05
                          ),
                          checkboxInput("indicator_saturation", "Run Indicator Saturation?", value = TRUE),
                          numericInput(
                            "ind_sat_pval",
                            "Indicator Saturation P-value:",
                            min = 0.01,
                            max = 0.5,
                            step = 0.01,
                            value = 0.05
                          ),
                          numericInput(
                            "max_block_size",
                            "Maximum Block Size:",
                            min = 0,
                            max = 20,
                            step = 1,
                            value = 20
                          ),
                          checkboxInput("constrain_to_minimum_sample", "Constrain to minimum sample?", value = TRUE),
                          textInput(
                            "save_file",
                            "Save Processed Input Data (must be file ending with RDS, rds, Rds, csv, xls, xlsx):",
                            value = "inputdata/processed_inputdata.csv"
                          )),
                   column(6,
                          "Forecast settings",
                          #exog predicitions: unclear how to implement.. file input?
                          numericInput(
                            "n_ahead",
                            "Periods to forecast ahead:",
                            min = 0,
                            max = 20,
                            step = 1,
                            value = 10
                          ),
                          #"ci.levels - how would we display the selection in UI?",
                          radioButtons(
                            "exog.fill.method",
                            "Exogenous fill method?",
                            choices = c("AR", "last"),
                            selected = "AR"
                          ),
                          numericInput(
                            "ar.fill.max",
                            "Maximum periods to fill in:",
                            min = 0,
                            max = 4,
                            step = 1,
                            value = 4
                          ),
                          #"plot.forecast: do we display it in another way as well?",
                          numericInput(
                            "uncertainty.sample",
                            "Number of draws to be made for the error bars (uncertainty_sample):",
                            min = 0,
                            max = 100,
                            step = 1,
                            value = 100
                          ),
                   )
                 ),

               ),
               tabPanel(
                 "Dictionary",
                 fileInput("dict", "Upload Dictionary (CSV)", accept = ".csv"),
                 DT::dataTableOutput("dictionary_table")
               ),
  ),

  actionButton("run_button", "Run Model", onclick ="location.href='#output';"), # uses onclick to scroll to section automatically
  actionButton("forecast_button", "Forecast Model", onclick ="location.href='#output';"),




  tags$div(
    `id` = "output",
    tags$h1("Output"),
  ),

  tabsetPanel(id = "OutputTab",
    tabPanel(title = "Model Results", value = "results",
             shinycssloaders::withSpinner(
               DT::DTOutput("model_output"),
               proxy.height = "800px"
             ),
             tags$head(tags$style("#model_output{min-height: 400px;}")) # since I don't have linebreaks in placeholder and no progress indicator yet
    ),
    tabPanel(title = "Model Forecasts", value = "forecast",
             dateRangeInput(inputId = "range_plot", label = "Date Range for Plots",
                            start = as.Date("1960-01-01"), end = Sys.Date()),
             plotOutput("forecast_output")
    ),
    tabPanel("Diagnostics", #fluid = TRUE,
             mainPanel(
               DT::DTOutput("diag")
             )
    ),
    tabPanel("Dependency Network Graph", #fluid = TRUE,
             mainPanel(
               plotOutput("network", height = "600", width = "800")
             )
    )

  )
)

# Default dictionary
default_dict <- aggregate.model::dict

default_spec <- dplyr::tibble(
  type = c(
    "d",
    "n",
    "n",
    "n"# ,
    # "d"
  ),
  dependent = c(
    "TOTS",
    "Import",
    "EmiCO2Combustion",
    "EmiCO2Industry"# ,
    #"EmiGHGTotal" # commented out as it returned NA, leading to problems. Reason is not clear to me.
  ),
  independent = c(
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "HDD + HICP_Gas + HICP_Electricity + GValueAdd",
    "HICP_Gas + HICP_Electricity + GValueAddIndus"# ,
    # "EmiCO2Combustion + EmiCO2Industry + EmiCH4Livestock + EmiN2OTotal"
  )
)

default_input <- aggregate.model::sample_input



as.lm.custom <- function(object) {
  print(typeof(object))
  y <- object$aux$y
  x <- object$aux$mX
  colnames(x) <- object$aux$mXnames
  yx <- data.frame(y, x)
  result <- lm(formula = y ~ . - 1, data = yx)
  return(result)
}




# Define server logic
server <- function(input, output, session) {

  # Define a reactive values object to store the user's input data, dictionary, and specification
  rv <- reactiveValues(
    inputdata = default_input,
    dictionary = default_dict,
    specification = default_spec,
    save_file = "",
    #table_output = NULL,
    model_list = NULL
  )

  #is it nessecary to do rv <- input for all the variables? I am keeping the implementation but have to ask Moritz
  observe({ # should the varnames rather be the same in both lists or are some different on purpose?
    rv$max_ar <- input$max_ar
    rv$max_dl <- input$max_dl

    #newly added
    rv$use.logs <- input$use.logs
    rv$trend <- input$trend
    rv$ardl.or.ecm <- input$ardl.or.ecm
    rv$max_block_size <- input$max_block_size
    rv$constrain_to_minimum_sample <- input$constrain_to_minimum_sample


    rv$saturation <- input$indicator_saturation
    rv$gets_select <- input$gets_selection
    rv$ind_sat_pval <- input$ind_sat_pval
    rv$gets_pval <- input$gets_pvalue

    #newly added: forecast
    rv$n_ahead <- input$n_ahead
    rv$exog.fill.method <- input$exog.fill.method
    rv$ar.fill.max <- input$ar.fill.max
    rv$uncertainty.sample <- input$uncertainty.sample
  })

  # Load the user's input data
  observe({
    req(input$data)
    rv$inputdata <- readr::read_csv(input$data$datapath, show_col_types = FALSE)
    rv$inputdirectory <- dirname(input$data$datapath)
  })

  # Load the user's dictionary
  observe({
    req(input$dict)
    rv$dictionary <- readr::read_csv(input$dict$datapath, show_col_types = FALSE)
  })

  # Load the user's specification
  observe({
    req(input$spec)
    rv$specification <- readr::read_csv(input$spec$datapath, show_col_types = FALSE)
  })

  observeEvent(
    input$addRow,
    {rv$specification <- rv$specification %>% rbind(c("n", "", ""))}
  )

  observeEvent(
    input$deleteRows, {
      if (!is.null(input$specification_table_rows_selected)) {
        rv$specification <- rv$specification[-as.numeric(input$specification_table_rows_selected), ]
      }
  })

  # Render the input data table
  output$input_data_table <- renderDT({
    datatable(rv$inputdata, editable = FALSE)
  })

  # Render the dictionary table
  output$dictionary_table <- renderDT({
    datatable(rv$dictionary, editable = TRUE)
  })

  # Render the specification table
  output$specification_table <- renderDT({
    datatable(rv$specification, editable = TRUE, options = list(dom = 't')) # dom = t to hide datatable UI
  })

  # Update the reactive values object with the edited input data table
  observeEvent(input$input_data_table_cell_edit, {
    info <- input$input_data_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$inputdata[row, col] <- value
  })

  # Update the reactive values object with the edited dictionary table
  observeEvent(input$dictionary_table_cell_edit, {
    info <- input$dictionary_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$dictionary[row, col] <- value
  })

  # Update the reactive values object with the edited specification table
  observeEvent(input$specification_table_cell_edit, {
    info <- input$specification_table_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value

    rv$specification[row, col] <- value
  })

  # Update the reactive values object with the save file
  observe({
    rv$save_file <- input$save_file
  })

  # Function to run the model
  run_model_shiny <- function() {
    print(reactiveValuesToList(rv)) # to display reactive variables at time of function call
    model_output <- aggregate.model::run_model(specification = rv$specification,
                                               dictionary = rv$dictionary,
                                               inputdata_directory = if (is.null(rv$inputdirectory)) { NULL } else { dirname(input$data$datapath) }, # inputdata
                                               primary_source = if (is.null(rv$inputdirectory)) { "download" } else { "local" },#  swtichting to "download" makes it slower? Could be a setting with a warning "might take longer". Reading the cached files takes long as well.
                                               save_to_disk = rv$save_file,
                                               present = FALSE,
                                               quiet = FALSE,
                                               use_logs = rv$use.logs,
                                               trend = rv$trend,
                                               ardl_or_ecm = rv$ardl.or.ecm,
                                               max.ar = rv$max_ar,
                                               max.dl = rv$max_dl,
                                               saturation = rv$saturation,
                                               saturation.tpval = rv$ind_sat_pval,
                                               max.block.size = rv$max_block_size,
                                               gets_selection = rv$gets_select,
                                               selection.tpval = rv$gets_pval,
                                               constrain.to.minimum.sample = rv$constrain_to_minimum_sample)

    # # Print or process the model output as needed
    #print(unlist(model_output))
    return(model_output)
  }

  # Function to forecast the model
  forecast_model_shiny <- function(){
    print(reactiveValuesToList(rv)) # to display reactive variables at time of function call
    aggregate.model::forecast_model(
      model = rv$model_output,

      ## implement the additional options ##

      #exog_predictions = NULL,
      n.ahead = rv$n_ahead,
      #ci.levels = c(0.5, 0.66, 0.95),
      exog_fill_method = rv$exog.fill.method,
      ar.fill.max = rv$ar.fill.max,
      #plot.forecast = TRUE,
      uncertainty_sample = rv$uncertainty.sample,
      #quiet = FALSE
    )
    # Add code to handle forecasting
  }

  # Run model when "Run Model" button is clicked
  observeEvent(input$run_button, {
    rv$table_output <- NULL
    rv$model_output <- NULL # needed for progress indicator package to work

    showNotification("Estimation is currently running... (might take up to 5 minutes on first run)", type = "message") # could update? Or we figure out updating placeholder text
    updateTabsetPanel(session, "OutputTab",
                      selected = "results") # only updates after the whole function ran, with final values. Can this be paced manually?
    rv$model_output <- run_model_shiny()



    rv$model_output_notnull <- rv$model_output$module_collection$model[-which(sapply(rv$model_output$module_collection$model, is.null))]
    rv$model_list <- lapply(rv$model_output_notnull, as.lm.custom)
    rv$table_output <- modelsummary::modelsummary(
      rv$model_list,
      coef_omit = "iis|sis",
      gof_omit = "R",
      title = "Final models run for each sub-module.",
      notes = "Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.", # depending on settings
      stars = TRUE,
      output = "data.frame" # could later be output more beautifully but
    )
    rv$table_output <- DT::datatable(rv$table_output, options = list(pageLength = 50))


  })

  # Forecast model when "Forecast Model" button is clicked
  observeEvent(input$forecast_button, {
    showNotification("Forecasting is currently running...", type = "message")
    updateTabsetPanel(session, "OutputTab",
                      selected = "forecast") # only updates after the whole function ran, with final values. Can this be paced manually?
    rv$forecast_output <- plot(forecast_model_shiny())
  })

  # Display the model output
  output$model_output <- DT::renderDT(rv$table_output)
  #   renderPrint({
  #   if (is.null(rv$model_output)) { # to fix the placeholder text mismatch (starts as string, but model result is list.)
  #     return(cat("Model results will appear here"))
  #   } else {
  #     rv$model_output
  #   }
  # })

  # Display the forecast output
  output$forecast_output <- renderPlot({rv$forecast_output})

  # Display the diagnostics output
  output$diag <- DT::renderDT({
    req(rv$model_output)
    diagnostics_model(rv$model_output) %>%
      DT::datatable() %>%
      DT::formatStyle(columns = c("AR", "ARCH"),
                      backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
      DT::formatRound(columns = c("AR", "ARCH", "indicator_share"),
                      digits = 4)

  })

  # Display the network graph output
  output$network <- renderPlot({
    req(rv$model_output)
    network(rv$model_output)
  })
}




























































# Define UI for app
# ui <- fluidPage(
#   titlePanel("The Aggregate Model"),
#
#   sidebarLayout(
#
#     sidebarPanel(
#       fileInput("data", "Upload Input Data (CSV)", accept = ".csv"),
#       fileInput("dict", "Upload Dictionary (CSV)", accept = ".csv"),
#
#       tabsetPanel(
#         tabPanel("Input Data",
#                  DT::dataTableOutput("input_data_table")
#         ),
#         tabPanel("Dictionary",
#                  DT::dataTableOutput("dictionary_table")
#         )
#       ),
#
#       selectInput("lags", "Maximum Lags:", choices = 0:4, selected = 1),
#       checkboxInput("gets_selection", "Run GETS Selection?", value = FALSE),
#       numericInput("gets_pvalue", "GETS P-value:", min = 0.01, max = 0.5, value = 0.05),
#       checkboxInput("indicator_saturation", "Run Indicator Saturation?", value = FALSE),
#       numericInput("ind_sat_pvalue", "Indicator Saturation P-value:", min = 0.01, max = 0.5, value = 0.05),
#
#       actionButton("run_button", "Run Model")
#     ),
#
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Model Output",
#                  verbatimTextOutput("model_output")
#         )
#       )
#     )
#   )
# )





# # Define UI for app
# ui <- navbarPage(title = "The Aggregate Model",
#                  tabPanel("Input",
#                           sidebarPanel(
#                             selectInput("lags", "Maximum lags:",
#                                         choices = 0:4, selected = 1),
#                             radioButtons("gets", "Run GETS selection?",
#                                          choices = list("Yes" = TRUE, "No" = FALSE),
#                                          selected = FALSE),
#                             numericInput("pvalue", "Select p-value:", value = 0.05, step = 0.01)
#                           ),
#                           mainPanel(
#                             tabsetPanel(
#                               tabPanel("Dictionary",
#                                        DT::dataTableOutput("dict_table")
#                               ),
#                               tabPanel("Input Data",
#                                        DT::dataTableOutput("input_table")
#                               )
#                             ),
#                             br(),
#                             actionButton("run_model", "Run Model")
#                           )
#                  ),
#                  tabPanel("Model Output",
#                           verbatimTextOutput("model_output")
#                  )
# )
#
# # Define server logic
# server <- function(input, output, session) {
#   # Load default dictionaries and specs
#   dictionary <- aggregate.model::dict
#   specs <- aggregate.model::spec
#
#   # Render dictionary table
#   output$dict_table <- DT::renderDataTable({
#     DT::datatable(dictionary, editable = TRUE, rownames = FALSE)
#   })
#
#   # Render input table
#   output$input_table <- DT::renderDataTable({
#     DT::datatable(specs, editable = TRUE, rownames = FALSE)
#   })
#
#   # Run model when button is clicked
#   run_model <- eventReactive(input$run_model, {
#     # Get edited dictionaries and specs
#     edited_dict <- input$dict_table_cell_edit
#     edited_dict_rows <- edited_dict$row
#     edited_dict_cols <- edited_dict$col
#     edited_dict_value <- edited_dict$value
#
#     edited_spec <- input$input_table_cell_edit
#     edited_spec_rows <- edited_spec$row
#     edited_spec_cols <- edited_spec$col
#     edited_spec_value <- edited_spec$value
#
#     # Update dictionary and specs with edited values
#     dictionary[cbind(edited_dict_rows, edited_dict_cols)] <- edited_dict_value
#     specs[cbind(edited_spec_rows, edited_spec_cols)] <- edited_spec_value
#
#     # Call run_model function
#     model_output <- aggregate.model::run_model(specs, dictionary, input$lags, input$gets, input$pvalue)
#
#     # Return model output
#     return(model_output)
#   })
#
#   # Render model output
#   output$model_output <- renderPrint({
#     run_model()
#   })
# }
#
# # Run the app
# shinyApp(ui, server)
#








# ui <- fluidPage(
#
#   tabsetPanel(
#     tabPanel("Input", fluid = TRUE,
#              mainPanel(
#                fileInput("upload", NULL, buttonLabel = "Upload Stored Model from Disk...", multiple = FALSE, accept = ".Rds"),
#                tableOutput("files"),
#                tags$hr(),
#                h1("Specification"),
#                DT::DTOutput("spec"),
#                h1("Dependency"),
#                plotOutput("network", height = "600", width = "800")#,
#                #tableOutput(outputId = "test")
#              )
#     ),
#     tabPanel("Graphs", fluid = TRUE,
#              dateRangeInput(inputId = "range_plot", label = "Date Range for Plots",
#                             start = as.Date("1960-01-01"), end = Sys.Date()),
#              mainPanel(
#                plotOutput("plots", height = "600", width = "800")
#              )
#     ),
#     tabPanel("Equations", fluid = TRUE,
#              mainPanel(
#                verbatimTextOutput("equations")
#              )
#     ),
#     tabPanel("Diagnostics", fluid = TRUE,
#              mainPanel(
#                DT::DTOutput("diag")
#              )
#     )
#   )
#
# )
#
# server <- function(input, output) {
#
#   # data <- reactive({
#   #   req(input$upload)
#   # })
#
#   # input$upload <- observe({
#   #   if (is.null(input$upload)){
#   #     input$upload <- getShinyOption("object", model)
#   #   }
#   # })
#
#
#   aggmod <- reactive({
#     if (is.null(input$upload)) {
#       getShinyOption("aggmodel_direct")
#     } else {
#       readRDS(file = input$upload$datapath)
#     }
#   })
#
#   wide <- reactive({
#     aggmod()$full_data %>%
#       tidyr::pivot_wider(names_from = na_item, values_from = values)
#   })
#
#   sel <- reactive({
#     f <- colnames(wide())[grep(colnames(wide()), pattern = "\\.hat")]
#     basef <- sub("\\.hat*", "", f)
#     sub <- wide() %>%
#       dplyr::select(time, union(basef, f)) %>%
#       tidyr::pivot_longer(cols = !time, names_to = c("variable", "type"), names_sep = "\\.", values_to = "value") %>%
#       dplyr::mutate(type = dplyr::case_when(is.na(type) ~ "observed",
#                                             type == "hat" ~ "fitted"))
#     return(sub)
#   })
#
#   eq <- reactive({
#     modulesprint <- aggmod()$module_collection %>%
#       filter(type == "n")
#     wholeprint <- ""
#     for (i in 1:NROW(modulesprint)) {
#       wholeprint <- capture.output(print(modulesprint[[i, "model"]]), file = NULL)
#     }
#     return(wholeprint)
#   })
#
#   # get_file_or_default <- reactive({
#   #   if (is.null(input$upload)) {
#   #     getShinyOption("aggmodel_direct ")
#   #   } else {
#   #     readRDS(file = input$upload$datapath)
#   #   }
#   # })
#   #
#   # output$test <- renderTable({
#   #   #if(is.null(input$upload)){
#   #    # tibble("test")
#   #   #} else {
#   #     get_file_or_default()$args$specification
#   #     #object$args$specification
#   #   #}
#   # })
#
#   output$test <- renderText(input$range_plot)
#
#   output$files <- renderTable(input$upload)
#   output$spec <- DT::renderDT(aggmod()$module_order)
#   output$plots <- renderPlot({
#     sel() %>%
#       filter(time >= as.Date(input$range_plot[1]) & time <= as.Date(input$range_plot[2])) %>%
#       ggplot2::ggplot(ggplot2::aes(x = time, y = value, color = type)) +
#       ggplot2::geom_line() +
#       ggplot2::facet_wrap(facets = "variable", scales = "free", nrow = length(unique(sel()$variable))) +
#       ggplot2::theme_minimal(base_size = 20) +
#       ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
#       ggplot2::labs(x = NULL, y = NULL)
#   })
#   output$equations <- renderPrint(eq(), width = 1000)
#   output$diag <- DT::renderDT({
#
#     diagnostics_model(aggmod()) %>%
#       DT::datatable() %>%
#       DT::formatStyle(columns = c("AR", "ARCH"),
#                       backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
#       DT::formatRound(columns = c("AR", "ARCH", "indicator_share"),
#                       digits = 4)
#
#   })
#   output$network <- renderPlot({
#     req(aggmod())
#     network(aggmod())
#   })
#
# }


shinyApp(ui = ui, server = server)


