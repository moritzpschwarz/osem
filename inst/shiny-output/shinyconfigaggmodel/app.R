library(shiny)
library(DT)


library(aggregate.model)
spec <- dplyr::tibble(
  type = c(
    #"d",
    #"d",
    "n",
    "n",
    "n",
    "n",
    "d",
    "n",
    "n",
    #"d",
    "n",
    "n"
  ),
  dependent = c(
    #"StatDiscrep",
    #"TOTS",
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "Emissions",
    "GDP",
    "GValueAddGov", # as in NAM, technical relationship
    "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
    #"DomDemand", # as in NAM
    "GValueAddConstr" ,
    "GValueAddWholesaletrade"
  ),
  independent = c(
    #"TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    #"GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "",
    "FinConsExpGov + FinConsExpHH",
    "GDP + Export + GValueAddIndus",
    "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
    "FinConsExpGov", # as in NAM, technical relationship
    "Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
    #"FinConsExpHH + FinConsExpGov + GCapitalForm",
    "LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
    "Export + LabCostService"
  )
)


fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
fc <- list(geo = "AT", unit = "THS_T")
fd <- list(geo = "AT", s_adj = "SCA")
fe <- list(geo = "AT", s_adj = "SCA", unit = "I15")
ff <- list(geo = "AT", s_adj = "SCA", unit = "I16")

filter_list <- list(
  "P7" = fa,
  "YA0" = fb,
  "P31_S14_S15" = fa,
  "P5G" = fa,
  "B1G" = fa,
  "P3_S13" = fa,
  "P6" = fa,
  "GHG" = fc,
  "B1GQ" = fa,
  "PSQM" = fe,
  "LM-LCI-TOT" = ff
)




# Define UI for app
ui <- navbarPage(title = "The Aggregate Model",
                 tabPanel(
                   "Input",
                   sidebarPanel(
                     sliderInput("lags", "Maximum lags:",min = 0, max = 4, value = 4),
                     radioButtons("download_data", "Download Data directly from EUROSTAT?",
                                  choices = list("Yes" = TRUE, "No" = FALSE),
                                  selected = FALSE),
                     textInput("path_to_files", "File Path where to save to (when downloading)\nor where to load from (when not downloading).",
                               value = "C:/Users/morit/Documents/Github/aggregate.model/data-raw/csv/"),
                     radioButtons("trend", "Include a Trend?",
                                  choices = list("Yes" = TRUE, "No" = FALSE),
                                  selected = TRUE),
                     radioButtons("gets", "Run GETS selection?",
                                  choices = list("Yes" = TRUE, "No" = FALSE),
                                  selected = TRUE),
                     numericInput("pvalue_gets", "Select p-value:", value = 0.05, step = 0.01),
                     radioButtons("isat", "Run Indicator Saturation?",
                                  choices = list("Yes" = TRUE, "No" = FALSE),
                                  selected = TRUE),
                     numericInput("pvalue_isat", "Select p-value:", value = 0.05, step = 0.01)
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Model Specification",
                                DT::dataTableOutput("input_table"),
                                br(),
                                actionButton("add_btn_spec", "Add Row at the End"),
                                actionButton("delete_btn_spec", "Delete Last Row")
                       ),
                       tabPanel("Dictionary",
                                DT::dataTableOutput("dict_table"),
                                br(),
                                actionButton("add_btn_dict", "Add Row at the End"),
                                actionButton("delete_btn_dict", "Delete Last Row")
                       )
                     ),
                     br(),
                     h2("Click to run the Model (output in separate tab):"),
                     br(),
                     actionButton("run_model", "Run Model")
                   )
                 ),
                 tabPanel("Model Output",
                          verbatimTextOutput("model_output"),
                          br(),
                          plotOutput("plot_output")
                 )
)

# Define server logic
server <- function(input, output, session) {
  # Load default dictionaries and specs
  dictionary <- aggregate.model::dict
  specs <- spec

  # Create dictionary table
  dict_table <- reactiveVal(dictionary)
  observeEvent(input$add_btn_dict, {
    #t = bind_rows(dict_table(), tibble(""))
    t = add_row(dict_table())
    dict_table(t)
  })

  observeEvent(input$delete_btn_dict, {
    t = slice(dict_table(), 1:(nrow(dict_table())-1))
    dict_table(t)
  })

  # Create input table
  spec_table <- reactiveVal(spec)
  observeEvent(input$add_btn_spec, {
    #t = bind_rows(spec_table(), tibble(""))
    t = add_row(spec_table())
    spec_table(t)
  })

  observeEvent(input$delete_btn_spec, {
    t = slice(spec_table(), 1:(nrow(spec_table())-1))
    spec_table(t)
  })


  # Render dictionary table
  output$dict_table <- DT::renderDataTable({
    DT::datatable(dict_table(), editable = TRUE, rownames = FALSE)
  })

  # Render input table
  output$input_table <- DT::renderDataTable({
    DT::datatable(spec_table(), editable = TRUE, rownames = FALSE)
  })

  # Run model when button is clicked
  run_model <- eventReactive(input$run_model, {
    # Get edited dictionaries and specs
    # edited_dict <- input$dict_table_cell_edit
    # edited_dict_rows <- edited_dict$row
    # edited_dict_cols <- edited_dict$col
    # edited_dict_value <- edited_dict$value
    #
    # edited_spec <- input$input_table_cell_edit
    # edited_spec_rows <- edited_spec$row
    # edited_spec_cols <- edited_spec$col
    # edited_spec_value <- edited_spec$value
    #
    # # Update dictionary and specs with edited values
    # dictionary[cbind(edited_dict_rows, edited_dict_cols)] <- edited_dict_value
    # specs[cbind(edited_spec_rows, edited_spec_cols)] <- edited_spec_value

    # Call run_model function
    model_output <- aggregate.model::run_model(
      specification = spec_table(),
      dictionary = dict_table(),
      max.lag = input$lags,
      gets_selection = input$gets,
      selection.tpval = input$pvalue_gets,
      saturation = input$isat,
      saturation.tpval = input$pvalue_isat,
      filter_list = filter_list,
      download = as.logical(input$download_data),
      save_to_disk = if(as.logical(input$download_data)){paste0(input$path_to_files,"/input_data.csv")}else{NULL},
      inputdata_directory = if(!as.logical(input$download_data)){input$path_to_files}else{NULL}
    )

    model_forecast <- forecast_model(model_output)
    forecast_plot <- plot(model_forecast)

    out <- list()
    out$model_output <- model_output
    out$model_forecast <- model_forecast
    out$forecast_plot <-forecast_plot

    # Return model output
    return(out)
  })


  # # Plot model when button is clicked
  # output$plot_model <- eventReactive(input$run_model, {
  #   a <- forecast_model(run_model())
  #   b <- plot(a)
  #
  #   # Return plot output
  #   return(b)
  # })

  # Render model output
  output$model_output <- renderPrint({
    run_model()$model_output
  })

  # Render model output
  output$plot_output <- renderPlot({
    run_model()$forecast_plot
  })

  # output$model_plot <- renderPrint({
  #   plot_model()
  # })
}

# Run the app
shinyApp(ui, server)
