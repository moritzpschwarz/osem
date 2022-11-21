

ui <- fluidPage(

  tabsetPanel(
    tabPanel("Input", fluid = TRUE,
             mainPanel(
               fileInput("upload", NULL, buttonLabel = "Upload Stored Model from Disk...", multiple = FALSE, accept = ".Rds"),
               tableOutput("files"),
               tags$hr(),
               h1("Specification"),
               DT::DTOutput("spec"),
               h1("Dependency"),
               plotOutput("network")#,
               #tableOutput(outputId = "test")
             )
    ),
    tabPanel("Graphs", fluid = TRUE,
                dateRangeInput(inputId = "range_plot", label = "Date Range for Plots",
                               start = as.Date("1960-01-01"), end = Sys.Date()),
             mainPanel(
               plotOutput("plots")
             )
    ),
    tabPanel("Equations", fluid = TRUE,
             mainPanel(
               verbatimTextOutput("equations")
             )
    ),
    tabPanel("Diagnostics", fluid = TRUE,
             mainPanel(
               DT::DTOutput("diag")
             )
    )
  )

)

server <- function(input, output) {

  # data <- reactive({
  #   req(input$upload)
  # })

  # input$upload <- observe({
  #   if (is.null(input$upload)){
  #     input$upload <- getShinyOption("object", model)
  #   }
  # })


  aggmod <- reactive({
    if (is.null(input$upload)) {
      getShinyOption("aggmodel_direct")
    } else {
      readRDS(file = input$upload$datapath)
    }
  })

  wide <- reactive({
    aggmod()$full_data %>%
      pivot_wider(names_from = na_item, values_from = values)
  })

  sel <- reactive({
    f <- colnames(wide())[grep(colnames(wide()), pattern = "\\.hat")]
    basef <- sub("\\.hat*", "", f)
    sub <- wide() %>%
      select(time, union(basef, f)) %>%
      pivot_longer(cols = !time, names_to = c("variable", "type"), names_sep = "\\.", values_to = "value") %>%
      mutate(type = case_when(is.na(type) ~ "observed",
                              type == "hat" ~ "fitted"))
    return(sub)
  })

  eq <- reactive({
    modulesprint <- aggmod()$module_collection %>%
      filter(type == "n")
    wholeprint <- ""
    for (i in 1:NROW(modulesprint)) {
      wholeprint <- capture.output(print(modulesprint[[i, "model"]]), file = NULL)
    }
    return(wholeprint)
  })

  # get_file_or_default <- reactive({
  #   if (is.null(input$upload)) {
  #     getShinyOption("aggmodel_direct ")
  #   } else {
  #     readRDS(file = input$upload$datapath)
  #   }
  # })
  #
  # output$test <- renderTable({
  #   #if(is.null(input$upload)){
  #    # tibble("test")
  #   #} else {
  #     get_file_or_default()$args$specification
  #     #object$args$specification
  #   #}
  # })

  output$test <- renderText(input$range_plot)

  output$files <- renderTable(input$upload)
  output$spec <- DT::renderDT(aggmod()$module_order_eurostatvars)
  output$plots <- renderPlot({
    sel() %>%
      filter(time >= as.Date(input$range_plot[1]) & time <= as.Date(input$range_plot[2])) %>%
    ggplot(aes(x = time, y = value, color = type)) +
      geom_line() +
      facet_wrap(facets = "variable", scales = "free", nrow = length(unique(sel()$variable)))
  })
  output$equations <- renderPrint(eq(), width = 1000)
  output$diag <- DT::renderDT({

    diagnostics_model(aggmod()) %>%
      DT::datatable() %>%
      DT::formatStyle(columns = c("AR", "ARCH"),
                      backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
      DT::formatRound(columns = c("AR", "ARCH"),
                      digits = 4)

    })
  output$network <- renderPlot({
    req(aggmod())
    network(aggmod())
  })

}


shinyApp(ui = ui, server = server)


