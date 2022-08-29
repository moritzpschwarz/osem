

ui <- fluidPage(

  tabsetPanel(
    tabPanel("Input", fluid = TRUE,
             mainPanel(
               fileInput("upload", NULL, buttonLabel = "Upload Stored Model from Disk...", multiple = FALSE, accept = ".Rds"),
               tableOutput("files"),
               tags$hr(),
               h1("Specification"),
               tableOutput("spec"),
               tableOutput(outputId = "test")
             )
    ),
    tabPanel("Graphs", fluid = TRUE,
             mainPanel(
               plotOutput("plots")
             )
    ),
    tabPanel("Equations", fluid = TRUE,
             mainPanel(
               verbatimTextOutput("equations")
             ))
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

  output$files <- renderTable(input$upload)
  output$spec <- renderTable(head(aggmod()$module_order_eurostatvars))
  output$plots <- renderPlot({
    ggplot(data = sel(), aes(x = time, y = value, color = type)) +
      geom_line() +
      facet_wrap(facets = "variable", scales = "free", nrow = length(unique(sel()$variable)))
  })
  output$equations <- renderPrint(eq(), width = 1000)

}


shinyApp(ui = ui, server = server)


