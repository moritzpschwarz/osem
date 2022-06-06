ui <- fluidPage(

  tabsetPanel(
    tabPanel("Input", fluid = TRUE,
             mainPanel(
               fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE, accept = ".Rds"),
               tableOutput("files"),
               tags$hr(),
               h1("Specification"),
               tableOutput("spec")
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

  data <- reactive({
    req(input$upload)
  })

  aggmod <- reactive({
    req(input$upload)
    readRDS(file = input$upload$datapath)
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


