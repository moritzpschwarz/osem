library(shiny)
library(tidyverse)
library(DT)

# Define the `%notin%` operator
`%notin%` <- Negate(`%in%`)

# Pre-load datasets
codes <- read_csv("eurostat_availability_codes.csv", guess_max = 10000000)
labels <- read_csv("eurostat_availability_labels.csv", guess_max = 10000000)

# Prepare columns for search and intermediate dataset
cols_to_search <- codes %>%
  select(-c(freq, geo, n, min_time, max_time, dataset_id)) %>%
  names()

intermed <- bind_cols(
  codes,
  labels %>%
    select(all_of(cols_to_search)) %>%
    rename_with(~paste0("label_", .x), .cols = everything())
)

# Relocate label columns next to original columns
for (i in seq_along(cols_to_search)) {
  intermed <- intermed %>%
    relocate(paste0("label_", cols_to_search[i]), .after = cols_to_search[i])
}

# Define UI
ui <- fluidPage(
  titlePanel("Eurostat Data Search Tool"),

  sidebarLayout(
    sidebarPanel(
      textInput("search_term", "Search Term", value = "", placeholder = "Enter a keyword..."),
      selectInput("geo_filter", "Select Country (geo)",
                  choices = c("All" = "All", setNames(unique(intermed$geo), unique(intermed$geo))),
                  multiple = TRUE),
      selectInput("freq_filter", "Select Frequency (freq)",
                  choices = c("All" = "All", setNames(unique(intermed$freq), unique(intermed$freq))),
                  multiple = TRUE),
      numericInput("min_obs", "Minimum Observations (n)", value = 0, min = 0),
      actionButton("apply_filters", "Apply Filters")
    ),

    mainPanel(
      DTOutput("search_results")
    )
  )
)

# Define Server
server <- function(input, output, session) {

  # Reactive filtering
  filtered_data <- reactive({
    data <- intermed

    # Filter by geo
    if (!is.null(input$geo_filter) && "All" %notin% input$geo_filter) {
      data <- data %>% filter(geo %in% input$geo_filter)
    }

    # Filter by freq
    if (!is.null(input$freq_filter) && "All" %notin% input$freq_filter) {
      data <- data %>% filter(freq %in% input$freq_filter)
    }

    # Filter by n
    if (!is.null(input$min_obs)) {
      data <- data %>% filter(n >= input$min_obs)
    }

    # Search term filter
    if (!is.null(input$search_term) && input$search_term != "") {
      data <- data %>%
        filter_all(any_vars(grepl(input$search_term, x = ., ignore.case = TRUE)))
    }

    data %>%
      janitor::remove_empty("cols")
  })

  # Render search results
  output$search_results <- renderDT({
    req(filtered_data())
    # Select only labeled columns for display
    search_results <- filtered_data() %>%
      select(n, min_time, max_time, any_of("dataset_id"), starts_with("label_")) %>%
      rename_with(.cols = everything(), ~gsub("label_", "", .x))
    datatable(search_results, options = list(pageLength = 100), rownames = FALSE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
