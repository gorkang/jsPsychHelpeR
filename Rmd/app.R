library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(tidyr)


DF_analysis = data.table::fread(here::here("outputs/data/DF_analysis.csv"))
                              

# targets::tar_load(DF_analysis)

names_variables = names(DF_analysis)[-1]


# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(""),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,
                 
                 selectInput(inputId = "variable", 
                             label = "Name variable:",
                             choices = names_variables, multiple = TRUE,
                             size = 20, 
                             selectize = FALSE,
                             selected = names_variables[1]),
                 
                 textInput(inputId = "bins", label = "Bins histogram", value = "30"),
                 
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 10,
              
              # Output: Tabset w/ plot, summary, and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("Plot", plotOutput("plot", height = "800px")),
                          tabPanel("Summary", dataTableOutput("summary")),
                          tabPanel("Table", dataTableOutput("table"))
              )
              
    )
  )
)

server <- function(input, output) {
  
  d <- reactive({
    DF_analysis %>% 
     dplyr::select(id, input$variable) %>%
      tidyr::pivot_longer(2:ncol(.))
  })
  
  d_table <- reactive({
    DF_analysis %>% 
     dplyr::select(id, input$variable)
  })
  
  output$plot <- renderPlot({
    d() %>% 
      
      ggplot2::ggplot(ggplot2::aes(value, name, fill = name)) + 
      ggridges::geom_density_ridges(stat = "binline", bins = input$bins, scale = 0.95, draw_baseline = FALSE, show.legend = FALSE) +
      ggridges::geom_density_ridges(jittered_points = TRUE, position = "raincloud", alpha = 0.7, scale = 0.9, show.legend = FALSE) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme_minimal()
    
    # ggplot2::ggplot(aes_string(input$variable)) + 
    # ggplot2::geom_histogram(bins = input$bins) +
    # ggplot2::theme_minimal()
  })
  
  # Generate a summary of the data ----
  output$summary <- renderDataTable({
    skimr::skim(d_table() %>% dplyr::select(-id))
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderDataTable({
    d_table()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
