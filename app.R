# April Ainsworth
# Shiny dashboard final project: avocado sales

library(plotly)
library(shiny)
library(dplyr)

avocados <- read.csv("C:/ncf-graduate-school/semester-2/stats-II/final-project/Tidy_Avocado.csv")

ui <- fluidPage(
  
  titlePanel("Avocado Sales Over Time By Region"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Which year:",
                  min = 2015,
                  max = 2023,
                  value = 2015,
                  sep = "",
                  animate = animationOptions(loop = FALSE, playButton = "Play")
      ),
      
      selectInput(
        inputId = "myregion",
        label = "Which region?",
        choices = NULL 
      ),
      
      selectInput(
        inputId = "mycountry",
        label = "Which country?",
        choices = NULL 
      )
    ),
    
    
    
    mainPanel(
      #plotlyOutput("bubble_plot"),
      tabsetPanel(
        id = "tabset",
        tabPanel("Dataset Source", value = "source", plotlyOutput("bubble_plot"), selected = TRUE),
        # tabPanel("Dataset Source", value = "source"),
        tabPanel("Data Dictionary", value = "dictionary", img(src = "data_dictionary.jpg", width = 800, height = 600)),
        tabPanel("Regions Detail", value = "regions")
      )
    )
  ) # end sidebarLayout
)

server <- function(session, input, output) {
  
  #output$panel <- renderText({
  #  paste("Current panel: ", input$tabset)
  #})
  
  filtered_data <- reactive({
    avocado_subset <- subset(avocados, Year == input$year)
    if (input$myregion != "US") {
      avocado_subset <- subset(avocado_subset, Region == input$myregion)
    }
    
    if (input$mycountry != "All") {
      avocado_subset <- subset(avocado_subset, Region == input$myregion)
    }
    return(avocado_subset)
  })
  
  # Update select input choices using observeEvent()
  observe({
    updateSelectInput(session, "myregion", choices = c("US", unique(avocados$Region)))
  })
  
  observeEvent(input$myregion, {
    just_regions <- subset(avocados, Region == input$myregion)
    updateSelectInput(session, "mycountry", choices = c("All", unique(just_regions$Region)))
  })
  
  output$bubble_plot <- renderPlotly({
    
    plot_ly(data = filtered_data(), 
            type = "scatter",
            mode = "markers",
            x = ~Avg_Prod_Price,
            y = ~Total_Volume,
            color = ~Region,
            size = ~Spec_Volume,
            # hovertext = ~Packaging,
            config = list(displayModeBar = FALSE)
    ) %>%
      layout(
        xaxis = list(range = c(0.20, 3.5)),
        yaxis = list(range = c(200, 11000000)),
        title = list(text="Avocado Sales Per Year"))
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
