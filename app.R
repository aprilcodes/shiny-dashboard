library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

avocados <- read.csv("C:/ncf-graduate-school/semester-2/stats-II/_final-project/Tidy_Avocado.csv")

ui <- fluidPage(
  theme = shinytheme("journal"),
  
  titlePanel("Avocado Sales: 2015 - 2023"),
  
  tabsetPanel(
    id = "mainTabset",  # Ensure this ID is set for the entire tabsetPanel
    tabPanel("Intro",
             mainPanel(
               div(style = "text-align: center;",
                img(src = "Avocado-tree-text.jpg", width = 900, height = 600, style = "display: block; margin-left: auto; margin-right: auto;")
               )
             )
    ),
    tabPanel("US Trends", value = "source",
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
                 plotlyOutput("bubble_plot")
                 # img(src = "data_dictionary.jpg", width = 800, height = 600)
               )
             )
    ),
    tabPanel("Data Dictionary", value = "dictionary",
             img(src = "data_dictionary.jpg", width = 800, height = 600, style = "display: block; margin-left: auto; margin-right: auto;")
    ),
    tabPanel("Regions Detail", value = "regions",
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
                 plotOutput("lollipop_plot")
               )
             )
    )
  ) # end tabsetPanel
)

server <- function(session, input, output) {
  
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
  
  observe({
    updateSelectInput(session, "myregion", choices = c("US", unique(avocados$Region)))
  })
  
  observeEvent(input$myregion, {
    just_regions <- subset(avocados, Region == input$myregion)
    updateSelectInput(session, "mycountry", choices = c("All", unique(just_regions$Region)))
  })
  
  # cado_colors <- brewer.pal(n = length(unique(filtered_data()$Region)), name = "BrBG")
  
  output$bubble_plot <- renderPlotly({
    plot_ly(data = filtered_data(), 
            type = "scatter",
            mode = "markers",
            x = ~Avg_Prod_Price,
            y = ~Total_Volume,
            color = ~Region,
            size = ~Spec_Volume,
            #marker = list(
            #  size = ~Spec_Volume,
            #  color = ~Region,  # Map color to 'Region'
            colorscale = "Earth",  # Applying the Portland color scale
            #  showscale = TRUE  # Optionally show the color scale bar
            #),
            config = list(displayModeBar = FALSE)
    ) %>%
      layout(
        xaxis = list(range = c(0.20, 3.5)),
        yaxis = list(range = c(200, 11000000)),
        title = list(text="Avocado Sales Per Year"))
  }) 
  
  output$lollipop_plot <- renderPlot({
    ggplot(avocados, aes(x=Year, y=Spec_Volume)) +
      geom_point(aes(color=factor(Year))) + 
      geom_segment(aes(x=Year, xend=Year, y=0, yend=Spec_Volume, color=factor(Year))) + 
      scale_color_brewer(palette="BrBG")
  }) 
}

shinyApp(ui = ui, server = server)