library(plotly)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(viridis)
library("colorspace")

avocados <- read.csv("C:/ncf-graduate-school/semester-2/stats-II/_final-project/Tidy_Avocado.csv")
avocados_summary <- read.csv("C:/ncf-graduate-school/semester-2/data-visualizations/_final_project_avocados/avocados/avocados_2020_summary.csv")

avocados <- avocados %>% filter(Region %in% c("California","West","Plains","Southeast","GreatLakes","Midsouth","Northeast", "SouthCentral")) %>% 
  filter(!grepl("bag", Packaging, ignore.case = TRUE))

ui <- fluidPage(
  theme = shinytheme("journal"),
  
  titlePanel("US Avocado Sales: 2015 - 2023"),
  
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
                 #selectInput(
                #   inputId = "mycountry",
                #   label = "Which country?",
                #   choices = NULL 
                # )
               ),
               mainPanel(
                 plotlyOutput("bubble_plot")
               )
             )
    ),
    tabPanel("Regional Trends", value = "year",
             sidebarLayout(
               sidebarPanel(
                 switchInput(
                   inputId = "organic_switch",
                   label = "What kind of avocados?",
                   value = FALSE,
                   onLabel = "Conventional",
                   offLabel = "Organic"
                 ),
                 selectInput(
                   inputId = "regional_region",
                   label = "Region?",
                   choices = NULL 
                 ),
                 selectInput(
                   inputId = "regional_packaging",
                   label = "Packaging Type?",
                   choices = NULL 
                 ),
               ),
               mainPanel(
                 plotOutput("lollipop_plot")
               )
             )
    ),
    tabPanel("Data Dictionary", value = "dictionary",
             img(src = "data_dictionary.jpg", width = 800, height = 600, style = "display: block; margin-left: auto; margin-right: auto;")
    )
  ) # end tabsetPanel
)

server <- function(session, input, output) {
  
  filtered_data <- reactive({
    avocado_subset <- subset(avocados, Year == input$year)
    if (input$myregion != "US") {
      avocado_subset <- subset(avocado_subset, Region == input$myregion)
    }
    
    #if (input$mycountry != "All") {
    #  avocado_subset <- subset(avocado_subset, Region == input$myregion)
    #}
    return(avocado_subset)
  })
  
  lollipop_filter <- reactive({
    lollipop_subset <- avocados_summary # changed from avocados to avocados_summary
    if (input$regional_region != "All") {
      lollipop_subset <- subset(lollipop_subset, Region == input$regional_region)
    }
    
    if (input$organic_switch == FALSE) {
      lollipop_subset <- subset(lollipop_subset, Produce_Type == "organic")
    } else {
      lollipop_subset <- subset(lollipop_subset, Produce_Type == "conventional")
    }
    
    if (input$regional_packaging != "All") {
      lollipop_subset <- subset(lollipop_subset, Packaging == input$regional_packaging)
    }
    
    return(lollipop_subset)
  })
  
  observe({
    updateSelectInput(session, "myregion", choices = c("US", unique(avocados$Region)))
  })
  
  observe({
    updateSelectInput(session, "regional_region", choices = c("All", unique(avocados$Region)))
  })
  
  observe({
    updateSelectInput(session, "regional_packaging", choices = c("All", unique(avocados$Packaging)))
  })
  
  #observeEvent(input$myregion, {
  #  just_regions <- subset(avocados, Region == input$myregion)
  #  updateSelectInput(session, "mycountry", choices = c("All", unique(just_regions$Region)))
  #})
  
  # cado_colors <- brewer.pal(n = length(unique(filtered_data()$Region)), name = "BrBG")
  
  
  
  output$bubble_plot <- renderPlotly({
    data = filtered_data()
    data$Region_numeric <- as.numeric(factor(data$Region))
    
    plot_ly(data, 
            type = "scatter",
            mode = "markers",
            x = ~Avg_Prod_Price,
            y = ~Total_Volume,
            # color = ~Region,
            size = ~Spec_Volume,
            #text = ~hover_text,
            #marker = list(
            #  size = ~Spec_Volume,
            color = ~Region_numeric,  # Map color to 'Region'
            # colorscale = viridis_scale,  # does viridis need to be called as library()?
            #  showscale = TRUE  # Optionally show the color scale bar
            #),
            config = list(displayModeBar = FALSE)
    ) %>%
      layout(
        xaxis = list(range = c(0.20, 3.5)),
        yaxis = list(range = c(200, 11000000)),
        title = list(text="Avocado Sales Per Region Over Time"))
  }) 
  
  output$lollipop_plot <- renderPlot({
    lollipop_data <- lollipop_filter()
    lollipop_data$Month <- factor(lollipop_data$Month, levels = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)
    pal <- sequential_hcl(12, "ag_GrnYl")
    
    ggplot(lollipop_data, aes(x=Month, y=Average_Spec_Volume)) +
      geom_point(aes(color=factor(Month)), size = 3) + 
      geom_segment(aes(x=Month, xend=Month, y=0, yend=Average_Spec_Volume, color=factor(Month)), size = 1) + 
      scale_color_manual(values = pal) + labs(color = "Month", y = "Total Sales Volume (Lbs)", title = "Year 2020 Avocado Sales")
  }) 
}

shinyApp(ui = ui, server = server)