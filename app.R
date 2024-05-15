library(plotly)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(viridis)
library(colorspace)
library(DT)

avocados <- read.csv("Tidy_Avocado.csv")
avocados_summary <- read.csv("avocados_2020_summary.csv")

avocados <- avocados %>% filter(Region %in% c("California","West","Plains","Southeast","GreatLakes","Midsouth","Northeast", "SouthCentral")) %>% 
  filter(!grepl("bag", Packaging, ignore.case = TRUE))

ui <- fluidPage(
  theme = shinytheme("journal"),
  
  titlePanel("US Hass Avocado Sales: 2015 - 2023"),
  
  tabsetPanel(
    id = "mainTabset",
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
                 checkboxInput("show_price", "Show Average Product Price", value = FALSE),  # Add toggle input
                 conditionalPanel(
                   condition = "input.show_price == true",  # Show text when checkbox is checked
                   p("( * = Highest Gross Revenue)", style = "color: red;")
                 ),
                 checkboxInput("show_freight", "Show Freight Calculator", value = FALSE),  # Add freight calculator toggle
                 conditionalPanel(
                   condition = "input.show_freight == true",
                   numericInput("distance", "Distance To Market (miles):", value = 0, min = 0),
                   numericInput("freight_cost", "Freight Cost Per Mile:", value = 0.67, min = 0),
                   # p("Round-Trip Freight Cost:"),
                   verbatimTextOutput("break_even")
                 )
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
                 )
               ),
               mainPanel(
                 plotOutput("lollipop_plot")
               )
             )
    ),
    tabPanel("Full Dataset", value = "dictionary",
             DTOutput("data_table")  # Update to show a data table
    )
  )
)

server <- function(session, input, output) {
  
  filtered_data <- reactive({
    avocado_subset <- avocados %>% filter(Year == input$year)
    
    avocado_summary <- avocado_subset %>%
      group_by(Region) %>%
      summarize(
        Summarized_Volume = sum(Spec_Volume, na.rm = TRUE),
        Mean_Avg_Prod_Price = mean(Avg_Prod_Price, na.rm = TRUE)  # Calculate mean price
      ) %>%
      ungroup()
    
    avocado_summary <- avocado_summary %>%
      mutate(Prod_Price_Volume = Summarized_Volume * Mean_Avg_Prod_Price)  # Calculate product
    
    max_prod_price_volume <- max(avocado_summary$Prod_Price_Volume, na.rm = TRUE)  # Identify max product
    
    avocado_summary <- avocado_summary %>%
      mutate(Price_Label = ifelse(Prod_Price_Volume == max_prod_price_volume, 
                                  paste("$", round(Mean_Avg_Prod_Price, 2), "*"),  # Add asterisk for max product
                                  paste("$", round(Mean_Avg_Prod_Price, 2))))
    
    return(avocado_summary)
  })
  
  lollipop_filter <- reactive({
    lollipop_subset <- avocados_summary
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
    updateSelectInput(session, "regional_region", choices = c("All", unique(avocados$Region)))
  })
  
  observe({
    updateSelectInput(session, "regional_packaging", choices = c("All", unique(avocados$Packaging)))
  })
  
  output$bubble_plot <- renderPlotly({
    data = filtered_data()
    data$Region_numeric <- as.factor(data$Region)
    
    p <- plot_ly(data, 
                 type = "bar",
                 x = ~Region,
                 y = ~Summarized_Volume,
                 color = ~Region_numeric,
                 colors = viridis_pal(option = "D", direction = 1)(length(unique(data$Region))),
                 text = ~paste("Total Volume: ", round(Summarized_Volume, 2)),
                 marker = list(
                   showscale = FALSE,
                   colorbar = list(title = "Region")
                 ),
                 config = list(displayModeBar = FALSE)
    ) %>%
      layout(
        xaxis = list(title = "U.S. Region"),
        yaxis = list(title = "Sales Volume In Lbs"),
        title = list(text = "Avocado Sales Per Region Over Time")
      )
    
    if (input$show_price) {
      p <- p %>%
        add_annotations(
          text = ~Price_Label,  # show asterisk if applicable
          x = ~Region,
          y = ~Summarized_Volume,
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
    }
    
    p
  })
  
  output$lollipop_plot <- renderPlot({
    lollipop_data <- lollipop_filter()
    
    # Filter to keep only the maximum value for each Month
    top_lollipop_data <- lollipop_data %>%
      group_by(Month) %>%
      top_n(1, wt = Average_Spec_Volume) %>%
      ungroup()
    
    top_lollipop_data$Month <- factor(top_lollipop_data$Month, levels = rev(1:12), labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), ordered = TRUE)
    pal <- sequential_hcl(12, "ag_GrnYl")
    
    ggplot(top_lollipop_data, aes(x = Month, y = Average_Spec_Volume)) +
      geom_point(aes(color = factor(Month)), size = 3) + 
      geom_segment(aes(x = Month, xend = Month, y = 0, yend = Average_Spec_Volume, color = factor(Month)), size = 1) + 
      scale_color_manual(values = pal) + 
      labs(color = "Month", y = "Total Sales Volume (Lbs)", title = "Year 2020 Avocado Sales") + coord_flip() + theme(legend.position = "None")
  }) 

  output$data_table <- renderDT({
    datatable(avocados, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$break_even <- renderText({
    req(input$distance, input$freight_cost)  # Ensure all inputs are provided
    break_even_point <- (input$distance * input$freight_cost * 2)
    paste("Round-Trip Freight Cost:", round(break_even_point, 2))
  })
}

shinyApp(ui = ui, server = server)
