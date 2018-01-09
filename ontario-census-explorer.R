# Load libraries
library(dplyr)
library(leaflet)
library(ggplot2)
library(shiny)
library(shinythemes)
library(DT)
library(scales)

data <- read.csv("data/ontario-data.csv")
indicator.names = c("Population", "Bachelors degree or higher (%)", "Aboriginal Identity (%)",
                    "Immigrants to Canada (%)", "Median Income", "Unemployment rate (%)")

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  
  # App title ----
  titlePanel("Ontario Census Data Explorer: Selected Indicators by Census Division"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("indicator",
                  label = "Select indicator:",
                  choices = indicator.names),
      
      h4("Source: Statistics Canada, 2016 Census of Population."),
      br(),
      h4("Indicator descriptions:"),
      p("Population: Total reported population, Catalogue no. 98-400-X2016003."),
      p("Immigrants to Canada (%): Immigrants to Canada, as a percentage of the total population, Catalogue no. 98-400-X2016185."),
      p("Aboriginal Identity (%): Total reported Aboriginal identity, as a percentage of the total population, Catalogue no. 98-400-X2016156."),
      p("Bachelors degree or higher (%): Persons aged 25 to 64 with a Bachelors degree or higher, as a percentage of the total population, Catalogue no. 98-400-X2016242."),
      p("Median Income: Median before tax income for all households, Catalogue no. 98-400-X2016099."),
      p("Unemployment rate (%): Unemployment rate for the total population, aged 15 years and over, Catalogue no. 98-404-X2016001"),
      br(),
      p(em("Built with love,", 
           a(href="https://www.r-project.org/", "R", target="_blank"), 
           "and",
           a(href="https://shiny.rstudio.com/", "Shiny", target="_blank"), 
           "by",
           a(href="mailto:mkoroniak@gmail.com", "Mandy Koroniak"),
           "."))
      
    ), # sidebarPanel end
    
    mainPanel(
    
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Map View", br(),
                           h4("Click for more data - you may need to zoom in to click on small circles!"),
                           leafletOutput("mymap")),
                  
                  tabPanel("Ranked View", br(),
                           plotOutput('plot1', height = "750px")),
                  
                  # tabPanel("Correlation", br(),
                  #          plotOutput('plot2')),
                  
                  tabPanel("Data Table", br(), 
                           DT::dataTableOutput("table"))
      )
    ) # mainPanel end
  ) # End sidebarLayout 
) # fluidPage end

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    filtered <- 
      data %>%
      filter(indicator == input$indicator)
    
    leaflet() %>% 
      addTiles() %>%
      addCircles(data=filtered, lng = ~long, lat = ~lat, weight = 1,
                 radius = ~sqrt(value) *80,
                 fillOpacity = 0.1,
                 popup = paste(data$place, "<br>", input$indicator, ":", filtered$value)
                 )
  })
  
  output$plot1 <- renderPlot({
    
    filtered <- 
      data %>%
      filter(indicator == input$indicator) 
    
    filtered <- 
      filtered[order(filtered$value), ]
    
    filtered$place <- factor(filtered$place, levels = filtered$place) 

    ggplot(filtered, 
           aes(x=place, y=value)) + 
      geom_bar(stat="identity", width=0.8, fill="darkolivegreen3") + 
      coord_flip() +
      labs(x = "",
           y = "") +
      scale_y_continuous(labels = comma, sec.axis = dup_axis()) +
      theme(axis.text.x = element_text(face = "bold", size = 14),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "italic", size = 16),
            legend.title = element_text(face = "bold", size = 16),
            legend.text = element_text(size = 14),
            plot.title = element_text(face = "bold", size = 18),
            panel.background = element_rect(fill = 'white'),
            panel.grid.major = element_line(colour = "lightgrey")
            )

  })
  
  
  output$table = DT::renderDataTable({
    filtered <- 
      data %>%
      select(place, indicator, value) %>%
      filter(indicator == input$indicator)
  })
  
}

shinyApp(ui, server)