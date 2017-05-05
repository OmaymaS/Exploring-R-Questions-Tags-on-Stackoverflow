#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)

tags <- read_rds("tags.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("R Tags"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tags","R Tags",
                  sort(tags$Tag),
                  selectize = T,
                  selected = "ggplot2"),
      uiOutput("range")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(
        column(width = 12,
               tabsetPanel(
                 tabPanel("Most Frequent Tags",
                          uiOutput("pairs_bar_sized")),
                 tabPanel("Tags Table")
               ))
      )
      
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  
  ## filter data based on inputs -------------------------
  tag_data <- reactive({
    tags %>% 
      filter(Tag == input$tags) %>% 
      unnest 
  })
  
  
  output$range <- renderUI({
    req(tag_data())
    sliderInput("top", "Most",
                min = 1,
                max = nrow(tag_data()),
                value = 10,
                step = 1)
  })
  
  ## bar plot of most frequent pairs ----------------------
  output$pairs_bar <- renderPlot({
    req(tag_data(), input$top)
    
    tag_data() %>% 
      .[1:input$top, ] %>% 
      ggplot(aes(x = reorder(T2,pair_count),
                 y = pair_count,
                 fill = -pair_count))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = pair_count), hjust = -0.25)+
      coord_flip()+
      theme_classic()+
      theme(text = element_text(size = 16))+
      xlab("Tag")+
      ylab("Count")+
      ggtitle(paste0("Top",input$top, " tags with ", input$tags))+
      guides(fill = F)
  })
  
  output$pairs_bar_sized <- renderUI({
    req(input$top)
    plotOutput("pairs_bar",
               height = 100+20*input$top)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

