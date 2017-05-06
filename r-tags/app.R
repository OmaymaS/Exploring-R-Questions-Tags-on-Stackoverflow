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
library(DT)

tags <- read_rds("tags.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("yeti"),
  
  # Application title
  titlePanel("R Tags"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("tags","Main Tag",
                  sort(tags$Tag),
                  selectize = T,
                  selected = "ggplot2"),
      uiOutput("range")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(

      # dataTableOutput("tag_dt", height = "500px")
      fluidRow(
      column(width = 12,
               tabsetPanel(
                 tabPanel("Most Frequent Tag-Pairs",
                          uiOutput("pairs_bar_sized")),
                 tabPanel("Tag-Pairs Table",
                          dataTableOutput("tag_dt", height = "500px"))
               )
               )
               
               
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
      unnest %>% 
      select(-N)
  })
  
  
  output$range <- renderUI({
    req(tag_data())
    sliderInput("top", paste0("Number of Top Tags with ", input$tags),
                min = 1,
                max = nrow(tag_data()),
                value = ifelse(nrow(tag_data())>=10, 10, nrow(tag_data())),
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
      ggtitle(paste0("Top ",input$top, " tags with ", input$tags))+
      guides(fill = F)
  })
  
  output$pairs_bar_sized <- renderUI({
    req(input$top)
    plotOutput("pairs_bar",
               height = 100+20*input$top)
  })
  
  
  ## data table  --------------
  output$tag_dt <- renderDataTable({
    DT::datatable(tag_data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

