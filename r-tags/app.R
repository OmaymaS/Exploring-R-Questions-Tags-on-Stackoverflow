library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinythemes)
library(DT)

# load data ----------------------------
tags <- read_rds("tags.rds")

# Define UI ----------------------------
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # Application title
                titlePanel("R Questions Tag Pairs on Stackoverflow"),
                
                # Sidebar ----------------------------------
                sidebarLayout(
                  sidebarPanel(
                    ## list of tags --------------------------------
                    selectInput("tags","Main Tag",
                                sort(tags$Tag),
                                selectize = T,
                                selected = "ggplot2"),
                    
                    ## slider input for the number of top tags to plot ------------------------
                    uiOutput("range"),
                    
                    ## help text about the dataset------------------
                    helpText(HTML("<p><a href='https://www.kaggle.com/stackoverflow/rquestions'>Data Source</a><br> Dataset released by Stackoverflow on Kaggle, including questions about R posted till 19 October 2016.<br>
License: <a href = 'https://creativecommons.org/licenses/by-sa/3.0/' >CC-BY-SA 3.0</a>
                                  </p>"))
                    
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    fluidRow(
                      column(width = 12,
                             tabsetPanel(
                               ## bar plot of top tags 
                               tabPanel("Most Frequent Tag-Pairs",
                                        uiOutput("pairs_bar_sized")),
                               ## datatable 
                               tabPanel("Tag-Pairs Table",
                                        dataTableOutput("tag_dt", height = "500px")))
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
      unnest
  })
  
  ## slider to select number of tags to plot --------------
  output$range <- renderUI({
    req(tag_data())
    sliderInput("top", paste0("Number of Top Tags to Plot with ", input$tags),
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
      ylab("Number of Questions")+
      ggtitle(paste0("Top ",input$top, " tags with ", input$tags))+
      guides(fill = F)
  })
  
  ## renderUI to set variable plot height --------------
  output$pairs_bar_sized <- renderUI({
    req(input$top)
    plotOutput("pairs_bar",
               height = 100+20*input$top)
  })
  
  ## data table of tag pairs --------------
  output$tag_dt <- renderDataTable({
    DT::datatable(tag_data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

