#########################################################################################################
# Ahmad Niaz                                                                                            #
# First Shiny App                                                                                       #
# Dataset : https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs?resource=download   #
#########################################################################################################


library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)


billboard100 <- read.csv("charts.csv")

ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  "Hot 100",
                  tabPanel(
                    "Top Songs by Artist",
                    sidebarPanel(
                      tags$h3("Input:"),
                      textInput("txt1", "Artist", ""),
                      actionButton("btn1", 
                                   "Submit", 
                                   class = "btn btn-primary")
                    ),
                    mainPanel(
                      tags$label(h3('Output')), # Status/Output Text Box
                      verbatimTextOutput('contents1'),
                      tableOutput('tabledata')
                    )
                  ),
                  tabPanel("Song Progression",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt2", "Song", ""),
                             actionButton("btn2", 
                                          "Submit", 
                                          class = "btn btn-primary")
                           ),
                           mainPanel(
                             tags$label(h3('Output')), # Status/Output Text Box
                             verbatimTextOutput('contents2'),
                             plotOutput("chart")
                           )
                  ),
                  
                  
                  
                )
                
                
                
                
)

server <- function(input, output, session) {
  
  
  
  songs <-reactive({
    billboard100 %>% 
      select(artist, song, "peak.rank") %>% 
      filter(tolower(artist) == tolower(!! input$txt1) ) %>% 
      distinct() %>% 
      arrange(peak.rank, song)
  })
  
  
  
  output$tabledata <-renderTable({
    if (input$btn1>0) { 
      isolate(songs()) 
    } 
  })
  
  
  filtered_data <- reactive({
    billboard100 %>% 
      filter(tolower(song) == tolower(!! input$txt2))
  })
  
  output$chart <- renderPlot({
    if(input$btn2>0){
      req(input$txt2)
      ggplot(filtered_data(),aes(x = date,y = rank, group=song, color=song))+
        geom_line()+
        labs(title = paste("Billboard Chart Progression for",input$txt2), x="Date", y="Rank")+
        theme_minimal()
    }
  })
  
}


shinyApp(ui, server)
