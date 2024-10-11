library(tidyr)
library(dbplyr)
library(tidyselect)
library(tidyverse)
library(DT)
library(scales)
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(RSQLite)
library(DBI)
library(mgcv)
library(ggforce)
library(caret)
library(zoo)
library(gt)
library(randomForest)
library(purrr)
library(MAP)
library(cowplot)
library(shiny)

data <- read.csv("C:\\Users\\EvanNg21\\Desktop\\Baseball\\R\\TM24Szn.csv")

StrikeZone <- function(){
  square_coordinates <- data.frame(
    x = c(-0.85, -0.85,0.85,0.85, -0.85),  
    y = c(1.6, 3.5, 3.5, 1.6, 1.6)   
  )
    geom_path(aes(.data$x,.data$y),data=square_coordinates,lwd=1,color = "black",inherit.aes = FALSE) 

           
}

Pitches <- function(df,name,date){
  df%>%
    filter(Pitcher==name,Date==date)%>%
    ggplot()+
    geom_point(aes(x=PlateLocSide,y=PlateLocHeight,fill = TaggedPitchType),size= 4, shape = 21,stroke= 1, color = "black", alpha = 1)+
    StrikeZone()+
    scale_color_manual(values = c("Fastball"= "red", "ChangeUp"="green",
                                  "Slider"="orange", "Curveball"="cyan",
                                  "Cutter"= "yellow", "Sinker"="grey",
                                  "Splitter" = "navy"),name = "Pitch Type")+
    scale_fill_manual(values = c("Fastball"= "red", "ChangeUp"="green",
                                  "Slider"="orange", "Curveball"="cyan",
                                  "Cutter"= "yellow", "Sinker"="grey",
                                  "Splitter" = "navy"),name="Pitch Type")+
    coord_equal()+
    xlim(-3,3)+
    ylim(0,5)+
    labs(
      title = "Total Pitches",
      y = "",
      x = "Pitcher Perspective"
    )
    
}

ui <- fluidPage(
  titlePanel("Baseball Pitch Analysis"),
  
  fluidPage(
      column(6,selectInput("pitcher", "Select Pitcher:",
                  choices = unique(data$Pitcher))),
      column(6,dateInput("date", "Select Date:",
                value = "2024-02-16"))
    ),
    
    mainPanel(
      plotOutput("pitchPlot")
    )
  )


# Define server logic
server <- function(input, output) {
  output$pitchPlot <- renderPlot({
    Pitches(data, input$pitcher, input$date)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


