if (!require("shiny"))
{
  install.packages("shiny")
  library(shiny)
}

setwd("c:/TFM/Code/Shiny")

shinyUI(fluidPage(
  
  titlePanel(title="First principal components behavior"),
  sidebarLayout(
    sidebarPanel(
    sliderInput(inputId = "A",
                label = "A:",
                min = -200, max = 200,
                value = 0,step=10),
    sliderInput(inputId = "PC",
                label = "Principal component",
                min = 1,
                max = 10,
                value = 1,step=1),
    sliderInput(inputId = "Theta",
                label = "Theta angle",
                min = 0,
                max = 360,
                value = 90,step=5),
    sliderInput(inputId = "Phi",
                label = "Phi angle",
                min = 0,
                max = 360,
                value = 0,step=5),
    radioButtons(inputId="Subject", label="Subject", 
                 choices=c("Victor","Ceci"))
  ),
    #sidebarPanel("This is side bar panel"),
    #mainPanel("This is the main panel text, output is displayed here")
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "skeleton")
    ))
  )
)