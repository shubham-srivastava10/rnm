#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("R&M report"),       
    sidebarLayout(
      sidebarPanel(
        downloadButton("downloadData", "Download"),
        uiOutput("RGInput"),
        uiOutput("CHInput"),
        uiOutput("SHInput"),
        uiOutput("GMInput"),
        uiOutput("ESMInput"),
        uiOutput("propInput"),
        uiOutput("Date"),
        uiOutput("levelInput")
      ),
      mainPanel(
        splitLayout(
          plotOutput("statePlot"),
          plotOutput("pie")
        ),
        dataTableOutput("ticket")
      )
    )
  ))
