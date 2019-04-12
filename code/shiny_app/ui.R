
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Tic Tac Toe Battle"),

    mainPanel(
      plotOutput('board', click = "plot_click"),
      verbatimTextOutput("request"),
      checkboxInput("cross", "Cross", FALSE),
      checkboxInput("circle", "Circle", FALSE)
    )

))
