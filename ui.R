
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
    fluidPage(
    theme = "main.css",

    # Application title
#     titlePanel("Text Prediction Project"),

    # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             textInput(inputId = "test",label = "test:", value = "type your phrase here"),
#         ),


        fluidRow(
            column(6, offset = 3,
                h1("Type your phrase here ... "),
                textInput(inputId = "text",label = ""),
                textOutput("prediction"),
                textOutput("autocomplete"),
                numericInput(inputId = "numPred", label = "Number of Options Predicted", min = 1, max = 10,step = 1,value = 3),
                style = "margin-top: 10%; text-align:center;"
#         )
        )
    )
))
