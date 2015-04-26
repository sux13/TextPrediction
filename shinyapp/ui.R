
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
    fluidPage(
        includeScript("http://twitter.github.io/typeahead.js/releases/latest/typeahead.bundle.js"),
        includeScript("main.js"),
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
                br(), br(),
                h1("Type your phrase here ... "), br(),br(),
                p(textInput(inputId = "text",label = ""),
                textOutput("autocomplete", inline = TRUE),
                    textOutput("prediction", inline = TRUE)),
                br(), em(strong("Note: "), span("orange", style = "color: #d16527;"), " indicates the autocomplete hints, ", span("blue", style = "color: #176de3;"), " indicates the predicted word", style = "color: #AAA"),
                br(), br(),checkboxInput("details", "Details", FALSE),
                conditionalPanel(
                    # style = "border: 1px solid #CCC;",
                    condition = "input.details == true",
                    wellPanel(
                    p(strong("Tokenized Phrase: "), textOutput("parsed", inline = TRUE)),
                    numericInput(inputId = "numPred", label = "Number of Options Predicted:", min = 1, max = 5,step = 1,value = 1))
#         )
        )
    )
)))
