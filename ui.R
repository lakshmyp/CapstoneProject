library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Text Predictor"),
  helpText("This app takes in any words you type in and predicts the next possible word in that sentence similar to the SwiftKey App on your phone. Try it out now!"),

  
  fluidRow(
    column(3,
           h3("Input Your Text"),
           br(),
           textInput('text1', 'Enter a phrase', value=""),
           tags$head(tags$style(type='text/css', "#text1 { width: 800px }" )),

           br(),
           actionButton("goButton", "PREDICT!"),
           br(),
           br()
    )

  ),
  
  h3("The next predicted word is ... "),
  textOutput("oid1"), 
  br(),
  
  h4("Other possible words... "),
  textOutput("oid2"),
  br(),
  br(),
  br(),
  br(),
  
  p("The full code is available at", 
    a("My GitHub", 
      href="https://github.com/lakshmyp/")),
  p("The pitch for this app is located at",
    a("My RPubs profile",
      href = "http://rpubs.com/lakshmyp")),
  p("Created by Lakshmy Priya, 24 April 2016")

))