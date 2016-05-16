
# user-interface definition of the
# TextPredictor, a Shiny web application.
# Capstone Project for the Data Science 
# Specialization (Johns Hopkins, Coursera)

library(shiny)

shinyUI(fluidPage(
    titlePanel("TextPredictor App"),
    sidebarLayout(
        sidebarPanel(
            textInput("text_i", label = h3("Enter your text:"), 
                      value = ""),
            submitButton("Predict"),
            br(),
            helpText("Note: The Text Predictor App uses english vocabulary only. Digits, punctuation chars (except contractions like I'm, aren't, ..), non-ASCII chars are not currently managed."),
            br(),
            img(src = "test.jpg", height = 256, width = 467),
            "shiny is a product of ", 
            span("RStudio", style = "color:blue")
        ),
        mainPanel(
            h2("Next Words:"),
            tableOutput("prediction_o")
            
            )
        )
    )
)