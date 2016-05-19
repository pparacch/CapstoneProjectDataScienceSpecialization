
# user-interface definition of the
# TextPredictor, a Shiny web application.
# Capstone Project for the Data Science 
# Specialization (Johns Hopkins, Coursera)

library(shiny)

shinyUI(fluidPage(theme = "bootstrap.min.css",
    titlePanel("TextPredictor App"),
    sidebarLayout(
        sidebarPanel(
            h4("Enter your text:"),
            tags$textarea(id="text_i", rows=3, cols=40, label = h3("Enter your text:")),
            actionButton("predict", "Predict"), 
            br(),
            helpText("Note: The Text Predictor App uses english vocabulary only. Digits, punctuation chars (except contractions like I'm, aren't, ..), non-ASCII chars are not currently managed."),
            br(),
            img(src = "wordcloud.png", width = 250),
            br(),
            br(),
            span("'Data Science Specialization' Capstone Project", style = "color:blue"),
            "empowered by", 
            span("Johns Hopkins University & SwiftKey", style = "color:blue") 
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("App Info"),
                tabPanel("Next Words",
                         br(),
                         textOutput("text_o"),
                         br(),
                         uiOutput("possibleWords"),
                         uiOutput("addNextWord")),
                tabPanel("Next Words: details", 
                         h2("Examining the Language Model:"),
                         br(),
                         p("Here it is possible to see from where the next words come from - specifically trigrams, bigrams and unigrams. Trigrams take precedence over bigrams, and bigrams take precedence over unigrams when building up the list of the 5 next possible words."),
                         br(),
                         h3("... based on Trigrams:"),
                         p("If trigrams are available, the table shows the possible next words ordered by Stupid Backoff score (log base 2) in decreasing order."),
                         p(),
                         div(dataTableOutput("predictionTrigramsAll_o"), style = "font-size:90%"), 
                         br(),
                         h3("... based on Bigrams:"),
                         p("If bigrams are available, the table shows the possible next words ordered by Stupid Backoff score (log base 2) in decreasing order."),
                         p(),
                         div(dataTableOutput("predictionBigramsAll_o"), style = "font-size:90%"),
                         br(),
                         h3("... based on Most Frequent Unigrams:"),
                         p("The table shows the more probable (15) unigrams ordered by Stupid Backoff score (log base 2) in decreasing order. These unigrams are going to be suggested as possible next words when there are no/ or limited selection of trigrams and bigrams (e.g. in the case of an unknown word)."),
                         p(),
                         div(dataTableOutput("predictionUnigramsTop_o"), style = "font-size:90%")),
                tabPanel("Next Words: wordclouds",
                         br(),
                         p("A visual representation of the bigrams and trigrams supporting the suggested next words using wordclouds."),
                         #plotOutput("bigramWordcloud"),
                         #plotOutput("trigramWordcloud"))
                         fluidRow(column(4, plotOutput("trigramWordcloud")),
                                  column(8, plotOutput("bigramWordcloud"))
                         ))
            )
            
        )
    )
))