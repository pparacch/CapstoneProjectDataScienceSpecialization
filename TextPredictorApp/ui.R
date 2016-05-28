
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
            br() 
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("App Info",
                         br(),
                         HTML("<p>I am <a href='https://www.linkedin.com/in/pierlorenzoparacchini'>Pier Lorenzo Paracchini</a> currently working as a software developer/ agile coach with a <b>new passion for data science</b>. A challenging and exciting <b>passion</b> where I would like to invest more of my time and grow professionally.</p>"),
                         HTML("<p>The application has been developed as part of the <span style = 'color:red'>Data Science Specialization' Capstone Project</span> empowered by <span style = 'color:red'>Johns Hopkins University & SwiftKey</span>."),
                         br(),
                         h4("Usage"),
                         HTML("<ul>"),
                         HTML("<li>Enter the <b>text used for prediction</b> in the <b>text area</b></li>"),
                         HTML("<li>Click the <b>\"Predict\"</b> button</li>"),
                         HTML("<li>Enter the <b>other tabs</b> to see predicted word and more</li>"),
                         HTML("</ul>"),
                         h5("\"Next Words\" tab"),
                         p("Enter this tab to see the 5 next words that are made as prediction, based on the entered text, through a set of radio buttons. Next words are listed in decreasing probability order. Note! '</s>' is a special tag used to mark the end of a sentence."),
                         p("The next words are selected using the following priorities: trigrams, bigrams and unigrams (Stupid Backoff Model)."),
                         HTML("<ul>"),
                         HTML("<li>Select the <b>next word</b> to be added</li>"),
                         HTML("<li>Click the <b>\"Add Selected Word...\"</b> button</li>"),
                         HTML("<li>The <b>selected word</b> is added to the entered text</li>"),
                         HTML("<li>Click the <b>\"Predict\"</b> button to predict the next word</li>"),
                         HTML("<li>Go on and have <b>FUN</b></li>"),
                         HTML("</ul>"),
                         h5("\"Next Words: details\" tab"),
                         p("Enter this tab to see more info abot the selected words and the language model. Trigrams, bigrams and unigrams and their (Stupid Backoff) scores are accessible as tables."),
                         h5("\"Next Words: details\" tab"),
                         p("Enter this tab to see the predicted next words, based on relevant trigrams and bigrams, as wordclouds.")
                ),
                
                tabPanel("Next Words",
                         br(),
                         textOutput("text_o"),
                         br(),
                         uiOutput("possibleWords"),
                         uiOutput("addNextWord")),
                
                tabPanel("Next Words: details", 
                         h4("Examining the Language Model:"),
                         br(),
                         p("Here it is possible to see from where the next words come from - specifically trigrams, bigrams and unigrams. Trigrams take precedence over bigrams, and bigrams take precedence over unigrams when building up the list of the 5 next possible words."),
                         br(),
                         h5("... based on Trigrams:"),
                         p("If trigrams are available, the table shows the possible next words ordered by Stupid Backoff score (log base 2) in decreasing order."),
                         p(),
                         div(dataTableOutput("predictionTrigramsAll_o"), style = "font-size:90%"), 
                         br(),
                         h5("... based on Bigrams:"),
                         p("If bigrams are available, the table shows the possible next words ordered by Stupid Backoff score (log base 2) in decreasing order."),
                         p(),
                         div(dataTableOutput("predictionBigramsAll_o"), style = "font-size:90%"),
                         br(),
                         h5("... based on Most Frequent Unigrams:"),
                         p("The table shows the more probable (15) unigrams ordered by Stupid Backoff score (log base 2) in decreasing order. These unigrams are going to be suggested as possible next words when there are no/ or limited selection of trigrams and bigrams (e.g. in the case of an unknown word)."),
                         p(),
                         div(dataTableOutput("predictionUnigramsTop_o"), style = "font-size:90%")),
                
                tabPanel("Next Words: wordclouds",
                         br(),
                         p("A visual representation of the bigrams and trigrams supporting the suggested next words using wordclouds."),
                         fluidRow(column(4, plotOutput("trigramWordcloud")),
                                  column(8, plotOutput("bigramWordcloud"))
                         ))
            )
            
        )
    )
))