
# server behaviour of the
# TextPredictor, a Shiny web application.
# Capstone Project for the Data Science 
# Specialization (Johns Hopkins, Coursera)

library(shiny)


shinyServer(function(input, output) {

    prediction.f <- function(text){
        
        result <- NULL
            
        if(text != "") {
            words <- unlist(strsplit(text, " "))
            words.n <- length(words)
            bigram <- paste(words[words.n - 1], words[words.n])
            tmp.3g <- trigrams.findNextWord(bigram, d.3g)
            tmp.2g <- bigrams.findNextWord(words[words.n], d.2g)
            tmp.1g <- unigrams.mostProbableWords(d.1g)
            
            result <- rbind(tmp.3g, tmp.2g)
            result <- rbind(result, tmp.1g)
            
        }
        result
        
    }
    
    output$prediction_o <- renderTable({
        prediction.f(input$text_i)
    })
})
