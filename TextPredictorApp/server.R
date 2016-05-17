
# server behaviour of the
# TextPredictor, a Shiny web application.
# Capstone Project for the Data Science 
# Specialization (Johns Hopkins, Coursera)

library(shiny)


shinyServer(function(input, output) {
    
    current.text <- reactive({ input$text_i })
    
    next.words <- reactive({prediction.f(current.text())})
    next.trigrams.all <- reactive({getallTrigrams(current.text())})
    next.bigrams.all <- reactive({getallBigrams(current.text())})
    next.unigrams.top <- reactive({gettopUnigrams(current.text())})
    
    output$text_o <- renderText({ 
        current.text()
    })
    
    output$predictionTrigramsAll_o <- renderDataTable({
        next.trigrams.all()
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    output$predictionBigramsAll_o <- renderDataTable({
        next.bigrams.all()
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    output$predictionUnigramsTop_o <- renderDataTable({
        next.unigrams.top()
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    output$word1 <- renderUI({
        word <- next.words()$next.word[1]
        if(!is.null(word)){
            actionButton("word1", word)    
        }
    })
    
    output$word2 <- renderUI({
        word <- next.words()$next.word[2]
        if(!is.null(word)){
            actionButton("word1", word)    
        }
    })

    output$word3 <- renderUI({
        word <- next.words()$next.word[3]
        if(!is.null(word)){
            actionButton("word1", word)    
        }
    })

    output$word4 <- renderUI({
        word <- next.words()$next.word[4]
        if(!is.null(word)){
            actionButton("word1", word)    
        }
    })

    output$word5 <- renderUI({
        word <- next.words()$next.word[5]
        if(!is.null(word)){
            actionButton("word1", word)    
        }
    })
})
