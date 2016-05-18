
# server behaviour of the
# TextPredictor, a Shiny web application.
# Capstone Project for the Data Science 
# Specialization (Johns Hopkins, Coursera)

library(shiny)


shinyServer(function(input, output, session) {
    
    current.text <- reactive({ 
        input$predict
        isolate(input$text_i) 
        })
    
    next.words <- reactive({prediction.f(current.text())})
    next.trigrams.all <- reactive({getallTrigrams(current.text())})
    next.bigrams.all <- reactive({getallBigrams(current.text())})
    next.unigrams.top <- reactive({gettopUnigrams(current.text())})
    
    output$text_o <- renderText({ 
       paste(current.text())
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
    
    output$possibleWords <- renderUI({
        if(input$predict > 0){
            result <- NULL
            for(i in 1:5){
                result <- c(result, as.character(next.words()$next.word[i]))
            }
            
            radioButtons("nextWord", "Next Word:", result)
        }
    })
    
    output$addNextWord <- renderUI({
        if(input$predict > 0){
            actionButton(inputId = "addWord", label = "Add Selected Word...") 
        }
    })
    
    observeEvent(input$addWord,{
        updateTextInput(session = session, inputId = "text_i", value = paste(input$text_i, input$nextWord))
    })
    
    # output$word1 <- renderUI({
    #     word <- next.words()$next.word[1]
    #     if(!is.null(word)){
    #         actionButton("word1", word)    
    #     }
    # })
    # 
    # output$word2 <- renderUI({
    #     word <- next.words()$next.word[2]
    #     if(!is.null(word)){
    #         actionButton("word1", word)    
    #     }
    # })
    # 
    # output$word3 <- renderUI({
    #     word <- next.words()$next.word[3]
    #     if(!is.null(word)){
    #         actionButton("word1", word)    
    #     }
    # })
    # 
    # output$word4 <- renderUI({
    #     word <- next.words()$next.word[4]
    #     if(!is.null(word)){
    #         actionButton("word1", word)    
    #     }
    # })
    # 
    # output$word5 <- renderUI({
    #     word <- next.words()$next.word[5]
    #     if(!is.null(word)){
    #         actionButton("word1", word)    
    #     }
    # })
})
