
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
    
    observeEvent(input$addWord,{
        updateTextInput(session = session, inputId = "text_i", value = paste(input$text_i, input$nextWord))
    })
    
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
        if(input$predict > 0 & !is.null(next.words())){
            result <- NULL
            for(i in 1:5){
                result <- c(result, as.character(next.words()$next.word[i]))
            }
            
            radioButtons("nextWord", "Next Word:", result)
        }
    })
    
    output$addNextWord <- renderUI({
        if(input$predict > 0 & !is.null(next.words())){
            actionButton(inputId = "addWord", label = "Add Selected Word...") 
        }
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    
    output$trigramWordcloud <- renderPlot({
       
        if(current.text() != ""){
            tmp <-  next.trigrams.all()
            if(!is.null(tmp)){
                terms <- paste(tmp$source, tmp$next.word)
                counts <- 1000 + (tmp$score * 100)
                
                wordcloud_rep(terms, counts, scale=c(4,0.5),
                              min.freq = 1, random.order = F, rot.per = .15, max.words=20,
                              colors=brewer.pal(8, "Dark2"))
            }
        }
    })
    
    output$bigramWordcloud <- renderPlot({
        
        if(current.text() != ""){
            tmp <-  next.bigrams.all()
            if(!is.null(tmp)){
                terms <- paste(tmp$source, tmp$next.word)
                counts <- 1000 + (tmp$score * 100)
                
                wordcloud_rep(terms, counts, scale=c(4,0.5),
                              min.freq = 1, random.order = F, rot.per = .15, max.words=20,
                              colors=brewer.pal(8, "Dark2"))
            }
        }
    })
    
})
