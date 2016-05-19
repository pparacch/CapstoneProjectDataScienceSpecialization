cleanup_entered_words <- function(text, useStartMarker = T){
    #to lowercase
    text <- tolower(text)
    #force to ascii - replace non ascii char to empty char
    text <- iconv(x = text, from = localeToCharset(), to = "ASCII", " ")
    #remove numbers
    text <- gsub(pattern = "[[:digit:]]+", replacement = " ", x = text)
    #remove punctuation
    text <- gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = text, perl = T)
    
    if(useStartMarker){
        #add start-sentence marker
        text <- paste("<s>", text)   
    }
    
    #normalize spaces
    text <- trimws(gsub(pattern = " {2,}", replacement = " ", x = text))
    return(text)
}

getallTrigrams <- function(text){
    result <- NULL
    
    if(text != "") {
        words <- unlist(strsplit(text, " "))
        words.n <- length(words)
        bigram <- paste(words[words.n - 1], words[words.n])
        result <- trigrams.findNextWord(bigram, d.3g, maxNoOfItems = Inf) 
    }
    result
}

getallBigrams <- function(text){
    result <- NULL
    
    if(text != "") {
        words <- unlist(strsplit(text, " "))
        words.n <- length(words)
        result <- bigrams.findNextWord(words[words.n], d.2g, maxNoOfItems = Inf) 
    }
    result
}

gettopUnigrams <- function(text){
    result <- NULL
    
    if(text != "") {
        result <- unigrams.mostProbableWords(d.1g, maxNoOfItems = 15) 
    }
    result
}


prediction.f <- function(text){
    result <- NULL
    
    if(text != "") {
        words <- unlist(strsplit(text, " "))
        words.n <- length(words)
        bigram <- paste(words[words.n - 1], words[words.n])
        tmp.3g <- trigrams.findNextWord(bigram, d.3g)
        tmp.2g <- bigrams.findNextWord(words[words.n], d.2g)
        tmp.1g <- unigrams.mostProbableWords(d.1g)
        
        words <- tmp.3g$next.word
        for(i in words){
            idx <- which(tmp.2g$next.word == i)
            if(length(idx) != 0){
                tmp.2g <- tmp.2g[-idx,]
            }
        }
        
        result <- rbind(tmp.3g, tmp.2g)
        
        words <- result$next.word
        for(i in words){
            idx <- which(tmp.1g$next.word == i)
            if(length(idx) != 0){
                tmp.1g <- tmp.1g[-idx,]
            }
        }
        
        result <- rbind(result, tmp.1g)
        
    }
    result
}


trigrams.findNextWord <- function(bigram, t.df, maxNoOfItems = 5){
    idx <- which(t.df$term == bigram)
    
    if(length(idx) == 0){
        return(NULL)
    }
    
    if(is.infinite(maxNoOfItems)){
        maxNoOfItems <- length(idx)
    }else if(length(idx) < maxNoOfItems){
        maxNoOfItems = length(idx)
    }
    
    tmp <- t.df[idx[1: maxNoOfItems],]
    
    data.frame(source = tmp$term, next.word = tmp$next.word, score = log(tmp$score, 2))
}

bigrams.findNextWord <- function(word, b.df, maxNoOfItems = 5){
    idx <- which(b.df$term == word)
    
    if(length(idx) == 0){
        return(NULL)
    }
    
    if(is.infinite(maxNoOfItems)){
        maxNoOfItems <- length(idx)
    }else if(length(idx) < maxNoOfItems){
        maxNoOfItems = length(idx)
    }
    
    tmp <- b.df[idx[1: maxNoOfItems], ]
    data.frame(source = tmp$term, next.word = tmp$next.word, score = log(tmp$score, 2))
}

unigrams.mostProbableWords <- function(u.df, maxNoOfItems = 5){
    tmp <- u.df[1: maxNoOfItems, ]
    data.frame(source = "Unigram", next.word = tmp$term, score = log(tmp$score, 2))
}