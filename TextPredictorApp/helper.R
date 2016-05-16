
trigrams.findNextWord <- function(bigram, t.df, maxNoOfItems = 5){
    idx <- which(t.df$before.bigram == bigram)
    
    if(length(idx) == 0){
        return(NULL)
    }
    
    if(length(idx) < maxNoOfItems){
        maxNoOfItems = length(idx)
    }
    
    tmp <- t.df[idx[1: maxNoOfItems],]
    
    data.frame(source = "Trigram", next.word = tmp$next.word)
}

bigrams.findNextWord <- function(word, b.df, maxNoOfItems = 5){
    idx <- which(grepl(paste("^", word, " ", sep = ""), b.df$terms))
    
    if(length(idx) == 0){
        return(NULL)
    }
    
    if(length(idx) < maxNoOfItems){
        maxNoOfItems = length(idx)
    }
    
    
    tmp <- b.df[idx[1: maxNoOfItems], ]
    tmp$next.word <- sapply(tmp$terms, function(x){unlist(strsplit(x, split = " "))[2]})
    data.frame(source = "Bigram", next.word = tmp$next.word)
}

unigrams.mostProbableWords <- function(u.df, maxNoOfItems = 5){
    tmp <- u.df[1: maxNoOfItems, ]
    data.frame(source = "Unigram", next.word = tmp$terms)
}