########################
####### UNIGRAMS #######
#### NO SMOOTHING #####
########################


unigrams.countForWord <- function(word, u.words, u.counters, errorIfWordMissing = T){
    idx <- which(u.words == word)
    result <- 0
    if(length(idx) == 1){
        result <- u.counters[which(u.words == word)]
    }else{
        if(errorIfWordMissing){
            stop(paste("unigrams.countForWord function::", word, "::not found", sep = ""))   
        }
    }
    result
}

unigrams.probabilityForWord <- function(word, u.words, u.counters){
    idx <- which(u.words == word)
    result <- -1
    if(length(idx) == 1){
        result <- u.counters[which(u.words == word)]/ sum(u.counters)
    }else{
        stop(paste("unigrams.probabilityForWord function::", word, "::not found", sep = ""))
    }
    result
}


# Create the Unigram model as a list
# $u.words -> the list of unigrams
# $u.probability -> the list of probabilities
#
# $u.probability[i] (probability) is related to $u.words[i] (unigram)
unigrams.model <- function(u.words, u.counters){
    size <- length(u.words)
    result.words <- character(size)
    result.prob <- numeric(size)
    
    for(i in 1:size){
        tmp <- u.words[i]
        result.words[i] <- tmp
        result.prob[i] <- unigrams.probabilityForWord(word = tmp, u.words = u.words, u.counters = u.counters)
        if(i %% 1000 == 0) print(paste("unigrams.model::processed", i, "of", size))
    }
    
    result_ls <- list(u.words = result.words, u.probability = result.prob)
}

unigramModel.probabilityForWord <- function(u.model, word){
    result <- NULL
    idx <- which(u.model$u.words == word)
    if(length(idx) == 1){
        result <- u.model$u.probability[idx]
    }else{
        stop(paste("unigramModel.probabilityForWord::", word, "::not found"))
    }
    result
}


#######################
####### BIGRAMS #######
#### NO SMOOTHING #####
#######################

bigrams.countForTerm <- function(term, b.terms, b.counters, errorIfTermMissing = T){
    idx <- which(b.terms == term)
    result <- 0
    if(length(idx) == 1){
        result <- b.counters[idx]
    }else{
        if(errorIfTermMissing){
            stop(paste("bigrams.countForTerm function::", term, "::not found", sep = ""))    
        }
    }
    result
}

#Get the probability for a birgrams
# term -> the bigrams as "word_i-1 word_i"
# $b.terms -> the list of bigrams (language model)
# $b.counters -> the list of counts for each bigram
# $u.words -> the list of unigramss (language model)
# $u.counters -> the list of counts for each unigram
# Note! for bigram b.terms[i], respective count is b.counters[i]
# Note! for unigram u.words[i], respective count is u.counters[i]

# Return probability for the term (bigram)
# The probability for term = "w_i-1 wi" is calculated as P(w_i|w_i-1) = count(w_i-1, w_i)/ count(w_i-1)

bigrams.probabilityForTerm <- function(term, b.terms, b.counters, u.words, u.counters){
    result <- -1
    idx <- which(b.terms == term)
    term.words <- ngramTokenize(y = term, ng = 1)

    if(length(idx) != 0){
        w.previous <- term.words[1]
        w.previous.count <- unigrams.countForWord(word = w.previous, u.words = u.words, u.counters = u.counters)
        if(w.previous.count > 0){
            result <- b.counters[which(b.terms == term)]/ w.previous.count   
        }else{
            stop(paste("WARNING::bigrams.probabilityForTerm function::'", term, "'::skipped cause::'", w.previous, "'::missing from unigram list.", sep = ""))
        }
    }else{
        stop(paste("bigrams.probabilityForTerm function::", term, "::not found", sep = ""))
    }
    result
}

# Create the Bigram model as a list
# $b.word -> the list of word (w_i-1)
# $b.nextWord -> the list of word (w_i)
# $b.probability -> the list of probabilities
#
# Note!! $b.probability[i] (probability) is related to "$b.word[i] $b.nextWord[i]"(bigram)
# The probability for term = "w_i-1 wi" is calculated as P(w_i|w_i-1) = count(w_i-1, w_i)/ count(w_i-1)

bigrams.model <- function(b.terms, b.counters, u.words, u.counters){
    size <- length(b.terms)
    result.word <- character(size)
    result.nextWord <- character(size)
    result.prob <- numeric(size)
    
    for(i in 1:size){
        tmp <- b.terms[i]
        tmp.words <- ngramTokenize(y = tmp, ng = 1)
        result.word[i] <- tmp.words[1]
        result.nextWord[i] <- tmp.words[2]
        result.prob[i] <- bigrams.probabilityForTerm(term = tmp, 
                                                    b.terms = b.terms, b.counters = b.counters,
                                                    u.words = u.words, u.counters = u.counters)
        if(i %% 1000 == 0) print(paste("bigrams.model::processed", i, "of", size))
    }
    
    result_ls <- list(b.word = result.word, b.nextWord = result.nextWord,  b.probability = result.prob)
}

########################
####### TRIGRAMS #######
#### NO SMOOTHING #####
########################

trigrams.countForTerm <- function(term, t.terms, t.counters, errorIfTermMissing = T){
    idx <- which(t.terms == term)
    result <- 0
    if(length(idx) == 1){
        result <- t.counters[idx]
    }else{
        if(errorIfTermMissing){
            stop(paste("trigrams.countForTerm function::", term, "::not found", sep = ""))   
        }
    }
    result
}

#Get the probability for a trigrams
# term -> the trigrams as "word_i-2 word_i-1 word_i"
# $t.terms -> the list of trigrams (language model)
# $t.counters -> the list of counts for each trigram
# $b.terms -> the list of bigrams (language model)
# $b.counters -> the list of counts for each bigram
# Note! for bigram b.terms[i], respective count is b.counters[i]
# Note! for trigram t.words[i], respective count is t.counters[i]

# Return probability for the term (trigram)
# The probability for term = "w_i-2 w_i-1 wi" is calculated as 
# P(w_i|w_i-2, w_i-1) = count(w_i-2,w_i-1, w_i)/ count(w_i-2, w_i-1)

trigrams.probabilityForTerm <- function(term, t.terms, t.counters, b.terms, b.counters){
    result <- -1
    idx <- which(t.terms == term)
    tmp.words <- ngramTokenize(y = term, ng = 1)
    
    if(length(idx) == 1){
        tmp.previous.bigram <- paste(tmp.words[1], tmp.words[2])
        tmp.previous.bigram.count <- bigrams.countForTerm(term = tmp.previous.bigram, 
                                                          b.terms = b.terms, b.counters = b.counters)
        result <- t.counters[idx]/ tmp.previous.bigram.count
    }else{
        stop(paste("trigrams.probabilityForTerm function::", term, "::not found", sep = ""))
    }
    result
}

# Create the Trigram model as a list
# $t.bigram -> the list of bigrams ("w_i-2 w_i-1")
# $t.nextWord -> the list of word (w_i)
# $t.count -> the count of c(w_i-2, w_i-1, w_i)
# $t.probability -> the list of probabilities that the bigram is followed by that next word
#
# Note!! $t.probability[i] (probability) is related to "$t.bigram[i] $b.nextWord[i]"(bigram/ nextWord)
# The probability for term = "w_i-2 w_i-1 wi" is calculated as 
# P(w_i|w_i-2,w_i-1) = count(w_i-2, w_i-1, w_i)/ count(w_i-2, w_i-1)

trigrams.model <- function(t.terms, t.counters, b.terms, b.counters){
    size <- length(t.terms)
    result.bigram <- character(size)
    result.nextWord <- character(size)
    result.count <- integer(size)
    result.prob <- numeric(size)
    
    
    print(paste("trigrams.model::process::starting::size::", size))
    
    for(i in 1:size){
        tmp <- t.terms[i]
        tmp.words <- ngramTokenize(y = tmp, ng = 1)
        result.bigram[i] <- paste(tmp.words[1], tmp.words[2])
        result.nextWord[i] <- tmp.words[3]
        result.count[i] <- t.counters[i]
        result.prob[i] <- trigrams.probabilityForTerm(term = tmp, 
                                                     t.terms = t.terms, t.counters = t.counters,
                                                     b.terms = b.terms, b.counters = b.counters)
        if(i %% 100 == 0) print(paste("trigrams.model::processed", i, "of", size))
    }
    
    result_ls <- list(t.bigram = result.bigram, t.nextWord = result.nextWord, t.count = result.count,  t.probability = result.prob)
}