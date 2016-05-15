# Implementation of the stupid backoff algorithm
# No discount just use relative frequencies
sb_factor <- 0.4

stupidBackoff.trigrams <- function(word_i, bigramBeforeWord_i, t.terms, t.counters, b.terms, b.counters, u.words, u.counters){
    # print(paste("  --sb_support.tri::word_i '", word_i, "', bi_i-1 '",bigramBeforeWord_i, "'", sep = ""))
    el.t.p <- sb_support.tri(word_i = word_i, bigramBeforeWord_1 = bigramBeforeWord_i,
                             t.terms = t.terms, t.counters = t.counters,
                             b.terms = b.terms, b.counters = b.counters)
    if(el.t.p > 0){
        return(el.t.p)
    }
    
    bigramBeforeWord_i.words <- ngramTokenize(y = bigramBeforeWord_i, ng = 1)
    # print(paste("  --sb_support.bi::word_i '", word_i, "', word_i-1 '",bigramBeforeWord_i.words[2], "'", sep = ""))
    el.b <- paste(bigramBeforeWord_i.words[2], word_i)
    el.b.p <- sb_support.bi(word_i = word_i, word_i_m1 = bigramBeforeWord_i.words[2],
                            b.terms = b.terms, b.counters = b.counters,
                            u.words = u.words, u.counters = u.counters)
    if(el.b.p > 0){
        return(el.b.p)
    }
    
    # print(paste("  --sb_support.uni::word_i '", word_i, "'", sep = ""))
    el.u.p <- sb_support.uni(word_i = word_i, 
                             u.words = u.words, u.counters = u.counters)
    if(el.u.p > 0){
        return(el.u.p)
    }
    
    #TO DO IMPLEMENT A SMOOTHING tecnique
    stop()
}

sb_support.tri <- function(word_i, bigramBeforeWord_1, t.terms, t.counters, b.terms, b.counters){
    prob.w_i.given.bigram(word_i = word_i, bigramBeforeWord_1 = bigramBeforeWord_1,
                          t.terms = t.terms, t.counters = t.counters,
                          b.terms = b.terms, b.counters = b.counters)
}

sb_support.bi <- function(word_i, word_i_m1, b.terms, b.counters, u.words, u.counters){
    sb_factor * prob.w_i.given.word(word_i = word_i, word_i_m1 = word_i_m1,
                                    b.terms = b.terms, b.counters,
                                    u.words = u.words, u.counters = u.counters)
}

sb_support.uni <- function(word_i, u.words, u.counters){
    prob <- prob.w_i(word_i = word_i, u.words = u.words, u.counters = u.counters)
    sb_factor * prob
}

prob.w_i.given.bigram <- function(word_i, bigramBeforeWord_1, t.terms, t.counters, b.terms, b.counters){
    el.t <- paste(bigramBeforeWord_1, word_i)
    el.t.c <- trigrams.countForTerm(term = el.t, t.terms = t.terms, t.counters = t.counters, errorIfTermMissing = F)
    if(el.t.c != 0){
        #if it find the trigram, then look for the bigram
        bigramBeforeWord_1.c <- bigrams.countForTerm(term = bigramBeforeWord_1, b.terms = b.terms, b.counters = b.counters)
        (el.t.c/ bigramBeforeWord_1.c)
    }else{
        #if it does not find the trigram then return 0
        0
    }
}

prob.w_i.given.word <- function(word_i, word_i_m1, b.terms, b.counters, u.words, u.counters){
    el.b <- paste(word_i_m1, word_i)
    el.b.c <- bigrams.countForTerm(term = el.b, b.terms = b.terms, b.counters = b.counters, errorIfTermMissing = F)
    if(el.b.c != 0){
        #If it finds the bigram
        word_i_m1.c <- unigrams.countForWord(word = word_i_m1, u.words = u.words, u.counters = u.counters, errorIfWordMissing = F)
        (el.b.c/ word_i_m1.c)
    }else{
        #if it does not find the bigram then return 0
        0
    }
}

prob.w_i <- function(word_i, u.words, u.counters){
    word_i.c <- unigrams.countForWord(word = word_i,u.words = u.words, u.counters = u.counters, errorIfWordMissing = F)
    word_i.c / sum(u.counters)
}

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

unigrams.countForWord <- function(word, u.words, u.counters, errorIfWordMissing = T){
    idx <- which(u.words == word)
    result <- 0
    if(length(idx) == 1){
        result <- u.counters[which(u.words == word)]
    }else{
        if(errorIfWordMissing){
            stop(paste("unigrams.countForWord function::", word, "::not found", sep = ""))   
        }else{
            # print(paste("  --unigrams.countForWord::word_i 'OTH'", sep = ""))
            result <- u.counters[which(u.words == "OTH")]
        }
    }
    
    result
}


#############################################################################
# estimateSentenceProbabilities                                             #
# Probability Calculation for a sentence using the ChainRule                #
# Trigram Language Model Aprroximation                                      #
# log(Proability) = log(p1) + log(p2) + .. + log(pn)                        #
# Perplexity Calculation for a sentence                                     #
# Trigram Language Model Aprroximation                                      #
# S: sentence, N: number of words in S, PP = perplexity                     #
# log(PP) = -1/N * log(Proability) = -1/N * log(p1) + log(p2) + .. + log(pn)#
#############################################################################
# sentence -> normalized sentence, e.g. <s> and </s> added, toLowerCase, remove punctuations and numbers
estimateSentenceProbabilities <- function(s, t.terms, t.counters, b.terms, b.counters, u.words, u.counters){
    s.words <- ngramTokenize(y = s, ng = 1)
    N <- length(s.words) - 1 #Do not count the beginning of the sentence marker <s>
    
    # print(paste("########sentenceProbability"))
    # print(paste("##### sentence:", s, ", NoOfWords:", N))
    
    result <- 0
    s.trigrams <- ngramTokenize(y = s, ng = 3)
    
    for(i in 1:length(s.trigrams)){
        d.t <- s.trigrams[i]
        d.t.ws <- ngramTokenize(d.t,1)
        d.t.w_i <- d.t.ws[3]
        d.t.b <- paste(d.t.ws[1], d.t.ws[2])
        
        # print(paste("##### bigram_i-1:", d.t.b))
        # print(paste("##### word_i:", d.t.w_i))
        tmp <- log(stupidBackoff.trigrams(word_i = d.t.w_i, bigramBeforeWord_i = d.t.b,
                                          t.terms = t.terms, t.counters = t.counters,
                                          b.terms = b.terms, b.counters = b.counters,
                                          u.words = u.words, u.counters = u.counters), base = 2)
        result <- result + tmp
        # print(paste("##### prob(log):", tmp, ", total(log):", result))
    }
    
    data.frame(sentence = s, probability_log_base2 = result, noOfWords.sentence = N, stringsAsFactors = F)
}
