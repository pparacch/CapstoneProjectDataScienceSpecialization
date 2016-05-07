
Sys.setlocale(category = "LC_ALL",locale = "English_United States.1252")

# Load the Term Document Matrix created for the Sample Corpora 
# Twitter, News, Blogs

ngramTokenize <- function(y, ng) RWeka::NGramTokenizer(y, RWeka::Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!"))

load.twitter.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = twitter.corpora.tdm.1g, tcv = allTerms.1g)
}

load.twitter.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = twitter.corpora.tdm.2g, tcv = allTerms.2g)
}

load.twitter.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = twitter.corpora.tdm.3g, tcv = allTerms.3g)
}

load.news.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = news.corpora.tdm.1g, tcv = allTerms.1g)
}

load.news.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = news.corpora.tdm.2g, tcv = allTerms.2g)
}

load.news.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = news.corpora.tdm.3g, tcv = allTerms.3g)
}

load.blogs.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = blogs.corpora.tdm.1g, tcv = allTerms.1g)
}

load.blogs.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = blogs.corpora.tdm.2g, tcv = allTerms.2g)
}

load.blogs.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpora.allTermsFrequency
    load(file_url_tdm)
    
    list(tdm = blogs.corpora.tdm.3g, tcv = allTerms.3g)
}


orderElementsByFrequency <- function(tcv, decreasing = F){
    tmp.terms <- rownames(tcv$tcv)
    tmp.counters <- tcv$tcv$freq
    
    #Order by frequency (decreasing)
    idx <- order(tmp.counters, decreasing = decreasing)
    
    list(terms = tmp.terms[idx], counters = tmp.counters[idx])

}

# Merge the list of available n-grams for the different corpora
collapseToOneList <- function(twitter.ng, news.dg, blogs.ng, ng){
    t.3g.termCounters <- orderElementsByFrequency(tcv = twitter.ng, decreasing = T)
    n.3g.termCounters <- orderElementsByFrequency(tcv = news.dg, decreasing = T)
    b.3g.termCounters <- orderElementsByFrequency(tcv = blogs.ng, decreasing = T)
    
    
    d1.df <- data.frame(terms = t.3g.termCounters$terms, counters = t.3g.termCounters$counters, stringsAsFactors = F)
    d2.df <- data.frame(terms = n.3g.termCounters$terms, counters = n.3g.termCounters$counters,stringsAsFactors = F)
    d3.df <- data.frame(terms = b.3g.termCounters$terms, counters = b.3g.termCounters$counters, stringsAsFactors = F)
    
    d.ng.df <- merge(d1.df, d2.df, by = "terms", all = T)
    d.ng.df <- merge(d.ng.df, d3.df, by = "terms", all = T)
    
    names(d.ng.df) <- c("terms", "twitter.count", "news.count", "blogs.count")
    
    d.ng.df$twitter.count[is.na(d.ng.df$twitter.count)] = 0
    d.ng.df$news.count[is.na(d.ng.df$news.count)] = 0
    d.ng.df$blogs.count[is.na(d.ng.df$blogs.count)] = 0
    d.ng.df$total = d.ng.df$twitter.count + d.ng.df$news.count + d.ng.df$blogs.count
    
    filename <- paste("./../data/processed/allCorpora_aggregated_allTermsFrequency.", ng, "g.rdata", sep = "")
    save(d.ng.df, file = filename)
    filename
}


########################
####### UNIGRAMS #######
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

# Implementation of the stupid backoff algorithm
# No discount just use relative frequencies
sb_factor <- 0.4

stupidBackoff.trigrams <- function(word_i, bigramBeforeWord_i, t.terms, t.counters, b.terms, b.counters, u.words, u.counters){
    print(paste("##sb_support.tri::word_i '", word_i, "', bi_i-1 '",bigramBeforeWord_i, "'", sep = ""))
    el.t.p <- sb_support.tri(word_i = word_i, bigramBeforeWord_1 = bigramBeforeWord_i,
                             t.terms = t.terms, t.counters = t.counters,
                             b.terms = b.terms, b.counters = b.counters)
    if(el.t.p > 0){
        return(el.t.p)
    }
    
    bigramBeforeWord_i.words <- ngramTokenize(y = bigramBeforeWord_i, ng = 1)
    print(paste("##sb_support.bi::word_i '", word_i, "', word_i-1 '",bigramBeforeWord_i.words[2], "'", sep = ""))
    el.b <- paste(bigramBeforeWord_i.words[2], word_i)
    el.b.p <- sb_support.bi(word_i = word_i, word_i_m1 = bigramBeforeWord_i.words[2],
                            b.terms = b.terms, b.counters = b.counters,
                            u.words = u.words, u.counters = u.counters)
    if(el.b.p > 0){
        return(el.b.p)
    }
    
    print(paste("##sb_support.uni::word_i '", word_i, "'", sep = ""))
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
    sb_factor * prob.w_i(word_i = word_i, u.words = u.words, u.counters = u.counters)
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
        word_i_m1.c <- unigrams.countForWord(word = word_i_m1, u.words = u.words, u.counters = u.counters)
        (el.b.c/ word_i_m1.c)
    }else{
        #if it does not find the bigram then return 0
        0
    }
}

prob.w_i <- function(word_i, u.words, u.counters){
    word_i.c <- unigrams.countForWord(word = word_i,u.words = u.words, u.counters = u.counters, errorIfWordMissing = F)
    (word_i.c/ sum(u.counters))
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
    N <- length(s.words)
    
    print(paste("#sentenceProbability"))
    print(paste("#    sentence:", s, ", NoOfWords:", N))

    result <- 0
    s.trigrams <- ngramTokenize(y = s, ng = 3)
    
    for(i in 1:length(s.trigrams)){
        d.t <- s.trigrams[i]
        d.t.ws <- ngramTokenize(d.t,1)
        d.t.w_i <- d.t.ws[3]
        d.t.b <- paste(d.t.ws[1], d.t.ws[2])
        
        print(paste("#         word_i:", d.t.w_i))
        print(paste("#     bigram_i-1:", d.t.b))
        tmp <- log(stupidBackoff.trigrams(word_i = d.t.w_i, bigramBeforeWord_i = d.t.b,
                                          t.terms = t.terms, t.counters = t.counters,
                                          b.terms = b.terms, b.counters = b.counters,
                                          u.words = u.words, u.counters = u.counters), base = 2)
        result <- result + tmp
        print(paste("#    prob(log):", tmp, ", total(log):", result))
    }
    
    list(probability_ln <- result, perplexity_ln <- (-1/ N)* result)
}