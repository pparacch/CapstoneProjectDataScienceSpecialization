########################
####### NGRAMS   #######
#### GT SMOOTHING  #####
########################

ngrams.gtsmoothing.probabilityForWord <- function(term, terms, counters){
    gt.count <- getGoodTuringCount(term = term, terms = terms, counters = counters)
    result <- gt.count/ sum(counters)
    print(paste("    >>>> Term:", term, " - probabilityForWord:", result))
    result
}

trigrams.gtsmoothing.probabilityForTerm <- function(term, lambda_1, lambda_2, lambda_3,
                                                    t.terms, t.counters,
                                                    b.terms, b.counters,
                                                    u.terms, u.counters){
    
    p.3gram <- ngrams.gtsmoothing.probabilityForWord(term = term, terms = t.terms, counters = t.counters)
    
    term.words <- ngramTokenize(y = term, ng = 1)
    p.2gram <- ngrams.gtsmoothing.probabilityForWord(term = paste(term.words[2], term.words[3]), terms = b.terms, counters = b.counters)
    p.1gram <- ngrams.gtsmoothing.probabilityForWord(term = term.words[3], terms = u.terms, counters = u.counters)
    
    (p.3gram * lambda_1) + (p.2gram * lambda_2) + (p.1gram * lambda_3)
    
}

estimateSentProb.linearInterpolation.model.withGoodTuring.smoothing <- function(s, 
                                                                                t.terms, t.counters,
                                                                                b.terms, b.counters,
                                                                                u.terms, u.counters,
                                                                                lambda_1 = 0.8, lambda_2 = 0.15, lambda_3 = 0.5){
    s.words <- ngramTokenize(y = s, ng = 1)
    s.N <- length(s.words) - 1 #Do not count the start marker <s>

    print(paste("###########sentenceProbability - Ngram linearInterpolation - Good-Turing Smoothing"))
    print(paste("#       sentence:'", s, "', NoOfWords:", s.N))
    print(paste("#    lambda_1(3):", lambda_1, " lambda_2(2):", lambda_2, ", lambda_3(1):", lambda_3))
    
    result <- 0
    s.3grams <- ngramTokenize(y = s, ng = 3)
    
    for(i in 1:length(s.3grams)){
        d.t <- s.3grams[i]
        print(paste("#          3gram:", d.t))
        p <- trigrams.gtsmoothing.probabilityForTerm(term = d.t, lambda_1 = lambda_1, lambda_2 = lambda_2, lambda_3 = lambda_3,
                                                     t.terms = t.terms, t.counters = t.counters,
                                                     b.terms = b.terms, b.counters = b.counters,
                                                     u.terms = u.terms, u.counters = u.counters)
        tmp <- log(x = p, base = 2)

        result <- result + tmp
        print(paste("#    prob(log):", tmp, ", total(log):", result))
    }
    
    data.frame(sentence = s, probability_log_base2 = result, noOfWords.sentence = s.N, stringsAsFactors = F)
}

