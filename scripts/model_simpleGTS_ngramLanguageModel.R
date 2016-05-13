# NGRAM LAnguage model - based on simple Good Turing Smoothing
# If ngram is present, use a discounted count to calculate the probability
# Otherwise return a value (redistribution of probability mass)

# disc/ N, where N is the total number of ngrams (terms) observed

estimateSentenceProbabilities.ng.model.simple.withGoodTuring.smoothing <- function(s, terms, counters, ng){
    s.words <- ngramTokenize(y = s, ng = 1)
    s.N <- length(s.words) - 1
    N <- sum(counters)
    
    print(paste("###########sentenceProbability - Ngram Simple - Good-Turing Smoothing"))
    print(paste("#    sentence:'", s, "', NoOfWords:", s.N))
    print(paste("#          ng:", ng, ", totalNoOfTerms:", N))
    
    result <- 0
    s.ngrams <- ngramTokenize(y = s, ng = ng)
    
    for(i in 1:length(s.ngrams)){
        d.t <- s.ngrams[i]
        print(paste("#       ngram:", d.t))
        gt.count <- gts_getGoodTuringCount(term = d.t, terms = terms, counters = counters)
        tmp <- log(x = (gt.count/N), base = 2)
        
        result <- result + tmp
        print(paste("#    prob(log):", tmp, ", total(log):", result))
        print("--------------------------------------------")
    }
    print("###########")
    
    data.frame(sentence = s, probability_log_base2 = result, noOfWords.sentence = s.N, stringsAsFactors = F)
}
