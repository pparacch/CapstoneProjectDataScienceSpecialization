# GOOD-TURING SMOOTHING
#
# Implementation of a (simple) Good Turing smoothing algorithm
# based on simplification count_gt = count - 0.75


# Calculate  the GoodTuring count (generalized for ngrams)
# (i) term: the term looking for
# (i) terms: the list of term available in our ngram domain
# (i) counters: the frequency of the terms observed in our ngram domain
#
# (o) the good turing count (discounted count for the term)

gts_getGoodTuringCount <- function(term, terms, counters){
    #Simple implementation based on Good-Turing numbers
    print(paste("    >>>> term:", term))
    
    result <- NULL
    idx <- which(terms == term)
    
    
    if(length(idx) == 0){
        #A new term that I have never seen before so
        result <- gts_support_getGoodTuringCountByActualCount(0)
    }else{
        result <- gts_support_getGoodTuringCountByActualCount(counters[idx])
    }
    return(result)
}


# Supporting method for the calculation of the Good-Turing Count

gts_support_getGoodTuringCountByActualCount <- function(actualCount){
    result <- NULL
    if(actualCount == 0){
        #A new term that I have never seen before so
        result <- 0.000027
    }else{
        result <- actualCount - 0.75
    }
    print(paste("    >>>> Actual count:", actualCount, "- Good-Turing Count:", result))
    return(result)
}
