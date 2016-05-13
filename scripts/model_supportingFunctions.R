
# Ngram Tokenizer - same used for the corpora processing
ngramTokenize <- function(y, ng) {
    RWeka::NGramTokenizer(y, RWeka::Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!"))
}



calculatePerplexity <- function(evaluation.as.list){
    
    elements <- dim(evaluation.as.list)[1]
    probs <- unlist(evaluation.as.list[(elements + 1):(elements * 2)])
    Ns <- unlist(evaluation.as.list[(elements * 2) + 1 : (elements * 3)])
    
    print(paste("Probs:", paste(probs, collapse = ", ")))
    print(paste("   Ns:", paste(Ns, collapse = ", ")))
    
    if(sum(is.infinite(probs))){
        return(NA)
    }else{
        s.p <- sum(probs)
        s.Ns <- sum(Ns)
        print(paste("Sum Probabilities:", s.p, ", Total NoOfTerms:", s.Ns))
        return(-1 * sum(probs)/ sum(Ns))
    }
}