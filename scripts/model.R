
Sys.setlocale(category = "LC_ALL",locale = "English_United States.1252")

# Load the Term Document Matrix created for the Sample Corpora 
# Twitter, News, Blogs

folder <- "./datasetDumps/"

ngramTokenize <- function(y, ng) RWeka::NGramTokenizer(y, RWeka::Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!"))

load.twitter.1g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.1g.Rdata", sep = ""))
    twitter.allTerms.1g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.1g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.1g, allTermsCounters = twitter.allTerms.1g)
}

load.twitter.2g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.2g.Rdata", sep = ""))
    twitter.allTerms.2g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.2g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.2g, allTermsCounters = twitter.allTerms.2g)
}

load.twitter.3g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.3g.Rdata", sep = ""))
    twitter.allTerms.3g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.3g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.3g, allTermsCounters = twitter.allTerms.3g)
}


unigrams.countForWord <- function(word, u.words, u.counters){
    idx <- which(u.words == word)
    result <- -1
    if(length(idx) == 1){
        result <- u.counters[which(u.words == word)]
    }else{
        stop(paste("unigrams.countForWord function::", word, "::not found", sep = ""))
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
    }
    
    result_ls <- list(u.words = result.words, u.probability = result.prob)
}

