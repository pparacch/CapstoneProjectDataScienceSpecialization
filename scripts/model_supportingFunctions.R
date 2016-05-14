Sys.setlocale(category = "LC_ALL",locale = "English_United States.1252")

# Ngram Tokenizer - same used for the corpora processing
ngramTokenize <- function(y, ng) {
    RWeka::NGramTokenizer(y, RWeka::Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!"))
}

# Load the Term Document Matrix created for the Sample Corpora 
# Twitter, News, Blogs

ngramTokenize <- function(y, ng) RWeka::NGramTokenizer(y, RWeka::Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!"))

load.twitter.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = twitter.corpus.tdm.1g, tcv = allTerms.1g)
}

load.twitter.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = twitter.corpus.tdm.2g, tcv = allTerms.2g)
}

load.twitter.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = twitter.corpus.tdm.3g, tcv = allTerms.3g)
}

load.twitter.4g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.4g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = twitter.corpus.tdm.4g, tcv = allTerms.4g)
}

load.news.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = news.corpus.tdm.1g, tcv = allTerms.1g)
}

load.news.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = news.corpus.tdm.2g, tcv = allTerms.2g)
}

load.news.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = news.corpus.tdm.3g, tcv = allTerms.3g)
}

load.news.4g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.4g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = news.corpus.tdm.4g, tcv = allTerms.4g)
}

load.blogs.1g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.1g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = blogs.corpus.tdm.1g, tcv = allTerms.1g)
}

load.blogs.2g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.2g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = blogs.corpus.tdm.2g, tcv = allTerms.2g)
}

load.blogs.3g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.3g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = blogs.corpus.tdm.3g, tcv = allTerms.3g)
}

load.blogs.4g.data <- function(file_url_allTermsFrequency, file_url_tdm){
    load(file_url_allTermsFrequency)
    allTerms.4g <- corpus.allTermsFrequency
    load(file_url_tdm)
    list(tdm = blogs.corpus.tdm.4g, tcv = allTerms.4g)
}

visualizeCumuluativeCoverage <- function(freqs, terms, title){
    idx <- order(freqs, decreasing = T)
    terms.term <- terms[idx]
    terms.freq <- freqs[idx]
    
    terms.count <- sum(terms.freq)
    terms.unique <- length(unique(terms.term))
    terms.cumulativeCoverage <- (cumsum(terms.freq)/ terms.count) * 100
    
    
    plot(terms.cumulativeCoverage, type = "l", xlab = "Number Of Words", ylab = "% Coverage", main = title)
    abline(v = which(terms.cumulativeCoverage >= 50)[1], col = "orange", lwd = 3)
    abline(v = which(terms.cumulativeCoverage >= 90)[1], col = "red", lwd = 3)
    legend(x = "topright", lty=c(1,1), lwd=c(3,3), col=c("orange", "red"), legend = c("50% coverage", "90% coverage")) 
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
    
    filename <- paste("./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.", ng, "g.rdata", sep = "")
    save(d.ng.df, file = filename)
    filename
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