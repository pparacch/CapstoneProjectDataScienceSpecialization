require(tm)
require(RWeka)

collapseAllSampleToOneList <- function(outputFolder){
    print("* Loading all samples data...")
    load("./data/03_termFrequency/randomSample_1_collapsed_term_frequency.rdata")
    s1.1g <- term_frequency_1g[, c("terms", "total")]
    s1.2g <- term_frequency_2g[, c("terms", "total")]
    s1.3g <- term_frequency_3g[, c("terms", "total")]
    
    load("./data/03_termFrequency/randomSample_2_collapsed_term_frequency.rdata")
    s2.1g <- term_frequency_1g[, c("terms", "total")]
    s2.2g <- term_frequency_2g[, c("terms", "total")]
    s2.3g <- term_frequency_3g[, c("terms", "total")]
    
    load("./data/03_termFrequency/randomSample_3_collapsed_term_frequency.rdata")
    s3.1g <- term_frequency_1g[, c("terms", "total")]
    s3.2g <- term_frequency_2g[, c("terms", "total")]
    s3.3g <- term_frequency_3g[, c("terms", "total")]
    
    load("./data/03_termFrequency/randomSample_4_collapsed_term_frequency.rdata")
    s4.1g <- term_frequency_1g[, c("terms", "total")]
    s4.2g <- term_frequency_2g[, c("terms", "total")]
    s4.3g <- term_frequency_3g[, c("terms", "total")]
    
    load("./data/03_termFrequency/randomSample_5_collapsed_term_frequency.rdata")
    s5.1g <- term_frequency_1g[, c("terms", "total")]
    s5.2g <- term_frequency_2g[, c("terms", "total")]
    s5.3g <- term_frequency_3g[, c("terms", "total")]
    
    load("./data/03_termFrequency/randomSample_6_collapsed_term_frequency.rdata")
    s6.1g <- term_frequency_1g[, c("terms", "total")]
    s6.2g <- term_frequency_2g[, c("terms", "total")]
    s6.3g <- term_frequency_3g[, c("terms", "total")]
    
    print("* Merging all 1g...")
    s.1g <- i_merge_all_samples(s1.1g, s2.1g, s3.1g, s4.1g, s5.1g, s6.1g)
    rm(s1.1g, s2.1g, s3.1g, s4.1g, s5.1g, s6.1g)
    print("* Merging all 2g...")
    s.2g <- i_merge_all_samples(s1.2g, s2.2g, s3.2g, s4.2g, s5.2g, s6.2g)
    rm(s1.2g, s2.2g, s3.2g, s4.2g, s5.2g, s6.2g)
    print("* Merging all 3g...")
    s.3g <- i_merge_all_samples(s1.3g, s2.3g, s3.3g, s4.3g, s5.3g, s6.3g)
    rm(s1.3g, s2.3g, s3.3g, s4.3g, s5.3g, s6.3g)
    
    outputFilePath <- paste(outputFolder,"randomSample_all_collapsed_term_frequency.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'...", sep = ""))
    save(s.1g, s.2g, s.3g, file = outputFilePath)
    return(outputFilePath)
}

i_merge_all_samples <- function(s1.ng, s2.ng, s3.ng, s4.ng, s5.ng, s6.ng){
    d.ng.df <- merge(x = s1.ng, y = s2.ng, by = "terms", all = T)
    names(d.ng.df) <- c("terms", "sample1.count", "sample2.count")
    d.ng.df <- merge(d.ng.df, s3.ng, by = "terms", all = T)
    d.ng.df <- merge(d.ng.df, s4.ng, by = "terms", all = T)
    names(d.ng.df) <- c("terms", "sample1.count", "sample2.count", "sample3.count", "sample4.count")
    d.ng.df <- merge(d.ng.df, s5.ng, by = "terms", all = T)
    d.ng.df <- merge(d.ng.df, s6.ng, by = "terms", all = T)
    names(d.ng.df) <- c("terms", "sample1.count", "sample2.count", "sample3.count", 
                        "sample4.count", "sample5.count", "sample6.count")
    
    d.ng.df$sample1.count[is.na(d.ng.df$sample1.count)] = 0
    d.ng.df$sample2.count[is.na(d.ng.df$sample2.count)] = 0
    d.ng.df$sample3.count[is.na(d.ng.df$sample3.count)] = 0
    d.ng.df$sample4.count[is.na(d.ng.df$sample4.count)] = 0
    d.ng.df$sample5.count[is.na(d.ng.df$sample5.count)] = 0
    d.ng.df$sample6.count[is.na(d.ng.df$sample6.count)] = 0
    
    d.ng.df$total = d.ng.df$sample1.count + d.ng.df$sample2.count + d.ng.df$sample3.count + d.ng.df$sample4.count +
        d.ng.df$sample5.count + d.ng.df$sample6.count
    
    return(d.ng.df)
}



collapse_term_frequency <- function(sourceFilePath, outputFolder, sampleNo){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder,"randomSample_", sampleNo, "_collapsed_term_frequency.rdata",sep = "")
    
    print("**** collapsing term frequency for 1-grams...")
    term_frequency_1g <- i_collapseToOneList(twitter.ng = term_frequency_1g$twitterCorpus,
                                             news.dg =  term_frequency_1g$newsCorpus,
                                             blogs.ng = term_frequency_1g$blogsCorpus)
    
    print("**** collapsing term frequency for 2-grams...")
    term_frequency_2g <- i_collapseToOneList(twitter.ng = term_frequency_2g$twitterCorpus,
                                             news.dg =  term_frequency_2g$newsCorpus,
                                             blogs.ng = term_frequency_2g$blogsCorpus)
    
    print("**** collapsing term frequency for 3-grams...")
    term_frequency_3g <- i_collapseToOneList(twitter.ng = term_frequency_3g$twitterCorpus,
                                             news.dg =  term_frequency_3g$newsCorpus,
                                             blogs.ng = term_frequency_3g$blogsCorpus)
    
    print(paste("* Saving as '", outputFilePath, "'...", sep = ""))
    save(term_frequency_1g, term_frequency_2g, term_frequency_3g, file = outputFilePath)
    return(outputFilePath)
}


i_collapseToOneList <- function(twitter.ng, news.dg, blogs.ng){
    t.ng.termCounters <- i_orderElementsByFrequency(tcv = twitter.ng, decreasing = T)
    n.ng.termCounters <- i_orderElementsByFrequency(tcv = news.dg, decreasing = T)
    b.ng.termCounters <- i_orderElementsByFrequency(tcv = blogs.ng, decreasing = T)
    
    
    d1.df <- data.frame(terms = t.ng.termCounters$terms, counters = t.ng.termCounters$counters, stringsAsFactors = F)
    d2.df <- data.frame(terms = n.ng.termCounters$terms, counters = n.ng.termCounters$counters,stringsAsFactors = F)
    d3.df <- data.frame(terms = b.ng.termCounters$terms, counters = b.ng.termCounters$counters, stringsAsFactors = F)
    
    d.ng.df <- merge(d1.df, d2.df, by = "terms", all = T)
    d.ng.df <- merge(d.ng.df, d3.df, by = "terms", all = T)
    
    names(d.ng.df) <- c("terms", "twitter.count", "news.count", "blogs.count")
    
    d.ng.df$twitter.count[is.na(d.ng.df$twitter.count)] = 0
    d.ng.df$news.count[is.na(d.ng.df$news.count)] = 0
    d.ng.df$blogs.count[is.na(d.ng.df$blogs.count)] = 0
    d.ng.df$total = d.ng.df$twitter.count + d.ng.df$news.count + d.ng.df$blogs.count

    return(d.ng.df)
}

i_orderElementsByFrequency <- function(tcv, decreasing = F){
    tmp.terms <- rownames(tcv)
    tmp.counters <- tcv$freq
    
    #Order by frequency (decreasing)
    idx <- order(tmp.counters, decreasing = decreasing)
    
    list(terms = tmp.terms[idx], counters = tmp.counters[idx])
    
}


reduce_to_term_frequency <- function(sourceFilePath, outputFolder, sampleNo){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder,"randomSample_", sampleNo, "_term_frequency.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'...", sep = ""))
    
    print("* Generating term frequency for 1-grams...")
    term_frequency_1g <- lapply(tdm_1g_corpus, i_generate_terms_frequency)
    
    print("* Generating term frequency for 2-grams...")
    term_frequency_2g <- lapply(tdm_2g_corpus, i_generate_terms_frequency)
    
    print("* Generating term frequency for 3-grams...")
    term_frequency_3g <- lapply(tdm_3g_corpus, i_generate_terms_frequency)
    
    save(term_frequency_1g, term_frequency_2g, term_frequency_3g, file = outputFilePath)
    return(outputFilePath)
}



i_generate_terms_frequency <- function(corpus.tdm){
    print("**** i_getAllTermsFrequencyInCorpora...")
    print(paste("**** tdm - no Of terms:", dim(corpus.tdm)[1]))
    all.terms <- findFreqTerms(corpus.tdm) #get all of the terms
    result <- data.frame(freq = tm_term_score(x = corpus.tdm, terms = all.terms, FUN = slam::row_sums))
    print(paste("**** term frequency - no Of terms:", dim(result)[1]))
    return(result)
}

tm_generate_ng <- function(sourceFilePath, outputFolder){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder, tail(unlist(strsplit(sourceFilePath, "/")),1), "_tdm_ng.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'...", sep = ""))
    
    print("* Generating tmd for 1-grams...")
    tdm_1g_corpus <- lapply(tm_corpus, i_tdm_generate_ng, ng = 1)
    print("* Generating tmd for 2-grams...")
    tdm_2g_corpus <- lapply(tm_corpus, i_tdm_generate_ng, ng = 2)
    print("* Generating tmd for 3-grams...")
    tdm_3g_corpus <- lapply(tm_corpus, i_tdm_generate_ng, ng = 3)
    
    save(tdm_1g_corpus, tdm_2g_corpus, tdm_3g_corpus, file = outputFilePath)
    
    return(outputFilePath)
}

i_tdm_generate_ng <- function(corpus, ng = 1){
    # MAC OS Manadtory if not using doMC library
    #options(mc.cores=1) 
    ngramTokenizer <- function(y) NGramTokenizer(y, Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!")) 
    # create n-grams
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer, wordLengths = c(1, Inf)))
    tdm
}


corpora_transform <- function(sourceFilePath, outputFolder){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder, tail(unlist(strsplit(sourceFilePath, "/")),1), "_tmCorpus.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'...", sep = ""))
    tm_corpus <- lapply(result, i_corpus_transform)
    save(tm_corpus, file = outputFilePath)
    
    return(outputFilePath)
}

#return a tdm::Corpus
i_corpus_transform <- function(x){
    cat("\n")
    corpus <- Corpus(VectorSource(x))
    print("tolower...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    print("removeWords::profanityWords...")
    corpus <- tm_map(corpus, removeWords, i_get_profanity_words())
    print("removeNumbers...")
    corpus <- tm_map(corpus, removeNumbers) 
    print("removePunctuations.except apostrophe '...")
    corpus <- tm_map(corpus, content_transformer(i_removePunctuations_exceptApostrophe))
    print("addStartEndMarkers...")
    corpus <- tm_map(corpus, content_transformer(i_addStartEndMarkers))
    print("strpWhitespaces...")
    corpus <- tm_map(corpus, stripWhitespace)
    corpus
}

i_get_profanity_words <- function(){
    con <- file("./../data/original/bad-words.txt", "r") 
    stopwords.profanityWords <- readLines(con, skipNul = T)
    close(con)
    con <- NULL
    return(stopwords.profanityWords)
}

##In order to keep contractions like I'm or I'll ....
i_removePunctuations_exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

##Add start sentence (<s>) and end sentence (</s>) markers in the sentences
i_addStartEndMarkers <- function(texts){
    paste("<s>", texts, "</s>")
}