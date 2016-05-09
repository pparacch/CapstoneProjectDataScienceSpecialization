require(tm)
require(wordcloud)
require(RWeka)
require(ggplot2)
require(RColorBrewer)

load_original_corpora <- function(){
    twitter.all <- load_original_corpus("./../data/original/final/en_US/en_US.twitter.txt")
    news.all <- load_original_corpus("./../data/original/final/en_US/en_US.news.txt")
    blogs.all <- load_original_corpus("./../data/original/final/en_US/en_US.blogs.txt")
    
    list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
}

load_blogs_corpus <- function(){
    blogs.all <- load_original_corpus("./../data/original/final/en_US/en_US.blogs.txt")
    list(blogsCorpus = blogs.all)
}


load_twitter_corpus <- function(){
    twitter.all <- load_original_corpus("./../data/original/final/en_US/en_US.twitter.txt")
    list(twitterCorpus = twitter.all)
}

load_news_corpus <- function(){
    news.all <- load_original_corpus("./../data/original/final/en_US/en_US.news.txt")
    
    list(newsCorpus = news.all)
}

load_original_corpus <- function(corpus.filepath){
    con <- file(corpus.filepath, "r") 
    data.all <- readLines(con, skipNul = T)
    close(con)
    data.all
}

#corpora as a list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
sample_corpora <- function(corpora, seed, twitter.perc = 0.1, news.perc = 0.1, blogs.perc = 0.1){
    twitter.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$twitterCorpus), seed = seed, percentageOfSuccess = twitter.perc)
    news.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$newsCorpus), seed = seed, percentageOfSuccess = news.perc)
    blogs.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$blogsCorpus), seed = seed, percentageOfSuccess = blogs.perc)
    
    list(twitterCorpus = corpora$twitterCorpus[twitter.sampling == 1], 
         newsCorpus = corpora$newsCorpus[news.sampling == 1], 
         blogsCorpus = corpora$blogsCorpus[blogs.sampling == 1])
}

#corpora as a list(blogsCorpus = all)
sample_blogs_corpus <- function(corpora, seed, blogs.perc = 0.1){
    blogs.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$blogsCorpus), seed = seed, percentageOfSuccess = blogs.perc)
    list(blogsCorpus = corpora$blogsCorpus[blogs.sampling == 1])
}

#corpora as a list(newsCorpus = news.all)
sample_news_corpus <- function(corpora, seed, news.perc = 0.1){
    news.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$newsCorpus), seed = seed, percentageOfSuccess = news.perc)
    
    list(newsCorpus = corpora$newsCorpus[news.sampling == 1])
}

#corpora as a list(twitterCorpus = twitters)
sample_twitter_corpus <- function(corpora, seed, twitter.perc = 0.1){
    twitter.sampling <- biased_dice_outcome(noOfThrowings = length(corpora$twitterCorpus), seed = seed, percentageOfSuccess = twitter.perc)
    list(twitterCorpus = corpora$twitterCorpus[twitter.sampling == 1])
}

biased_dice_outcome <- function(seed, noOfThrowings, percentageOfSuccess = 0.5){
    set.seed(seed)
    rbinom(noOfThrowings, 1, percentageOfSuccess)
}

#corpora as a list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
corpora_remove_short_entries <- function(corpora, minNoOfChars = 20){
    lapply(corpora, corpus_remove_short_entries, minNoOfChars)
}

#corpus is a character vector
corpus_remove_short_entries <- function(corpus, minNoOfChars){
    corpus[nchar(corpus) >= minNoOfChars]
}

#corpora as a list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
corpora_text_cleaning <- function(corpora){
    
    print("corpora_cleaning::remove::gremlings(coerced to ASCII)")
    tmp <- lapply(corpora, iconv, from = localeToCharset(), to = "ASCII", "")
    
    print("corpora_cleaning::remove::RT_retweeted")
    tmp <- lapply(tmp, remove_RT_retweetted)
    
    print("corpora_cleaning::replace::remove_contractions")
    tmp <- lapply(tmp, remove_contractions)
    
    print("corpora_cleaning::normalize_abbreviation")
    tmp <- lapply(tmp, normalize_abbreviations)
    
    print("corpora_cleaning::manage_apostrophe")
    tmp <- lapply(tmp, manage_apostrophe)
    
    tmp
}

remove_RT_retweetted <- function(texts){
    gsub(pattern = "RT", replacement = " ", x = texts, ignore.case = F, perl = T)
}

## TODO Test the pattern is not working completely
# remove_links <- function(texts, ignoreCase = T){
#     gsub(pattern = "(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?", 
#          replacement = " ", x = texts, ignore.case = ignoreCase)
# }

remove_contractions <- function(theTexts){
    rem.contr.tmp <- replace_contraction(texts = theTexts, contraction = " u ", replaceWith = " you ", ignoreCase = T)
    rem.contr.tmp <- replace_contraction(texts = rem.contr.tmp, contraction = " r ", replaceWith = " are ", ignoreCase = T)
    rem.contr.tmp <- replace_contraction(texts = rem.contr.tmp, contraction = "c'mon", replaceWith = "come on",ignoreCase = T)
    rem.contr.tmp <- replace_contraction(texts = rem.contr.tmp, contraction = "doin'", replaceWith = "doing",ignoreCase = T)
    rem.contr.tmp <- replace_contraction(texts = rem.contr.tmp, contraction = "[yY]a?'a?ll", replaceWith = "you all",ignoreCase = T)
    rem.contr.tmp <- replace_contraction(texts = rem.contr.tmp, contraction = "ma'am", replaceWith = "madam",ignoreCase = T)
    rem.contr.tmp
}

replace_contraction <- function(texts, contraction, replaceWith, ignoreCase = F){
    gsub(pattern = contraction, replacement = replaceWith, x = texts, ignore.case = ignoreCase, perl = T)
}

normalize_abbreviations <- function(theTexts){
    tmp <- replace_contraction(texts = theTexts, contraction = "^u.s.([[:blank:]|[:punct:]])", replaceWith = "usa\\1", ignoreCase = T)
    tmp <- replace_contraction(texts = tmp, contraction = "([[:blank:]|[:punct:]])u.s.$", replaceWith = "\\1usa", ignoreCase = T)
    tmp <- replace_contraction(texts = tmp, contraction = "([[:blank:]|[:punct:]])u.s.([[:blank:]|[:punct:]])", replaceWith = "\\1usa\\2", ignoreCase = T)
    tmp
}

manage_apostrophe <- function(theTexts){
    tmp <- remove_multipleConsecutiveApostrophes(theTexts)
    normalize_wordsBetweenApostrophes(tmp)
}

normalize_wordsBetweenApostrophes <- function(theTexts){
    replace_contraction(texts = theTexts, contraction = "'([[:alpha:]]+)'", replaceWith = "\\1", ignoreCase = T)
}

remove_multipleConsecutiveApostrophes <- function(theTexts){
    replace_contraction(texts = theTexts, contraction = "'{2,}", replaceWith = "", ignoreCase = T)
}



## Profanity Words Loading list of words
con <- file("./../data/original/bad-words.txt", "r") 
stopwords.profanityWords <- readLines(con, skipNul = T)
close(con)
con <- NULL

#corpora as a list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
#return a tdm::Corpus
corpora_transform <- function(corpora){
    lapply(corpora, corpus_transform)
}

#return a tdm::Corpus
corpus_transform <- function(x){
    corpus <- Corpus(VectorSource(x))
    print("tolower...")
    corpus <- tm_map(corpus, content_transformer(tolower))
    print("removeWords::profanityWords...")
    corpus <- tm_map(corpus, removeWords, stopwords.profanityWords)
    print("removeNumbers...")
    corpus <- tm_map(corpus, removeNumbers) 
    print("removePunctuations.except apostrophe '...")
    corpus <- tm_map(corpus, content_transformer(removePunctuations.exceptApostrophe))
    print("addStartEndMarkers...")
    corpus <- tm_map(corpus, content_transformer(addStartEndMarkers))
    print("strpWhitespaces...")
    corpus <- tm_map(corpus, stripWhitespace)
    corpus
}

##In order to keep contractions like I'm or I'll ....
removePunctuations.exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

##Add start sentence (<s>) and end sentence (</s>) markers in the sentences
addStartEndMarkers <- function(texts){
    paste("<s>", texts, "</s>")
}


tdm.generate.ng <- function(corpus, ng = 1){
    # MAC OS Manadtory if not using doMC library
    #options(mc.cores=1) 
    ngramTokenizer <- function(y) NGramTokenizer(y, Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!")) 
    # create n-grams
    tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer, wordLengths = c(1, Inf)))
    tdm
}

getTermFrequencyInformationOrderedByTermFrequency <- function(aTdm, lowFrequency){
    ft.lf <- findFreqTerms(aTdm,lowfreq = lowFrequency)
    aTdm.l <- aTdm[ft.lf,]
    aTdm.l.asMatrix <- as.matrix(aTdm.l)
    # calculate frequency of each term
    aTdm.l.termFreq <- rowSums(aTdm.l.asMatrix)
    # create data frame from subset of terms
    aTdm.l.termFreq.df <- data.frame(term = names(aTdm.l.termFreq), freq = aTdm.l.termFreq)
    # sort by subset DataFrame frequency
    aTdm.l.termFreq.df[with(aTdm.l.termFreq.df, order(-aTdm.l.termFreq.df$freq)), ]
}

getAllTermsFrequencyInCorpora.as.df <- function(corpus.tdm, chunck = 2000){
    
    print(corpus.tdm)
    
    isFinished <- F
    i <- 0
    
    result <- NULL
    
    while (!isFinished){
        
        i.next <- i + 1
        start <- 1
        end <- chunck
        
        if(i != 0){
            start <- i * chunck + 1
            end <- i.next * chunck
            if((i.next * chunck) > dim(corpus.tdm)[1]){
                end <- dim(corpus.tdm)[1]
                isFinished <- T
            }
        }
        
        range <- start : end
        
        print(paste("Processing Chunck:", start, end))
        x <- corpus.tdm[range,]
        x.asMatrix <- as.matrix(x)
        
        result <- c(result, rowSums(x.asMatrix))
        
        i <- i + 1
    }
    
    allTermsFrequencies.df <- data.frame(freq = result)
}


getAllTermsFrequencyInCorpora.as.df.i <- function(corpus.tdm){
    all.terms <- findFreqTerms(corpus.tdm) #get all of the terms
    data.frame(freq = tm_term_score(x = corpus.tdm, terms = all.terms, FUN = slam::row_sums))
}


visualizeBarPlot <- function(ftm.df, colorBars = "grey40", titleBarPlot = ""){
    
    ggplot(ftm.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) +
        geom_bar(stat = "identity", fill=colorBars) +
        xlab("Terms") + ylab("Frequency (* 1000)")+
        ggtitle(paste(titleBarPlot, "(Top 30)"))  + coord_flip()
    
}

visualizeWordcloud <- function(ftm.df){
    mypal <- brewer.pal(8,"Dark2")
    wordcloud(words = ftm.df$term,
              freq = ftm.df$freq, 
              colors = mypal, 
              scale=c(6,.5),
              random.order = F, max.words = 200)
}

visualizeCumuluativeCoverage <- function(allFtm.df, title, filter = NULL){
    idx <- order(allFtm.df$freq, decreasing = T)
    terms.term <- rownames(allFtm.df)[idx]
    terms.freq <- allFtm.df$freq[idx]
    
    if(!is.null(filter)){
        # print(paste("Removing idx", filter))
        terms.term <- terms.term[-filter]
        terms.freq <- terms.freq[-filter]
    }
    
    terms.count <- sum(terms.freq)
    terms.unique <- length(unique(terms.term))
    terms.cumulativeCoverage <- (cumsum(terms.freq)/ terms.count) * 100
    
    
    plot(terms.cumulativeCoverage, type = "l", xlab = "Number Of Words", ylab = "% Coverage", main = title)
    abline(v = which(terms.cumulativeCoverage >= 50)[1], col = "orange", lwd = 3)
    abline(v = which(terms.cumulativeCoverage >= 90)[1], col = "red", lwd = 3)
    legend(x = "topright", lty=c(1,1), lwd=c(3,3), col=c("orange", "red"), legend = c("50% coverage", "90% coverage")) 
}

getSomeInfoABoutCorpora <- function(allFtm.df, title, filter = NULL){
    idx <- order(allFtm.df$freq, decreasing = T)
    terms.term <- rownames(allFtm.df)[idx]
    terms.freq <- allFtm.df$freq[idx]
    
    if(!is.null(filter)){
        # print(paste("Removing idx", filter))
        terms.term <- terms.term[-filter]
        terms.freq <- terms.freq[-filter]
    }
    
    terms.count <- sum(terms.freq)
    terms.unique <- length(unique(terms.term))
    terms.cumulativeCoverage <- (cumsum(terms.freq)/ terms.count) * 100
    coverage.50 <- which(terms.cumulativeCoverage >= 50)[1]
    coverage.90 <- which(terms.cumulativeCoverage >= 90)[1]
    
    list(N = terms.count, V = terms.unique, C50 = coverage.50, C90 = coverage.90)
}