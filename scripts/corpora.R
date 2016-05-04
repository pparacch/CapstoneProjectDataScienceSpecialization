
require(tm)
require(wordcloud)
require(RWeka)
require(ggplot2)
require(RColorBrewer)

replaceContraction <- function(texts, contraction, replaceWith, ignoreCase = F){
    gsub(pattern = contraction, replacement = replaceWith, x = texts, ignore.case = ignoreCase)
}

remove_RT_retweetted <- function(texts){
    gsub(pattern = "RT", replacement = " ", x = texts, ignore.case = F)
}

remove_links <- function(texts, ignoreCase = T){
    gsub(pattern = "^(https?:\\/\\/)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\/\\w \\.-]*)*\\/?$", 
         replacement = " ", x = texts, ignore.case = ignoreCase)
}

remove_contractions <- function(theTexts){
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = " u ", replaceWith = " you ", ignoreCase = T)
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = " r ", replaceWith = " are ", ignoreCase = T)
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = "c'mon", replaceWith = "come on",ignoreCase = T)
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = "doin'", replaceWith = "doing",ignoreCase = T)
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = "[yY]a?'a?ll", replaceWith = "you all",ignoreCase = T)
    rem.contr.tmp <- replaceContraction(texts = theTexts, contraction = "ma'am", replaceWith = "madam",ignoreCase = T)
    rem.contr.tmp
}


##In order to keep contractions like I'm or I'll ....
removePunctuations.exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

# test <- "I'm I'll I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
# test.expected <- "I'm I'll I like    to    chew   gum  but don't like      bubble      gum  "
# result <- removePunctuations.exceptApostrophe(texts = test)
# test.expected == result
# result

##Add start sentence (<s>) and end sentence (</s>) markers in the sentences
addStartEndMarkers <- function(texts){
    paste("<s>", texts, "</s>")
}

# test <- c("I love nlp.", "I like the sea.")
# test.expected <- c("<s> I love nlp. </s>", "<s> I like the sea. </s>")
# result <- addStartEndMarkers(texts = test)
# test.expected == result
# result

## Removal of Profanity Words
con <- file("./../data/original/bad-words.txt", "r") 
stopwords.profanityWords <- readLines(con, skipNul = T)
close(con)

corpora.transform <- function(x){
    corpus <- Corpus(VectorSource(x))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeWords, stopwords.profanityWords)
    corpus <- tm_map(corpus, removeNumbers) 
    corpus <- tm_map(corpus, content_transformer(removePunctuations.exceptApostrophe))
    corpus <- tm_map(corpus, content_transformer(addStartEndMarkers))
    corpus <- tm_map(corpus, stripWhitespace)
    corpus
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

getAllTermsFrequencyInCorpora.as.df <- function(corpora.tdm, chunck = 2000){
    
    print(corpora.tdm)
    
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
            if((i.next * chunck) > dim(corpora.tdm)[1]){
                end <- dim(corpora.tdm)[1]
                isFinished <- T
            }
        }
        
        range <- start : end
        
        print(paste("Processing Chunck:", start, end))
        x <- corpora.tdm[range,]
        x.asMatrix <- as.matrix(x)
        
        result <- c(result, rowSums(x.asMatrix))
        
        i <- i + 1
    }
    
    allTermsFrequencies.df <- data.frame(freq = result)
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