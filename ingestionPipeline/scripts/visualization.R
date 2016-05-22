require(wordcloud)
require(ggplot2)
require(RColorBrewer)
require(tm)

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