# Twitter Corpora Creation Sample 2
Pier Lorenzo Paracchini  
4 mai 2016  



## Twitter Corpora Sample 2

# Exploring the (Sample) Corpora

Exploration of the corpora is done using __natural language processing techniques__ - specifically term frequency analysis using ngrams (1-gram, 2-gram and 3-gram). Before tokenizing the corpora the following steps are performed:

* transform to lower case
* remove profanity words
* remove numbers
* remove punctuations - except of the `'` (apostrophe) in order to not lose contractions (e.g. I'll, I'm, etc)
* add a `<s> ` marker at the beginning of each entry (tweet, news, blog)
* add a ` </s>` marker at the end of each entry (tweet, news, blog) 

`Wordclouds` and `barplots` are used to visualize the most frequent words/ tokens for the different n-grams. When plotting the 'barplots' only the first most frequent terms (top 30) are shown and max 200 terms in the wordclouds. __Note:__ For 2-grams and 3-grams a token like `<s> at the` refers to `at the` at the beginning of the entry (tweet, news or blog), while `the top </s>` refers to `the top` at the end of the entry (tweet, news or blog).


```r
rm(list = ls())

##In order to keep contractions like I'm or I'll ....
removePunctuations.exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

test <- "I'm I'll I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
test.expected <- "I'm I'll I like    to    chew   gum  but don't like      bubble      gum  "
result <- removePunctuations.exceptApostrophe(texts = test)
test.expected == result
result


##Add start sentence (<s>) and end sentence (</s>) markers in the sentences
addStartEndMarkers <- function(texts){
    paste("<s>", texts, "</s>")
}

test <- c("I love nlp.", "I like the sea.")
test.expected <- c("<s> I love nlp. </s>", "<s> I like the sea. </s>")
result <- addStartEndMarkers(texts = test)
test.expected == result
result

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
```


```r
load(file = "./tmp/twitter_step2_sample.2.Rdata")


#Unigrams
twitter.corpora.transformed <- corpora.transform(data.twitter.sample)
lapply(twitter.corpora.transformed[1:5], as.character)
save(twitter.corpora.transformed, file = "./tmp/twitter.sample2.corpora.transformed.Rdata")

twitter.corpora.tdm.1g <- tdm.generate.ng(twitter.corpora.transformed)
twitter.corpora.tdm.1g
lapply(twitter.corpora.transformed[1:5], as.character)
save(twitter.corpora.tdm.1g, file = "./tmp/twitter.sample2.tdm.1g.Rdata")
rm(twitter.corpora.tdm.1g)


## Bi-grams
twitter.corpora.tdm.2g <- tdm.generate.ng(twitter.corpora.transformed,ng = 2)
twitter.corpora.tdm.2g
lapply(twitter.corpora.transformed[1:5], as.character)
save(twitter.corpora.tdm.2g, file = "./tmp/twitter.sample2.tdm.2g.Rdata")
rm(twitter.corpora.tdm.2g)

## Tri-grams
twitter.corpora.tdm.3g <- tdm.generate.ng(twitter.corpora.transformed,ng = 3)
twitter.corpora.tdm.3g
lapply(twitter.corpora.transformed[1:5], as.character)
save(twitter.corpora.tdm.3g, file = "./tmp/twitter.sample2.tdm.3g.Rdata")
rm(twitter.corpora.tdm.3g)

rm(list = ls())
```



## Twitter Corpora


```r
rm(list = ls())
load("./tmp/twitter.sample2.tdm.1g.Rdata")
load("./tmp/twitter.sample2.tdm.2g.Rdata")
load("./tmp/twitter.sample2.tdm.3g.Rdata")

load("./tmp/twitter.sample2.allTermsFrequency.1g.Rdata")
twitter.allTerms.1g <- corpora.allTermsFrequency
corpora.allTermsFrequency <- NULL

load("./tmp/twitter.sample2.allTermsFrequency.2g.Rdata")
twitter.allTerms.2g <- corpora.allTermsFrequency
corpora.allTermsFrequency <- NULL

load("./tmp/twitter.sample2.allTermsFrequency.3g.Rdata")
twitter.allTerms.3g <- corpora.allTermsFrequency
corpora.allTermsFrequency <- NULL
```


### 1-grams


```r
require(tm)
require(wordcloud)
require(RWeka)
require(ggplot2)
require(RColorBrewer)

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


corpora.tdm <- twitter.corpora.tdm.1g

ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 1500)

visualizeBarPlot(ftm.df = frequentTermsLimited.df[-c(1,2),], titleBarPlot = "Frequent 1-grams")
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_1g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df[-c(1,2),])
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_1g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = twitter.allTerms.1g, filter = c(1:2))
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1430395 | 60256 | 121 | 4996 |



```r
visualizeCumuluativeCoverage(allFtm.df = twitter.allTerms.1g, title = "% Coverage By no of Unique Words (1-grams)", filter = c(1:2))
```

![](02_01_TwittersCorporaSample_2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 2-grams


```r
corpora.tdm <- twitter.corpora.tdm.2g

ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 1000)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 2-grams")
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_2g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_2g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = twitter.allTerms.2g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1537798 | 547201 | 16860 | 393422 |



```r
visualizeCumuluativeCoverage(allFtm.df = twitter.allTerms.2g, title = "% Coverage By no of Unique Words (2-grams)")
```

![](02_01_TwittersCorporaSample_2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 3-grams


```r
corpora.tdm <- twitter.corpora.tdm.3g

ft.lf.150 <- findFreqTerms(corpora.tdm,lowfreq = 150)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 150)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 3-grams")
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_3g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_01_TwittersCorporaSample_2_files/figure-html/visualizeTwitterData_3g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = twitter.allTerms.3g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1430395 | 1064700 | 349503 | 921661 |



```r
visualizeCumuluativeCoverage(allFtm.df = twitter.allTerms.3g, title = "% Coverage By no of Unique Words (3-grams)")
```

![](02_01_TwittersCorporaSample_2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
