# Assignment: Milestone Report (Capstone Prject)
Pier Lorenzo Paracchini  
25 april 2016  



## Summary

The main objective of this __MIlestone Report__ is to display and explain only the major features of the data you have identified and briefly summarize your next" plans for creating the prediction algorithm and Shiny app behind the final product. Specifically as stated in the assignment description:

* Demonstrate that the data has been downloaded and loaded
* Create a basic report of summary statistics about the data sets (twitters, news, blogs)
* Report any interesting findings and eventual considerations/ implications 
* Get feedback on the "next" plans for creating a prediction algorithm and Shiny app

## The Data

The data is from a corpus called [HC Corpora](www.corpora.heliohost.org) and it can be downloded at the following [link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The corpora have been collected from publicly available sources by a web crawler and includes tweets, blogs and news in engligh, german, finnish and russian (separated in different set of files for each language).

From the iformation available [About the Corpora](http://webcache.googleusercontent.com/search?q=cache:dzpVyq5etNYJ:www.corpora.heliohost.org/aboutcorpus.html+&cd=3&hl=en&ct=clnk&gl=us):

    ' You may still find lines of entirely different languages in the corpus. There are 2 main reasons for that: 1. Similar languages. Some languages are very similar, and the automatic language checker could therefore erroneously accept the foreign language text. 2. "Embedded" foreign languages. While a text may be mainly in the desired language there may be parts of it in another language. Since the text is then split up into individual lines, it is possible to see entire lines written in a foreign language.Whereas number 1 is just an out-and-out error, I think number 2 is actually desirable, as it will give a picture of when foreign language is used within the main language.'

Note! The focus of the analysis has been on the english language only ('en_US') - covering: tweets (twitter), news and blogs.



### Some basic statics about the data



sources     noOfLines   maxNoOfChar   minNoOfChar
---------  ----------  ------------  ------------
twitters      2360148           213             2
news          1010242         11384             1
blogs          899288         40835             1



Some considerations

* Amount of data available considering the tweets, news and blogs entry - such analysis is done using a representative sample can be used to infer facts about a population (considering limitations connected with the available processing power and hardware)
* The minimum size, in terms of number of characters, for teh different entries. There are tweets, news and blogs with few characters - are they relevant?

### Encoding Issues (Gremlings)

When loading the data using the following locale/ encoding has been used __English_United States.1252 \ `rlocaleToCharset()`__. Inspecting the loaded data it is possible to identify some encoding issues (gremlings) due to the unrecognized characters (not supported languages, emoticons, etc)

    'I'm doing it!ðŸ‘¦'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon AppÃ©tit'
    'Everything is good in its season é¬¼ã‚‚åå…«ç•ªèŒ¶ã‚‚å‡ºèŠ±'


In order to remove such gremlings the following __strategy__ and __simplification__ has been considered: limit the set of available characters to the __ASCII__ charset, removing non ASCII characters.



```r
##Using iconv to replace non ASCII char with an empty char
data.twitter.all.ascii <-  iconv(data.twitter.all, from = localeToCharset(), to = "ASCII", "")
data.news.all.ascii <-  iconv(data.news.all, from = localeToCharset(), to = "ASCII", "")
data.blogs.all.ascii <-  iconv(data.blogs.all, from = localeToCharset(), to = "ASCII", "")

data.twitter.all <- data.twitter.all.ascii
data.news.all <- data.news.all.ascii
data.blogs.all <- data.blogs.all.ascii
```

    'I'm doing it!'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon Apptit'
    'Everything is good in its season '




```r
data.sources <- c("twitters", "news", "blogs")
data.noOfLines <- c(length(data.twitter.all), length(data.news.all), length(data.blogs.all))
data.maxNoOfChars <- c(max(data.twitter.all.nchar), max(data.news.all.nchar), max(data.blogs.all.nchar))
data.minNoOfChars <- c(min(data.twitter.all.nchar), min(data.news.all.nchar), min(data.blogs.all.nchar))

data.info.df <- data.frame(sources = data.sources, 
                           noOfLines = data.noOfLines, 
                           maxNoOfChar = data.maxNoOfChars,
                           minNoOfChar = data.minNoOfChars)

data.info.df

#######Twitters
hist(nchar(data.twitter.all), main = "No Of Chars per Twitter")

#How many twitter are under 20 chars?
sum(data.twitter.all.nchar <= 20)
twitter.lessThanOr3chars <- data.twitter.all[data.twitter.all.nchar <= 20]
head(twitter.lessThanOr3chars, 20)
tail(twitter.lessThanOr3chars, 20)

#Removing such short tweets
data.twitter.all <- data.twitter.all[data.twitter.all.nchar > 20]
data.twitter.all.nchar <- nchar(data.twitter.all)

twitter.df <- data.frame(text = data.twitter.all, nchar = data.twitter.all.nchar)

##What about twitters more than 140? Analysis

######News
hist(nchar(data.news.all), main = "No Of Chars per News")

#Let's focus on news
a <- data.news.all.nchar < 1500
sum(a)
hist(nchar(data.news.all[a]), main = "No Of Chars per News (< 1500)", breaks = 100)

sum(!a)
hist(nchar(data.news.all[!a]), main = "No Of Chars per News (>= 1500)", breaks = 100)

a <- data.news.all.nchar <= 20
sum(a)
head(data.news.all[a], 40)
tail(data.news.all[a], 40)

#Removing such short news
data.news.all <- data.news.all[data.news.all.nchar > 20]
data.news.all.nchar <- nchar(data.news.all)

news.df <- data.frame(text = data.news.all, nchar = data.news.all.nchar)

######Blogs
hist(nchar(data.blogs.all), main = "Data Blogs No Of Chars per Blogs")

a <- data.blogs.all.nchar < 1300
sum(a)
hist(nchar(data.blogs.all[a]), main = "Data Blogs No Of Chars per Blogs (< 1300)")

sum(!a)
hist(nchar(data.blogs.all[!a]), main = "Data Blogs No Of Chars per Blogs (>= 1300)", breaks = 10000)

a <- data.blogs.all.nchar <= 20
sum(a)
head(data.blogs.all[a], 40)
tail(data.blogs.all[a], 40)

#Removing such short blogss
data.blogs.all <- data.blogs.all[data.blogs.all.nchar > 20]
data.blogs.all.nchar <- nchar(data.blogs.all)

blogs.df <- data.frame(text = data.blogs.all, nchar = data.blogs.all.nchar)

data1.sources <- c("twitters", "news", "blogs")
data1.noOfLines <- c(length(data.twitter.all), length(data.news.all), length(data.blogs.all))
data1.maxNoOfChars <- c(max(data.twitter.all.nchar), max(data.news.all.nchar), max(data.blogs.all.nchar))
data1.minNoOfChars <- c(min(data.twitter.all.nchar), min(data.news.all.nchar), min(data.blogs.all.nchar))

data1.info.df <- data.frame(sources = data1.sources, 
                           noOfLines = data1.noOfLines, 
                           maxNoOfChar = data1.maxNoOfChars,
                           minNoOfChar = data1.minNoOfChars)
data1.info.df
```



### Sampling


```r
set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.twitter.all), 1, 0.10)
table(coin.biased.outcome)
data.twitter.sample <- data.twitter.all[coin.biased.outcome == 1]

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.news.all), 1, 0.20)
table(coin.biased.outcome)
data.news.sample <- data.news.all[coin.biased.outcome == 1]

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.blogs.all), 1, 0.20)
table(coin.biased.outcome)
data.blogs.sample <- data.blogs.all[coin.biased.outcome == 1]

save(data.twitter.sample, data.news.sample, data.blogs.sample, file = "./../data/processed/datasets_sample.Rdata")
```


```r
load("./../data/processed/datasets_sample.Rdata")

require(tm)
require(wordcloud)
require(RWeka)
require(ggplot2)


removePunctuations.exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

test <- "I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
test.expected <- "I like    to    chew   gum  but don't like      bubble      gum  "
test.expected == removePunctuations.exceptApostrophe(texts = test)

tdm.generate <- function(x){
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removeWords, stopwords.badWords)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, content_transformer(removePunctuations.exceptApostrophe))
  corpus <- tm_map(corpus, stripWhitespace)
  tdm <- TermDocumentMatrix(corpus)
  tdm
}


tdm.1g <- tdm.generate(data.twitter.sample[1:50000])
tdm.1g

findFreqTerms(tdm.1g,lowfreq = 100)
findFreqTerms(tdm.1g,lowfreq = 300)
findFreqTerms(tdm.1g,lowfreq = 500)

a <- findFreqTerms(tdm.1g,lowfreq = 500)
tdm.1g.l <- tdm.1g[a,]
tdm.1g.l
tdm.1g.l.asMatrix <- as.matrix(tdm.1g.l)

frequentTerms<-findFreqTerms(tdm.1g,lowfreq = 500)
highFreqTerms <- findFreqTerms(tdm.1g, lowfreq = 1000)

# calculate frequency of each term
term.freq <- rowSums(tdm.1g.l.asMatrix)

# picking only a subset
subsetterm.freq <- subset(term.freq, term.freq >= 700)

# create data frame from subset of terms
frequentTermsSubsetDF <- data.frame(term = names(subsetterm.freq), freq = subsetterm.freq)

# create data frame with all terms
frequentTermsDF <- data.frame(term = names(term.freq), freq = term.freq)

# sort by subset DataFrame frequency
frequentTermsSubsetDF <- frequentTermsSubsetDF[with(frequentTermsSubsetDF, order(-frequentTermsSubsetDF$freq)), ]

# sort by complete DataFrame frequency
frequentTermsDF <- frequentTermsDF[with(frequentTermsDF, order(-frequentTermsDF$freq)), ]

# words by frequency from subset data frame
ggplot(frequentTermsSubsetDF, aes(x = reorder(term,freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency") + coord_flip()
wordcloud(words = frequentTermsSubsetDF$term,freq = frequentTermsSubsetDF$freq)


tdm.generate.ngrams <- function(x, ng){
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, removeWords, stopwords.badWords)
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, content_transformer(removePunctuations.exceptApostrophe))
  corpus <- tm_map(corpus, stripWhitespace)
  # MAC OS Manadtory if not using doMC library
  #options(mc.cores=1) 
  ngramTokenizer <- function(y) NGramTokenizer(y, Weka_control(min = ng, max = ng, delimiters = " \\r\\n\\t.,;:\"()?!")) 
  # create n-grams
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer)) # create tdm from n-grams
  tdm
}

tdm.2g <- tdm.generate.ngrams(x = data.twitter.sample[1:50000],ng = 2)
tdm.2g

findFreqTerms(tdm.2g,lowfreq = 100)
findFreqTerms(tdm.2g,lowfreq = 200)
findFreqTerms(tdm.2g,lowfreq = 300)
findFreqTerms(tdm.2g,lowfreq = 500)

b <- findFreqTerms(tdm.2g,lowfreq = 300)
tdm.2g.l <- tdm.2g[b,]
tdm.2g.l
tdm.2g.l.asMatrix <- as.matrix(tdm.2g.l)

term.2g.freq <- rowSums(tdm.2g.l.asMatrix)
frequentTermsDF.2g <- data.frame(term = names(term.2g.freq), freq = term.2g.freq)

frequentTermsDF.2g <- frequentTermsDF.2g[with(frequentTermsDF.2g, order(-frequentTermsDF.2g$freq)), ]

# words by frequency from subset data frame
ggplot(frequentTermsDF.2g, aes(x = reorder(term,freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency") + coord_flip()
wordcloud(words = frequentTermsDF.2g$term,freq = frequentTermsDF.2g$freq)


tdm.3g <- tdm.generate.ngrams(x = data.twitter.sample[1:50000],ng = 3)
tdm.3g

findFreqTerms(tdm.3g,lowfreq = 30)
findFreqTerms(tdm.3g,lowfreq = 40)
findFreqTerms(tdm.3g,lowfreq = 50)
findFreqTerms(tdm.3g,lowfreq = 60)

c <- findFreqTerms(tdm.3g,lowfreq = 50)
tdm.3g.l <- tdm.3g[c,]
tdm.3g.l
tdm.3g.l.asMatrix <- as.matrix(tdm.3g.l)

term.3g.freq <- rowSums(tdm.3g.l.asMatrix)
frequentTermsDF.3g <- data.frame(term = names(term.3g.freq), freq = term.3g.freq)

frequentTermsDF.3g <- frequentTermsDF.3g[with(frequentTermsDF.3g, order(-frequentTermsDF.3g$freq)), ]

# words by frequency from subset data frame
ggplot(frequentTermsDF.3g, aes(x = reorder(term,freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency") + coord_flip()
wordcloud(words = frequentTermsDF.3g$term,freq = frequentTermsDF.3g$freq)
```

