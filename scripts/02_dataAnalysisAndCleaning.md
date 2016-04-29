# Assignment: Milestone Report (Capstone Project)
Pier Lorenzo Paracchini  
25 april 2016  



# Summary

The main objective of this __MIlestone Report__ is to display and explain only the major features of the data you have identified and briefly summarize your next" plans for creating the prediction algorithm and Shiny app behind the final product. Specifically as stated in the assignment description:

* Demonstrate that the data has been downloaded and loaded
* Create a basic report of summary statistics about the data sets (twitters, news, blogs)
* Report any interesting findings and eventual considerations/ implications 
* Get feedback on the "next" plans for creating a prediction algorithm and Shiny app

# The Data

The data is from a corpus called [HC Corpora](www.corpora.heliohost.org) and it can be downloded at the following [link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The corpora have been collected from publicly available sources by a web crawler and includes tweets, blogs and news in engligh, german, finnish and russian (separated in different set of files for each language).

From the information available at [About the Corpora](http://webcache.googleusercontent.com/search?q=cache:dzpVyq5etNYJ:www.corpora.heliohost.org/aboutcorpus.html+&cd=3&hl=en&ct=clnk&gl=us):

    ' You may still find lines of entirely different languages in the corpus. There are 2 main reasons for that: 1. Similar languages. Some languages are very similar, and the automatic language checker could therefore erroneously accept the foreign language text. 2. "Embedded" foreign languages. While a text may be mainly in the desired language there may be parts of it in another language. Since the text is then split up into individual lines, it is possible to see entire lines written in a foreign language.Whereas number 1 is just an out-and-out error, I think number 2 is actually desirable, as it will give a picture of when foreign language is used within the main language.'

Note! The focus of the analysis is on the __english language only ('en_US')__ - covering: tweets (twitter), news and blogs.



## Some basic statistics about the Corpora



sources     noOfLines   maxNoOfChar   minNoOfChar
---------  ----------  ------------  ------------
twitters      2360148           213             2
news          1010242         11384             1
blogs          899288         40835             1



Some considerations

* the __amount of data available__ considering the tweets, news and blogs entries. For simplification the __exporation__ has been done using a representative sample to infer facts about a population (considering also limitations connected with the available processing hardware)

* the minimum size, in terms of number of characters, for the different entries. There are tweets, news and blogs with few characters - __are they relevant__?

## Encoding Issues (Gremlings)

When loading the data the following locale/ encoding has been used __English_United States.1252 \ ISO8859-1__. Inspecting the loaded data it is possible to identify some encoding issues (gremlings) due to the unrecognized characters (not supported languages, emoticons, etc)

    'I'm doing it!ðŸ‘¦'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon AppÃ©tit'
    'Everything is good in its season é¬¼ã‚‚åå…«ç•ªèŒ¶ã‚‚å‡ºèŠ±'


In order to remove such gremlings the following __strategy__ and __simplification__ has been considered: limit the set of available characters to the __ASCII__ charset, removing non ASCII characters.



```r
##Using iconv to replace non ASCII char with an empty char
data.twitter.all.ascii <-  iconv(data.twitter.all, from = localeToCharset(), to = "ASCII", "")
data.news.all.ascii <-  iconv(data.news.all, from = localeToCharset(), to = "ASCII", "")
data.blogs.all.ascii <-  iconv(data.blogs.all, from = localeToCharset(), to = "ASCII", "")


##Reset working data after removing Gremlings
data.twitter.all <- data.twitter.all.ascii
data.twitter.all.nchar <- nchar(data.twitter.all)

data.news.all <- data.news.all.ascii
data.news.all.nchar <- nchar(data.news.all)

data.blogs.all <- data.blogs.all.ascii
data.blogs.all.nchar <- nchar(data.blogs.all)
```

    'I'm doing it!'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon Apptit'
    'Everything is good in its season '

## Entries with a limited number of chars

### Twitter Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/tweetsDistribution-1.png)

There are around 214097 tweets (0.09% ) that are less than 20 chars long. Few examples of such tweets can be found below:


```
##  [1] "send me beats fam"    "My moms so annoying!" "knowledge is power!" 
##  [4] "126 square blocks"    "thanks, love! :)"     "fun :D"              
##  [7] "oh no!"               "Missing my hubby..."  "M.O.B"               
## [10] "Gonna be a long day"  "Ok brotha thanks!!!"
```

Because of the limited number of such tweets and the "irrelevance" of their content (especially the ones with less than 10 chars), it has been decided to remove them from the __twitter corpora__.



### News Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/newsDistribution-1.png)

There are around 30644 news (0.03% ) that are less than 20 chars long. Few examples of such news can be found below:


```
##  [1] "BL  Knight 9."        "In other trading:"    "Drage Vukcevich"     
##  [4] "Chain"                "10. Youngstown"       "Aberdeen"            
##  [7] "Radio Radio"          "A gust of popularity" "(Ticker Tape)"       
## [10] "last."
```

Because of the limited number of such news and the "irrelevance" of their content , it has been decided to remove them from the __news corpora__.



### Blogs Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/blogsDistribution-1.png)

There are around 77241 blogs __(0.09%)__ that are less than __20 chars__ long. Few examples of such blogs can be found below:


```
##  [1] "If I were a bear,"    "Tis all."             "1/3 cup tomato paste"
##  [4] "3 T ketchup"          "M. Blakeman Ingle"    "Sphere: V = 4/3"     
##  [7] "Rm25"                 "So "                  "In shrouds of words,"
## [10] "You die?"
```

Because of the limited number of such blogs and the "irrelevance" of their content , it has been decided to remove them from the __blogs corpora__.





sources     noOfLines   maxNoOfChar   minNoOfChar
---------  ----------  ------------  ------------
twitters      2146051           140            21
news           979598         11384            21
blogs          822047         40832            21



## Removal of Profanity Words



Some examples ...

    !++-~| G1 CERTIFIED WET TSHIRT CONTEST --- FRIDAY CLUB DRAMA --WANT TO GET IN FOR FREE?? TXT ME I WILL TELL YOU HOW---214 609 3316 --
    Wisconsin Governor Walker Attacks Sex Ed



Profanity words will be removed from the Corpora (as stopwords). An external [resource](http://www.cs.cmu.edu/~biglou/resources/) providing a comprehensive list of __1383 profanity words__ is used.

## Others


```r
replaceContraction <- function(texts, contraction, replaceWith, ignoreCase = F){
    gsub(pattern = contraction, replacement = replaceWith, x = texts, ignore.case = ignoreCase)
}

remove_RT_retweetted <- function(texts, ignoreCase = T){
    a <- gsub(pattern = " RT :? ?", replacement = " ", x = texts, ignore.case = ignoreCase)
    gsub(pattern = "^RT ", replacement = "", x = a, ignore.case = ignoreCase)
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

# test.u <- "I'm coo... Jus at work hella tired are u ever in cali"
# test.u.expected <- "I'm coo... Jus at work hella tired are you ever in cali"
# test.u.expected == replaceContraction(texts = test.u, contraction = " u ", replaceWith = " you ", ignoreCase = T)
# 
# test.r <- "I'm coo... Jus at work hella tired r you ever in cali"
# test.r.expected <- "I'm coo... Jus at work hella tired are you ever in cali"
# test.r.expected == replaceContraction(texts = test.r, contraction = " r ", replaceWith = " are ", ignoreCase = T)
# 
# test.RT <- c("I'm cool... RT : Just at work RT tired r you ever in cali", "RT I'm cool...")
# test.RT.expected <- c("I'm cool... Just at work tired r you ever in cali", "I'm cool...")
# result <- remove_RT_retweetted(test.RT)
# test.RT.expected[1] == result[1]
# test.RT.expected[2] == result[2]

## Remove RT (retweet)
data.twitter.all <- remove_RT_retweetted(data.twitter.all)

## REmove Links
data.twitter.all <- remove_links(texts =  data.twitter.all)
data.news.all <- remove_links(texts =  data.news.all)
data.blogs.all <- remove_links(texts = data.blogs.all)

## Remove Contractions
data.twitter.all <- remove_contractions(theTexts = data.twitter.all)
data.news.all <- remove_contractions(theTexts = data.news.all)
data.blogs.all <- remove_contractions(theTexts = data.blogs.all)

save(stopwords.profanityWords, data.twitter.all, data.news.all, data.blogs.all, file = "./../data/processed/step1_afterCleaning.Rdata")
```


## Sampling of the Corpora



For this analysis it is not needed use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. A "biased coin" approach has been used to select the tweets, news and blogs to be included in the analysis based on the following percentages

* __5%__ of the tweets (__107110__ tweets)
* __10%__ of the news (__97775__ news)
* __10%__ of the blogs (__82168__ blogs)

# Exploring the (Sample) Corpora

Exploration of the corpora is done using __natural language processing techniques__ - specifically identify the most frequents ngrams (1-gram, 2-gram and 3-gram) using __term document matrices__. Before tokenizing the corpora the following steps are performed:

* transform to lower case
* remove profanity words
* remove numbers
* remove punctuations - except of the `'` (apostrophe) in order to not lose contractions (e.g. I'll, I'm, etc)




```r
twitter.corpora.tdm.1g <- tdm.generate.1g(data.twitter.sample)
save(twitter.corpora.tdm.1g, file = "./../data/processed/twitter.tdm.1g.Rdata")
rm(twitter.corpora.tdm.1g)

news.corpora.tdm.1g <- tdm.generate.1g(data.news.sample)
save(news.corpora.tdm.1g, file = "./../data/processed/news.tdm.1g.Rdata")
rm(news.corpora.tdm.1g)

blogs.corpora.tdm.1g <- tdm.generate.1g(data.blogs.sample)
save(blogs.corpora.tdm.1g, file = "./../data/processed/blogs.tdm.1g.Rdata")
rm(blogs.corpora.tdm.1g)

## Bi-grams
twitter.corpora.tdm.2g <- tdm.generate.ng(x = data.twitter.sample,ng = 2)
save(twitter.corpora.tdm.2g, file = "./../data/processed/twitter.tdm.2g.Rdata")
rm(twitter.corpora.tdm.2g)

news.corpora.tdm.2g <- tdm.generate.ng(x = data.news.sample, ng = 2)
save(news.corpora.tdm.2g, file = "./../data/processed/news.tdm.2g.Rdata")
rm(news.corpora.tdm.2g)

blogs.corpora.tdm.2g <- tdm.generate.ng(x = data.blogs.sample, ng = 2)
save(blogs.corpora.tdm.2g, file = "./../data/processed/blogs.tdm.2g.Rdata")
rm(blogs.corpora.tdm.2g)

## Tri-grams
twitter.corpora.tdm.3g <- tdm.generate.ng(x = data.twitter.sample,ng = 3)
save(twitter.corpora.tdm.3g, file = "./../data/processed/twitter.tdm.3g.Rdata")
rm(twitter.corpora.tdm.3g)

news.corpora.tdm.3g <- tdm.generate.ng(x = data.news.sample, ng = 3)
save(news.corpora.tdm.3g, file = "./../data/processed/news.tdm.3g.Rdata")
rm(news.corpora.tdm.3g)

blogs.corpora.tdm.3g <- tdm.generate.ng(x = data.blogs.sample, ng = 3)
save(blogs.corpora.tdm.3g, file = "./../data/processed/blogs.tdm.3g.Rdata")
rm(blogs.corpora.tdm.3g)
```

## Twitter Corpora


```r
load("./../data/processed/twitter.tdm.1g.Rdata")
load("./../data/processed/twitter.tdm.2g.Rdata")
load("./../data/processed/twitter.tdm.3g.Rdata")

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

corpora.tdm <- twitter.corpora.tdm.1g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 1500)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-1.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-2.png)

```r
corpora.tdm <- twitter.corpora.tdm.2g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 1000)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-3.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-4.png)

```r
corpora.tdm <- twitter.corpora.tdm.3g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.150 <- findFreqTerms(corpora.tdm,lowfreq = 150)
ft.lf.200 <- findFreqTerms(corpora.tdm,lowfreq = 200)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 150)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-5.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData-6.png)

## News Corpora


```r
load("./../data/processed/news.tdm.1g.Rdata")
load("./../data/processed/news.tdm.2g.Rdata")
load("./../data/processed/news.tdm.3g.Rdata")


corpora.tdm <- news.corpora.tdm.1g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)
ft.lf.2000 <- findFreqTerms(corpora.tdm,lowfreq = 2000)
ft.lf.2500 <- findFreqTerms(corpora.tdm,lowfreq = 2500)
ft.lf.3500 <- findFreqTerms(corpora.tdm,lowfreq = 3500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 3500)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-1.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-2.png)

```r
corpora.tdm <- news.corpora.tdm.2g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)
ft.lf.2000 <- findFreqTerms(corpora.tdm,lowfreq = 2000)
ft.lf.2500 <- findFreqTerms(corpora.tdm,lowfreq = 2500)
ft.lf.3500 <- findFreqTerms(corpora.tdm,lowfreq = 3500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 2000)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-3.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-4.png)

```r
corpora.tdm <- news.corpora.tdm.3g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.150 <- findFreqTerms(corpora.tdm,lowfreq = 150)
ft.lf.200 <- findFreqTerms(corpora.tdm,lowfreq = 200)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 300)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-5.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData-6.png)

## Blogs Corpora


```r
load("./../data/processed/blogs.tdm.1g.Rdata")
load("./../data/processed/blogs.tdm.2g.Rdata")
load("./../data/processed/blogs.tdm.3g.Rdata")


corpora.tdm <- blogs.corpora.tdm.1g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)
ft.lf.2000 <- findFreqTerms(corpora.tdm,lowfreq = 2000)
ft.lf.2500 <- findFreqTerms(corpora.tdm,lowfreq = 2500)
ft.lf.3500 <- findFreqTerms(corpora.tdm,lowfreq = 3500)
ft.lf.5000 <- findFreqTerms(corpora.tdm,lowfreq = 5000)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 5000)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-1.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-2.png)

```r
corpora.tdm <- blogs.corpora.tdm.2g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)
ft.lf.1000 <- findFreqTerms(corpora.tdm,lowfreq = 1000)
ft.lf.1500 <- findFreqTerms(corpora.tdm,lowfreq = 1500)
ft.lf.2000 <- findFreqTerms(corpora.tdm,lowfreq = 2000)
ft.lf.2500 <- findFreqTerms(corpora.tdm,lowfreq = 2500)
ft.lf.3500 <- findFreqTerms(corpora.tdm,lowfreq = 3500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 2500)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-3.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-4.png)

```r
corpora.tdm <- blogs.corpora.tdm.3g

ft.lf.100 <- findFreqTerms(corpora.tdm,lowfreq = 100)
ft.lf.150 <- findFreqTerms(corpora.tdm,lowfreq = 150)
ft.lf.200 <- findFreqTerms(corpora.tdm,lowfreq = 200)
ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)
ft.lf.500 <- findFreqTerms(corpora.tdm,lowfreq = 500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 300)
ggplot(frequentTermsLimited.df[1:30,], aes(x = reorder(term,freq), y = freq/1000)) + 
        geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency (* 1000)") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-5.png)

```r
wordcloud(words = frequentTermsLimited.df$term,freq = frequentTermsLimited.df$freq)
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): going to be could not be fit on page. It
## will not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): there is a could not be fit on page. It will
## not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): you have to could not be fit on page. It
## will not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): it was a could not be fit on page. It will
## not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): one of my could not be fit on page. It will
## not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsLimited.df$term, freq =
## frequentTermsLimited.df$freq): the fact that could not be fit on page. It
## will not be plotted.
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData-6.png)


```r
#load("./../data/processed/datasets_sample.Rdata")

# require(tm)
# require(wordcloud)
# require(RWeka)
# require(ggplot2)

# removePunctuations.exceptApostrophe <- function(texts){
#     gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
# }


# test <- "I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
# test.expected <- "I like    to    chew   gum  but don't like      bubble      gum  "
# test.expected == removePunctuations.exceptApostrophe(texts = test)

# tdm.generate <- function(x){
#   corpus <- Corpus(VectorSource(x))
#   corpus <- tm_map(corpus, content_transformer(tolower))
#   #corpus <- tm_map(corpus, removeWords, stopwords.badWords)
#   corpus <- tm_map(corpus, removeNumbers) 
#   corpus <- tm_map(corpus, content_transformer(removePunctuations.exceptApostrophe))
#   corpus <- tm_map(corpus, stripWhitespace)
#   tdm <- TermDocumentMatrix(corpus)
#   tdm
# }


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

