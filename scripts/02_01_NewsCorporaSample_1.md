# News Corpora Creation
Pier Lorenzo Paracchini  
4 mai 2016  




```r
rm(list = ls())
source("./corpora.R")

con <- file("./../data/original/final/en_US/en_US.news.txt", "r") 
data.news.all <- readLines(con, skipNul = T)
close(con)

data.news.all.nchar <- nchar(data.news.all)

data.news.all[200:203]

##Using iconv to replace non ASCII char with an empty char
data.news.all.ascii <-  iconv(data.news.all, from = localeToCharset(), to = "ASCII", "")


##Reset working data after removing Gremlings
data.news.all <- data.news.all.ascii
data.news.all.nchar <- nchar(data.news.all)

data.news.all[200:203]


#Let's focus on news
a <- data.news.all.nchar < 1500
par(mfrow=c(1,2))
hist(nchar(data.news.all[a]), main = "No Of Chars per News (< 1500)", breaks = 100, xlab = "no of characters")
hist(nchar(data.news.all[!a]), main = "No Of Chars per News (>= 1500)", breaks = 100, xlab = "no of characters")

shortNews.maxNoOfChar <- 20
a <- data.news.all.nchar <= shortNews.maxNoOfChar
head(data.news.all[a], 10)

#Removing such short news
data.news.all <- data.news.all[data.news.all.nchar > shortNews.maxNoOfChar]
data.news.all.nchar <- nchar(data.news.all)

head(data.news.all, 10)

## REmove Links
data.news.all <- remove_links(texts =  data.news.all)
tail(data.news.all)

## Remove Contractions
data.news.all <- remove_contractions(theTexts = data.news.all)
tail(data.news.all)

data.news.all.df <- data.frame(data.news.all)
save(data.news.all, data.news.all.df, file = "./tmp/news_step1_all.Rdata")
rm(list = ls())
```


```r
rm(list = ls())
source("corpora.R")
load(file = "./tmp/news_step1_all.Rdata")

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.news.all), 1, 0.05)
#table(coin.biased.outcome)
data.news.sample <- data.news.all[coin.biased.outcome == 1]

save(data.news.sample, file = "./tmp/news_step2_sample.Rdata")
rm(list = ls())
```





## News Corpora


```r
source("corpora.R")
load("./tmp/news.sample.tdm.1g.Rdata")
load("./tmp/news.sample.tdm.2g.Rdata")
load("./tmp/news.sample.tdm.3g.Rdata")

load("./tmp/news.sample.allTermsFrequency.1g.Rdata")
news.allTerms.1g <- corpora.allTermsFrequency

load("./tmp/news.sample.allTermsFrequency.2g.Rdata")
news.allTerms.2g <- corpora.allTermsFrequency

load("./tmp/news.sample.allTermsFrequency.3g.Rdata")
news.allTerms.3g <- corpora.allTermsFrequency
```

### 1-grams


```r
corpora.tdm <- news.corpora.tdm.1g

ft.lf.3500 <- findFreqTerms(corpora.tdm,lowfreq = 3500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 3500)
visualizeBarPlot(ftm.df = frequentTermsLimited.df[-c(2,3),],titleBarPlot = "Frequent 1-grams")
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_1g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df[-c(2,3),])
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_1g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = news.allTerms.1g, filter = c(2,3))
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1669187 | 63992 | 185 | 7579 |


```r
visualizeCumuluativeCoverage(allFtm.df = news.allTerms.1g, title = "% Coverage By no of Unique Words (1-grams)", filter = c(2,3))
```

![](02_01_NewsCorporaSample_1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 2-grams


```r
corpora.tdm <- news.corpora.tdm.2g

ft.lf.2000 <- findFreqTerms(corpora.tdm,lowfreq = 2000)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 2000)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 2-grams")
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_2g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_2g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = news.allTerms.2g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1717871 | 689190 | 37292 | 517403 |



```r
visualizeCumuluativeCoverage(allFtm.df = news.allTerms.2g, title = "% Coverage By no of Unique Words  (2-grams)")
```

![](02_01_NewsCorporaSample_1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 3-grams


```r
corpora.tdm <- news.corpora.tdm.3g

ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 300)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 3-grams")
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_3g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_01_NewsCorporaSample_1_files/figure-html/visualizeNewsData_3g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = news.allTerms.3g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1669187 | 1336386 | 501793 | 1169468 |



```r
visualizeCumuluativeCoverage(allFtm.df = news.allTerms.3g, title = "% Coverage By no of Unique Words (3-grams)")
```

![](02_01_NewsCorporaSample_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
