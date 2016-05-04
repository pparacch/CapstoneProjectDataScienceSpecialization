# Blogs Corpora Creation
Pier Lorenzo Paracchini  
4 mai 2016  




```r
rm(list = ls())
source("./corpora.R")

con <- file("./../data/original/final/en_US/en_US.blogs.txt", "r") 
data.blogs.all <- readLines(con, skipNul = T)
close(con)

data.blogs.all.nchar <- nchar(data.blogs.all)

data.blogs.all[200:203]

##Using iconv to replace non ASCII char with an empty char
data.blogs.all.ascii <-  iconv(data.blogs.all, from = localeToCharset(), to = "ASCII", "")


##Reset working data after removing Gremlings
data.blogs.all <- data.blogs.all.ascii
data.blogs.all.nchar <- nchar(data.blogs.all)

data.blogs.all[200:203]


#Let's focus on 
a <- data.blogs.all.nchar < 1300
par(mfrow=c(1,2))
hist(nchar(data.blogs.all[a]), main = "No Of Chars per Blogs (< 1300)", xlab = "no of characters")
hist(nchar(data.blogs.all[!a]), main = "No Of Chars per Blogs (>= 1300)", breaks = 10000, xlab = "no of characters")

shortBlogs.maxNoOfChar <- 20

a <- data.blogs.all.nchar <= shortBlogs.maxNoOfChar
head(data.blogs.all[a], 10)

#Removing such short blogss
data.blogs.all <- data.blogs.all[data.blogs.all.nchar > shortBlogs.maxNoOfChar]
data.blogs.all.nchar <- nchar(data.blogs.all)

head(data.blogs.all, 10)

## REmove Links
data.blogs.all <- remove_links(texts =  data.blogs.all)
data.blogs.all.nchar <- nchar(data.blogs.all)
tail(data.blogs.all)

## Remove Contractions
data.blogs.all <- remove_contractions(theTexts = data.blogs.all)
data.blogs.all.nchar <- nchar(data.blogs.all)
tail(data.blogs.all)

data.blogs.all.df <- data.frame(data.blogs.all)
save(data.blogs.all, data.blogs.all.df, file = "./tmp/blogs_step1_all.Rdata")
rm(list = ls())
```


```r
rm(list = ls())
source("corpora.R")
load(file = "./tmp/blogs_step1_all.Rdata")

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.blogs.all), 1, 0.05)
#table(coin.biased.outcome)
data.blogs.sample <- data.blogs.all[coin.biased.outcome == 1]

save(data.blogs.sample, file = "./tmp/blogs_step2_sample.Rdata")
rm(list = ls())
```





## Blogs Corpora


```r
rm(list = ls())
source("corpora.R")

load("./tmp/blogs.sample.tdm.1g.Rdata")
load("./tmp/blogs.sample.tdm.2g.Rdata")
load("./tmp/blogs.sample.tdm.3g.Rdata")

load("tmp/blogs.sample.allTermsFrequency.1g.Rdata")
blogs.allTerms.1g <- corpora.allTermsFrequency

load("tmp/blogs.sample.allTermsFrequency.2g.Rdata")
blogs.allTerms.2g <- corpora.allTermsFrequency

load("tmp/blogs.sample.allTermsFrequency.3g.Rdata")
blogs.allTerms.3g <- corpora.allTermsFrequency
```

### 1-grams


```r
corpora.tdm <- blogs.corpora.tdm.1g

ft.lf.5000 <- findFreqTerms(corpora.tdm,lowfreq = 5000)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 5000)

visualizeBarPlot(ftm.df = frequentTermsLimited.df[-c(3,4),],titleBarPlot = "Frequent 1-grams")
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_1g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df[-c(3,4),])
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_1g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = blogs.allTerms.1g, filter = c(3,4))
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1803556 | 63955 | 108 | 6103 |



```r
visualizeCumuluativeCoverage(allFtm.df = blogs.allTerms.1g, title = "% Coverage By no of Unique Words (1-grams)", filter = c(3,4))
```

![](02_03_BlogsCorporaSample_1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 2-grams


```r
corpora.tdm <- blogs.corpora.tdm.2g

ft.lf.2500 <- findFreqTerms(corpora.tdm,lowfreq = 2500)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 2500)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 2-grams")
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_2g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_2g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = blogs.allTerms.2g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1860654 | 674854 | 24955 | 488789 |



```r
visualizeCumuluativeCoverage(allFtm.df = blogs.allTerms.2g, title = "% Coverage By no of Unique Words (2-grams)")
```

![](02_03_BlogsCorporaSample_1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### 3-grams


```r
corpora.tdm <- blogs.corpora.tdm.3g

ft.lf.300 <- findFreqTerms(corpora.tdm,lowfreq = 300)

frequentTermsLimited.df <- getTermFrequencyInformationOrderedByTermFrequency(corpora.tdm, 300)

visualizeBarPlot(ftm.df = frequentTermsLimited.df,titleBarPlot = "Frequent 3-grams")
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_3g-1.png)<!-- -->

```r
visualizeWordcloud(ftm.df = frequentTermsLimited.df)
```

![](02_03_BlogsCorporaSample_1_files/figure-html/visualizeBlogsData_3g-2.png)<!-- -->

```r
a <- getSomeInfoABoutCorpora(allFtm.df = blogs.allTerms.3g)
```

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1819794 | 1399824 | 489927 | 1217845 |



```r
visualizeCumuluativeCoverage(allFtm.df = blogs.allTerms.3g, title = "% Coverage By no of Unique Words (3-grams)")
```

![](02_03_BlogsCorporaSample_1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
