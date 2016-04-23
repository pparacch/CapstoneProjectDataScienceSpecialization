# Data Acquisition & Cleaning
Pier Lorenzo Paracchini  
18 april 2016  



## Data Acquisition

Original data has been downloaded from the following [link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

## Loading the Data

Original data in _"\.\\data\\original\\final\\en\_US"_ - using the __"en_US"__ LOCALE.  

__THINKING POINT__ 

* how to load the data? Should I load everything line by line or I can find another more efficient way?
* encoding? What type of encoding shoudl be used when loading the data? Native encodi

[link to corpora info](webcachec.googleusercontent.com/search?q=cache:dzpVyq5etNYJ:www.corpora.heliohost.org/aboutcorpus.html+&cd=3&hl=en&ct=clnk&gl=us)

    'The corpora are collected from publicly available sources by a web crawler. The crawler checks for language, so as to mainly get texts consisting of the desired language*. ... Corpus Sample * You may still find lines of entirely different languages in the corpus. There are 2 main reasons for that: 1. Similar languages. Some languages are very similar, and the automatic language checker could therefore erroneously accept the foreign language text. 2. "Embedded" foreign languages. While a text may be mainly in the desired language there may be parts of it in another language. Since the text is then split up into individual lines, it is possible to see entire lines written in a foreign language. Whereas number 1 is just an out-and-out error, I think number 2 is actually desirable, as it will give a picture of when foreign language is used within the main language.'

__Tips, tricks, and hints from the Capstone Project material__

__Loading the data in (tip & trick)__

    'This dataset is fairly large. We emphasize that you don't necessarily need to load the entire dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a smaller subset of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. You can also loop over each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks at a time. Reading pieces of the file at a time will require the use of a file connection in R.'



```r
#setwd("./scripts")
con <- file("./../data/original/final/en_US/en_US.twitter.txt", "r") 
readLines(con, 1) ## Read the first line of text 
## [1] "How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long."
readLines(con, 1) ## Read the next line of text 
## [1] "When you meet someone special... you'll know. Your heart will beat more rapidly and you'll smile for no reason."
readLines(con, 5) ## Read in the next 5 lines of text 
## [1] "they've decided its more fun if I don't."                                                             
## [2] "So Tired D; Played Lazer Tag & Ran A LOT D; Ughh Going To Sleep Like In 5 Minutes ;)"                 
## [3] "Words from a complete stranger! Made my birthday even better :)"                                      
## [4] "First Cubs game ever! Wrigley field is gorgeous. This is perfect. Go Cubs Go!"                        
## [5] "i no! i get another day off from skool due to the wonderful snow (: and THIS wakes me up...damn thing"
close(con) ## It's important to close the connection when you are done

con <- file("./../data/original/final/en_US/en_US.blogs.txt", "r") 
readLines(con, 1) ## Read the first line of text 
## [1] "In the years thereafter, most of the Oil fields and platforms were named after pagan \342\200\234gods\342\200\235."
readLines(con, 1) ## Read the next line of text 
## [1] "We love you Mr. Brown."
readLines(con, 1) ## Read the next line of text 
## [1] "Chad has been awesome with the kids and holding down the fort while I work later than usual! The kids have been busy together playing Skylander on the XBox together, after Kyan cashed in his $$$ from his piggy bank. He wanted that game so bad and used his gift card from his birthday he has been saving and the money to get it (he never taps into that thing either, that is how we know he wanted it so bad). We made him count all of his money to make sure that he had enough! It was very cute to watch his reaction when he realized he did! He also does a very good job of letting Lola feel like she is playing too, by letting her switch out the characters! She loves it almost as much as him."

close(con) ## It's important to close the connection when you are done

con <- file("./../data/original/final/en_US/en_US.news.txt", "r") 
readLines(con, 1) ## Read the first line of text 
## [1] "He wasn't home alone, apparently."
readLines(con, 1) ## Read the next line of text 
## [1] "The St. Louis plant had to close. It would die of old age. Workers had been making cars there since the onset of mass automotive production in the 1920s."
readLines(con, 1) ## Read the next line of text 
## [1] "WSU's plans quickly became a hot topic on local online sites. Though most people applauded plans for the new biomedical center, many deplored the potential loss of the building."
close(con) ## It's important to close the connection when you are done
```

Original data:

* no headers
* one feature, the text (a twitter message, a blog or a piece of news)


__Sampling (tip & trick)__

    `To reiterate, to build models you don't need to load in and use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. Remember your inference class and how a representative sample can be used to infer facts about a population. You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.`


```r
con <- file("./../data/original/final/en_US/en_US.twitter.txt", "r") 
data.twitter.all <- readLines(con, skipNul = T)
# data.twitter.all <- readLines(con, skipNul = F)
#Some lines have embedded NULLS - the lines are actually truncated if skipNul = F
# data.twitter.all[167155]
# data.twitter.all[167156]
close(con)

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.twitter.all), 1, 0.20)
table(coin.biased.outcome)
## coin.biased.outcome
##       0       1 
## 1887604  472544

data.twitter.sample <- data.twitter.all[coin.biased.outcome == 1]
writeLines(data.twitter.sample, "./../data/processed/en_US.twitter.sample.txt")

con <- file("./../data/original/final/en_US/en_US.news.txt", "r") 
data.news.all <- readLines(con, skipNul = T)
close(con)

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.news.all), 1, 0.20)
table(coin.biased.outcome)
## coin.biased.outcome
##      0      1 
## 808351 201891

data.news.sample <- data.news.all[coin.biased.outcome == 1]
writeLines(data.news.sample, "./../data/processed/en_US.news.sample.txt")

con <- file("./../data/original/final/en_US/en_US.blogs.txt", "r") 
data.blogs.all <- readLines(con, skipNul = T)
close(con)

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.blogs.all), 1, 0.20)
table(coin.biased.outcome)
## coin.biased.outcome
##      0      1 
## 719632 179656

data.blogs.sample <- data.blogs.all[coin.biased.outcome == 1]
writeLines(data.blogs.sample, "./../data/processed/en_US.blogs.sample.txt")


save(data.twitter.all, data.twitter.sample, file = "./../data/processed/en_US.twitter.Rdata")
save(data.blogs.all, data.blogs.sample, file = "./../data/processed/en_US.blogs.Rdata")
save(data.news.all, data.news.sample, file = "./../data/processed/en_US.news.Rdata")
```

## Some data exploration

### Lines available in the different datasets


```r
length(data.twitter.all)
## [1] 2360148
length(data.news.all)
## [1] 1010242
length(data.blogs.all)
## [1] 899288
```

### Longest line


```r
data.twitter.all.nchar <- nchar(data.twitter.all)
summary(data.twitter.all.nchar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     2.0    37.0    64.0    68.8   100.0   213.0

data.news.all.nchar <- nchar(data.news.all)
summary(data.news.all.nchar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0   110.0   185.0   201.7   268.0 11380.0

data.blogs.all.nchar <- nchar(data.blogs.all)
summary(data.blogs.all.nchar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1.0    47.0   157.0   231.7   331.0 40840.0
```


### 'hate' and 'love' in the twitter dataset

    'In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?'
    

```r
twitter.love <- grepl("love", data.twitter.all, ignore.case = T)
twitter.hate <- grepl("hate", data.twitter.all, ignore.case = T)
sum(twitter.love)/ sum(twitter.hate)
```

```
## [1] 4.669125
```

    'The one tweet in the en_US twitter data set that matches the word "biostats" says what?'


```r
twitter.biostats <- grepl("biostats", data.twitter.all, ignore.case = T)
sum(twitter.biostats)
## [1] 1
data.twitter.all[twitter.biostats]
## [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
```

    'How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)'


```r
twitter.sentence <- grepl("A computer once beat me at chess, but it was no match for me at kickboxing", data.twitter.all, ignore.case = F)
sum(twitter.sentence)
## [1] 3
data.twitter.all[twitter.sentence]
## [1] "A computer once beat me at chess, but it was no match for me at kickboxing"
## [2] "A computer once beat me at chess, but it was no match for me at kickboxing"
## [3] "A computer once beat me at chess, but it was no match for me at kickboxing"
```

## Left Over To Be Reviewed


```r

require(R.utils)
## Loading required package: R.utils
## Warning: package 'R.utils' was built under R version 3.1.3
## Loading required package: R.oo
## Warning: package 'R.oo' was built under R version 3.1.3
## Loading required package: R.methodsS3
## Warning: package 'R.methodsS3' was built under R version 3.1.3
## R.methodsS3 v1.7.1 (2016-02-15) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.20.0 (2016-02-17) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## R.utils v2.2.0 (2015-12-09) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## The following object is masked from 'package:utils':
## 
##     timestamp
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
con.bin <- file("./../data/original/final/en_US/en_US.twitter.txt", "rb") 
data.twitter.info <- countLines(con.bin)
close(con.bin)

noOfTwitters <- data.twitter.info[1]
data.twitter.sample.id <- sample(1:noOfTwitters, noOfTwitters * 0.001, replace = F)
```


[bigrams in tm](http://tm.r-forge.r-project.org/faq.html)
