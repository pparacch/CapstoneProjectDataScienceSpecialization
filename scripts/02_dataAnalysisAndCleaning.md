# Text Prediction - Milestone Report (Capstone Project)
Pier Lorenzo Paracchini  
29 april 2016  



# Synopsis

The main objective of this __Milestone Report__ is to display and explain only the major features of the data you have identified and briefly summarize your next" plans for creating the prediction algorithm and Shiny app behind the final product. Specifically as stated in the assignment description:

* Demonstrate that the data has been downloaded and loaded
* Create a basic report of summary statistics about the data sets (twitters, news, blogs)
* Report any interesting findings and eventual considerations/ implications 
* Get feedback on the "next" plans for creating a prediction algorithm and Shiny app

# The Data

The data is originated from a corpus called [HC Corpora](www.corpora.heliohost.org) and it can be downloded at the following [link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The corpora have been collected from publicly available sources by a web crawler and includes tweets, blogs and news in english, german, finnish and russian.

A meaningful excerpt from the [About the Corpora](http://webcache.googleusercontent.com/search?q=cache:dzpVyq5etNYJ:www.corpora.heliohost.org/aboutcorpus.html+&cd=3&hl=en&ct=clnk&gl=us) informational page:

    ' You may still find lines of entirely different languages in the corpus. There are 2 main reasons for that: 1. Similar languages. Some languages are very similar, and the automatic language checker could therefore erroneously accept the foreign language text. 2. "Embedded" foreign languages. While a text may be mainly in the desired language there may be parts of it in another language. Since the text is then split up into individual lines, it is possible to see entire lines written in a foreign language.Whereas number 1 is just an out-and-out error, I think number 2 is actually desirable, as it will give a picture of when foreign language is used within the main language.'

Note! The focus of the analysis is on the __english language only ('en_US')__ - covering: tweets (twitter), news and blogs.






Table: Some basic statistics about the Corpora

sources     noOfLines   maxNoOfChar   minNoOfChar
---------  ----------  ------------  ------------
twitters      2360148           213             2
news          1010242         11384             1
blogs          899288         40835             1



__Some considerations__

* the __amount of data available__ considering the tweets, news and blogs entries. For simplification, the __exploration__ has been done using a representative sample to infer facts about a population (considering also limitations connected with the available processing hardware)

* the minimum size, in terms of number of characters, for the different entries. There are tweets, news and blogs with few characters - __are they relevant__?

# Cleaning the Data

## Encoding Issues (Gremlings)

When loading the data the following locale/ encoding has been used __English_United States.1252 \ ISO8859-1__. Inspecting the loaded data it is possible to identify some encoding issues (gremlings) due to the unrecognized characters (not supported languages, emoticons, etc)

    'I'm doing it!ðŸ‘¦'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon AppÃ©tit'
    'Everything is good in its season é¬¼ã‚‚åå…«ç•ªèŒ¶ã‚‚å‡ºèŠ±'


In order to remove such gremlings the following __strategy__ and __simplification__ has been considered: limit the set of available characters to the __ASCII__ charset, removing non ASCII characters.




    'I'm doing it!'
    'Wilted Greens Salad with Squash, Apples, and Country Ham Recipe from Bon Apptit'
    'Everything is good in its season '

## Entries with a limited number of chars

### Twitter Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/tweetsDistribution-1.png)

There are around __214097 tweets__ (__0.09%__) that are less than __20__ chars long. Few examples of such tweets can be found below:


```
##  [1] "send me beats fam"    "My moms so annoying!" "knowledge is power!" 
##  [4] "126 square blocks"    "thanks, love! :)"     "fun :D"              
##  [7] "oh no!"               "Missing my hubby..."  "M.O.B"               
## [10] "Gonna be a long day"  "Ok brotha thanks!!!"
```

Because of the limited number of such tweets and the "irrelevance" of their content (especially the ones with less than 10 chars), it has been decided to remove them from the __twitter corpora__.



### News Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/newsDistribution-1.png)

There are around __30644 news__ (__0.03%__) that are less than __20__ chars long. Few examples of such news can be found below:


```
##  [1] "BL  Knight 9."        "In other trading:"    "Drage Vukcevich"     
##  [4] "Chain"                "10. Youngstown"       "Aberdeen"            
##  [7] "Radio Radio"          "A gust of popularity" "(Ticker Tape)"       
## [10] "last."
```

Because of the limited number of such news and the "irrelevance" of their content , it has been decided to remove them from the __news corpora__.



### Blogs Corpora

![](02_dataAnalysisAndCleaning_files/figure-html/blogsDistribution-1.png)

There are around __77241 blogs__ (__0.09%__) that are less than __20 chars__ long. Few examples of such blogs can be found below:


```
##  [1] "If I were a bear,"    "Tis all."             "1/3 cup tomato paste"
##  [4] "3 T ketchup"          "M. Blakeman Ingle"    "Sphere: V = 4/3"     
##  [7] "Rm25"                 "So "                  "In shrouds of words,"
## [10] "You die?"
```

Because of the limited number of such blogs and the "irrelevance" of their content , it has been decided to remove them from the __blogs corpora__.



## Removal of Profanity Words



Note that the data contain words of offensive and profane meaning. Some examples ...

    !++-~| G1 CERTIFIED WET TSHIRT CONTEST --- FRIDAY CLUB DRAMA --WANT TO GET IN FOR FREE?? TXT ME I WILL TELL YOU HOW---214 609 3316 --
    Wisconsin Governor Walker Attacks Sex Ed



Profanity words will be removed from the Corpora (as stopwords). An external [resource](http://www.cs.cmu.edu/~biglou/resources/) providing a comprehensive list of __1383 profanity words__ is used.

## Others 

* replace contractions __"u, r, c'mon, doin', y'all, ya'll, ma'am"__ with __"you, are, come on, doing, you all, madam"__
* remove __links__ (e.g. "https://www.coursera.org/" or "http://www.coursera.org/")
* remove __"RT"__ (only for the twitter corpora)




## Sampling of the Corpora



For this analysis it is not needed use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. A "biased coin" approach has been used to select the tweets, news and blogs to be included in the analysis based on the following percentages

* __5%__ of the tweets (__107110__ tweets)
* __10%__ of the news (__97775__ news)
* __10%__ of the blogs (__82168__ blogs)

# Exploring the (Sample) Corpora

Exploration of the corpora is done using __natural language processing techniques__ - specifically term frequency analysis using ngrams (1-gram, 2-gram and 3-gram). Before tokenizing the corpora the following steps are performed:

* transform to lower case
* remove profanity words
* remove numbers
* remove punctuations - except of the `'` (apostrophe) in order to not lose contractions (e.g. I'll, I'm, etc)
* add a `<s> ` marker at the beginning of each entry (tweet, news, blog)
* add a ` </s>` marker at the end of each entry (tweet, news, blog) 

`Wordclouds` and `barplots` are used to visualize the most frequent words/ tokens for the different n-grams. When plotting the 'barplots' only the first most frequent terms (top 30) are shown and max 200 terms in the wordclouds. __Note:__ For 2-grams and 3-grams a token like `<s> at the` refers to `at the` at the beginning of the entry (tweet, news or blog), while `the top </s>` refers to `the top` at the end of the entry (tweet, news or blog).







## Twitter Corpora




### 1-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_1g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_1g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1103869 | 59091 | 211 | 6772 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-1-1.png)

### 2-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_2g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_2g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1533686 | 544485 | 16697 | 391117 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-2-1.png)

### 3-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_3g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeTwitterData_3g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 1426576 | 1061053 | 347765 | 918396 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-3-1.png)

## News Corpora



### 1-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_1g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_1g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 2731479 | 88107 | 395 | 9581 |

![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-4-1.png)

### 2-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_2g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_2g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 3445777 | 1170126 | 39386 | 825549 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-5-1.png)

### 3-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_3g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeNewsData_3g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 3348002 | 2508621 | 834620 | 2173821 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-6-1.png)

## Blogs Corpora



### 1-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_1g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_1g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 2863955 | 91345 | 237 | 8284 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-7-1.png)

### 2-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_2g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_2g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 3750369 | 1154501 | 25933 | 779465 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-8-1.png)

### 3-grams

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_3g-1.png)![](02_dataAnalysisAndCleaning_files/figure-html/visualizeBlogsData_3g-2.png)

__How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?__

| N = number of tokens | V = vocabulary size | 50% coverage | 90% coverage |
| ------------- |:-------------:| -----:|-----:|-----:|
| 3668201 | 2632369 | 798269 | 2265549 |


![](02_dataAnalysisAndCleaning_files/figure-html/unnamed-chunk-9-1.png)

# Next Steps

* Investigate the possibility to increase sampling of the different Corpora (twitter, news, blogs)
* Identify a common vocabulary between the different Corpora (twitter, news, blogs)
* Create language models using 1-grams, 2-grams and 3-grams
* Think on how ot use the product/ define a simple GUI
