# WIP Exploration and Cleaning
Pier Lorenzo Paracchini  
25 april 2016  



## Summary


## Load the data


```r
con <- file("./../data/original/final/en_US/en_US.twitter.txt", "r") 
data.twitter.all <- readLines(con, skipNul = T)
data.twitter.all.nchar <- nchar(data.twitter.all)
close(con)

con <- file("./../data/original/final/en_US/en_US.news.txt", "r") 
data.news.all <- readLines(con, skipNul = T)
data.news.all.nchar <- nchar(data.news.all)
close(con)

con <- file("./../data/original/final/en_US/en_US.blogs.txt", "r") 
data.blogs.all <- readLines(con, skipNul = T)
data.blogs.all.nchar <- nchar(data.blogs.all)
close(con)
```

### Gremlings due to Encoding issues

__Strategy__ -> limit the set of available characters to ASCII charset.


```r
data.twitter.all.ascii <-  iconv(data.twitter.all, from = localeToCharset(), to = "ASCII", "")
data.news.all.ascii <-  iconv(data.news.all, from = localeToCharset(), to = "ASCII", "")
data.blogs.all.ascii <-  iconv(data.blogs.all, from = localeToCharset(), to = "ASCII", "")

data.twitter.all[7036:7047]
```

```
##  [1] "Project Trick- -into-becoming-a-plus-size-model is going according to plan."                                                  
##  [2] "Welcome back to the Bay Boss...Great day to be here!"                                                                         
##  [3] "Finally got his number :D . I meet up with him today"                                                                         
##  [4] "lol he the one going crazy"                                                                                                   
##  [5] "Awww, hope the little guy feels better soon! It's heartbreaking knowing there's little you can do to get him better faster"   
##  [6] "Nice to meet you as well. Cool stuff you're doing!"                                                                           
##  [7] "that's butt. Lol"                                                                                                             
##  [8] "21st century book clubs with and Beatrice Gerrish. Want to do next year..love for reviewing!"                                 
##  [9] "#IRA2012 attendees-if you're concerned about keeping kids safe online or teaching kids tech, find & meet ZillyDilly creator !"
## [10] "Proverbs 27:6Proverbs 27:6 Faithful [are] the wounds of a friend; but the kisses of an enemy [are] deceitful."                
## [11] "Everything is good in its season é¬¼ãåå«çªè¶ãåºè±"                                                                 
## [12] "I hope you are kid free."
```

```r
data.twitter.all.ascii[7036:7047]
```

```
##  [1] "Project Trick- -into-becoming-a-plus-size-model is going according to plan."                                                  
##  [2] "Welcome back to the Bay Boss...Great day to be here!"                                                                         
##  [3] "Finally got his number :D . I meet up with him today"                                                                         
##  [4] "lol he the one going crazy"                                                                                                   
##  [5] "Awww, hope the little guy feels better soon! It's heartbreaking knowing there's little you can do to get him better faster"   
##  [6] "Nice to meet you as well. Cool stuff you're doing!"                                                                           
##  [7] "that's butt. Lol"                                                                                                             
##  [8] "21st century book clubs with and Beatrice Gerrish. Want to do next year..love for reviewing!"                                 
##  [9] "#IRA2012 attendees-if you're concerned about keeping kids safe online or teaching kids tech, find & meet ZillyDilly creator !"
## [10] "Proverbs 27:6Proverbs 27:6 Faithful [are] the wounds of a friend; but the kisses of an enemy [are] deceitful."                
## [11] "Everything is good in its season "                                                                                            
## [12] "I hope you are kid free."
```

```r
data.twitter.all <- data.twitter.all.ascii
data.news.all <- data.news.all.ascii
data.blogs.all <- data.blogs.all.ascii
```


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
##    sources noOfLines maxNoOfChar minNoOfChar
## 1 twitters   2360148         213           2
## 2     news   1010242       11384           1
## 3    blogs    899288       40835           1

#######Twitters
hist(nchar(data.twitter.all), main = "No Of Chars per Twitter")
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-1.png)

```r

#How many twitter are under 20 chars?
sum(data.twitter.all.nchar <= 20)
## [1] 212375
twitter.lessThanOr3chars <- data.twitter.all[data.twitter.all.nchar <= 20]
head(twitter.lessThanOr3chars, 20)
##  [1] "I'm doing it!"        "Tommorows the day..." "Exhaust leak! arrrgh"
##  [4] "I will <3"            "Sing it!"             "got a good one?"     
##  [7] "love chris brown"     "its sounds good"      "Enjoy!! Stay cool!!" 
## [10] "I'm taking Adam! :-)" "lets do this"         "Time for lunch!"     
## [13] "at six flags."        "Thanks Tom."          "#GET BETTER"         
## [16] "me too"               "dont break my butt"   "you're welcome(:"    
## [19] "Yes they can :)"      "#9 for Nelson!"
tail(twitter.lessThanOr3chars, 20)
##  [1] "Yes! Yes I do!"       "Kawhi Me Maybe?"      "i will lolol"        
##  [4] "Today is amazing."    "nf follow back"       "long fuckin day."    
##  [7] "Love & Hugs To You"   "I feel you lol."      "black vampire... Hmm"
## [10] "follow back? ;)"      "Going to bed."        "hey, thanks"         
## [13] "is represented."      "Life hard I'm harder" "Awww! Clean kitties."
## [16] "He Just a Baby Boy"   "oh man totally"       "Thank you !!"        
## [19] "u welcome"            "It is #RHONJ time!!"

#Removing such short tweets
data.twitter.all <- data.twitter.all[data.twitter.all.nchar > 20]
data.twitter.all.nchar <- nchar(data.twitter.all)

twitter.df <- data.frame(text = data.twitter.all, nchar = data.twitter.all.nchar)

##What about twitters more than 140? Analysis

######News
hist(nchar(data.news.all), main = "No Of Chars per News")
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-2.png)

```r

#Let's focus on news
a <- data.news.all.nchar < 1500
sum(a)
## [1] 1010096
hist(nchar(data.news.all[a]), main = "No Of Chars per News (< 1500)", breaks = 100)
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-3.png)

```r

sum(!a)
## [1] 146
hist(nchar(data.news.all[!a]), main = "No Of Chars per News (>= 1500)", breaks = 100)
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-4.png)

```r

a <- data.news.all.nchar <= 20
sum(a)
## [1] 30109
head(data.news.all[a], 40)
##  [1] "BL  Knight 9."        "In other trading:"    "Drage Vukcevich"     
##  [4] "Chain"                "10. Youngstown"       "Aberdeen"            
##  [7] "Radio Radio"          "A gust of popularity" "(Ticker Tape)"       
## [10] "last."                "White wine or water"  "\"Oh.\""             
## [13] "'GOLD RUSH'"          "Provenance"           "Workhaus Collective" 
## [16] "Not always."          "HEADING HOME"         "TICKS"               
## [19] "For oysters:"         "JUAN MONTOYA"         "Shocking Results"    
## [22] "Wayne Whitbeck"       "Tired Of Being Alive" "When: 5 p.m. today"  
## [25] "Now fight."           "LISTEN TO ELK BUGLE"  "Chris Carpenter"     
## [28] "So they broke up."    "So is it worth it?"   "ShoeMall"            
## [31] "\"Cover Me\""         "Joyful Noise"         "brewer"              
## [34] "96hours."             "15th District"        "Into the Silence"    
## [37] "1 teaspoon paprika"   "I'm warning you. ..." "THE GREY"            
## [40] "COUNCIL'S RESPONSE"
tail(data.news.all[a], 40)
##  [1] "But on the big ones." "So they did."         "WATERLOO SCHOOLS"    
##  [4] "BROKEN BANKING"       "The lesser issues:"   "Not so, says Roney." 
##  [7] "\"He's an American."  "USC at Utah"          "Expectations key"    
## [10] "Rushes 39 18"         "Fort Zumwalt North"   "-- Jeff Baker"       
## [13] "Other stories:"       "MENTOR SCHOOLS"       "8. Byrd, CF"         
## [16] "NAUGHTY AND NICE"     "John Yarmuth, D-Ky."  " Ryan Braun"         
## [19] "8 GP,"                "10. Boston Celtics"   "Sickle cell anemia"  
## [22] "1/4 cup apple juice"  "Friday, Dec. 16."     "Sally Garrison"      
## [25] "WINNER: John Cena"    "Roy knows it, too."   "Atlanta"             
## [28] "Woe is knee"          "partner Johnson."     "Number: 2"           
## [31] "Robyn Merino"         "Unbelievable."        "OUTSIDE"             
## [34] "Candice Meyer"        "Lilly (3-0)"          "Joel Gossman"        
## [37] "Jobs."                "SUV's fatal plunge"   "Game 12"             
## [40] "world's largest."

#Removing such short news
data.news.all <- data.news.all[data.news.all.nchar > 20]
data.news.all.nchar <- nchar(data.news.all)

news.df <- data.frame(text = data.news.all, nchar = data.news.all.nchar)

######Blogs
hist(nchar(data.blogs.all), main = "Data Blogs No Of Chars per Blogs")
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-5.png)

```r

a <- data.blogs.all.nchar < 1300
sum(a)
## [1] 894180
hist(nchar(data.blogs.all[a]), main = "Data Blogs No Of Chars per Blogs (< 1300)")
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-6.png)

```r

sum(!a)
## [1] 5108
hist(nchar(data.blogs.all[!a]), main = "Data Blogs No Of Chars per Blogs (>= 1300)", breaks = 10000)
```

![](02_dataAnalysisAndCleaning_files/figure-html/someDataStatistics-7.png)

```r

a <- data.blogs.all.nchar <= 20
sum(a)
## [1] 74741
head(data.blogs.all[a], 40)
##  [1] "If I were a bear,"    "Tis all."             "1/3 cup tomato paste"
##  [4] "3 T ketchup"          "M. Blakeman Ingle"    "Sphere: V = 4/3"     
##  [7] "Rm25"                 "So "                  "In shrouds of words,"
## [10] "You die?"             "hold, to not let go"  "1 Beauty Contest"    
## [13] "Just go away."        "2% Crystal 40"        "Night by night"      
## [16] "in her hair"          "Furioso dreadnought"  "My legs tight until" 
## [19] "Jen, who said"        "~ heehee ~"           "M: Hey babe?"        
## [22] "And a lot more trust" "Stella Telleria"      "680 gr. Mehl"        
## [25] "Preschool Literacy:"  " Jane Odiwe"          "Songs vary;"         
## [28] "05 Algeria"           "and the dust rises"   "Maximum Level: None" 
## [31] "No more diapers"      "Rating - 5 out of 5"  "4 nonetheless"       
## [34] "Sickos"               "Now without you"      "Jerry"               
## [37] "in stilettos and"     "Book-The Lost Hero"   "Avatar (2009)"       
## [40] "3 TBLS sugar"
tail(data.blogs.all[a], 40)
##  [1] "And that is forever." "handed you a"         "Favorite quotes."    
##  [4] "Lets Pretend"         "Make your own nachos" "4) Ready to serve."  
##  [7] "1 TS vanilla"         "come back..."         "reviewing:"          
## [10] "Fishing"              "caught"               "Matthew 11:28"       
## [13] "And one."             "Time Out."            "Plot"                
## [16] "PLEASE NOTE:"         "Leftover chicken"     "4. In the Pocket"    
## [19] "and so must sink,"    "None of it is there"  "1 books"             
## [22] "773 - 472 - 4505"     "Nita Jane Ayres"      "I write,"            
## [25] "Oil (for frying)"     "New and unknown."     "Simple Beginning"    
## [28] "Decent pricing"       "1 alert"              "E-mail"              
## [31] "still dripping wet"   "3B  Adrian Beltre"    "Okay"                
## [34] "DONG!"                "10 IBUs (0.5 oz.)"    "I got lost"          
## [37] "If not, now you do."  "2 interested"         "10 re"               
## [40] "Beloved:"

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
##    sources noOfLines maxNoOfChar minNoOfChar
## 1 twitters   2147773         140           6
## 2     news    980133       11384           6
## 3    blogs    824547       40832           0
```



### Sampling


```r
set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.twitter.all), 1, 0.10)
table(coin.biased.outcome)
## coin.biased.outcome
##       0       1 
## 1932771  215002
data.twitter.sample <- data.twitter.all[coin.biased.outcome == 1]

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.news.all), 1, 0.20)
table(coin.biased.outcome)
## coin.biased.outcome
##      0      1 
## 784361 195772
data.news.sample <- data.news.all[coin.biased.outcome == 1]

set.seed(19711004)
coin.biased.outcome <- rbinom(length(data.blogs.all), 1, 0.20)
table(coin.biased.outcome)
## coin.biased.outcome
##      0      1 
## 659806 164741
data.blogs.sample <- data.blogs.all[coin.biased.outcome == 1]

save(data.twitter.sample, data.news.sample, data.blogs.sample, file = "./../data/processed/datasets_sample.Rdata")
```


```r
load("./../data/processed/datasets_sample.Rdata")

require(tm)
```

```
## Loading required package: tm
```

```
## Loading required package: NLP
```

```r
require(wordcloud)
```

```
## Loading required package: wordcloud
```

```
## Loading required package: RColorBrewer
```

```r
require(RWeka)
```

```
## Loading required package: RWeka
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked from 'package:NLP':
## 
##     annotate
```

```r
removePunctuations.exceptApostrophe <- function(texts){
    gsub(pattern = "[^'[:^punct:]]", replacement = " ", x = texts, perl = T)
}

test <- "I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
test.expected <- "I like    to    chew   gum  but don't like      bubble      gum  "
test.expected == removePunctuations.exceptApostrophe(texts = test)
```

```
## [1] TRUE
```

```r
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
```

```
## <<TermDocumentMatrix (terms: 38828, documents: 50000)>>
## Non-/sparse entries: 501326/1940898674
## Sparsity           : 100%
## Maximal term length: 91
## Weighting          : term frequency (tf)
```

```r
findFreqTerms(tdm.1g,lowfreq = 100)
```

```
##   [1] "able"        "about"       "account"     "actually"    "after"      
##   [6] "afternoon"   "again"       "against"     "ago"         "agree"      
##  [11] "ain't"       "all"         "almost"      "already"     "also"       
##  [16] "always"      "amazing"     "american"    "and"         "another"    
##  [21] "any"         "anymore"     "anyone"      "anything"    "app"        
##  [26] "appreciate"  "are"         "around"      "art"         "ask"        
##  [31] "asked"       "ass"         "austin"      "away"        "awesome"    
##  [36] "baby"        "back"        "bad"         "ball"        "band"       
##  [41] "bar"         "beach"       "beat"        "beautiful"   "because"    
##  [46] "become"      "bed"         "been"        "beer"        "before"     
##  [51] "behind"      "being"       "believe"     "best"        "better"     
##  [56] "between"     "big"         "birthday"    "bit"         "bitch"      
##  [61] "black"       "blog"        "body"        "book"        "books"      
##  [66] "both"        "bout"        "boy"         "boys"        "break"      
##  [71] "bring"       "bro"         "brother"     "business"    "busy"       
##  [76] "but"         "buy"         "call"        "called"      "came"       
##  [81] "can"         "can't"       "cant"        "car"         "care"       
##  [86] "catch"       "cause"       "center"      "chance"      "change"     
##  [91] "check"       "chicago"     "christmas"   "city"        "class"      
##  [96] "close"       "club"        "coffee"      "cold"        "college"    
## [101] "com"         "come"        "comes"       "coming"      "community"  
## [106] "conference"  "congrats"    "cool"        "could"       "country"    
## [111] "couple"      "course"      "crazy"       "cute"        "cuz"        
## [116] "dad"         "damn"        "dance"       "date"        "day"        
## [121] "days"        "dead"        "deal"        "dear"        "definitely" 
## [126] "did"         "didn't"      "die"         "different"   "dinner"     
## [131] "does"        "doesn't"     "dog"         "doing"       "don't"      
## [136] "done"        "dont"        "down"        "dream"       "drink"      
## [141] "drive"       "dude"        "during"      "each"        "early"      
## [146] "easy"        "eat"         "eating"      "either"      "else"       
## [151] "email"       "end"         "enjoy"       "enough"      "even"       
## [156] "event"       "ever"        "every"       "everybody"   "everyone"   
## [161] "everything"  "excited"     "experience"  "face"        "facebook"   
## [166] "fall"        "family"      "fan"         "fans"        "far"        
## [171] "fast"        "favorite"    "feel"        "feeling"     "few"        
## [176] "fight"       "film"        "final"       "finally"     "find"       
## [181] "fine"        "finished"    "first"       "folks"       "follow"     
## [186] "followers"   "following"   "food"        "football"    "for"        
## [191] "forever"     "forget"      "forward"     "found"       "free"       
## [196] "friday"      "friend"      "friends"     "from"        "front"      
## [201] "fuck"        "fucking"     "full"        "fun"         "funny"      
## [206] "future"      "game"        "games"       "get"         "gets"       
## [211] "getting"     "gift"        "girl"        "girls"       "give"       
## [216] "giving"      "glad"        "god"         "goes"        "going"      
## [221] "gone"        "gonna"       "good"        "google"      "got"        
## [226] "gotta"       "great"       "green"       "group"       "guess"      
## [231] "guy"         "guys"        "gym"         "had"         "haha"       
## [236] "hahaha"      "hair"        "half"        "hand"        "happen"     
## [241] "happy"       "hard"        "has"         "hate"        "have"       
## [246] "haven't"     "having"      "he's"        "head"        "hear"       
## [251] "heard"       "heart"       "hell"        "hello"       "help"       
## [256] "her"         "here"        "hey"         "high"        "him"        
## [261] "his"         "history"     "hit"         "hold"        "home"       
## [266] "hope"        "hot"         "hour"        "hours"       "house"      
## [271] "how"         "huge"        "i'd"         "i'll"        "i'm"        
## [276] "i've"        "ice"         "idea"        "ideas"       "ill"        
## [281] "important"   "info"        "instead"     "interesting" "into"       
## [286] "isn't"       "it's"        "its"         "job"         "john"       
## [291] "join"        "just"        "keep"        "kid"         "kids"       
## [296] "kind"        "knew"        "know"        "lady"        "last"       
## [301] "late"        "later"       "learn"       "learning"    "least"      
## [306] "leave"       "left"        "less"        "let"         "let's"      
## [311] "lets"        "library"     "life"        "like"        "line"       
## [316] "list"        "listen"      "listening"   "little"      "live"       
## [321] "living"      "lmao"        "lol"         "long"        "look"       
## [326] "looking"     "looks"       "lose"        "lost"        "lot"        
## [331] "lots"        "love"        "loved"       "luck"        "lunch"      
## [336] "mad"         "made"        "make"        "makes"       "making"     
## [341] "man"         "many"        "matter"      "may"         "maybe"      
## [346] "mean"        "means"       "media"       "meet"        "meeting"    
## [351] "men"         "mention"     "met"         "might"       "mind"       
## [356] "mine"        "minutes"     "miss"        "missed"      "mom"        
## [361] "moment"      "monday"      "money"       "month"       "months"     
## [366] "more"        "morning"     "most"        "move"        "movie"      
## [371] "much"        "music"       "must"        "myself"      "name"       
## [376] "need"        "needs"       "never"       "new"         "news"       
## [381] "next"        "nice"        "night"       "not"         "nothing"    
## [386] "now"         "number"      "nyc"         "obama"       "off"        
## [391] "office"      "okay"        "old"         "omg"         "once"       
## [396] "one"         "online"      "only"        "open"        "order"      
## [401] "other"       "others"      "our"         "out"         "outside"    
## [406] "over"        "own"         "page"        "park"        "part"       
## [411] "party"       "pass"        "past"        "pay"         "people"     
## [416] "perfect"     "person"      "phone"       "pic"         "pick"       
## [421] "picture"     "place"       "plan"        "play"        "playing"    
## [426] "please"      "point"       "post"        "power"       "ppl"        
## [431] "pretty"      "probably"    "problem"     "project"     "proud"      
## [436] "put"         "question"    "radio"       "rain"        "rather"     
## [441] "read"        "reading"     "ready"       "real"        "really"     
## [446] "reason"      "red"         "remember"    "rest"        "right"      
## [451] "rock"        "room"        "run"         "running"     "sad"        
## [456] "safe"        "said"        "same"        "san"         "saturday"   
## [461] "saw"         "say"         "saying"      "says"        "school"     
## [466] "season"      "second"      "see"         "seeing"      "seems"      
## [471] "seen"        "self"        "send"        "sent"        "series"     
## [476] "seriously"   "service"     "session"     "set"         "share"      
## [481] "sharing"     "she"         "she's"       "shit"        "short"      
## [486] "should"      "shout"       "show"        "sick"        "side"       
## [491] "sign"        "since"       "single"      "site"        "sleep"      
## [496] "smh"         "smile"       "social"      "some"        "someone"    
## [501] "something"   "sometimes"   "son"         "song"        "songs"      
## [506] "soon"        "sorry"       "sound"       "sounds"      "special"    
## [511] "spring"      "stand"       "star"        "start"       "started"    
## [516] "starting"    "state"       "stay"        "still"       "stop"       
## [521] "store"       "story"       "street"      "students"    "stuff"      
## [526] "stupid"      "success"     "such"        "summer"      "sunday"     
## [531] "super"       "support"     "sure"        "sweet"       "take"       
## [536] "taking"      "talk"        "talking"     "team"        "tell"       
## [541] "text"        "than"        "thank"       "thanks"      "that"       
## [546] "that's"      "thats"       "the"         "their"       "them"       
## [551] "then"        "there"       "there's"     "these"       "they"       
## [556] "they're"     "thing"       "things"      "think"       "thinking"   
## [561] "this"        "tho"         "those"       "though"      "thought"    
## [566] "thoughts"    "three"       "through"     "thursday"    "thx"        
## [571] "tickets"     "till"        "time"        "times"       "tired"      
## [576] "today"       "together"    "told"        "tomorrow"    "tonight"    
## [581] "too"         "took"        "top"         "totally"     "tour"       
## [586] "town"        "trip"        "true"        "try"         "trying"     
## [591] "tuesday"     "turn"        "tweet"       "tweeting"    "tweets"     
## [596] "twitter"     "two"         "ugh"         "under"       "understand" 
## [601] "until"       "use"         "used"        "using"       "very"       
## [606] "via"         "video"       "visit"       "voice"       "vote"       
## [611] "wait"        "waiting"     "walk"        "wanna"       "want"       
## [616] "wanted"      "wants"       "was"         "wasn't"      "watch"      
## [621] "watching"    "water"       "way"         "we'll"       "we're"      
## [626] "wear"        "weather"     "website"     "week"        "weekend"    
## [631] "weeks"       "weird"       "welcome"     "well"        "went"       
## [636] "were"        "what"        "what's"      "when"        "where"      
## [641] "which"       "while"       "white"       "who"         "who's"      
## [646] "whole"       "why"         "will"        "win"         "wine"       
## [651] "wish"        "with"        "without"     "woman"       "women"      
## [656] "won't"       "wonder"      "wonderful"   "word"        "words"      
## [661] "work"        "working"     "works"       "world"       "worst"      
## [666] "worth"       "would"       "wow"         "write"       "writing"    
## [671] "wrong"       "www"         "y'all"       "yay"         "yea"        
## [676] "yeah"        "year"        "years"       "yes"         "yesterday"  
## [681] "yet"         "you"         "you'll"      "you're"      "you've"     
## [686] "young"       "your"        "yourself"
```

```r
findFreqTerms(tdm.1g,lowfreq = 300)
```

```
##   [1] "about"     "after"     "again"     "all"       "already"  
##   [6] "also"      "always"    "amazing"   "and"       "another"  
##  [11] "any"       "anyone"    "are"       "around"    "away"     
##  [16] "awesome"   "back"      "bad"       "beautiful" "because"  
##  [21] "been"      "before"    "being"     "best"      "better"   
##  [26] "big"       "birthday"  "but"       "call"      "can"      
##  [31] "can't"     "check"     "com"       "come"      "coming"   
##  [36] "cool"      "could"     "day"       "days"      "did"      
##  [41] "didn't"    "does"      "doesn't"   "doing"     "don't"    
##  [46] "done"      "dont"      "down"      "end"       "even"     
##  [51] "ever"      "every"     "everyone"  "excited"   "family"   
##  [56] "feel"      "find"      "first"     "follow"    "for"      
##  [61] "forward"   "free"      "friday"    "friend"    "friends"  
##  [66] "from"      "fun"       "game"      "get"       "getting"  
##  [71] "girl"      "give"      "glad"      "god"       "going"    
##  [76] "gonna"     "good"      "got"       "great"     "guys"     
##  [81] "had"       "haha"      "happy"     "hard"      "has"      
##  [86] "hate"      "have"      "having"    "hear"      "help"     
##  [91] "her"       "here"      "hey"       "him"       "his"      
##  [96] "home"      "hope"      "house"     "how"       "i'll"     
## [101] "i'm"       "i've"      "into"      "it's"      "its"      
## [106] "job"       "just"      "keep"      "know"      "last"     
## [111] "let"       "life"      "like"      "little"    "live"     
## [116] "lol"       "long"      "look"      "looking"   "lot"      
## [121] "love"      "made"      "make"      "makes"     "man"      
## [126] "many"      "may"       "maybe"     "mean"      "miss"     
## [131] "more"      "morning"   "most"      "much"      "music"    
## [136] "need"      "never"     "new"       "next"      "nice"     
## [141] "night"     "not"       "nothing"   "now"       "off"      
## [146] "old"       "one"       "only"      "other"     "our"      
## [151] "out"       "over"      "people"    "play"      "please"   
## [156] "pretty"    "put"       "ready"     "real"      "really"   
## [161] "right"     "said"      "same"      "say"       "school"   
## [166] "see"       "she"       "shit"      "should"    "show"     
## [171] "some"      "someone"   "something" "song"      "soon"     
## [176] "sorry"     "start"     "still"     "stop"      "sure"     
## [181] "take"      "talk"      "team"      "tell"      "than"     
## [186] "thank"     "thanks"    "that"      "that's"    "the"      
## [191] "their"     "them"      "then"      "there"     "these"    
## [196] "they"      "thing"     "things"    "think"     "this"     
## [201] "those"     "though"    "thought"   "through"   "time"     
## [206] "today"     "tomorrow"  "tonight"   "too"       "try"      
## [211] "trying"    "tweet"     "twitter"   "two"       "use"      
## [216] "very"      "wait"      "wanna"     "want"      "was"      
## [221] "watch"     "watching"  "way"       "we're"     "week"     
## [226] "weekend"   "well"      "were"      "what"      "what's"   
## [231] "when"      "where"     "while"     "who"       "why"      
## [236] "will"      "win"       "wish"      "with"      "work"     
## [241] "working"   "world"     "would"     "wow"       "yeah"     
## [246] "year"      "yes"       "yet"       "you"       "you're"   
## [251] "your"
```

```r
findFreqTerms(tdm.1g,lowfreq = 500)
```

```
##   [1] "about"     "after"     "all"       "always"    "and"      
##   [6] "any"       "are"       "awesome"   "back"      "because"  
##  [11] "been"      "being"     "best"      "better"    "big"      
##  [16] "but"       "can"       "can't"     "come"      "could"    
##  [21] "day"       "did"       "don't"     "down"      "even"     
##  [26] "ever"      "everyone"  "feel"      "first"     "follow"   
##  [31] "for"       "from"      "game"      "get"       "getting"  
##  [36] "going"     "gonna"     "good"      "got"       "great"    
##  [41] "had"       "haha"      "happy"     "has"       "have"     
##  [46] "her"       "here"      "hey"       "him"       "his"      
##  [51] "home"      "hope"      "how"       "i'll"      "i'm"      
##  [56] "i've"      "it's"      "its"       "just"      "keep"     
##  [61] "know"      "last"      "let"       "life"      "like"     
##  [66] "lol"       "look"      "looking"   "love"      "make"     
##  [71] "man"       "more"      "morning"   "much"      "need"     
##  [76] "never"     "new"       "next"      "night"     "not"      
##  [81] "now"       "off"       "one"       "only"      "our"      
##  [86] "out"       "over"      "people"    "please"    "really"   
##  [91] "right"     "say"       "see"       "she"       "should"   
##  [96] "show"      "some"      "something" "still"     "sure"     
## [101] "take"      "than"      "thank"     "thanks"    "that"     
## [106] "that's"    "the"       "their"     "them"      "then"     
## [111] "there"     "they"      "think"     "this"      "time"     
## [116] "today"     "tomorrow"  "tonight"   "too"       "twitter"  
## [121] "very"      "wait"      "want"      "was"       "way"      
## [126] "week"      "weekend"   "well"      "were"      "what"     
## [131] "when"      "where"     "who"       "why"       "will"     
## [136] "with"      "work"      "would"     "year"      "yes"      
## [141] "you"       "you're"    "your"
```

```r
a <- findFreqTerms(tdm.1g,lowfreq = 500)
tdm.1g.l <- tdm.1g[a,]
tdm.1g.l
```

```
## <<TermDocumentMatrix (terms: 143, documents: 50000)>>
## Non-/sparse entries: 212307/6937693
## Sparsity           : 97%
## Maximal term length: 9
## Weighting          : term frequency (tf)
```

```r
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
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-1.png)

```r
wordcloud(words = frequentTermsSubsetDF$term,freq = frequentTermsSubsetDF$freq)
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-2.png)

```r
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
```

```
## <<TermDocumentMatrix (terms: 288004, documents: 50000)>>
## Non-/sparse entries: 623624/14399576376
## Sparsity           : 100%
## Maximal term length: 97
## Weighting          : term frequency (tf)
```

```r
findFreqTerms(tdm.2g,lowfreq = 100)
```

```
##   [1] "a big"           "a few"           "a good"         
##   [4] "a great"         "a little"        "a long"         
##   [7] "a lot"           "a new"           "able to"        
##  [10] "about it"        "about the"       "about to"       
##  [13] "all day"         "all i"           "all my"         
##  [16] "all of"          "all the"         "am i"           
##  [19] "and a"           "and get"         "and i"          
##  [22] "and i'm"         "and it"          "and my"         
##  [25] "and the"         "and then"        "and you"        
##  [28] "are the"         "are you"         "as a"           
##  [31] "as well"         "at a"            "at all"         
##  [34] "at least"        "at pm"           "at the"         
##  [37] "back to"         "be a"            "be in"          
##  [40] "be the"          "be there"        "better than"    
##  [43] "but i"           "but i'm"         "but it"         
##  [46] "but the"         "by the"          "can't wait"     
##  [49] "can be"          "can i"           "can you"        
##  [52] "check out"       "come on"         "come to"        
##  [55] "day of"          "day to"          "did you"        
##  [58] "do it"           "do you"          "don't have"     
##  [61] "don't know"      "end of"          "excited to"     
##  [64] "feel like"       "follow me"       "for a"          
##  [67] "for all"         "for following"   "for me"         
##  [70] "for my"          "for our"         "for some"       
##  [73] "for the"         "for this"        "for you"        
##  [76] "for your"        "forward to"      "from the"       
##  [79] "get a"           "get it"          "get the"        
##  [82] "get to"          "go to"           "going to"       
##  [85] "gonna be"        "good luck"       "good morning"   
##  [88] "got a"           "had a"           "had to"         
##  [91] "happy birthday"  "has a"           "has been"       
##  [94] "have a"          "have been"       "have the"       
##  [97] "have to"         "have you"        "having a"       
## [100] "he is"           "he was"          "hope you"       
## [103] "how to"          "i'll be"         "i'm a"          
## [106] "i'm going"       "i'm not"         "i'm so"         
## [109] "i've been"       "i am"            "i can"          
## [112] "i can't"         "i could"         "i did"          
## [115] "i didn't"        "i do"            "i don't"        
## [118] "i dont"          "i feel"          "i get"          
## [121] "i got"           "i guess"         "i had"          
## [124] "i hate"          "i have"          "i hope"         
## [127] "i just"          "i know"          "i like"         
## [130] "i love"          "i miss"          "i need"         
## [133] "i really"        "i saw"           "i see"          
## [136] "i still"         "i think"         "i thought"      
## [139] "i wanna"         "i want"          "i was"          
## [142] "i will"          "i wish"          "i would"        
## [145] "if i"            "if u"            "if you"         
## [148] "if you're"       "in a"            "in my"          
## [151] "in the"          "in this"         "in your"        
## [154] "into the"        "is a"            "is going"       
## [157] "is in"           "is it"           "is my"          
## [160] "is not"          "is so"           "is that"        
## [163] "is the"          "it's a"          "it's not"       
## [166] "it in"           "it is"           "it out"         
## [169] "it to"           "it up"           "it was"         
## [172] "it will"         "it would"        "just a"         
## [175] "just got"        "kind of"         "know that"      
## [178] "know what"       "last night"      "let me"         
## [181] "like a"          "like that"       "like the"       
## [184] "like to"         "listen to"       "listening to"   
## [187] "lol i"           "look at"         "looking for"    
## [190] "looking forward" "looks like"      "lot of"         
## [193] "lots of"         "love it"         "love the"       
## [196] "love to"         "love you"        "make a"         
## [199] "make it"         "make sure"       "makes me"       
## [202] "me a"            "me and"          "me i"           
## [205] "me know"         "me to"           "miss you"       
## [208] "more than"       "my favorite"     "my life"        
## [211] "my mom"          "my new"          "need a"         
## [214] "need to"         "next week"       "no one"         
## [217] "not a"           "not sure"        "not the"        
## [220] "now i"           "of a"            "of course"      
## [223] "of it"           "of my"           "of our"         
## [226] "of the"          "of this"         "of you"         
## [229] "of your"         "off to"          "on a"           
## [232] "on my"           "on the"          "on this"        
## [235] "on twitter"      "on your"         "one of"         
## [238] "out of"          "out the"         "out there"      
## [241] "out to"          "part of"         "ready for"      
## [244] "ready to"        "right now"       "rt i"           
## [247] "see the"         "see you"         "should be"      
## [250] "so i"            "so many"         "so much"        
## [253] "some of"         "such a"          "take a"         
## [256] "thank you"       "thanks for"      "thanks to"      
## [259] "that i"          "that is"         "that the"       
## [262] "that was"        "that you"        "the best"       
## [265] "the day"         "the end"         "the first"      
## [268] "the follow"      "the game"        "the last"       
## [271] "the most"        "the new"         "the next"       
## [274] "the one"         "the only"        "the other"      
## [277] "the same"        "the th"          "the time"       
## [280] "the way"         "the word"        "the world"      
## [283] "there are"       "there is"        "they are"       
## [286] "think i"         "this is"         "this morning"   
## [289] "this week"       "this weekend"    "this year"      
## [292] "time for"        "time i"          "time to"        
## [295] "to a"            "to all"          "to be"          
## [298] "to come"         "to do"           "to find"        
## [301] "to follow"       "to get"          "to go"          
## [304] "to have"         "to hear"         "to know"        
## [307] "to make"         "to me"           "to meet"        
## [310] "to my"           "to say"          "to see"         
## [313] "to start"        "to take"         "to the"         
## [316] "to watch"        "to win"          "to work"        
## [319] "to you"          "to your"         "try to"         
## [322] "trying to"       "up and"          "up for"         
## [325] "up on"           "up the"          "up to"          
## [328] "up with"         "used to"         "wait for"       
## [331] "wait to"         "want to"         "was a"          
## [334] "was the"         "way to"          "we are"         
## [337] "we can"          "we have"         "we need"        
## [340] "we will"         "what a"          "what are"       
## [343] "what do"         "what i"          "what is"        
## [346] "what the"        "what you"        "when i"         
## [349] "when you"        "will be"         "wish i"         
## [352] "with a"          "with me"         "with my"        
## [355] "with the"        "with you"        "with your"      
## [358] "working on"      "would be"        "would love"     
## [361] "you a"           "you all"         "you and"        
## [364] "you are"         "you can"         "you do"         
## [367] "you don't"       "you for"         "you get"        
## [370] "you guys"        "you have"        "you in"         
## [373] "you know"        "you like"        "you need"       
## [376] "you should"      "you think"       "you to"         
## [379] "you want"        "you were"        "you will"
```

```r
findFreqTerms(tdm.2g,lowfreq = 200)
```

```
##   [1] "a good"          "a great"         "a little"       
##   [4] "a lot"           "a new"           "about the"      
##   [7] "all of"          "all the"         "and a"          
##  [10] "and i"           "and the"         "are you"        
##  [13] "as a"            "at the"          "back to"        
##  [16] "be a"            "but i"           "can't wait"     
##  [19] "did you"         "do you"          "follow me"      
##  [22] "for a"           "for me"          "for the"        
##  [25] "for you"         "forward to"      "from the"       
##  [28] "get a"           "go to"           "going to"       
##  [31] "had a"           "have a"          "have to"        
##  [34] "hope you"        "i'm not"         "i am"           
##  [37] "i can"           "i can't"         "i could"        
##  [40] "i don't"         "i get"           "i got"          
##  [43] "i had"           "i hate"          "i have"         
##  [46] "i hope"          "i just"          "i know"         
##  [49] "i love"          "i need"          "i think"        
##  [52] "i want"          "i was"           "i will"         
##  [55] "i wish"          "i would"         "if i"           
##  [58] "if you"          "in a"            "in my"          
##  [61] "in the"          "is a"            "is it"          
##  [64] "is not"          "is the"          "it is"          
##  [67] "it was"          "last night"      "let me"         
##  [70] "like a"          "looking forward" "love you"       
##  [73] "need to"         "of a"            "of my"          
##  [76] "of the"          "on a"            "on my"          
##  [79] "on the"          "one of"          "out of"         
##  [82] "right now"       "see you"         "so i"           
##  [85] "so much"         "thank you"       "thanks for"     
##  [88] "that i"          "that is"         "the best"       
##  [91] "the day"         "the first"       "the new"        
##  [94] "the same"        "the world"       "there is"       
##  [97] "this is"         "time to"         "to a"           
## [100] "to be"           "to do"           "to get"         
## [103] "to go"           "to have"         "to make"        
## [106] "to me"           "to my"           "to see"         
## [109] "to the"          "to you"          "trying to"      
## [112] "want to"         "was a"           "way to"         
## [115] "we are"          "we have"         "when i"         
## [118] "when you"        "will be"         "with a"         
## [121] "with my"         "with the"        "would be"       
## [124] "you are"         "you can"         "you for"        
## [127] "you guys"        "you have"        "you know"
```

```r
findFreqTerms(tdm.2g,lowfreq = 300)
```

```
##  [1] "a good"     "a great"    "all the"    "and i"      "and the"   
##  [6] "are you"    "at the"     "be a"       "but i"      "can't wait"
## [11] "do you"     "for a"      "for the"    "forward to" "go to"     
## [16] "going to"   "had a"      "have a"     "have to"    "i am"      
## [21] "i can"      "i don't"    "i have"     "i just"     "i know"    
## [26] "i love"     "i need"     "i think"    "i want"     "i was"     
## [31] "i will"     "if you"     "in a"       "in my"      "in the"    
## [36] "is a"       "is the"     "it is"      "it was"     "like a"    
## [41] "love you"   "need to"    "of a"       "of my"      "of the"    
## [46] "on a"       "on my"      "on the"     "one of"     "out of"    
## [51] "right now"  "see you"    "so much"    "thank you"  "thanks for"
## [56] "the best"   "this is"    "to be"      "to do"      "to get"    
## [61] "to go"      "to see"     "to the"     "want to"    "we are"    
## [66] "when i"     "when you"   "will be"    "with the"   "you are"   
## [71] "you can"    "you have"   "you know"
```

```r
findFreqTerms(tdm.2g,lowfreq = 500)
```

```
##  [1] "a great"    "and i"      "are you"    "at the"     "for a"     
##  [6] "for the"    "going to"   "have a"     "i am"       "i don't"   
## [11] "i have"     "i just"     "i love"     "i think"    "i was"     
## [16] "if you"     "in the"     "is a"       "is the"     "need to"   
## [21] "of the"     "on the"     "thank you"  "thanks for" "to be"     
## [26] "to get"     "to see"     "to the"     "want to"    "will be"   
## [31] "you are"
```

```r
b <- findFreqTerms(tdm.2g,lowfreq = 300)
tdm.2g.l <- tdm.2g[b,]
tdm.2g.l
```

```
## <<TermDocumentMatrix (terms: 73, documents: 50000)>>
## Non-/sparse entries: 39511/3610489
## Sparsity           : 99%
## Maximal term length: 10
## Weighting          : term frequency (tf)
```

```r
tdm.2g.l.asMatrix <- as.matrix(tdm.2g.l)

term.2g.freq <- rowSums(tdm.2g.l.asMatrix)
frequentTermsDF.2g <- data.frame(term = names(term.2g.freq), freq = term.2g.freq)

frequentTermsDF.2g <- frequentTermsDF.2g[with(frequentTermsDF.2g, order(-frequentTermsDF.2g$freq)), ]

# words by frequency from subset data frame
ggplot(frequentTermsDF.2g, aes(x = reorder(term,freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-3.png)

```r
wordcloud(words = frequentTermsDF.2g$term,freq = frequentTermsDF.2g$freq)
```

```
## Warning in wordcloud(words = frequentTermsDF.2g$term, freq =
## frequentTermsDF.2g$freq): thanks for could not be fit on page. It will not
## be plotted.
```

```
## Warning in wordcloud(words = frequentTermsDF.2g$term, freq =
## frequentTermsDF.2g$freq): a great could not be fit on page. It will not be
## plotted.
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-4.png)

```r
tdm.3g <- tdm.generate.ngrams(x = data.twitter.sample[1:50000],ng = 3)
tdm.3g
```

```
## <<TermDocumentMatrix (terms: 478512, documents: 50000)>>
## Non-/sparse entries: 576817/23925023183
## Sparsity           : 100%
## Maximal term length: 102
## Weighting          : term frequency (tf)
```

```r
findFreqTerms(tdm.3g,lowfreq = 30)
```

```
##   [1] "a bunch of"           "a chance to"          "a couple of"         
##   [4] "a good day"           "a great day"          "a great time"        
##   [7] "a great weekend"      "a long time"          "a lot of"            
##  [10] "all of the"           "all the time"         "all the way"         
##  [13] "and i have"           "are going to"         "are the best"        
##  [16] "are you doing"        "are you going"        "as long as"          
##  [19] "at the end"           "at the same"          "awkward moment when" 
##  [22] "be a good"            "be able to"           "be sure to"          
##  [25] "but i don't"          "can't wait for"       "can't wait to"       
##  [28] "check it out"         "check out our"        "check out the"       
##  [31] "day to all"           "did you know"         "do you have"         
##  [34] "do you know"          "do you think"         "do you want"         
##  [37] "don't want to"        "end of the"           "every time i"        
##  [40] "for a great"          "for all the"          "for me to"           
##  [43] "for the ff"           "for the first"        "for the follow"      
##  [46] "for the next"         "for the rt"           "forward to it"       
##  [49] "forward to seeing"    "forward to the"       "get ready for"       
##  [52] "getting ready for"    "go back to"           "go to the"           
##  [55] "going to be"          "going to the"         "gonna be a"          
##  [58] "had a great"          "happy birthday to"    "happy mother's day"  
##  [61] "happy mothers day"    "happy new year"       "have a good"         
##  [64] "have a great"         "have to be"           "have to go"          
##  [67] "hit me up"            "hope to see"          "hope you have"       
##  [70] "how are you"          "how do you"           "i'm going to"        
##  [73] "i am a"               "i am not"             "i am so"             
##  [76] "i can't believe"      "i can't wait"         "i can get"           
##  [79] "i don't even"         "i don't have"         "i don't know"        
##  [82] "i don't like"         "i don't think"        "i don't want"        
##  [85] "i feel like"          "i get a"              "i get to"            
##  [88] "i got a"              "i had a"              "i had to"            
##  [91] "i hate when"          "i have a"             "i have been"         
##  [94] "i have no"            "i have the"           "i have to"           
##  [97] "i hope you"           "i just want"          "i know i"            
## [100] "i love it"            "i love my"            "i love that"         
## [103] "i love the"           "i love you"           "i love your"         
## [106] "i miss you"           "i need a"             "i need to"           
## [109] "i think i"            "i think it's"         "i think you"         
## [112] "i used to"            "i want a"             "i want to"           
## [115] "i was just"           "i will be"            "i wish i"            
## [118] "i wonder if"          "i would love"         "if you are"          
## [121] "if you don't"         "if you have"          "if you need"         
## [124] "if you want"          "in a row"             "in front of"         
## [127] "in love with"         "in my life"           "in the morning"      
## [130] "in the world"         "is a great"           "is going to"         
## [133] "is in the"            "is not a"             "is one of"           
## [136] "is the best"          "it's going to"        "it was a"            
## [139] "it will be"           "it would be"          "just want to"        
## [142] "know how to"          "know if you"          "let me know"         
## [145] "let us know"          "let you know"         "look forward to"     
## [148] "looking for a"        "looking forward to"   "looks like a"        
## [151] "love to see"          "make sure you"        "me know if"          
## [154] "much for the"         "my way to"            "need to be"          
## [157] "need to get"          "of my life"           "of the best"         
## [160] "of the day"           "of the week"          "of the year"         
## [163] "oh my god"            "on my way"            "on the way"          
## [166] "one of my"            "one of the"           "one of those"        
## [169] "out of the"           "part of the"          "please follow me"    
## [172] "ready for the"        "rest of the"          "rt if you"           
## [175] "s o to"               "see you there"        "shout out to"        
## [178] "so i can"             "so much for"          "some of the"         
## [181] "sounds like a"        "spread the word"      "thank you for"       
## [184] "thank you so"         "thank you to"         "thanks for following"
## [187] "thanks for sharing"   "thanks for the"       "thanks for your"     
## [190] "thanks so much"       "thanks to all"        "that was a"          
## [193] "that would be"        "the end of"           "the first time"      
## [196] "the only one"         "the rest of"          "the same time"       
## [199] "there is a"           "there is no"          "this is a"           
## [202] "this is the"          "time to get"          "to all of"           
## [205] "to all the"           "to be a"              "to be in"            
## [208] "to be on"             "to be the"            "to follow me"        
## [211] "to get a"             "to get my"            "to get to"           
## [214] "to go to"             "to have a"            "to have you"         
## [217] "to make it"           "to meet you"          "to see my"           
## [220] "to see the"           "to see you"           "to take a"           
## [223] "to talk about"        "to talk to"           "trying to get"       
## [226] "used to be"           "wait to see"          "want to be"          
## [229] "want to go"           "want to know"         "want to see"         
## [232] "was going to"         "we have a"            "we need to"          
## [235] "we will be"           "what a great"         "what are you"        
## [238] "what do you"          "when i was"           "where are you"       
## [241] "will be a"            "wish i could"         "wish i was"          
## [244] "would like to"        "would love to"        "you at the"          
## [247] "you for the"          "you going to"         "you guys are"        
## [250] "you have a"           "you have to"          "you know that"       
## [253] "you know what"        "you need to"          "you so much"         
## [256] "you want to"
```

```r
findFreqTerms(tdm.3g,lowfreq = 40)
```

```
##   [1] "a good day"           "a great day"          "a great time"        
##   [4] "a great weekend"      "a lot of"             "all the time"        
##   [7] "are going to"         "are you going"        "as long as"          
##  [10] "at the end"           "be able to"           "be sure to"          
##  [13] "can't wait for"       "can't wait to"        "check it out"        
##  [16] "do you have"          "do you think"         "don't want to"       
##  [19] "end of the"           "for the first"        "for the follow"      
##  [22] "for the rt"           "go to the"            "going to be"         
##  [25] "going to the"         "gonna be a"           "had a great"         
##  [28] "happy birthday to"    "happy mothers day"    "have a good"         
##  [31] "have a great"         "hope to see"          "how are you"         
##  [34] "how do you"           "i'm going to"         "i am a"              
##  [37] "i can't wait"         "i don't have"         "i don't know"        
##  [40] "i don't think"        "i feel like"          "i got a"             
##  [43] "i had a"              "i had to"             "i have a"            
##  [46] "i have to"            "i hope you"           "i know i"            
##  [49] "i love it"            "i love my"            "i love that"         
##  [52] "i love the"           "i love you"           "i miss you"          
##  [55] "i need a"             "i need to"            "i think i"           
##  [58] "i think you"          "i want to"            "i will be"           
##  [61] "i wish i"             "if you are"           "if you have"         
##  [64] "if you want"          "in my life"           "in the morning"      
##  [67] "in the world"         "is going to"          "is the best"         
##  [70] "it was a"             "it will be"           "it would be"         
##  [73] "let me know"          "let us know"          "look forward to"     
##  [76] "looking for a"        "looking forward to"   "need to get"         
##  [79] "of the day"           "of the year"          "on my way"           
##  [82] "on the way"           "one of my"            "one of the"          
##  [85] "one of those"         "out of the"           "please follow me"    
##  [88] "ready for the"        "see you there"        "shout out to"        
##  [91] "so i can"             "so much for"          "some of the"         
##  [94] "thank you for"        "thank you so"         "thank you to"        
##  [97] "thanks for following" "thanks for the"       "thanks so much"      
## [100] "that would be"        "the end of"           "the first time"      
## [103] "the rest of"          "there is a"           "there is no"         
## [106] "this is a"            "this is the"          "to all the"          
## [109] "to be a"              "to be in"             "to be the"           
## [112] "to go to"             "to have a"            "to make it"          
## [115] "to meet you"          "to see the"           "to see you"          
## [118] "wait to see"          "want to be"           "want to go"          
## [121] "we need to"           "what are you"         "what do you"         
## [124] "wish i could"         "would like to"        "would love to"       
## [127] "you for the"          "you going to"         "you guys are"        
## [130] "you have a"           "you have to"          "you know what"       
## [133] "you need to"          "you so much"          "you want to"
```

```r
findFreqTerms(tdm.3g,lowfreq = 50)
```

```
##  [1] "a great day"          "a lot of"             "all the time"        
##  [4] "be able to"           "can't wait for"       "can't wait to"       
##  [7] "check it out"         "do you have"          "do you think"        
## [10] "don't want to"        "for the follow"       "for the rt"          
## [13] "go to the"            "going to be"          "had a great"         
## [16] "have a great"         "hope to see"          "how are you"         
## [19] "how do you"           "i'm going to"         "i can't wait"        
## [22] "i don't know"         "i don't think"        "i feel like"         
## [25] "i had a"              "i have a"             "i have to"           
## [28] "i hope you"           "i love it"            "i love the"          
## [31] "i love you"           "i miss you"           "i need a"            
## [34] "i need to"            "i think i"            "i want to"           
## [37] "i will be"            "i wish i"             "if you are"          
## [40] "if you have"          "if you want"          "in the morning"      
## [43] "in the world"         "is going to"          "is the best"         
## [46] "it was a"             "it would be"          "let me know"         
## [49] "let us know"          "look forward to"      "looking forward to"  
## [52] "of the day"           "one of my"            "one of the"          
## [55] "out of the"           "please follow me"     "so i can"            
## [58] "so much for"          "thank you for"        "thanks for following"
## [61] "thanks for the"       "thanks so much"       "the end of"          
## [64] "the first time"       "the rest of"          "there is no"         
## [67] "to be a"              "to go to"             "to have a"           
## [70] "to see the"           "to see you"           "wait to see"         
## [73] "want to be"           "we need to"           "what are you"        
## [76] "what do you"          "wish i could"         "would like to"       
## [79] "would love to"        "you for the"          "you have a"          
## [82] "you have to"          "you need to"          "you so much"         
## [85] "you want to"
```

```r
findFreqTerms(tdm.3g,lowfreq = 60)
```

```
##  [1] "a great day"          "a lot of"             "be able to"          
##  [4] "can't wait for"       "can't wait to"        "do you have"         
##  [7] "for the follow"       "for the rt"           "going to be"         
## [10] "had a great"          "have a great"         "how are you"         
## [13] "i'm going to"         "i can't wait"         "i don't know"        
## [16] "i feel like"          "i had a"              "i have a"            
## [19] "i have to"            "i hope you"           "i love you"          
## [22] "i need to"            "i think i"            "i want to"           
## [25] "i will be"            "i wish i"             "if you are"          
## [28] "if you want"          "in the world"         "is going to"         
## [31] "is the best"          "it was a"             "let me know"         
## [34] "let us know"          "look forward to"      "looking forward to"  
## [37] "of the day"           "one of my"            "one of the"          
## [40] "out of the"           "so i can"             "so much for"         
## [43] "thank you for"        "thanks for following" "thanks for the"      
## [46] "thanks so much"       "the end of"           "to be a"             
## [49] "to go to"             "to see the"           "to see you"          
## [52] "wait to see"          "we need to"           "what do you"         
## [55] "wish i could"         "would love to"        "you for the"         
## [58] "you have a"           "you have to"          "you so much"         
## [61] "you want to"
```

```r
c <- findFreqTerms(tdm.3g,lowfreq = 50)
tdm.3g.l <- tdm.3g[c,]
tdm.3g.l
```

```
## <<TermDocumentMatrix (terms: 85, documents: 50000)>>
## Non-/sparse entries: 7537/4242463
## Sparsity           : 100%
## Maximal term length: 20
## Weighting          : term frequency (tf)
```

```r
tdm.3g.l.asMatrix <- as.matrix(tdm.3g.l)

term.3g.freq <- rowSums(tdm.3g.l.asMatrix)
frequentTermsDF.3g <- data.frame(term = names(term.3g.freq), freq = term.3g.freq)

frequentTermsDF.3g <- frequentTermsDF.3g[with(frequentTermsDF.3g, order(-frequentTermsDF.3g$freq)), ]

# words by frequency from subset data frame
ggplot(frequentTermsDF.3g, aes(x = reorder(term,freq), y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Frequency") + coord_flip()
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-5.png)

```r
wordcloud(words = frequentTermsDF.3g$term,freq = frequentTermsDF.3g$freq)
```

```
## Warning in wordcloud(words = frequentTermsDF.3g$term, freq =
## frequentTermsDF.3g$freq): for the follow could not be fit on page. It will
## not be plotted.
```

```
## Warning in wordcloud(words = frequentTermsDF.3g$term, freq =
## frequentTermsDF.3g$freq): of the day could not be fit on page. It will not
## be plotted.
```

```
## Warning in wordcloud(words = frequentTermsDF.3g$term, freq =
## frequentTermsDF.3g$freq): i want to could not be fit on page. It will not
## be plotted.
```

```
## Warning in wordcloud(words = frequentTermsDF.3g$term, freq =
## frequentTermsDF.3g$freq): you have a could not be fit on page. It will not
## be plotted.
```

![](02_dataAnalysisAndCleaning_files/figure-html/visualizeData-6.png)

