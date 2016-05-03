# Language Models
Pier Lorenzo Paracchini  
3 mai 2016  



# Language Model Creation - some Notes

## Unigram Language Model


```r
tw.1g.list <- load.twitter.1g.data("./../data/processed/")

tw.1g.terms <- rownames(tw.1g.list$allTermsCounters)
tw.1g.counters <- tw.1g.list$allTermsCounters$freq
tw.1g.N <- sum(tw.1g.counters)
tw.1g.V <- length(unique(tw.1g.terms))

#Order by frequency (decreasing)
idx <- order(tw.1g.counters, decreasing = T)
tw.1g.counters <- tw.1g.counters[idx]
tw.1g.terms <- tw.1g.terms[idx]
tw.1g.cumulativeCoverage <- (cumsum(tw.1g.counters)/ tw.1g.N) * 100
idx <- NULL

idx <- which(tw.1g.counters > 1)
length(idx) #25997 words 
## [1] 25997

plot(tw.1g.cumulativeCoverage)
```

![](04_languageModels_files/figure-html/unigramModelCreate-1.png)

```r
idx <- which(tw.1g.cumulativeCoverage <= 90)
length(idx) #5248 words 
## [1] 5248

##Simplification: consider the words in order to have a max cumulative coverage of 98%
idx <- which(tw.1g.cumulativeCoverage <= 98)
tw.1g.counters <- tw.1g.counters[idx]
tw.1g.terms <- tw.1g.terms[idx]
tw.1g.N <- sum(tw.1g.counters)
tw.1g.V <- length(unique(tw.1g.terms))
tw.1g.cumulativeCoverage <- (cumsum(tw.1g.counters)/ tw.1g.N) * 100
plot(tw.1g.cumulativeCoverage)
```

![](04_languageModels_files/figure-html/unigramModelCreate-2.png)

```r


##Create the twitter unigram model
Rprof(tmp <- tempfile())
tw.1g.model <- unigrams.model(u.words = tw.1g.terms, u.counters = tw.1g.counters)
## [1] "unigrams.model::processed 1000 of 32731"
## [1] "unigrams.model::processed 2000 of 32731"
## [1] "unigrams.model::processed 3000 of 32731"
## [1] "unigrams.model::processed 4000 of 32731"
## [1] "unigrams.model::processed 5000 of 32731"
## [1] "unigrams.model::processed 6000 of 32731"
## [1] "unigrams.model::processed 7000 of 32731"
## [1] "unigrams.model::processed 8000 of 32731"
## [1] "unigrams.model::processed 9000 of 32731"
## [1] "unigrams.model::processed 10000 of 32731"
## [1] "unigrams.model::processed 11000 of 32731"
## [1] "unigrams.model::processed 12000 of 32731"
## [1] "unigrams.model::processed 13000 of 32731"
## [1] "unigrams.model::processed 14000 of 32731"
## [1] "unigrams.model::processed 15000 of 32731"
## [1] "unigrams.model::processed 16000 of 32731"
## [1] "unigrams.model::processed 17000 of 32731"
## [1] "unigrams.model::processed 18000 of 32731"
## [1] "unigrams.model::processed 19000 of 32731"
## [1] "unigrams.model::processed 20000 of 32731"
## [1] "unigrams.model::processed 21000 of 32731"
## [1] "unigrams.model::processed 22000 of 32731"
## [1] "unigrams.model::processed 23000 of 32731"
## [1] "unigrams.model::processed 24000 of 32731"
## [1] "unigrams.model::processed 25000 of 32731"
## [1] "unigrams.model::processed 26000 of 32731"
## [1] "unigrams.model::processed 27000 of 32731"
## [1] "unigrams.model::processed 28000 of 32731"
## [1] "unigrams.model::processed 29000 of 32731"
## [1] "unigrams.model::processed 30000 of 32731"
## [1] "unigrams.model::processed 31000 of 32731"
## [1] "unigrams.model::processed 32000 of 32731"
Rprof(NULL)
summaryRprof(tmp)
## $by.self
##                               self.time self.pct total.time total.pct
## "=="                              43.80    90.38      43.80     90.38
## "which"                            3.72     7.68      47.52     98.06
## "sum"                              0.62     1.28       0.62      1.28
## "unigrams.model"                   0.22     0.45      48.46    100.00
## "unigrams.probabilityForWord"      0.08     0.17      48.22     99.50
## "%%"                               0.02     0.04       0.02      0.04
## 
## $by.total
##                               total.time total.pct self.time self.pct
## "unigrams.model"                   48.46    100.00      0.22     0.45
## "<Anonymous>"                      48.46    100.00      0.00     0.00
## "block_exec"                       48.46    100.00      0.00     0.00
## "call_block"                       48.46    100.00      0.00     0.00
## "eval"                             48.46    100.00      0.00     0.00
## "evaluate_call"                    48.46    100.00      0.00     0.00
## "handle"                           48.46    100.00      0.00     0.00
## "in_dir"                           48.46    100.00      0.00     0.00
## "process_file"                     48.46    100.00      0.00     0.00
## "process_group"                    48.46    100.00      0.00     0.00
## "process_group.block"              48.46    100.00      0.00     0.00
## "withCallingHandlers"              48.46    100.00      0.00     0.00
## "withVisible"                      48.46    100.00      0.00     0.00
## "unigrams.probabilityForWord"      48.22     99.50      0.08     0.17
## "which"                            47.52     98.06      3.72     7.68
## "=="                               43.80     90.38     43.80    90.38
## "sum"                               0.62      1.28      0.62     1.28
## "%%"                                0.02      0.04      0.02     0.04
## 
## $sample.interval
## [1] 0.02
## 
## $sampling.time
## [1] 48.46

save(tw.1g.terms, tw.1g.counters, tw.1g.V, tw.1g.N, tw.1g.cumulativeCoverage, tw.1g.model,
     file = "./../data/processed/model.twitter.1g.Rdata")


Rprof(tmp <- tempfile())
unigramModel.probabilityForWord(u.model = tw.1g.model, word = "<s>")
## [1] 0.08292
Rprof(NULL)
summaryRprof(tmp)
## $by.self
## [1] self.time  self.pct   total.time total.pct 
## <0 rows> (or 0-length row.names)
## 
## $by.total
## [1] total.time total.pct  self.time  self.pct  
## <0 rows> (or 0-length row.names)
## 
## $sample.interval
## [1] 0.02
## 
## $sampling.time
## [1] 0


Rprof(tmp <- tempfile())
unigramModel.probabilityForWord(u.model = tw.1g.model, word = "i'm")
## [1] 0.004767261
Rprof(NULL)
summaryRprof(tmp)
## $by.self
## [1] self.time  self.pct   total.time total.pct 
## <0 rows> (or 0-length row.names)
## 
## $by.total
## [1] total.time total.pct  self.time  self.pct  
## <0 rows> (or 0-length row.names)
## 
## $sample.interval
## [1] 0.02
## 
## $sampling.time
## [1] 0

Rprof(tmp <- tempfile())
unigramModel.probabilityForWord(u.model = tw.1g.model, word = "sentimental")
## [1] 1.548315e-06
Rprof(NULL)
summaryRprof(tmp)
## $by.self
## [1] self.time  self.pct   total.time total.pct 
## <0 rows> (or 0-length row.names)
## 
## $by.total
## [1] total.time total.pct  self.time  self.pct  
## <0 rows> (or 0-length row.names)
## 
## $sample.interval
## [1] 0.02
## 
## $sampling.time
## [1] 0
```
