generate.corpora.3gramModel <- function(noElements){
    
    load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.3g.rdata")
    data.3g <- d.ng.df
    load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.2g.rdata")
    data.2g <- d.ng.df
    d.ng.df <- NULL
    
    ##Term Counters Ordered By Frequency
    idx <- order(data.3g$total, decreasing = T)
    data.3g <- data.3g[idx,]
    
    ##Term Counters Ordered By Frequency
    idx <- order(data.2g$total, decreasing = T)
    data.2g <- data.2g[idx,]
    idx <- NULL
    
    data.3g.model <- trigrams.model(t.terms = data.3g$terms[1:noElements], t.counters = data.3g$total[1:noElements],
                                    b.terms = data.2g$terms, b.counters = data.2g$total)
    
    data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
                                   next.word= data.3g.model$t.nextWord, 
                                   count = data.3g.model$t.count,
                                   probability = data.3g.model$t.probability)
    
    
    
    filename <- paste("./../scripts/trigramLanguageModel/allCorpora.3gModel_", Sys.Date(), ".rdata", sep = "")
    save(data.3g, data.2g, data.3g.model, data.3g.model.df, file = filename)
    filename
}



generate.twitter.3gramModel <- function(folder, 
                                        file.allTermFrequency.3g, file.tdm.3g,
                                        file.allTermFrequency.2g, file.tdm.2g,
                                        noElements){
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
    f.tdm <- paste(folder, file.tdm.3g, sep = "")
    data.3g <- load.twitter.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                    file_url_tdm = f.tdm)
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
    f.tdm <- paste(folder, file.tdm.2g, sep = "")
    data.2g <- load.twitter.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                    file_url_tdm = f.tdm)
    
    ##Term Counters Ordered By Frequency
    data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
    data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
    
    
    data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
                                    b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
    
    data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
                                   next.word= data.3g.model$t.nextWord, 
                                   count = data.3g.model$t.count,
                                   probability = data.3g.model$t.probability)
    
    filename <- paste("./../scripts/trigramLanguageModel/twitter.3gModel_", Sys.Date(), ".rdata", sep = "")
    save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
    filename
}

generate.news.3gramModel <- function(folder, 
                                     file.allTermFrequency.3g, file.tdm.3g,
                                     file.allTermFrequency.2g, file.tdm.2g,
                                     noElements){
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
    f.tdm <- paste(folder, file.tdm.3g, sep = "")
    data.3g <- load.news.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                 file_url_tdm = f.tdm)
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
    f.tdm <- paste(folder, file.tdm.2g, sep = "")
    data.2g <- load.news.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                 file_url_tdm = f.tdm)
    
    ##Term Counters Ordered By Frequency
    data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
    data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
    
    
    data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
                                    b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
    
    data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
                                   next.word= data.3g.model$t.nextWord, 
                                   count = data.3g.model$t.count,
                                   probability = data.3g.model$t.probability)
    
    filename <- paste("./../scripts/trigramLanguageModel/news.3gModel_", Sys.Date(), ".rdata", sep = "")
    save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
    filename
}

generate.blogs.3gramModel <- function(folder, 
                                     file.allTermFrequency.3g, file.tdm.3g,
                                     file.allTermFrequency.2g, file.tdm.2g,
                                     noElements){
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
    f.tdm <- paste(folder, file.tdm.3g, sep = "")
    data.3g <- load.blogs.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                 file_url_tdm = f.tdm)
    
    f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
    f.tdm <- paste(folder, file.tdm.2g, sep = "")
    data.2g <- load.blogs.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
                                 file_url_tdm = f.tdm)
    
    ##Term Counters Ordered By Frequency
    data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
    data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
    
    
    data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
                                    b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
    
    data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
                                   next.word= data.3g.model$t.nextWord, 
                                   count = data.3g.model$t.count,
                                   probability = data.3g.model$t.probability)
    
    filename <- paste("./../scripts/trigramLanguageModel/blogs.3gModel_", Sys.Date(), ".rdata", sep = "")
    save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
    filename
}