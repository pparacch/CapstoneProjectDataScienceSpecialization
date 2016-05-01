
# Load the Term Document Matrix created for the Sample Corpora 
# Twitter, News, Blogs

# folder <- "./datasetDumps/"
# 
# load(paste(folder, "twitter.tdm.1g.Rdata", sep = ""))
# load(paste(folder, "twitter.tdm.2g.Rdata", sep = ""))
# load(paste(folder, "twitter.tdm.3g.Rdata", sep = ""))
# 
# load(paste(folder, "twitter.allTermsFrequency.1g.Rdata", sep = ""))
# twitter.allTerms.1g <- corpora.allTermsFrequency
# load(paste(folder, "twitter.allTermsFrequency.2g.Rdata", sep = ""))
# twitter.allTerms.2g <- corpora.allTermsFrequency
# load(paste(folder, "twitter.allTermsFrequency.3g.Rdata", sep = ""))
# twitter.allTerms.3g <- corpora.allTermsFrequency
# 
# corpora.allTermsFrequency <- NULL
# 
# #Size of allTerms frequency by Ngrams
# utils:::format.object_size(object.size(twitter.allTerms.1g), "auto")
# utils:::format.object_size(object.size(twitter.allTerms.2g), "auto")
# utils:::format.object_size(object.size(twitter.allTerms.3g), "auto")
# 
load.twitter.1g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.1g.Rdata", sep = ""))
    twitter.allTerms.1g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.1g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.1g, allTermsCounters = twitter.allTerms.1g)
}

load.twitter.2g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.2g.Rdata", sep = ""))
    twitter.allTerms.2g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.2g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.2g, allTermsCounters = twitter.allTerms.2g)
}

load.twitter.3g.data <- function(folder){
    load(paste(folder, "twitter.allTermsFrequency.3g.Rdata", sep = ""))
    twitter.allTerms.3g <- corpora.allTermsFrequency
    load(paste(folder, "twitter.tdm.3g.Rdata", sep = ""))
    
    list(tdm = twitter.corpora.tdm.3g, allTermsCounters = twitter.allTerms.3g)
}



## Start to investigate items with certain frequency
# a.terms <- rownames(twitter.allTerms.1g)
# a.counters <- twitter.allTerms.1g$freq
# a.N <- sum(twitter.allTerms.1g$freq)
# a.V <- length(unique(rownames(twitter.allTerms.1g)))
# hist(twitter.allTerms.1g$freq)
