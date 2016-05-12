##THIS IS VERY SLOW
# generate.corpora.3gramModel <- function(noElements){
#     
#     load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.3g.rdata")
#     data.3g <- d.ng.df
#     load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.2g.rdata")
#     data.2g <- d.ng.df
#     d.ng.df <- NULL
#     
#     ##Term Counters Ordered By Frequency
#     idx <- order(data.3g$total, decreasing = T)
#     data.3g <- data.3g[idx,]
#     
#     ##Term Counters Ordered By Frequency
#     idx <- order(data.2g$total, decreasing = T)
#     data.2g <- data.2g[idx,]
#     idx <- NULL
#     
#     
#     data.3g.model <- trigrams.model(t.terms = data.3g$terms[1:noElements], t.counters = data.3g$total[1:noElements],
#                                     b.terms = data.2g$terms, b.counters = data.2g$total)
#     
#     data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
#                                    next.word= data.3g.model$t.nextWord, 
#                                    count = data.3g.model$t.count,
#                                    probability = data.3g.model$t.probability)
#     
#     
#     
#     filename <- paste("./../scripts/trigramLanguageModel/allCorpora.3gModel_", Sys.Date(), ".rdata", sep = "")
#     save(data.3g, data.2g, data.3g.model, data.3g.model.df, file = filename)
#     filename
# }

generate.corpora.3gramModel.i <- function(noElements){
    
    load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.3g.rdata")
    data.3g <- d.ng.df
    load(file = "./../data/processed/04_s01_allCorpora_aggregated_termsFrequency.2g.rdata")
    data.2g <- d.ng.df
    d.ng.df <- NULL
    
    noElements = dim(data.3g)[1]
    print(paste("noElements:", noElements))
    data.3g.model <- data.3g[1:noElements, c("terms", "total")]
    names(data.3g.model) <- c("trigram", "trigram.count")
    
    print("Normalize trigram...")
    print("   Adding bigram...")
    data.3g.model$bigram <- sapply(data.3g.model$trigram, function(x){paste(unlist(strsplit(x, split = " "))[1:2], collapse = " ")})
    print("   Adding next.word...")
    data.3g.model$next.word <- sapply(data.3g.model$trigram, function(x){unlist(strsplit(x, split = " "))[3]})
    print("   Adding bigram.count...")
    a <- merge(x = data.3g.model, y = data.2g, by.x = "bigram", by.y = "terms", all.x = T)
    data.3g.model <- a[,c("trigram", "bigram", "next.word", "trigram.count", "total")]
    names(data.3g.model) <- c("trigram", "bigram", "next.word", "trigram.count", "bigram.count")
    print("   Adding probability...")
    data.3g.model$probability <- data.3g.model$trigram.count / data.3g.model$bigram.count
    
    a <- NULL
    print("   Creating data structure & saving locally...")
    ##Term Counters Ordered By Frequency
    data.3g.model <- data.3g.model[order(data.3g.model$probability, decreasing = T),]
    
    data.3g.model.df <- data.frame(bigram = data.3g.model$bigram, 
                                   next.word= data.3g.model$next.word, 
                                   count = data.3g.model$trigram.count,
                                   count.b = data.3g.model$bigram.count,
                                   probability = data.3g.model$probability)
    
    filename <- paste("./../scripts/trigramLanguageModel/allCorpora.3gModel_i_", Sys.Date(), ".rdata", sep = "")
    save(data.3g, data.2g, data.3g.model, data.3g.model.df, file = filename)
    filename
}

#############################################################################
# estimateSentenceProbabilities                                             #
# Probability Calculation for a sentence using the ChainRule                #
# Trigram Language Model Aprroximation                                      #
# log(Proability) = log(p1) + log(p2) + .. + log(pn)                        #
# Trigram Language Model Aprroximation                                      #
# s: sentence, N: number of words in s                     #
#---------------------------------------------------------------------------#
# Inputs:                                                                   #
# s: sentence (normalized)                                                  #
# model.3g: dataframe representing the model                                #
# bigram, next.word,count, count.b, probability                             #
# (wi-2, wi-1), wi, c((wi-2, wi-1, wi)), c((wi-2, wi-1)),                   #
# probability = c((wi-2, wi-1, wi))/c((wi-2, wi-1))                         #
#############################################################################

# sentence -> normalized sentence, e.g. "<s>" and "</s>" added, 
# toLowerCase, remove punctuations and numbers
estimateSentenceProbabilities.mle <- function(s, x){
    s.words <- ngramTokenize(y = s, ng = 1)
    N <- length(s.words) - 1 #Do not count the beginning of the sentence marker <s>
    
    print(paste("###########sentenceProbability"))
    print(paste("#    sentence:", s, ", NoOfWords:", N))
    
    result <- 0
    s.trigrams <- ngramTokenize(y = s, ng = 3)
    
    for(i in 1:length(s.trigrams)){
        d.t <- s.trigrams[i]
        d.t.ws <- ngramTokenize(d.t,1)
        d.t.w_i <- d.t.ws[3]
        d.t.b <- paste(d.t.ws[1], d.t.ws[2])
        
        print(paste("#     bigram_i-1:", d.t.b))
        print(paste("#         word_i:", d.t.w_i))
        
        idx <- which(x$bigram == d.t.b & x$next.word == d.t.w_i)
        
        if(length(idx) == 1){
            entryFound <- x[which(x$bigram == d.t.b & x$next.word == d.t.w_i),]
            print(paste("# ------------->bigram:", entryFound$bigram, ", next.word:", entryFound$next.word ,", prob:", entryFound$probability))
            tmp <- log(entryFound$probability, base = 2)
            result <- result + tmp
            print(paste("#    prob(log):", tmp, ", total(log):", result))
        }else{
            print("# =============>Not Found")
            tmp <- log(0, base = 2)
            result <- result + tmp
            print(paste("#    prob(log):", tmp, ", total(log):", result))
            break
        }
    }
    
    data.frame(sentence = s, probability_log_base2 = result, noOfWords.sentence = N, stringsAsFactors = F)
}



calculatePerplexity <- function(evaluation.as.df){
    
    elements <- dim(evaluation.as.df)[1]
    probs <- unlist(evaluation.as.df[(elements + 1):(elements * 2)])
    Ns <- unlist(evaluation.as.df[(elements * 2) + 1 : (elements * 3)])
    
    print(paste("Probs:", paste(probs, collapse = ", ")))
    print(paste("   Ns:", paste(Ns, collapse = ", ")))
          
    if(sum(is.infinite(probs))){
        return(NA)
    }else{
        return(-1 * sum(probs)/ sum(Ns))
    }
}



# generate.twitter.3gramModel <- function(folder, 
#                                         file.allTermFrequency.3g, file.tdm.3g,
#                                         file.allTermFrequency.2g, file.tdm.2g,
#                                         noElements){
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
#     f.tdm <- paste(folder, file.tdm.3g, sep = "")
#     data.3g <- load.twitter.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                     file_url_tdm = f.tdm)
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
#     f.tdm <- paste(folder, file.tdm.2g, sep = "")
#     data.2g <- load.twitter.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                     file_url_tdm = f.tdm)
#     
#     ##Term Counters Ordered By Frequency
#     data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
#     data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
#     
#     
#     data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
#                                     b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
#     
#     data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
#                                    next.word= data.3g.model$t.nextWord, 
#                                    count = data.3g.model$t.count,
#                                    probability = data.3g.model$t.probability)
#     
#     filename <- paste("./../scripts/trigramLanguageModel/twitter.3gModel_", Sys.Date(), ".rdata", sep = "")
#     save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
#     filename
# }
# 
# generate.news.3gramModel <- function(folder, 
#                                      file.allTermFrequency.3g, file.tdm.3g,
#                                      file.allTermFrequency.2g, file.tdm.2g,
#                                      noElements){
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
#     f.tdm <- paste(folder, file.tdm.3g, sep = "")
#     data.3g <- load.news.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                  file_url_tdm = f.tdm)
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
#     f.tdm <- paste(folder, file.tdm.2g, sep = "")
#     data.2g <- load.news.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                  file_url_tdm = f.tdm)
#     
#     ##Term Counters Ordered By Frequency
#     data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
#     data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
#     
#     
#     data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
#                                     b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
#     
#     data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
#                                    next.word= data.3g.model$t.nextWord, 
#                                    count = data.3g.model$t.count,
#                                    probability = data.3g.model$t.probability)
#     
#     filename <- paste("./../scripts/trigramLanguageModel/news.3gModel_", Sys.Date(), ".rdata", sep = "")
#     save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
#     filename
# }
# 
# generate.blogs.3gramModel <- function(folder, 
#                                      file.allTermFrequency.3g, file.tdm.3g,
#                                      file.allTermFrequency.2g, file.tdm.2g,
#                                      noElements){
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.3g, sep = "")
#     f.tdm <- paste(folder, file.tdm.3g, sep = "")
#     data.3g <- load.blogs.3g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                  file_url_tdm = f.tdm)
#     
#     f.allTermFrequency <- paste(folder, file.allTermFrequency.2g, sep = "")
#     f.tdm <- paste(folder, file.tdm.2g, sep = "")
#     data.2g <- load.blogs.2g.data(file_url_allTermsFrequency = f.allTermFrequency,
#                                  file_url_tdm = f.tdm)
#     
#     ##Term Counters Ordered By Frequency
#     data.3g.tc.info <- orderElementsByFrequency(tcv = data.3g, decreasing = T)
#     data.2g.tc.info <- orderElementsByFrequency(tcv = data.2g, decreasing = T)
#     
#     
#     data.3g.model <- trigrams.model(t.terms = data.3g.tc.info$terms[1:noElements], t.counters = data.3g.tc.info$counters[1:noElements],
#                                     b.terms = data.2g.tc.info$terms, b.counters = data.2g.tc.info$counters)
#     
#     data.3g.model.df <- data.frame(bigram = data.3g.model$t.bigram, 
#                                    next.word= data.3g.model$t.nextWord, 
#                                    count = data.3g.model$t.count,
#                                    probability = data.3g.model$t.probability)
#     
#     filename <- paste("./../scripts/trigramLanguageModel/blogs.3gModel_", Sys.Date(), ".rdata", sep = "")
#     save(data.3g.tc.info, data.2g.tc.info, data.3g.model, data.3g.model.df, file = filename)
#     filename
# }