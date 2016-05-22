require(openNLP)
require(NLP)


extract_sentences <- function(sourceFilePath, outputFolder){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder, tail(unlist(strsplit(sourceFilePath, "/")),1), "_sentences.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'..."))
    
    #Segmentation of blog corpus
    #Remove less that 150 chars
    print("* Blogs - Removing with less than 150 characters....")
    corpus <- i_corpus_remove_short_entries(sample$blogsCorpus, 150)
    print(paste("**** Segmentation into sentences (", length(corpus), " blogs)....", sep = ""))
    blogs.s <- unlist(sapply(X = corpus, FUN = i_find_sentences), use.names = F)
    print(paste("**** Sentences (", length(blogs.s), ")....", sep = ""))
    
    #Segmentation of News corpus
    #Remove less that 150 chars
    print("* News - Removing with less than 150 characters....")
    corpus <- i_corpus_remove_short_entries(sample$newsCorpus, 150)
    print(paste("**** Segmentation into sentences (", length(corpus), " news)....", sep = ""))
    news.s <- unlist(sapply(X = corpus, FUN = i_find_sentences), use.names = F)
    print(paste("**** Sentences (", length(news.s), ")....", sep = ""))
    
    #Segmentation of twitters corpus
    #Remove less that 50 chars
    print("* Tweets - Removing with less than 60 characters....")
    corpus <- i_corpus_remove_short_entries(sample$twitterCorpus, 60)
    print(paste("**** Segmentation into sentences (", length(corpus), " tweets)....", sep = ""))
    twitter.s <- unlist(sapply(X = corpus, FUN = i_find_sentences), use.names = F)
    print(paste("**** Sentences (", length(twitter.s), ")....", sep = ""))
    
    result <- list(twitterCorpus = twitter.s, newsCorpus = news.s, blogsCorpus = blogs.s)
    save(result, file = outputFilePath)
    
    return(outputFilePath)
}

#corpora: list(twitterCorpus, newsCorpus, blogsCorpus), each being a character vector
i_corpora_remove_short_entries <- function(corpora, minNoOfChars = 20){
    lapply(corpora, i_corpus_remove_short_entries, minNoOfChars)
}

#corpus is a character vector
i_corpus_remove_short_entries <- function(corpus, minNoOfChars){
    corpus[nchar(corpus) >= minNoOfChars]
}


i_find_sentences <- function(entry, annotator = Maxent_Sent_Token_Annotator()){
    entry.asString <-  as.String(entry)
    tmp <- annotate(entry.asString, annotator)
    return(as.vector(entry.asString[tmp]))
}

#corpora as a list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
corpora_text_cleaning <- function(sourceFilePath, outputFolder){
    print(paste("* Loading '", sourceFilePath, "'..."))
    load(sourceFilePath)
    outputFilePath <- paste(outputFolder, tail(unlist(strsplit(sourceFilePath, "/")),1), "_cleaned.rdata",sep = "")
    print(paste("* Saving as '", outputFilePath, "'..."))
    
    print("* corpora_cleaning::remove::gremlings(coerced to ASCII)")
    tmp <- lapply(result, iconv, from = localeToCharset(), to = "ASCII", "")
    
    print("* corpora_cleaning::remove::RT_retweeted")
    tmp <- lapply(tmp, i_remove_RT_retweetted)
    
    print("* corpora_cleaning::replace::remove_contractions")
    tmp <- lapply(tmp, i_remove_contractions)
    
    print("* corpora_cleaning::normalize_abbreviation")
    tmp <- lapply(tmp, i_normalize_abbreviations)
    
    print("* corpora_cleaning::manage_apostrophe")
    tmp <- lapply(tmp, i_manage_apostrophe)
    
    result <- tmp
    tmp <- NULL
    
    save(result, file = outputFilePath)
    
    return(outputFilePath)
}


i_remove_RT_retweetted <- function(texts){
    gsub(pattern = "RT", replacement = " ", x = texts, ignore.case = F, perl = T)
}

i_remove_contractions <- function(theTexts){
    rem.contr.tmp <- i_replace_contraction(texts = theTexts, contraction = " u ", replaceWith = " you ", ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = " r ", replaceWith = " are ", ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "c'mon", replaceWith = "come on",ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "doin'", replaceWith = "doing",ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "[yY]a?'a?ll", replaceWith = "you all",ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "ma'am", replaceWith = "madam",ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "a\\.m\\.?", replaceWith = "am",ignoreCase = T)
    rem.contr.tmp <- i_replace_contraction(texts = rem.contr.tmp, contraction = "p\\.m\\.?", replaceWith = "pm",ignoreCase = T)
    rem.contr.tmp
}

i_replace_contraction <- function(texts, contraction, replaceWith, ignoreCase = F){
    gsub(pattern = contraction, replacement = replaceWith, x = texts, ignore.case = ignoreCase, perl = T)
}

i_normalize_abbreviations <- function(theTexts){
    tmp <- i_replace_contraction(texts = theTexts, contraction = "^u.s.([[:blank:]|[:punct:]])", replaceWith = "usa\\1", ignoreCase = T)
    tmp <- i_replace_contraction(texts = tmp, contraction = "([[:blank:]|[:punct:]])u.s.$", replaceWith = "\\1usa", ignoreCase = T)
    tmp <- i_replace_contraction(texts = tmp, contraction = "([[:blank:]|[:punct:]])u.s.([[:blank:]|[:punct:]])", replaceWith = "\\1usa\\2", ignoreCase = T)
    tmp
}

i_manage_apostrophe <- function(theTexts){
    tmp <- i_remove_multipleConsecutiveApostrophes(theTexts)
    i_normalize_wordsBetweenApostrophes(tmp)
}

i_normalize_wordsBetweenApostrophes <- function(theTexts){
    i_replace_contraction(texts = theTexts, contraction = "'([[:alpha:]]+)'", replaceWith = "\\1", ignoreCase = T)
}

i_remove_multipleConsecutiveApostrophes <- function(theTexts){
    i_replace_contraction(texts = theTexts, contraction = "'{2,}", replaceWith = "", ignoreCase = T)
}

