print("##########################################")
print("###############Unigrams###################")
print("##########################################")

rm(list = ls())
source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)

u.test.input.words <- c("the", "time", "of", "love")
for(i in 1:length(u.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- unigrams.countForWord(word = u.test.input.words[i], u.words = u.test.words, u.counters = u.test.counter)
    test.passed <- (u.test.counter[i] == test.result)
    print(paste(test.name, "::unigrams.countForWord::", u.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}



cat("\n")
print("###############################################################")
print("#Test - Term not present - Error is thrown (default)          #")
print("###############################################################")
u.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    unigrams.countForWord(word = u.test.input.words, u.words = u.test.words, u.counters = u.test.counter)
    print(paste(test.name, "::unigrams.countForWord::", u.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::unigrams.countForWord::", u.test.input.words, "::", TRUE, sep = ""))
})
if(!is.error) stop()
print("###############################################################")

cat("\n")
print("######################################################################")
print("#Test - Term not present - Error is not thrown (errorIfTermMissing=F)#")
print("######################################################################")
test.name <- "Test Term Missing - Return 0"
t.test.input.words <- "doesNotExist"
test.result <- unigrams.countForWord(word = u.test.input.words, u.words = u.test.words, u.counters = u.test.counter,
                                     errorIfWordMissing = F)
test.passed <- (0 == test.result)
print(paste(test.name, "::bigrams.countForTerm::", t.test.input.words, "::value::",test.result, "::", test.passed , sep = ""))
if(!test.passed) stop()
print("######################################################################")

cat("\n")
print("##########################################")
rm(list = ls())
source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)
u.text.expected.prob <- c(0.1, 0.2, 0.3, 0.4)


u.test.input.words <- c("the", "time", "of", "love")
for(i in 1:length(u.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- unigrams.probabilityForWord(word = u.test.input.words[i], u.words = u.test.words, u.counters = u.test.counter)
    test.passed <- (u.text.expected.prob[i] == test.result)
    print(paste(test.name, "::unigrams.probabilityForWord::", u.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}


u.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    unigrams.probabilityForWord(word = u.test.input.words, u.words = u.test.words, u.counters = u.test.counter)
    print(paste(test.name, "::unigrams.probabilityForWord::", u.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::unigrams.probabilityForWord::", u.test.input.words, "::", TRUE, sep = ""))
})

if(!is.error) stop()

print("##########################################")
rm(list = ls())
source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)
u.text.expected.prob <- c(0.1, 0.2, 0.3, 0.4)


test.name <- "Test"
test.result <- unigrams.model(u.words = u.test.words, u.counters = u.test.counter)
check1 <- all(test.result$u.words == u.test.words)
check2 <- all(test.result$u.probability == u.text.expected.prob)
print(paste(test.name, "::unigrams.model::", (check1 & check2), sep = ""))

cat("\n\n\n")
print("##########################################")
print("################Bigrams###################")
print("##########################################")
rm(list = ls())
source("model.R")

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)

cat("\n")
print("###############################################################")
print("#Test - Term is present - Found the term with the proper count#")
print("###############################################################")

b.test.input.words <- c("the time", "time of", "of love", "love </s>")
for(i in 1:length(b.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- bigrams.countForTerm(term = b.test.input.words[i], 
                                              b.terms = b.test.words,b.counters = b.test.counter)
    
    test.passed <- (b.test.counter[i] == test.result)
    print(paste(test.name, "::bigrams.countForTerm::", b.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}
print("###############################################################")

cat("\n")
print("###############################################################")
print("#Test - Term not present - Error is thrown (default)          #")
print("###############################################################")

b.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    bigrams.countForTerm(term = b.test.input.words, 
                         b.terms = b.test.words, b.counters = b.test.counter)
    print(paste(test.name, "::bigrams.countForTerm::errorNotThrown::", b.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::bigrams.counterForTerm::errorThrown::", b.test.input.words, "::", TRUE, sep = ""))
})
print("###############################################################")

cat("\n")
print("######################################################################")
print("#Test - Term not present - Error is not thrown (errorIfTermMissing=F)#")
print("######################################################################")
test.name <- "Test Term Missing - Return 0"
t.test.input.words <- "doesNotExist"
test.result <- bigrams.countForTerm(term = b.test.input.words, 
                                     b.terms = b.test.words, b.counters = b.test.counter,
                                     errorIfTermMissing = F)
test.passed <- (0 == test.result)
print(paste(test.name, "::bigrams.countForTerm::", t.test.input.words, "::value::",test.result, "::", test.passed , sep = ""))
if(!test.passed) stop()
print("######################################################################")

cat("\n")
print("##########################################")
rm(list = ls())
source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)
b.test.expected.prob <- c(0.1, 0.2, 0.1, 0.5)

b.test.input.words <- c("the time", "time of", "of love", "love </s>")
for(i in 1:length(b.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- bigrams.probabilityForTerm(term = b.test.input.words[i], 
                                              b.terms = b.test.words,b.counters = b.test.counter,
                                              u.words = u.test.words, u.counters = u.test.counter)
    test.passed <- (b.test.expected.prob[i] == test.result)
    print(paste(test.name, "::bigrams.probabilityForTerm::", b.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}


b.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    bigrams.probabilityForTerm(term = b.test.input.words, 
                               b.terms = b.test.words, b.counters = b.test.counter,
                               u.words = u.test.words, u.counters = u.test.counter)
    print(paste(test.name, "::bigrams.probabilityForTerm::", b.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::bigrams.probabilityForTerm::", b.test.input.words, "::", TRUE, sep = ""))
})

if(!is.error) stop()

print("##########################################")
rm(list = ls())
source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)
b.test.expected.prob <- c(0.1, 0.2, 0.1, 0.5)

b.expected.word <- c("the", "time", "of", "love")
b.expected.nextWord <- c("time", "of", "love", "</s>")

test.name <- "Test"
test.result <- bigrams.model(b.terms = b.test.words, b.counters = b.test.counter,
                             u.words = u.test.words, u.counters = u.test.counter)
check1 <- !is.null(test.result$b.word) & all(test.result$b.word == b.expected.word)
check2 <- !is.null(test.result$b.nextWord) & all(test.result$b.nextWord == b.expected.nextWord)
check3 <- !is.null(test.result$b.probability) & all(test.result$b.probability == b.test.expected.prob)
test.passed <- check1 & check2 & check3
print(paste(test.name, "::bigrams.model::", (check1 & check2 & check3), sep = ""))

if(!test.passed) stop()

cat("\n\n\n")
print("##########################################")
print("###############Trigrams###################")
print("##########################################")
rm(list = ls())
source("model.R")

t.test.words <- c("the time of", "time of love", "of love </s>")
t.test.counter <- c(10, 20, 21)

cat("\n")
print("###############################################################")
print("#Test - Term is present - Found the term with the proper count#")
print("###############################################################")

t.test.input.words <- c("the time of", "time of love", "of love </s>")
for(i in 1:length(t.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- trigrams.countForTerm(term = t.test.input.words[i], 
                                        t.terms = t.test.words, t.counters = t.test.counter)
    
    test.passed <- (t.test.counter[i] == test.result)
    print(paste(test.name, "::trigrams.countForTerm::", t.test.input.words[i], "::value::",test.result,"::", test.passed , sep = ""))
    if(!test.passed) stop()
}
print("###############################################################")

cat("\n")
print("#############################################################################")
print("#Test - Term is not present & erroIfTermMissing is default - error is thrown#")
print("#############################################################################")
t.test.input.words <- "doesNotExist"
test.name <- "Test Term Missing - Throw Error"
is.error <- T
test.result = tryCatch({
    trigrams.countForTerm(term = t.test.input.words, 
                         t.terms = t.test.words, t.counters = t.test.counter)
    print(paste(test.name, "::trigrams.countForTerm::errorNotThrown::", t.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::trigrams.counterForTerm::errorThrown::", t.test.input.words, "::", TRUE, sep = ""))
})
if(!is.error) stop()
print("#############################################################################")

cat("\n")
print("################################################################")
print("#Test - Term is not present & erroIfTermMissing is F - return 0#")
print("################################################################")
test.name <- "Test Term Missing - Return 0"
t.test.input.words <- "doesNotExist"
test.result <- trigrams.countForTerm(term = t.test.input.words, 
                      t.terms = t.test.words, t.counters = t.test.counter,
                      errorIfTermMissing = F)
test.passed <- (0 == test.result)
print(paste(test.name, "::trigrams.countForTerm::", t.test.input.words, "::value::",test.result, "::", test.passed , sep = ""))
if(!test.passed) stop()
print("################################################################")


cat("\n")
print("##########################################")
rm(list = ls())
source("model.R")

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)

t.test.words <- c("the time of", "time of love", "of love </s>")
t.test.counter <- c(10, 20, 21)
t.test.expected.prob <- c(10, 5, 7)

t.test.input.words <- c("the time of", "time of love", "of love </s>")
for(i in 1:length(t.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- trigrams.probabilityForTerm(term = t.test.input.words[i],
                                              t.terms = t.test.words, t.counters = t.test.counter, 
                                              b.terms = b.test.words, b.counters = b.test.counter)
    test.passed <- (t.test.expected.prob[i] == test.result)
    print(paste(test.name, "::trigrams.probabilityForTerm::", t.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}


t.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    trigrams.probabilityForTerm(term = t.test.input.words, 
                               t.terms = t.test.words, t.counters = t.test.counter,
                               b.terms = b.test.words, b.counters = b.test.counter)
    print(paste(test.name, "::trigrams.probabilityForTerm::", t.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::trigrams.probabilityForTerm::", t.test.input.words, "::", TRUE, sep = ""))
})

if(!is.error) stop()

cat("\n")
print("#######################################################################")
print("#Test - Trigram Model Creation - Probability = c(t)/c(b)               #")
print("# t = 'w.i-2 w.i-1 w.i', b = 'w.i-2 w.i-1'                            #")
print("#######################################################################")
rm(list = ls())
source("model.R")

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)

t.test.words <- c("the time of", "time of love", "of love </s>")
t.test.counter <- c(10, 20, 21)

t.expected.bigram <- c("the time", "time of", "of love")
t.expected.nextWord <- c("of", "love", "</s>")
t.expected.count <- c(10, 20, 21)
t.expected.prob <- c(10, 5, 7)

test.name <- "Test"
test.result <- trigrams.model(t.terms = t.test.words, t.counters = t.test.counter,
                             b.terms = b.test.words, b.counters = b.test.counter)
check1 <- !is.null(test.result$t.bigram) & all(test.result$t.bigram == t.expected.bigram)
check2 <- !is.null(test.result$t.nextWord) & all(test.result$t.nextWord == t.expected.nextWord)
check3 <- !is.null(test.result$t.probability) & all(test.result$t.probability == t.expected.prob)
check4 <- !is.null(test.result$t.count) & all(test.result$t.count == t.expected.count)
test.passed <- check1 & check2 & check3 & check4
print(paste(test.name, "::trigrams.model::", test.passed, sep = ""))

if(!test.passed) stop()

print("##########################################")


cat("\n")
print("###############################################################")
print("# Test - Merging ngrams terms/counters                        #")
print("###############################################################")
rm(list = ls())
source("model.R")
d1.terms <- c("i", "am", "tired", "but", "excited", "about")
d1.counters <- c(1,2,3,4,5,6)

d2.terms <- c("about", "the", "trip", "but", "still", "excited")
d2.counters <- c(10,20,30,40,50,60)

exp.terms <- c("i", "am", "tired", "but", "excited", "about", "the", "trip", "still")
exp.counters <- c(1,2,3,44,65,16, 20, 30, 50)

test.name <- "Test - Merging ngrams terms/counters"
test.result <- collapseToOneList(d1.terms = d1.terms, d1.counters = d1.counters, 
                                 d2.terms = d2.terms, d2.counters = d2.counters)
test.passed <- (identical(exp.terms,test.result$terms) & identical(exp.counters,test.result$counters))
print(paste(test.name, "::collapseToOneList::", test.passed , sep = ""))
if(!test.passed)stop()



print("###############################################################")
