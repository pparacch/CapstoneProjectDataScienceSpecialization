rm(list = ls())
source("model.R")

print("##########################################")
print("###############Unigrams###################")
print("##########################################")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)
u.text.expected.prob <- c(0.1, 0.2, 0.3, 0.4)

u.test.input.words <- c("the", "time", "of", "love")
for(i in 1:length(u.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- unigrams.countForWord(word = u.test.input.words[i], u.words = u.test.words, u.counters = u.test.counter)
    test.passed <- (u.test.counter[i] == test.result)
    print(paste(test.name, "::unigrams.countForWord::", u.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}


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

print("##########################################")

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

test.name <- "Test"
test.result <- unigrams.model(u.words = u.test.words, u.counters = u.test.counter)
check1 <- all(test.result$u.words == u.test.words)
check2 <- all(test.result$u.probability == u.text.expected.prob)
print(paste(test.name, "::unigrams.model::", (check1 & check2), sep = ""))

print("##########################################")
print("################Bigrams###################")
print("##########################################")

b.test.words <- c("the time", "time of", "of love", "love </s>")
b.test.counter <- c(1, 4, 3, 20)
b.test.expected.prob <- c(0.1, 0.2, 0.1, 0.5)


b.test.input.words <- c("the time", "time of", "of love", "love </s>")
for(i in 1:length(b.test.input.words)){
    test.name <- paste("Test", i)
    test.result <- bigrams.countForTerm(term = b.test.input.words[i], 
                                              b.terms = b.test.words,b.counters = b.test.counter)
    
    test.passed <- (b.test.counter[i] == test.result)
    print(paste(test.name, "::bigrams.countForTerm::", b.test.input.words[i], "::", test.passed , sep = ""))
    if(!test.passed) stop()
}


b.test.input.words <- "doesNotExist"
test.name <- "Test Error"
is.error <- T
test.result = tryCatch({
    bigrams.countForTerm(term = b.test.input.words, 
                         b.terms = b.test.words, b.counters = b.test.counter)
    print(paste(test.name, "::bigrams.countForTerm::", b.test.input.words, "::", FALSE, sep = ""))
    is.error <- F
}, error = function(e) {
    print(paste(test.name, "::bigrams.counterForTerm::", b.test.input.words, "::", TRUE, sep = ""))
})

print("##########################################")

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

print("##########################################")
print("###############Trigrams###################")
print("##########################################")