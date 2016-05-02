source("model.R")

u.test.words <- c("the", "time", "of", "love")
u.test.counter <- c(10, 20, 30, 40)
u.text.expected.prob <- c(0.1, 0.2, 0.3, 0.4)

print("##########################################")

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
