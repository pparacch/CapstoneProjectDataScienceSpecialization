source("model_supportingFunctions.R")
source("model_stupidBackoff.R")

cat("\n\n\n")
print("####################################################")
print("############Stupid BackOff Implementation###########")
print("####################################################")

u.test.words <- c("the", "time", "of", "love", "</s>", "OTH")
u.test.counters <- c(10, 20, 30, 40, 890, 10)

b.test.terms <- c("the time", "time of", "of love", "love </s>")
b.test.counters <- c(100, 40, 600, 0)

t.test.terms <- c("the time of", "time of love", "of love </s>")
t.test.counters <- c(10, 20, 30)

print("###############################################################")

cat("\n")
print("#######################################################################")
print("#Test - Support Trigram Level - Probability = c(t)/c(b)               #")
print("# t = 'w.i-2 w.i-1 w.i', b = 'w.i-2 w.i-1'                            #")
print("#######################################################################")
# b.test.terms <- c("the time", "time of", "of love", "love </s>")
# b.test.counters <- c(100, 40, 600, 1)
# t.test.terms <- c("the time of", "time of love", "of love </s>")
# t.test.counters <- c(10, 20, 30)

test.input.terms <- c("the time of", "of love </s>", "the time doesNotExist")
test.expected.prob <- c(0.1, 0.05, 0)

test.name <- "Test StupidBackOff - Support Trigram Level"

for(i in 1:length(test.input.terms)){
    words <- ngramTokenize(y = test.input.terms[i], ng = 1)
    test.result <- sb_support.tri(word_i = words[3], bigramBeforeWord_1 = paste(words[1], words[2]), 
                                  t.terms = t.test.terms, t.counters = t.test.counters,
                                  b.terms = b.test.terms, b.counters = b.test.counters)
    test.passed <- (test.expected.prob[i]== test.result)
    print(paste(test.name, "::sb_support.tri::w_tri = '", test.input.terms[i],"' ::value::",test.result, "::", test.passed , sep = ""))
    if(!test.passed)stop()
} 

cat("\n")
print("######################################################################")
print("#Test - Support Bigram Level - Probability = 0.4 c(b)/c(u)           #")
print("# b = 'w.i-1 w.i', u = 'w.i-1'                                       #")
print("#####################################################################")
# u.test.words <- c("the", "time", "of", "love", "</s>")
# u.test.counters <- c(10, 20, 30, 40, 900)
# 
# b.test.terms <- c("the time", "time of", "of love", "love </s>")
# b.test.counters <- c(100, 40, 600, 1)

test.input.terms <- c("the time", "of love", "the doesNotExist")
test.expected.prob <- c(4, 8, 0)

test.name <- "Test StupidBackOff - Support Bigram Level"

for(i in 1:length(test.input.terms)){
    words <- ngramTokenize(y = test.input.terms[i], ng = 1)
    test.result <- sb_support.bi(word_i = words[2], word_i_m1 = words[1], 
                                 b.terms = b.test.terms, b.counters = b.test.counters, 
                                 u.words = u.test.words, u.counters = u.test.counters)
    test.passed <- (test.expected.prob[i]== test.result)
    print(paste(test.name, "::sb_support.bi::w_bi = '", test.input.terms[i],"' ::value::",test.result, "::", test.passed , sep = ""))
    if(!test.passed)stop()
} 

print("###############################################################")

cat("\n")
print("#####################################################################")
print("#Test - Support Unigram Level - Probability = 0.4 c(u)/N            #")
print("# u = 'w.i', N = 'sum(c(u)) for all u    '                          #")
print("#####################################################################")
# u.test.words <- c("the", "time", "of", "love", "</s>")
# u.test.counters <- c(10, 20, 30, 40, 900)

test.input.terms <- c("the", "time", "doesNotExist") #doesNotExist -> OTH is used
test.expected.prob <- c(0.004, 0.008, 0.004)

test.name <- "Test StupidBackOff - Support Unigram Level"

for(i in 1:length(test.input.terms)){
    test.result <- sb_support.uni(word_i = test.input.terms[i], u.words = u.test.words, u.counters = u.test.counters)
    test.passed <- (test.expected.prob[i] == test.result)
    print(paste(test.name, "::sb_support.uni::w_i = '", test.input.terms[i],"' ::value::",test.result, "::", test.passed , sep = ""))
    if(!test.passed)stop()
} 

print("###############################################################")

cat("\n")
print("###############################################################")
print("#Test - Trigram present - Probability calculates as c(t)/c(b) #")
print("# t = 'w.i-2 w.i-1 w.i', b = 'w.i-2 w.i-1' #")
print("###############################################################")
u.test.words <- c("the", "time", "of", "love", "</s>", "machine")
u.test.counters <- c(10, 20, 30, 40, 1800, 100)

b.test.terms <- c("the time", "time of", "of love", "love </s>", "of machine")
b.test.counters <- c(100, 40, 600, 0, 3)

t.test.terms <- c("the time of", "time of love", "of love </s>")
t.test.counters <- c(10, 20, 30)

test.expected.prob <- c(0.1, 0.5, 0.05, 0.04, 0.02)

test.name <- "Test StupidBackOff"
test.input.terms <- c("of", "the time", 
                      "love", "time of", 
                      "</s>", "of love",
                      "machine", "time of",
                      "machine", "the time")
j <- 1
for(i in c(1,3,5,7,9)){
    test.result <- stupidBackoff.trigrams(word_i = test.input.terms[i], bigramBeforeWord_i = test.input.terms[i + 1],
                                          t.terms = t.test.terms, t.counters = t.test.counters, 
                                          b.terms = b.test.terms, b.counters = b.test.counters,
                                          u.words = u.test.words, u.counters = u.test.counters)
    test.passed <- abs(test.expected.prob[j] - test.result) < 0.00000000001
    print(paste(test.name, "::stupidBackoff.trigrams::w_i = '", test.input.terms[i],"' - b_i-1 = '", test.input.terms[i + 1], "' ::value::",test.result, "::", test.passed , sep = ""))
    if(!test.passed)stop("Actual Value different from expected Value")
    j <- j + 1
} 
print("###############################################################")
