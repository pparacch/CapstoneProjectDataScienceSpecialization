source("goodTuringSmoothing.R")

# GOOD-TURING SMOOTHING UNIT TEST
#
# Implementation of a (simple) Good Turing smoothing algorithm
# based on simplification count_gt = count - 0.75



check.results <- function(input, expected, result){
    print("  > Case")
    print(paste("     Input:", input))
    print(paste("  Expected:", expected))
    print(paste("    Output:", result))
    test.outcome <- expected == result
    if(!test.outcome)stop("Test: Failed")    
}

test.run <- function(test.name, test.inputs, test.expected, fun.udt, fun.terms, fun.counts){
    print(test.name)
    result <- lapply(X = test.inputs, FUN = fun.udt, terms = fun.terms, counters = fun.counts)
    for(i in 1: length(test.inputs)){
        check.results(test.inputs[i], test.expected[i], result[i])
    }
}

cat("\n")
test.name <- "Test - Good-Turing Smoothing"
test.inputs <- c("people", "am going", "going to", "i am", "revolution")
test.inputs.terms <- c("i am", "am going", "going to")
test.inputs.counters <- c(1, 2, 10)
test.expected <- c(0.000027, 1.25, 9.25, 0.25, 0.000027)
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, 
         fun.udt = gts_getGoodTuringCount, fun.terms = test.inputs.terms, fun.counts = test.inputs.counters)