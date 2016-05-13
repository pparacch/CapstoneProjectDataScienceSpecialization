# dependent on:
source("model_supportingFunctions.R")
source("goodTuringSmoothing.R")
source("model_linearInterpolation.R")


check.results <- function(input, expected, result){
    print("  > Case")
    print(paste("     Input:", input))
    print(paste("  Expected:", expected))
    print(paste("    Output:", result))
    test.outcome <- abs(expected - unlist(result)) < 0.00000000001
    if(!test.outcome)stop("Test: Failed")    
}

test.run <- function(test.name, test.inputs, test.expected, fun.udt,
                     fun.lambda_1, fun.lambda_2, fun.lambda_3,
                     fun.t.terms, fun.t.counts,
                     fun.b.terms, fun.b.counts,
                     fun.u.terms, fun.u.counts){
    print(test.name)
    result <- lapply(X = test.inputs, FUN = fun.udt, lambda_1 = fun.lambda_1, lambda_2 = fun.lambda_2, lambda_3 = fun.lambda_3,
                     t.terms = fun.t.terms, t.counters = fun.t.counts,
                     b.terms = fun.b.terms, b.counters = fun.b.counts,
                     u.terms = fun.u.terms, u.counters = fun.u.counts)
    
    for(i in 1: length(test.inputs)){
        check.results(test.inputs[i], test.expected[i], result[i])
    }
}

cat("\n")
test.name <- "Test - Linear Interpolation Model with Good-Turing Smoothing"
test.inputs <- c("<s> i am", "going to hell")

test.inputs.u.terms <- c("<s>", "i", "am", "happy", "</s>")
test.inputs.u.counters <- c(10, 5, 2, 3, 10)

test.inputs.b.terms <- c("<s> i", "i am", "am happy", "happy </s>")
test.inputs.b.counters <- c(3, 1, 1, 1)

test.inputs.t.terms <- c("<s> i am", "i am happy", "am happy </s>")
test.inputs.t.counters <- c(3, 1, 1, 1)

lambda_1 = 0.8; lambda_2 = 0.4; lambda_3 = 0.2

test.expected <- c(
    ((3 - 0.75) * lambda_1 / 6) + ((1 - 0.75) * lambda_2 / 6) + ((2 - 0.75) * lambda_3 / 30), #<s> i am
    (0.000027 * lambda_1 / 6) + (0.000027 * lambda_2 / 6) + (0.000027 * lambda_3 / 30) #going to hell
)

test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, 
         fun.udt = trigrams.gtsmoothing.probabilityForTerm, 
         fun.lambda_1 = lambda_1, fun.lambda_2 = lambda_2, fun.lambda_3 = lambda_3,
         fun.t.terms = test.inputs.t.terms, fun.t.counts = test.inputs.t.counters,
         fun.b.terms = test.inputs.b.terms, fun.b.counts = test.inputs.b.counters,
         fun.u.terms = test.inputs.u.terms, fun.u.counts = test.inputs.u.counters)


