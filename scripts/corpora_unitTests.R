source("corpora.R")

check.results <- function(input, expected, result){
    print("  > Case")
    print(paste("     Input:", input))
    print(paste("  Expected:", expected))
    print(paste("    Output:", result))
    test.outcome <- expected == result
    if(!test.outcome)stop("Test: Failed")    
}

load("./../data/processed/02_s01_allCorpora_cleanedText.Rdata")
cat("\n")
print("Test - normalize.abbreviations - u.s. (usa)")
test <- c("U.S. planning a tour of national parks in the U.S. camping and volunteering...",
          "U.S.: ROCK! ROLL & AxeOff U.S. Friggin A! www.AxeOffUSA.com",
          "i could be wrong here, u.s., but they do trade carbon credits in the u.s.")
test.expected <- c("usa planning a tour of national parks in the usa camping and volunteering...",
          "usa: ROCK! ROLL & AxeOff usa Friggin A! www.AxeOffUSA.com",
          "i could be wrong here, usa, but they do trade carbon credits in the usa")

result <- lapply(X = test, FUN = normalize.abbreviations)
for(i in 1: length(test)){
    check.results(test[i], test.expected[i], result[i])
}



cat("\n")
print("Test - removePunctuations.exceptApostrophe")
test <- "I'm I'll I like %$@to*&, chew;: gum, but don't like|}{[] bubble@#^)( gum!?"
test.expected <- "I'm I'll I like    to    chew   gum  but don't like      bubble      gum  "
result <- removePunctuations.exceptApostrophe(texts = test)
print(paste(   "Input:", test))
print(paste("Expected:", test.expected))
print(paste("  Output:", result))
test.outcome <- test.expected == result
if(!test.outcome)stop("removePunctuations.exceptApostrophe - Test: Failed")
