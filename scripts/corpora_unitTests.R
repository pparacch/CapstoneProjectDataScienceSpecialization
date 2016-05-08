source("corpora.R")
# load("./../data/processed/02_s01_allCorpora_cleanedText.Rdata")

check.results <- function(input, expected, result){
    print("  > Case")
    print(paste("     Input:", input))
    print(paste("  Expected:", expected))
    print(paste("    Output:", result))
    test.outcome <- expected == result
    if(!test.outcome)stop("Test: Failed")    
}

test.run <- function(test.name, test.inputs, test.expected, function.udt){
    print(test.name)
    result <- lapply(X = test.inputs, FUN = function.udt)
    for(i in 1: length(test.inputs)){
        check.results(test.inputs[i], test.expected[i], result[i])
    }
}

cat("\n")
test.name <- "Test - normalize.wordsBetweenApostrophes - 'spy'-> spy"
test.inputs <- c("that's why they're now 'servers'.",
                 "'me' too!! btw did you change your 'name' here?",
                 "Ha! At least 'Jared' or 'Jarred' Weaver isn't 'trending'",
                 "I'm not a 'Football' expert, but this game seems bloody exciting suddenly? #Superbowl\"")
test.expected <- c("that's why they're now servers.",
                   "me too!! btw did you change your name here?",
                   "Ha! At least Jared or Jarred Weaver isn't trending",
                   "I'm not a Football expert, but this game seems bloody exciting suddenly? #Superbowl\"")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = normalize.wordsBetweenApostrophes)

cat("\n")
test.name <- "Test - normalize.abbreviations - u.s. (usa)"
test.inputs <- c("U.S. planning a tour of national parks in the U.S. camping and volunteering...",
                 "U.S.: ROCK! ROLL & AxeOff U.S. Friggin A! www.AxeOffUSA.com",
                 "i could be wrong here, u.s., but they do trade carbon credits in the u.s.")
test.expected <- c("usa planning a tour of national parks in the usa camping and volunteering...",
                   "usa: ROCK! ROLL & AxeOff usa Friggin A! www.AxeOffUSA.com",
                   "i could be wrong here, usa, but they do trade carbon credits in the usa")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = normalize.abbreviations)


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
