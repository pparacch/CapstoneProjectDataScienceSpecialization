source("02_cleaningData.R")

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
test.name <- "Test - manage_apostrophe"
test.inputs <- c("''Don't Fall in Love. 'Fall' 'off' a Bridge ,It hurts less.''",
                 "MT ''It's not surprising, then, they'll get bitter, they cling to guns or religion...''''",
                 "Just got voted ''most likely to 'marry' Justin Bieber''. '''Hahahahahaha'''.")
test.expected <- c("Don't Fall in Love. Fall off a Bridge ,It hurts less.",
                   "MT It's not surprising, then, they'll get bitter, they cling to guns or religion...",
                   "Just got voted most likely to marry Justin Bieber. Hahahahahaha.")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_manage_apostrophe)


cat("\n")
test.name <- "Test - remove.multipleConsecutiveApostrophes - (at least 2) - e.g. ''spy-> spy"
test.inputs <- c("''Don't Fall in Love. Fall off a Bridge ,It hurts less.''",
                 "MT ''It's not surprising, then, they'll get bitter, they cling to guns or religion...''''",
                 "Movies friday alone ''",
                 "Just got voted ''most likely to 'marry' Justin Bieber''. '''Hahahahahaha'''.")
test.expected <- c("Don't Fall in Love. Fall off a Bridge ,It hurts less.",
                   "MT It's not surprising, then, they'll get bitter, they cling to guns or religion...",
                   "Movies friday alone ",
                   "Just got voted most likely to 'marry' Justin Bieber. Hahahahahaha.")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_remove_multipleConsecutiveApostrophes)

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
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_normalize_wordsBetweenApostrophes)

cat("\n")
test.name <- "Test - normalize.abbreviations - u.s. (usa)"
test.inputs <- c("U.S. planning a tour of national parks in the U.S. camping and volunteering...",
                 "U.S.: ROCK! ROLL & AxeOff U.S. Friggin A! www.AxeOffUSA.com",
                 "i could be wrong here, u.s., but they do trade carbon credits in the u.s.")
test.expected <- c("usa planning a tour of national parks in the usa camping and volunteering...",
                   "usa: ROCK! ROLL & AxeOff usa Friggin A! www.AxeOffUSA.com",
                   "i could be wrong here, usa, but they do trade carbon credits in the usa")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_normalize_abbreviations)


cat("\n")
test.name <- "Test - remove_contractions"
test.inputs <- c("I'm coo... Jus at work hella c'mon tired are u ever in cali",
                 "I'm coo... Jus at work hella tired r you ever in cali",
                 "would have no issues waiting from 2:00 p.m. til 2:40 a.m. when the movie would start",
                 "would have no issues waiting from 2:00 p.m til 2:40 a.m when the movie would start")
test.expected <- c("I'm coo... Jus at work hella come on tired are you ever in cali",
                   "I'm coo... Jus at work hella tired are you ever in cali",
                   "would have no issues waiting from 2:00 pm til 2:40 am when the movie would start",
                   "would have no issues waiting from 2:00 pm til 2:40 am when the movie would start")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_remove_contractions)

cat("\n")
test.name <- "Test - remove_RT_retweeted"
test.inputs <- c("I'm cool... RT : Just at work RT tired r you ever in cali", "RT I'm cool...")
test.expected <- c("I'm cool...   : Just at work   tired r you ever in cali", "  I'm cool...")
test.run(test.name = test.name, test.inputs = test.inputs, test.expected = test.expected, function.udt = i_remove_RT_retweetted)