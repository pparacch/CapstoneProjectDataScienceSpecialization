Sys.setlocale(category = "LC_ALL",locale = "English_United States.1252")

getOption("encoding")
Sys.getlocale("LC_CTYPE")
localeToCharset()

con <- file("./../data/original/final/en_US/en_US.twitter.txt", "r") 
test.all <- readLines(con, skipNul = T)
close(con)

# iconvlist()

#Possible way to convert the char vetor to ASCII before doing any processing
test.limited <- test.all[1:1000]
test.limited.ascii <- iconv(test.limites, from = "ISO8859-1", to = "ASCII", "")

x <- data.frame(test.limited, test.limited.ascii)

test.limited <- test.all[1:1000]
test.limited.utf8 <- iconv(test.limites, from = "ISO8859-1", to = "UTF8", "")

y <- data.frame(test.limited, test.limited.ascii)

