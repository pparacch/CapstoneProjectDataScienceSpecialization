# Objects defined in global.R are loaded into the global environment of the R session; 
# all R code in a Shiny app is run in the global environment or a child of it.

library(wordcloud)
source("./helper.R")

d.1g <- readRDS(file = "./data/model.1g.rds")
#REMOVING START/END OF SENTENCE from UNIGRAMS
d.1g <- d.1g[-which(d.1g$terms == "<s>"),]
d.1g <- d.1g[-which(d.1g$terms == "</s>"),]

d.2g <- readRDS(file = "./data/model.2g.rds")
d.3g <- readRDS(file = "./data/model.3g.rds")

