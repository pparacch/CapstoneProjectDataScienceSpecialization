#######################################
# Random Sampling the original Corpora
#######################################

# Create 6 samples fo the Corpora, containing 10% of each corpus, using random sampling
# Save teh created samples in the provided output folder
#
#input:
#      - inputFolder where the Corpora is located , e.g. "./../data/original/final/en_US/"
#      - outputFolder where the samples are saved , e.g. "./data/01_randomSampling/"
#output:
#      - the filepath for the sample files saved 

generate_random_samples <- function(inputFolder, outputFolder){
    ori.corpora <- i_load_original_corpora(inputFolder)
    seeds <- c(19711004, 19760126, 19411016, 19430604, 19710425, 20020126)
    filenames <- i_random_sampling(corpora = ori.corpora, seeds = seeds, 
                                 noOfSamples = 6, outputFolder = outputFolder)
    
    return(filenames)
}

#
# Load the original corpora - en_US.twitter.txt, en_US.news.txt, en_US.blogs.txt
# input: folder where corpora is located (e.g. ./../roiginal/)
#
# output: list containing the 3 corpus: twitterCorpus, newsCorpus and blogsCorpus

i_load_original_corpora <- function(folder){
    print(paste("* Loading Corpora from '", folder, "'...", sep = ""))
    twitter.all <- i_load_original_corpus(paste(folder, "en_US.twitter.txt", sep = ""))
    news.all <- i_load_original_corpus(paste(folder, "en_US.news.txt", sep = ""))
    blogs.all <- i_load_original_corpus(paste(folder, "en_US.blogs.txt", sep = ""))
    
    list(twitterCorpus = twitter.all, newsCorpus = news.all, blogsCorpus = blogs.all)
}

i_load_original_corpus <- function(corpus.filepath){
    con <- file(corpus.filepath, "r") 
    data.all <- readLines(con, skipNul = T)
    close(con)
    data.all
}


i_random_sampling <- function(corpora, seeds, outputFolder, noOfSamples = 6, perc = 0.1){
    if(length(seeds) != noOfSamples){
        stop("One seed for each sample....")
    }
    print(paste("* Random Sampling Corpora - no of samples:'", noOfSamples, ", perc:'",perc, "'...", sep = ""))
    filenames <- character(noOfSamples)
    
    for(sampleNo in 1:noOfSamples){
        filePath <- paste(outputFolder, "corpora_randomSample_", sampleNo, ".rdata", sep = "")
        print(paste("* Samples '", sampleNo, ", filePath:'",filePath, "'...", sep = ""))
        sample <- i_sample_corpora(corpora = corpora, seed = seeds[sampleNo])
        save(sample, file = filePath)
        filenames[sampleNo] <- filePath
    }
    return(filenames)
}

#
# Sample the original corpora
# inputs: 
#        * corpora list(twitterCorpus, newsCorpus, blogsCorpus), each corpus being a character vector
#        * seed used for the sampling
#        * perc, the percentage of the corpus to be in the sample 
#
# output: list containing the 3 sample corpus: twitterCorpus, newsCorpus and blogsCorpus

i_sample_corpora <- function(corpora, seed, perc = 0.1){
    
    twitter.sampling <- i_biased_dice_outcome(noOfThrowings = length(corpora$twitterCorpus), 
                                            seed = seed, percentageOfSuccess = perc)
    
    news.sampling <- i_biased_dice_outcome(noOfThrowings = length(corpora$newsCorpus), 
                                         seed = seed, percentageOfSuccess = perc)
    
    blogs.sampling <- i_biased_dice_outcome(noOfThrowings = length(corpora$blogsCorpus), 
                                          seed = seed, percentageOfSuccess = perc)
    
    print(paste("**** Twitter noOfEntries:'", sum(twitter.sampling), sep = ""))
    print(paste("**** News    noOfEntries:'", sum(news.sampling), sep = ""))
    print(paste("**** Blogs   noOfEntries:'", sum(blogs.sampling), sep = ""))
    
    list(twitterCorpus = corpora$twitterCorpus[twitter.sampling == 1], 
         newsCorpus = corpora$newsCorpus[news.sampling == 1], 
         blogsCorpus = corpora$blogsCorpus[blogs.sampling == 1])
}

i_biased_dice_outcome <- function(seed, noOfThrowings, percentageOfSuccess = 0.5){
    set.seed(seed)
    rbinom(noOfThrowings, 1, percentageOfSuccess)
}
