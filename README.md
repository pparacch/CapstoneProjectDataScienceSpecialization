# CapstoneProjectDataScienceSpecialization
Capstone project of the Data Science Specialization (Johns Hopkins/ Coursera)

## The Challenge

“Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain.”

When someone types “I went to the”, the application should presents three options for what the next word might be. For example, the three words might be gym, store, restaurant.

## The Data

The data to used for building the __predictive model__ is coming from the __[HC corpora](http://www.corpora.heliohost.org/
)__. The __corpora__ is a collection of 3 different __corpus__ (twitter, news and blogs) with the aim of getting a varied and comprehensive corpus of current use of the languages.

The __original corpora__, with focus only on the __english language__ (*en_US*), includes:

* 2.360.148 tweets
* 1.010.242 news
* 899.288 blogs

## The artifacts

* __[Milestone Report - RPubs](https://rpubs.com/pparacch/177065)__
* __[TextPrediction APP - shiny.app.io]( https://pparacch.shinyapps.io/TextPredictorApplication/)__

Some extra reports have been created to give some more info about the ingestion process and the memory footprint challenge for the app.

* __[Ingestion Report - RPubs](https://rpubs.com/pparacch/184463)__
* __[Ngram Reduction Report - RPubs](https://rpubs.com/pparacch/184468)__

## The Repository (github)

The data has been downloaded from the provided [link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). The data has not been stored into teh repository because of the dimension of the files.

### Folders

__Important__:

* __ingestion pipeline__: the folder contains all of the scripts and reports used to create the final artifacts for the project. The original data is sampled (randomly) in 6 sample (10% of the original data each) and processed sample by sample for the creation of the term frequency matrices. The 6 term frequency matrices have been merged together to create the matrix used for the language model. All of the code has been optimized for efficiency and performance. 

* __TextPredictorApp__: the folder contains the shiny app that has been released as one of final artifacts for the project, including the language model ('data' folder).

* __slideDeck__: the folder contains the presentation released as one of final artifacts for the project.

* __notes__: a powerpoint presentation for the project


__Historical__:

* __scripts__/ __reports__/ __models__:  this folders contains the exploratory works done during the first 6 weeks for setting up the strategy on how to build the application. Scripts run but they are not optimized for the amount of data used to build the final model. Note the 'milestone' repost (milestone\_report\_textPrediction.Rmd) is included in the 'reports' folder.


