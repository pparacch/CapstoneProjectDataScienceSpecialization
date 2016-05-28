<style>

.footer {
    color: black; background: white;
    position: fixed; top: 90%;
    text-align:left; width:100%;
}

.col2 {
    columns: 2 200px;         /* number of columns and width in pixels*/
    -webkit-columns: 2 200px; /* chrome, safari */
    -moz-columns: 2 200px;    /* firefox */
}

</style>

A Text Predictor Application
========================================================
author: Pier Lorenzo Paracchini
date: 28.05.2016
autosize: true

The Challenge
========================================================

<h3>Developing a prediction model for next word</h3>

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain.

When someone, for example, types <b>"I went to the"</b> the application should <b>presents at least three options</b> for what the <b>next word might be</b> and it should be able to run as a mobile/ web app in a <b>responsive</b> way.

<div class="footer" style="font-size:40%;"> 
* The <b>challenge</b> is related to the <b>"Data Science Capstone" module</b> of the <b>Data Science Specialization</b> (Johns Hopkins University, Coursera)</div>


The Supporting Data
========================================================

The data to used for building the __predictive model__ is coming from the __[HC corpora](http://www.corpora.heliohost.org/
)__. The __corpora__ is a collection of 3 different __corpus__ (twitter, news and blogs) with the aim of getting a varied and comprehensive corpus of current use of the languages.

The __original corpora__, with focus only on the __english language__ (*en_US*), includes:

* 2.360.148 tweets
* 1.010.242 news
* 899.288 blogs

<div class="footer" style="font-size:40%;"> 
* More information about the original corpora can be found in the <a href="https://rpubs.com/pparacch/177065">milestone report</a></div>



The Ingestion Process
========================================================

![The Ingestion Process](images/ingestionProcess.png)

<div class="footer" style="font-size:40%;"> 
* Some more information about the ingestion process can be found in the <a href="http://rpubs.com/pparacch/184463">ingestion report</a></div>

The Language Model - Creation
========================================================

![The Model Creation](images/theModel_1.png)

<div class="footer" style="font-size:40%;"> 
* Some more information about the strategy around ngrams size reduction can be found in the <a href="https://rpubs.com/pparacch/184468">reduction report</a></div>

The Language Model: "Stupid" backoff
========================================================

<small>Different models have been implemented: n-grams (n = 1,2,3), linear interpolation (n-grams, n = 1,2,3) with Good Turing smoothing and "Stupid" backoff (with no discount).</small>

<small>The model evaluations has been done using the <b>perplexity</b> measurement and an <b>ad-hoc testing dataset</b> (around 40 sentences). The "Stupid" Backoff model was the one able to minimize the <b>perplexity</b> measurement.</small>

<img src="images/theModel_2.png" height="300" />


The Application - Basic Usage
========================================================

![The App](images/theApp.png)

<div class="footer" style="font-size:40%;"><a href="https://pparacch.shinyapps.io/TextPredictorApplication/">TextPredictor App</a> at <b>shiny.app.io</b>, <a href="https://github.com/pparacch/CapstoneProjectDataScienceSpecialization">Capstone Project Repository</a> at <b>github.com</b></div>


Kudos
========================================================

I would like to express my deepest appreciation to the <b>great professors</b> of <b>Johns Hopkins University</b> for making this specialization available at <b>Coursera</b>.  Special kudos to all of the participants of this Capstone project for the valuable discussions, tips and tricks made available in the forums. If you want to keep in contact please just add <a href="https://www.linkedin.com/in/pierlorenzoparacchini">my LinkedIn profile</a> to your <b>LinkedIn</b> connections.

It has been a <b>long and challenging journey</b> with ups and downs, <b>worth every single moment</b>. <b>Thank you to you all!!</b>

