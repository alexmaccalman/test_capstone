JHU Data Science Specialization Capstone Project
========================================================
author: Alex MacCalman
date: December 11, 2020
autosize: true
<style>
.reveal .slides section .slideContent ul li{
    font-size: 22pt;
    color: black;
}
</style>
Capstone Project
========================================================

- The purpose of the presentation is to describe the web-based application I developed and hosted using Shiny and R Connect.
- The objective of the project was to build a language model to predict the next word given a set of words entered by the user. 
- The data used to build the model can be fouund [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
- The link to the application can be found [here](https://alexmaccalman.shinyapps.io/predict_word/).
- The link to my GitHub repository is [here](https://github.com/alexmaccalman/test_capstone).

Preparing the Data
========================================================
- The data source included over 560 MB of data from a US blog and news reports and twitter feeds.
- To prepare the data, I performed a number of cleaning steps to imporve the model performance.
<ol>
<li> token into sentences using tidytext package, strip punctuation, and cast all word to lower case.
<li>  remove all non-ASCII characters.
<li>  merge multiple spaces to a single space and remove trailing/leading spaces.
<li>  remove URLs.
<li>  eliminate numbers.
<li>  remove profanity words. 
</ol>

The Stupud Back Off Language Model
========================================================
 Becasue the data size was so large, it was computationally too expensive to build a languaige model will all the data. Therefore, we experimented with different sampling of the origianl data and evaluated performance based on how well the model predicted the next word and how fast the algorithm produced a result. 

- I used a 5-gram model that looked for the input phrase in each n-gram model from 5 to 1 and built a list of predicted words based on the stupid backoff score.

- In order to reduce the size of the language model, I filterd out all n-grams with a count of less than 5. The number of sampled lines from teh original data set was 250K. 

User Interface Web Based Appication 
========================================================
- The user interface has a text box to enter in a phrase for the algoithm to predict 5 candidate words. 
- Once the user enters in the phrase, the app automaticaaly runs the algorithm and outputs the top 5 predicted words in order based on the stupid backoff model.
