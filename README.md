# Coursera_Word_Prediction_App
This repository has the work for the Coursera Data Science Specialization Capstone Project. The project is concerned with developing an application in Shiny which predicts the next word in a sentence.

Framing the Project: Goals and Data
========================================================
Project Objectives
- The project called for developing a predictive model, housed by Shiny, that would take a string of terms as input and guess the next term. This work falls under Natural Language Processing, a subfield of data and computer science dedicated machine reading comprehension.
- While this certainly seemed a distant goal, it provided a very concrete aim for the project.

Getting and Cleaning Data
- This course started with the basics: analyzing a large corpus of text documents to discover the structure in the data and how words are put together. Thereafter, the provided text data was cleaned and analyzed text data.
- The corpus included some 37 million words from Twitter, news organizations, and blogs. Due to computational constraints, my model operates on a sample containing 20% of the original set. However, this still includes nearly 9 million trigrams, bigrams, and individual terms so it's fairly robust.
- Initial data cleaning included constiuting these samples, filtering out special characters and profanity, and getting the data into usable data frames. For storage, I passed them into data.tables.

Proceeding with the Analysis
- I used tidy text to conduct the project to avoid using a large and computational costly document term matrix. This meant I split the corpus into three data frames: one for individual terms, one for bigrams (pairs of words), and trigrams (strings of three words).
- For reference, here is the book I relied on: http://tidytextmining.com/. It's an unreal resource so feel free to check it out!


Building the Predictive Model: Stupid Katz Back-Off
========================================================
Katz Back-Off Model: The Stupid Version
- As per Wikipedia, the chosen model described: "Katz back-off is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram. It accomplishes this estimation by "backing-off" to models with smaller histories under certain conditions. By doing so, the model with the most reliable information about a given history is used to provide the better results."
- Despite sounding overwhelming, the concept it pretty straightforward. The model sifts through the aforementioned word tables and find a match for the input terms. If a match is found in the trigram table, it returns final word with the highest likeliehood. These maximum likeliehoods are the other main feature of the data tables and they help sort to find the "best" match to return.
- What makes it stupid? Basically, if the model does not find a trigram it "backs off" to a smaller level (here, a bigram or unique term). But, in doing so, the model has to account for changing probabilities. Here, it "discounts" the next predictions probability by a set number instead of calculating new probabilities. As such, this back off method earned the colloquially dubious honour of being labelled stupid. However, as the paper describes, with a very large corpus the method is very functional and does reasonably well.
- Still confused? A link to an academic paper on the topic can be found here: http://www.aclweb.org/anthology/D07-1090.pdf

Operationalizing the model: Word Prediction Shiny App
========================================================
Using the App:
- The app is designed with ease of use and speed in mind. Given Shiny's size constraint, the data tables housing the n-grams and probabilities have to be compact. As such accuracy has been sacrificed but, though it still does pretty well.
- There are four simple steps: let the app load, enter a word or phrase, click predict, and observe the results.
- Additionally, the UI has been designed to be eye catching while recognizing the key drivers behind the project with logos for Coursera and Swiftkey.

The Pitch: Utility, Simplicity, and Elegance
========================================================
Deploying the App for Usage:

The technological landscape consumers inhabit is driven by speed and convenience. The app provides a quick and easy way to speed up typing reports, sending texts, or completing one final idea. Lost in your own thoughts? Writers block hit? Simply hit the predict next word button and see what comes out! Of course, it's utility can be assessed by using it. Check out the link here and play around with the app: https://markpreston.shinyapps.io/Word_Prediction_App/
