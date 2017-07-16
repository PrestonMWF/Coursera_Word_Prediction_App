# Coursera_Word_Prediction_App
This repository has the work for the Coursera Data Science Specialization Capstone Project. The project is concerned with developing an application in Shiny which predicts the next word in a sentence.

Framing the Project: Goals and Data
========================================================
Project Objectives and Familiarizing with the Data
- The project called for developing a predictive model, housed by Shiny, that would take a string of terms as input and guess the next term.
- This course started with the basics: analyzing a large corpus of text documents to discover the structure in the data and how words are put together.
- The corpus included some 37 million words from Twitter, news organizations, and blogs. Due to computational constraints, my model operates on a sample containing 20% of the original set.

Building the Predictive Model: Stupid Katz Back-Off
========================================================
- As per Wikipedia, the chosen model described: "Katz back-off is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram. It accomplishes this estimation by "backing-off" to models with smaller histories under certain conditions. By doing so, the model with the most reliable information about a given history is used to provide the better results."
- What makes it stupid? Basically, if the model does not find a trigram it "backs off" to a smaller level (here, a bigram or unique term). But, in doing so, the model has to account for changing probabilities. Here, it "discounts" the next predictions probability by a set number instead of calculating new probabilities.

Operationalizing the model: Word Prediction Shiny App
========================================================
Using the App:
- The app is designed with ease of use and speed in mind. Given Shiny's size constraint, the data tables housing the n-grams and probabilities have to be compact. As such accuracy has been sacrificed, though it still does pretty well.
- There are four simple steps: let the app load, enter a word or phrase, click predict, and observe the results.
- Additionally, the UI has been designed to be eye catching while recognizing the key drivers behind the project with logos for Coursera and Swiftkey.

The Pitch: Utility, Simplicity, and Elegance
========================================================

Use the App

The technological landscape consumers inhabit is driven by speed and convenience. The app provides a quick and easy way to speed up typing reports, sending texts, or completing one final idea. Lost in your own thoughts? Writers block hit? Simply hit the predict next word button and see what comes out! Of course, it's utility can be assessed by using it. Check out the link here and play around with the app: https://markpreston.shinyapps.io/Word_Prediction_App/
