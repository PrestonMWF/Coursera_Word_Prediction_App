library(shiny)
library(shinythemes)
library(tm)
library(stringr)

word_table<- readRDS(file= "./data/swiftkey_wordtable.RDS")
bi_table<- readRDS(file= "./data/swiftkey_bitable.RDS")
tri_table<- readRDS(file= "./data/swiftkey_tritable.RDS")

# Define UI for word prediction app
ui<- navbarPage("Coursera Data Science Capstone Project",
  theme= shinytheme("sandstone"),

tabPanel("Word Prediction App",
  sidebarLayout(
    sidebarPanel(
      h1("Getting Started with Word Prediction"),
      h4("How can I use the app?"),
      h3("Step 1: Let the app load. It should take a few seconds for everything to reach working order"),
      em("When ~[1] go~ pops up under the prediction section, the app is loaded; 
         it's the prediction for Ready to, which indicates the app is primed and ready for use!"),
      h3("Step 2: Look right and enter an individual word or phrase into the text box"),
      em("Hint: Leave out the last word you want- the app predicts it!"),
      h3("Step 3: Click the Predict Next Word button"),
      em("For added speed: type in the word or phrase of your choosing, then hit tab and enter to activate the button.
         If you're real slick, hit shift and tab to get back to the text box for more term entry."),
      h3("Step 4: Watch in amazement as the app predicts your next word"),
      em("The app predicts words using a Stupid Katz Back-Off Model, 
         which relies on trigrams, bigrams, and single terms to come up with the most suitable next word.")
    ),
    
    mainPanel(
      img(src="coursera.png", height="100", width="420"),
      img(src="swiftkey.png", height="100", width="410"),
       textInput("sentence",label = h1("Enter term or phrase here"), value = "Ready to"),
      submitButton(text = "Predict Next Word"),
      h1("Your word prediction is:"),
      h3(textOutput("predict_word")))
    )
  )
)