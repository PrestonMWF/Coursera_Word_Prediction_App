library(shiny)
library(shinythemes)
library(tm)
library(stringr)

word_table<- readRDS(file= "./data/swiftkey_wordtable.RDS")
bi_table<- readRDS(file= "./data/swiftkey_bitable.RDS")
tri_table<- readRDS(file= "./data/swiftkey_tritable.RDS")

server<- (function(input, output) {
  
  word_prediction<- reactive({
    alpha= .4 #Stupid back-off alpha amount
    sentence<- input$sentence
    sentence<- as.character(sentence)
    sentence<- tolower(sentence)
    sentence<- removePunctuation(sentence)
    interest_words <- unlist(str_split(sentence," "))
    character_split<- sapply(gregexpr("[A-z]\\W+", interest_words), length)
    if (length(character_split) > 1) {
      interest_words <- unlist(str_split(sentence," "))
      interest_words<- tail(interest_words,2)
      interest_words<- paste(interest_words[[1]],interest_words[[2]])
      tri_match<- subset(tri_table,tri_table$bigram==interest_words)
      if (nrow(tri_match) >= 1){
        tri_match<- subset(tri_table,tri_table$bigram==interest_words)
        tri_match<- tri_match[order(tri_match$mle, decreasing = T),]
        tri_word <- tri_match[[2]][1]
        final_word<- return(tri_word)
        
      } else {
        interest_words2 <- unlist(str_split(sentence," "))
        interest_words2<- tail(interest_words2,1)
        interest_words2<- paste(interest_words2[[1]])
        bi_match<- subset(bi_table,bi_table$word1==interest_words2)
        if (nrow(bi_match) >= 1){
          bi_match<- bi_match[order(bi_match$mle, decreasing = T),]
          bi_word <- bi_match[[2]][1]
          
          final_word<- return(bi_word)
          
        } else {
          high_term<- word_table[order(word_table$mle, decreasing = T),]
          the <- high_term[[1]][1]
          final_word<- return(the)
          
        }
      }
    } else {
      interest_word <- unlist(str_split(sentence," "))
      interest_word<- tail(interest_word,1)
      one_bi_match<- subset(bi_table,bi_table$word1==interest_word)
      if (nrow(one_bi_match) >= 1) {
        one_bi_match<- one_bi_match[order(one_bi_match$mle, decreasing = T),]
        one_term_bi <- one_bi_match[[2]][1]
        final_word<- return(one_term_bi)
        
      } else {
        high_term2<- word_table[order(word_table$mle, decreasing = T),]
        the2 <- high_term2[[1]][1]
        final_word<- return(the2)
      }
    }
    return(final_word)
  })
    
  output$predict_word <- renderPrint(word_prediction())
})
