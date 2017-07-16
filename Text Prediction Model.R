library(tidyverse)
library(tm)
library(tidytext)
library(igraph)
library(ggraph)
library(wordcloud)
library(stringr)
library(data.table)

twitter_df<- data.frame(Text=readLines(con="en_US.twitter.txt", encoding = "UTF-8"))
twitter_sample<- data.frame(sample(twitter_df$Text,nrow(twitter_df)*.2,replace = FALSE))
names(twitter_sample)[1]<- "Text"
twitter_sample<- mutate(twitter_sample,ID="NA",Text.Origin="Twitter")
twitter_sample<- twitter_sample[,c(2,1,3)]

news_df<- data.frame(Text=readLines(con="en_US.news.txt", encoding = "UTF-8"))
news_sample<- data.frame(sample(news_df$Text,nrow(news_df)*.2,replace = FALSE))
names(news_sample)[1]<- "Text"
news_sample<- mutate(news_sample, ID="NA", Text.Origin="News")
news_sample<- news_sample[,c(2,1,3)]

blog_df<- data.frame(Text=readLines(con="en_US.blogs.txt", encoding = "UTF-8"))
blog_sample<- data.frame(sample(blog_df$Text,nrow(blog_df)*.2,replace = FALSE))
names(blog_sample)[1]<- "Text"
blog_sample<- mutate(blog_sample, ID="NA", Text.Origin="Blog")
blog_sample<- blog_sample[,c(2,1,3)]

all_samples<- bind_rows(twitter_sample,blog_sample)
all_samples<- bind_rows(all_samples,news_sample)
all_samples<- mutate(all_samples, ID=1:nrow(all_samples))

all_samples$Text<- as.character(all_samples$Text)
all_samples<- mutate(all_samples,
                     Text=tolower(Text),
                     Text=removeNumbers(Text),
                     Text=removePunctuation(Text,preserve_intra_word_dashes = TRUE),
                     Text=stripWhitespace(Text))

#write.csv(all_samples,"Swiftkey_Data_Samples.csv")

#Unigram Splits

sample_words<- all_samples %>%
  unnest_tokens(word,Text)

sample_words<- subset(sample_words, sample_words$word!="")

sample_words%>%
  count(word,sort=TRUE)%>%
  ggplot(aes(n)) + geom_histogram() + 
  ggtitle("Term Frequency Distribution in Text Data")

source_words<- all_samples %>%
  unnest_tokens(word, Text) %>%
  count(Text.Origin, word, sort = TRUE) %>%
  ungroup()

total_words <- source_words %>% 
  group_by(Text.Origin) %>% 
  summarize(total = sum(n))

source_words <- left_join(source_words, total_words)

ggplot(source_words, aes(n/total, fill = Text.Origin)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~Text.Origin, scales = "free_y")+
  ggtitle("Term Frequency Distribution in Text Data by Sources")

word_count<- sample_words%>%
  count(word,sort=TRUE)

wordcloud(words = word_count$word,freq = word_count$n,
          max.words = 200,min.freq = 100,
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))

word_count%>%
  top_n(25)%>%
  ggplot(aes(n,word)) + 
  geom_bar(aes(x= reorder(word,n,sum), y=n),fill="blue",stat = "identity")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Sample Text Data- Top 25 Words",y="Frequency",x="Words")+
  coord_flip()

#Bigram Splits
sample_bigrams<- all_samples%>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

bigram_count<- sample_bigrams%>%
  count(bigram,sort=TRUE)

bigram_count%>%
  ggplot(aes(n)) + geom_histogram() + 
  ggtitle("Bigram Frequency Distribution in Text Data")

wordcloud(words = bigram_count$bigram,freq = bigram_count$n,
          max.words = 200,min.freq = 200,
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))

bigram_count%>%
  top_n(25)%>%
  ggplot(aes(n,bigram)) + 
  geom_bar(aes(x= reorder(bigram,n,sum), y=n),fill="blue",stat = "identity")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Sample Text Data- Top 25 Bigrams",y="Frequency",x="Pairs")+
  coord_flip()

bigrams_seperated <- sample_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_split_count <- bigrams_seperated %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_seperated%>%
  unite(bigram, word1, word2, sep = " ")

#Markov Chain Visualization
bigram_graph <- bigram_split_count %>%
  filter(n > 300) %>%
  graph_from_data_frame()

a <- arrow(type = "closed", length = unit(.15, "inches"))

set.seed(1018)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#Trigram Splits
sample_trigrams<- all_samples%>%
  unnest_tokens(trigram, Text, token = "ngrams", n = 3)

trigram_count<- sample_trigrams%>%
  count(trigram,sort=TRUE)

trigram_count%>%
  ggplot(aes(n)) + geom_histogram() + 
  ggtitle("Trigram Frequency Distribution in Text Data")

trigram_count%>%
  top_n(25)%>%
  ggplot(aes(n,trigram)) + 
  geom_bar(aes(x= reorder(trigram,n,sum), y=n),fill="blue",stat = "identity")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Sample Text Data- Top 25 Trigrams",y="Frequency",x="Combinations")+
  coord_flip()

trigrams_seperated <- sample_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")

trigram_split_count <- trigrams_seperated %>% 
  count(word1, word2, word3, sort = TRUE)

#Bigram Probability Table
names(word_count)[1]<- "word1"
names(word_count)[2]<- "word1.count"
names(bigram_split_count)[3]<- "bigram.count"
bigram_probabilities<- merge(word_count,bigram_split_count,by="word1")
bigram_probabilities<- bigram_probabilities%>%
  mutate(bigram.prob=bigram.count/word1.count,
         bigram=paste(bigram_probabilities$word1,
                      " ",bigram_probabilities$word2))
bigram_probabilities<- bigram_probabilities[,c(1,2,3,4,6,5)]

save(bigram_probabilities, file="bigram_probabilities.Rda")

#Trigram Probability Table
trigram_split_count<- mutate(trigram_split_count, 
                             bigram= paste(word1," ", word2))
names(trigram_split_count)[3] <- "tri.term"
names(trigram_split_count)[4] <- "tri.count"
names(bigram_count)[2]<- "bigram.count"

trigram_probabilities<- merge(bigram_count,trigram_split_count,by="bigram")
trigram_probabilities[3:4]<- NULL

trigram_probabilities<- trigram_probabilities%>%
  mutate(trigram.prob=(tri.count/bigram.count),
         trigram=paste(trigram_probabilities$bigram,trigram_probabilities$tri.term))

trigram_probabilities<- trigram_probabilities[,c(1,2,3,4,6,5)]

save(trigram_probabilities, file="trigram_probabilities.Rda")

#Final n-gram tables with maximum likeliehood estimates
tri_table<- trigram_probabilities[,c(1,3,6)]
names(tri_table)[3]<- "mle"
tri_table<- data.table(tri_table)
save(tri_table,file="swiftkey_trigrams.rda")

bi_table<- bigram_probabilities[,c(1,3,5)]
names(bi_table)[3]<- "mle"
bi_table<- data.table(bi_table)
save(bi_table,file="swiftkey_bigrams.rda")

word_table<- word_count[,c(1,3)]
names(word_table)[1:2]<- c("term","mle")
word_table<- data.table(word_table)
save(word_table,file="swiftkey_unigrams.rda")

#Basic Prediction Function with tri_table- needs at least two words
tri_predictor <- function(sentence) {
  sentence<- as.character(sentence)
  sentence<- str_trim(tolower(sentence))
  interest_words <- unlist(str_split(sentence," "))
  interest_words<- tail(interest_words,2)
  interest_words<- paste(interest_words[[1]],interest_words[[2]])
  bi_match<- subset(tri_table,tri_table$bigram==interest_words)
  bi_match<- bi_match[order(bi_match$mle, decreasing = T),]
  final_word <- sprintf("%s%s%s%.4f", "The highest probability prediction is >>> ", bi_match[[2]][1],
                    " <<< with a probability of", bi_match[[3]][1])
  return(final_word)
}

#Back Off Model with tri and bi tables- needs at least two words
backoff_predictor <- function(sentence) {
  alpha= .4 #Stupid back-off alpha amount
  
  sentence<- as.character(sentence)
  sentence<- str_trim(tolower(sentence))
  interest_words <- unlist(str_split(sentence," "))
  interest_words<- tail(interest_words,2)
  interest_words<- paste(interest_words[[1]],interest_words[[2]])
  bi_match<- subset(tri_table,tri_table$bigram==interest_words)
  if (nrow(bi_match) > 0){
  bi_match<- bi_match[order(bi_match$mle, decreasing = T),]
  final_word <- sprintf("%s%s%s%.4f", "The highest probability prediction is >>> ", 
                        bi_match[[2]][1],
                        " <<< with a probability of", bi_match[[3]][1])
  return(final_word)
  
  } else {
    interest_words2 <- unlist(str_split(sentence," "))
    interest_words2<- tail(interest_words2,1)
    interest_words2<- paste(interest_words2[[1]])
    uni_match<- subset(bi_table,bi_table$word1==interest_words2)
    uni_match<- uni_match[order(uni_match$mle, decreasing = T),]
    final_word2 <- sprintf("%s%s%s%.4f", "The highest probability prediction is >>> ", 
                           uni_match[[2]][1],
                          " <<< with a probability of", alpha * uni_match[[3]][1])
    return(final_word2)
  }
}

#Stupid Back Off Model with all three tables- handles one word +
word_predictor <- function(sentence) {
  alpha= .4 #Stupid back-off alpha amount
  
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
      tri_word <- sprintf("%s%s%s%.4f", "The highest probability prediction is >>> ", 
                          tri_match[[2]][1],
                          " <<< with a probability of ", tri_match[[3]][1])
      final_word<- return(tri_word)
      
    } else {
      interest_words2 <- unlist(str_split(sentence," "))
      interest_words2<- tail(interest_words2,1)
      interest_words2<- paste(interest_words2[[1]])
      bi_match<- subset(bi_table,bi_table$word1==interest_words2)
      if (nrow(bi_match) >= 1){
        bi_match<- bi_match[order(bi_match$mle, decreasing = T),]
        bi_word <- sprintf("%s%s%s%.4f", "The highest score following back off is >>> ", 
                               bi_match[[2]][1],
                               " <<< with a score of ", alpha * bi_match[[3]][1])
        final_word<- return(bi_word)
        
      } else {
        high_term<- word_table[order(word_table$mle, decreasing = T),]
        the <- sprintf("%s%s%s%.4f", "The highest score following back off is >>> ", 
                               high_term[[1]][1],
                               " <<< with a score of ", alpha * high_term[[2]][1])
        final_word<- return(the)
        
      }
    }
  } else {
    interest_word <- unlist(str_split(sentence," "))
    interest_word<- tail(interest_word,1)
    one_bi_match<- subset(bi_table,bi_table$word1==interest_word)
    if (nrow(one_bi_match) >= 1) {
      one_bi_match<- one_bi_match[order(one_bi_match$mle, decreasing = T),]
      one_term_bi <- sprintf("%s%s%s%.4f", "The highest probability prediction is >>> ", 
                             one_bi_match[[2]][1],
                             " <<< with a probability of ", one_bi_match[[3]][1])
      final_word<- return(one_term_bi)
      
    } else {
      high_term2<- word_table[order(word_table$mle, decreasing = T),]
      the2 <- sprintf("%s%s%s%.4f", "The highest score following back off is >>> ", 
                          high_term2[[1]][1],
                          " <<< with a score of ", alpha * high_term2[[2]][1])
      final_word<- return(the2)
    }
  }
   return(final_word)
}

dump("word_predictor", file="word_predictor.R")
#source("word_predictor.R")

#For Shiny- no probability or additional wording; model remains unchanged

shiny_word_predictor <- function(sentence) {
  alpha= .4 #Stupid back-off alpha amount
  
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
}

dump("shiny_word_predictor", file="shiny_word_predictor.R")
#source("shiny_word_predictor.R")

#Applying to data frame
test<- data.frame(Text=c("I am going","exploratory analysis","grover","my night has been","bag of potato"))
test$Prediction<- mapply(FUN = shiny_word_predictor,test$Text) #returns new column with word predictions
