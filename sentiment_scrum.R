####PACKAGES REQUIRED####
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(ggplot2)
library(igraph)
library(ggraph)

####EMILY DICKINSON####
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- data_frame(line = 1:4, text = text)

text_df

####convert to one token per row####
text_df <- text_df %>%
  unnest_tokens(word, text)

text_df

#word = name of new column
#text = input column
#line number word came from is retained
#punctuation, capitalization stripped


#######FOCUS GROUP######

####import dataset####
focus_group_df <- read.table("C:/Users/jwinitzk/Desktop/Focus groups transcripts ALL 20170814_1.txt",
                             header = TRUE, sep = "\t")

focus_group <- focus_group_df %>%
  group_by(Comment)%>%
  ungroup()
focus_group

str(focus_group)

#force Comment to be a character--$ doesn't work for factors because of its levels
focus_group$Comment <-
  as.character(focus_group[["Comment"]])

str(focus_group)

#rename groups
focus_group$Group <- factor(focus_group$Group,
                            levels = c("1", "2", "3"), 
                            labels = c("Redwood", "South City", "Jordan"))

#unnest tokens, create tibble
focus_tidy <- focus_group %>%
  unnest_tokens(word, Comment)
focus_tidy

# A tibble: 49,164 x 4
#Group   Speaker Comment_num       Word
#<int>    <fctr>       <int>      <chr>
# 1     1 Moderator           1       then
# 2     1 Moderator           1     campus
# 3     1 Moderator           1    climate
# 4     1 Moderator           1         is
# 5     1 Moderator           1          a
# 6     1 Moderator           1    measure
# 7     1 Moderator           1         we
# 8     1 Moderator           1        all
# 9     1 Moderator           1 appreciate
#10     1 Moderator           1        the
# ... with 49,154 more rows

#########remove moderator & stop words#########
participants <- subset(focus_tidy, Speaker != "Moderator")

data("stop_words")
participants <- participants %>%
  anti_join(stop_words)

#custom stop words
custom_stop_words <- bind_rows(data_frame(word = c("1", "2", "it.+s", "4", "it' s ", " it's", "i' m ", "don' t ", "don' t", "it`s","dont", 
                                                   "3", "7", "8", "9","5", "6", "20", "30", "50", "10", "15", "25", "100", "14", "24", 
                                                   "40", "104", "105","130", "17", "26", "27", "35", "65", "that's", "what's", "we're",
                                                   "you're", "i'm", "i've", "im")))
participants <- participants %>%
  anti_join(custom_stop_words)

#####create bar graph of most common words####
participant_count <- participants %>%
  count(word, sort = TRUE)

participant_count %>%
  filter(n>30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#00ABE1") +
  labs(title = "Most common words used", x="Word", y="Frequency") +
  theme_minimal() +
  coord_flip()

participant_count %>%
  with(wordcloud(word,n, max.words = 100))

########most commons words by group#########
participant_count_group <- participants %>%
  group_by(Group) %>%
  count(word, sort = TRUE)

participant_count_group_table <- participant_count_group %>%
  top_n(15, n) %>%
  mutate(word = reorder(word, n))

participant_count_group_table %>%
  group_by(Group) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = Group)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Most common words used per group", x = "Word", y = "Frequency") +
  facet_wrap(~Group, ncol = 3, scale = "free_y") +
  scale_fill_manual(values=c("#00ABE1","#FFCD00", "#833921"))+
  theme_minimal()+
  coord_flip()

####tf-idf####
#tf-idf stands for term frequency-inverse document frequency. It's a measure of how important
#a given word is to a document in a collection. Just because a words appears frequently
#does not mean it's important. Ex: the, and, to. TF counts up all the times the word is
#used in a document; IDF is (essentially) the log of the number of documents/number of
#documents containing the term.

#For our purposes, a document is, say, Redwood; the collection is all 3 campuses
participants_tf_idf <- participants %>%
  count(Group, word) %>%
  bind_tf_idf(word, Group, n) %>%
  arrange(desc(tf_idf))

participants_tf_idf

participants_tf_idf %>%
  group_by(Group) %>%
  top_n(7, tf_idf) %>%
  ungroup %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, ncol = 2, scales = "free_y") +
  labs(title = "Weighted relative frequency of words",
       y = "tf-idf",
       x = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("#00a8e1", "#ffcd00", "#833821")) +
  coord_flip()

#######SENTIMENT ANALYSIS#############

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#afinnpos <- get_sentiments("afinn") %>%
#  filter(score > 0)

#afinnneg <- get_sentiments("afinn") %>%
#  filter(score < 0)

afinn <- get_sentiments("afinn")

participants_sentiment <- participants %>%
  inner_join(afinn)

####most common positive and negative words####
sentiment_counts <- participants_sentiment %>%
  count(word, score, sort = TRUE) %>%
  ungroup()

sentiment_counts_group <- participants_sentiment %>%
  count(Group, word, score, sort = TRUE) %>%
  ungroup()

####overall sentiment plot####
sentiment_counts %>%
  group_by(score) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Overall sentiment",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

####sentiment by group####
#redwood sentiment
sentiment_counts_Redwood <- sentiment_counts_group %>%
  filter(Group == "Redwood")

sentiment_counts_Redwood %>%
  group_by(score) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Sentiment at Redwood",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

#South City sentiment
sentiment_counts_South <- sentiment_counts_group %>%
  filter(Group == "South City") 

sentiment_counts_South %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Sentiment at South City",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

#Jordan sentiment
sentiment_counts_Jordan <- sentiment_counts_group %>%
  filter(Group == "Jordan") 

sentiment_counts_Jordan %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Sentiment at Jordan",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

#######N-GRAMS#########
bigrams <- focus_group %>%
  unnest_tokens(bigram, Comment, token = "ngrams", n = 2)

####cleanup####
#remove moderator#
bigram_part <- subset(bigrams, Speaker != "Moderator")

#separate words into two columns
bigrams_sep <- bigram_part %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#remove stop words
bigrams_filt <- bigrams_sep %>%  
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#remove custom stop words
bigrams_filt <- bigrams_filt %>%  
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#even more custom stop words
more_stop_words <- bind_rows(data_frame(word = c("participant", "participants","speaker", "donÃ¢â?¬â?¢t")))
bigrams_filt <- bigrams_filt %>%  
  filter(!word1 %in% more_stop_words$word) %>%
  filter(!word2 %in% more_stop_words$word)

#count of bigrams
bigram_counts <- bigrams_filt %>%
  count(Group, word1, word2, sort = TRUE)

#re-merge bigrams into single column
bigram_unite <- bigrams_filt %>%
  unite(bigram, word1, word2, sep = " ")

#count bigrams
bigram_unite_count <- bigram_unite %>%
  count(Group, bigram, sort = TRUE)

####tf-idf for bigrams####
bigram_tf_idf <- bigram_unite %>%
  count(Group, bigram) %>%
  bind_tf_idf(bigram, Group, n) %>%
  arrange(desc(tf_idf))

#manually remove bigrams with apostrophes
bigram_tf_idf_noap <- bigram_tf_idf[-c(4), ]
bigram_tf_idf_noap <- bigram_tf_idf_noap[-c(54), ]

#plot
bigram_tf_idf_noap %>%
  group_by(Group) %>%
  top_n(7, tf_idf) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, ncol = 2, scales = "free_y") +
  labs(title = "Weighted relative frequency of bigrams",
       y = "tf-idf",
       x = NULL) +
  theme_minimal() +
  scale_fill_manual(values = c("#00a8e1", "#ffcd00", "#833821")) +
  coord_flip()

#######visualizing network of bigrams#############
bigram_counts_nogroup <- bigrams_filt %>%
  count(word1, word2, sort = TRUE)

bigram_graph_2 <- bigram_counts_nogroup %>%
  filter(n>2) %>%
  graph_from_data_frame()

set.seed(2017)
ggraph(bigram_graph_2, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
