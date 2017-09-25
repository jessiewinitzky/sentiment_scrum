####PACKAGES REQUIRED####
library(dplyr)
library(janeaustenr)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)


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
focus_group_df <- read.table("H:/My Documents/Secondary Data Research/Inclusivity & Equity/Focus group/Audio/Focus groups transcripts ALL 20170814_1.txt",
                             header = TRUE, sep = "\t")

as.data.frame(focus_group_df)

focus_group <- focus_group_df %>%
  group_by(Comment)%>%
  ungroup()
focus_group

#####unnest tokens, create tibble######
focus_tidy <- focus_group %>%
  unnest_tokens(Word, Comment)
#Error in check_input(x) : 
#Input must be a character vector of any length or a list of character
#vectors, each of which has a length of 1.

#force Comment to be a character--$ doesn't work for factors because of its levels
focus_group$Comment <-
  as.character(focus_group[["Comment"]])

str(focus_group)

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

#########Remove stop words###########
data("stop_words")
focus_tidy <- focus_tidy %>%
  anti_join(stop_words)

#custom stop words
custom_stop_words <- bind_rows(data_frame(word = c("1", "2", "it.+s", "4", "it' s ", " it's", "i' m ", "don' t ", "don' t", "it`s","dont", 
                                                   "3", "7", "8", "9","5", "6", "20", "30", "50", "10", "15", "25", "100", "14", "24", 
                                                   "40", "104", "105","130", "17", "26", "27", "35", "65", "that's", "what's", "we're",
                                                   "you're", "i'm", "i've", "im")))
focus_tidy <- focus_tidy %>%
  anti_join(custom_stop_words)

focus_tidy[grepl("\`", focus_tidy$word), ]
focus_tidy[grepl("\'", focus_tidy$word), ]
focus_tidy[grepl("it's", focus_tidy$word), ]
focus_tidy[grepl("it.+s", focus_tidy$word), ]
focus_tidy[grepl("participant", focus_tidy$word), ]


participant_count[grepl("\'", participant_count$word), ]

##########Find most common words#########
focus_tidy_count <- focus_tidy %>%
  count(word, sort = TRUE)
# A tibble: 3,141 x 2
#word     n
#<chr> <int>
# 1      people   319
# 2           2   175
# 3        feel   168
# 4     climate   163
# 5           1   162
#6         lot   131
# 7        it’s   122
# 8        dont   112
# 9      change   109
#10 participant   108
# ... with 3,131 more rows

focus_tidy_count %>%
  filter(n>50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#00ABE1") +
  labs(x="Most common words", y="Word frequency") +
  theme_minimal() +
  coord_flip()

#create word cloud of most common words
focus_tidy_count %>%
  with(wordcloud(word,n, max.words = 100))

#########remove moderator#########
participants <- subset(focus_tidy, Speaker != "Moderator")

participants <- participants %>%
  anti_join(custom_stop_words)

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

#create bar graph of most common words per group
participant_count_group <- participants %>%
  group_by(Group) %>%
  count(word, sort = TRUE)

participant_count_group_table <- participant_count_group %>%
  top_n(15, n) %>%
  mutate(word = reorder(word, n))

#rename groups
participant_count_group_table$Group <-
  as.character(participant_count_group_table[["Group"]])

participant_count_group_table$Group <- factor(participant_count_group_table$Group, 
                                              levels = c("1", "2", "3"), 
                                              labels = c("Redwood", "South City", "Jordan"))

focus_group$Group <- factor(focus_group$Group,
                            levels = c("1", "2", "3"), 
                            labels = c("Redwood", "South City", "Jordan"))

focus_tidy$Group <- factor(focus_tidy$Group,
                           levels = c("1", "2", "3"), 
                           labels = c("Redwood", "South City", "Jordan"))

focus_tidy_count$Group <- factor(focus_tidy_count$Group,
                                 levels = c("1", "2", "3"), 
                                 labels = c("Redwood", "South City", "Jordan"))

participants <- factor(participants$Group,
                       levels = c("1", "2", "3"), 
                       labels = c("Redwood", "South City", "Jordan"))

participant_count$Group <- factor(participant_count$Group,
                                  levels = c("1", "2", "3"), 
                                  labels = c("Redwood", "South City", "Jordan"))

participant_count_group$Group <- factor(participant_count_group$Group,
                                        levels = c("1", "2", "3"), 
                                        labels = c("Redwood", "South City", "Jordan"))

########most commons words by group#########
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

participant_count_group_table %>%
  group_by(Group) %>%
  top_n(15, n) %>%
  mutate(word = reorder(word, n))%>%
  ungroup()
# A tibble: 45 x 3
#word Group     n
#<fctr> <int> <int>
#  1           1     1   107
#2        it’s     3   106
#3       don’t     3    98
#4         i’m     3    81
#5      people     1    73
#6      people     3    71
#7      that’s     3    65
#8 participant     1    55
#9        feel     3    52
#10     there’s     3    42
# ... with 35 more rows

table(focus_tidy_participant_count_group$Group)
#1    2    3 
#1396  704 1001 

focus_tidy_participant_count_group <- focus_tidy_participant_count_group %>%
  count(word, sort = TRUE)

total_words <- focus_tidy_participants %>%
  group_by(Group) %>%
  summarize(total = sum(n))

######tf-idf#########

#calculate total words in each group
group_words <- left_join(participants, participant_count_group)

group_count <- group_count %>%
  as.data.frame()

colnames(group_count) <- c("Group", "total")

group_words <- left_join(group_words, group_count)
#whew! That took forever!!!

#distribution of n/total

ggplot(group_words, aes(n/total, fill = Group)) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_manual(values=c("#00ABE1","#FFCD00", "#833921"))+
  theme_minimal()+
  facet_wrap(~Group, ncol = 3, scales = "free_y")

#Zipf's law
freq_by_rank <- group_words %>%
  group_by(Group) %>%
  mutate(`term frequency` = n/total)

#bind tf-idf
group_words <- group_words %>%
  bind_tf_idf(word, Group, n)
# A tibble: 7,525 x 9
#Group              Speaker Comment_num        word     n total           tf        idf        tf_idf
#<fctr>               <fctr>       <int>       <chr> <int> <int>        <dbl>      <dbl>         <dbl>
#  1    Redwood Female Participant 1         160      active     1  3116 3.720792e-05  1.0986123  4.087708e-05
#2    Redwood   Male Participant 2         103 comfortable     2  3116 7.441584e-05 -0.6931472 -5.158113e-05
#3    Redwood Female Participant 1         162 comfortable     2  3116 7.441584e-05 -0.6931472 -5.158113e-05
#4 South City        Participant 4          82 comfortable     2  1355 2.464572e-04 -0.6931472 -1.708311e-04
#5 South City        Participant 5         115 comfortable     2  1355 2.464572e-04 -0.6931472 -1.708311e-04
#6     Jordan            Speaker 3           5 comfortable     2  3054 3.245699e-05 -0.6931472 -2.249747e-05
#7     Jordan            Speaker 3         251 comfortable     2  3054 3.245699e-05 -0.6931472 -2.249747e-05
#8    Redwood       Female Speaker          79    peaceful     1  3116 3.720792e-05  1.0986123  4.087708e-05
#9    Redwood       Female Speaker          79        roll     4  3116 1.488317e-04 -0.2876821 -4.281620e-05
#10    Redwood   Male Participant 2         103        roll     4  3116 1.488317e-04 -0.2876821 -4.281620e-05
# ... with 7,515 more rows

group_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

group_words_unique <- group_words %>%
  group_by(Group, tf_idf) %>%
  count(word, sort = TRUE)


#visualize high tf-idf words
plot_tfidf <- group_words_unique %>%
  group_by(Group) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
plot_tfidf$nn <- NULL

#plot_tfidf <- group_words %>%
#  arrange(Group, tf_idf) %>%
#  mutate(word = factor(word, levels = rev(unique(word))))

plot_tfidf <- plot_tfidf %>%
  group_by(Group) %>%
  top_n(20, tf_idf)

plot_tfidf <- plot_tfidf %>%
  group_by(Group) %>%
  filter(row_number() == 1:20)

plot_tfidf %>%
  ggplot(aes(tf_idf)) +
  geom_histogram()

group_words %>%
  ggplot(aes(tf_idf)) +
  geom_histogram()

plot_tfidf_table <- plot_tfidf %>%
  group_by(Group) %>%
  
  plot_tfidf %>%
  group_by(Group) %>%
  ggplot(aes(word, tf_idf, fill = Group)) +
  geom_col() +
  labs(x=NULL, y="tf-idf") +
  scale_fill_manual(values=c("#00ABE1","#FFCD00", "#833921"))+
  theme_minimal()+
  coord_flip()
#plot not working

plot_tfidf %>%
  group_by(Group) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  labs(x=NULL, y="tf-idf") +
  facet_wrap(~Group, ncol = 3, scales = "free_y") +
  scale_fill_manual(values=c("#00ABE1","#FFCD00", "#833921"))+
  theme_minimal()+
  coord_flip()

#######sentiment analysis#############
#use group_words

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#afinnpos <- get_sentiments("afinn") %>%
#  filter(score > 0)

#afinnneg <- get_sentiments("afinn") %>%
#  filter(score < 0)

afinn <- get_sentiments("afinn")

group_words_sentiment <- group_words %>%
  inner_join(afinn)

#most common positive and negative words
sentiment_counts <- group_words_sentiment %>%
  count(word, score, sort = TRUE) %>%
  ungroup()

sentiment_counts_group <- group_words_sentiment %>%
  count(Group, word, score, sort = TRUE) %>%
  ungroup()

#overall sentiment plot
sentiment_counts %>%
  group_by(score) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Overall sentiment",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

#redwood sentiment
sentiment_counts_Redwood <- sentiment_counts_group %>%
  filter(Group == "Redwood")

sentiment_counts_Redwood %>%
  group_by(score) %>% 
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Sentiment at Redwood",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()
str(sentiment_counts)

#South City sentiment
sentiment_counts_South <- sentiment_counts_group %>%
  filter(Group == "South City") 

sentiment_counts_South %>%
  group_by(score) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = score)) +
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
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y") +
  labs(title = "Sentiment at Jordan",
       y = "Contribution to sentiment",
       x = NULL) +
  theme_minimal() +
  coord_flip()

#######n-grams#########
bigrams <- focus_group %>%
  unnest_tokens(bigram, Comment, token = "ngrams", n = 2)

#remove moderator
bigram_part <- subset(bigrams, Speaker != "Moderator")

bigrams_sep <- bigram_part %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filt <- bigrams_sep %>%  
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filt <- bigrams_filt %>%  
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

more_stop_words <- bind_rows(data_frame(word = c("participant", "participants","speaker", "donâ€™t")))
bigrams_filt <- bigrams_filt %>%  
  filter(!word1 %in% more_stop_words$word) %>%
  filter(!word2 %in% more_stop_words$word)

bigram_counts <- bigrams_filt %>%
  count(Group, word1, word2, sort = TRUE)

bigram_counts

bigram_unite <- bigrams_filt %>%
  unite(bigram, word1, word2, sep = " ")

bigram_unite_count <- bigram_unite %>%
  count(Group, bigram, sort = TRUE)

#tf-idf for bigrams
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
  top_n(10, tf_idf) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = Group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Group, scales = "free_y") +
  labs(title = "tf-idf of bigrams",
       y = "tf-idf",
       x = NULL) +
  theme_minimal() +
  coord_flip()

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
  scale_fill_manual(values = c("#009CDE", "#0077C8", "#003865")) +
  coord_flip()

####topic modeling####
#remove moderator
focus_group_df_part <- subset(focus_group_df, Speaker != "Moderator")

#convert to document term matrix
part_dtm <- participants %>% cast_dtm(Group, Comment_num, word)
part_dtm

part_tdm  <- participants %>% cast_tdm(Group, Speaker, Comment_num, word)
part_tdm
