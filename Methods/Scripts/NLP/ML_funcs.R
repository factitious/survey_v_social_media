# For testing purposes

# t <- load_data('Twitter', 'Employment', 1) %>%
#   mutate(sentiment = ifelse(runif(nrow(.)) > 0.5, 'Positive', 'Negative'))
# 
# t_sent <- t %>% 
#   select(text, sentiment) %>% 
#   slice(1:15000)

library(tidyverse)
library(tidytext)
library(lubridate)
library(glue)
library(e1071)
library(gsubfn)
library(tm)
library(RTextTools)
library(SnowballC)
library(caTools)
library(caret)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

load_nlp_data <- function(source, topic, set = 1, level = 0, stage = '1b'){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  if(level == 0){
    data_path <- glue('Methods/Data/{source}/Raw/Aggregate')  
    df_name <- glue('{topic_short}_{set}.csv')
  } else if(level == 1){
    data_path <- glue('Methods/Data/{source}/Preprocessed/c{stage}')
    df_name <- glue('{topic_short}_{set}.rds')
  } else if(level == 2){
    data_path <- glue('Methods/Data/{source}/NLP/Lexicon/c{stage}') 
    df_name <- glue('{topic_short}_{set}.rds')
  }
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  print(glue("Loading {df_path}"))
  
  if(level == 0){
    df <- read.csv(df_path)
  } else {
    df <- readRDS(df_path) %>%
      select(-any_of("X")) %>%
      suppressWarnings()
  }
  
  return(df)
  
}

val_ids <- function(df){
  # Choose a random sample of observations, at least n from each day/week.
  
  set.seed(13)
  
  pos_ids <- df %>% 
    filter(bing_score > 0) %>% 
    select(id, date) %>% 
    mutate(week = week(date)) %>% 
    group_by(week) %>% 
    sample_n(50) %>% 
    ungroup()
  
  neg_ids <- df %>% 
    filter(bing_score < 0) %>% 
    select(id, date) %>% 
    mutate(week = week(date)) %>% 
    group_by(week) %>% 
    sample_n(50) %>% 
    ungroup()
  
  ids <- rbind(pos_ids, neg_ids)
  
  # test_ids <- df %>% 
  #   select(id, date) %>% 
  #   filter(!(id %in% train_ids$id)) %>% 
  #   mutate(week = week(date)) %>% 
  #   group_by(week) %>% 
  #   sample_n(20) %>% 
  #   ungroup()
  
  # return(list(train_ids$id, test_ids$id))
  
  return(ids$id)
}

doNB <- function(df){
  
  ids <- val_ids(df)
  
  df_sent <- df %>% 
    filter(id %in% ids) %>% 
    mutate(sent = ifelse(bing_score < 0, 'negative', 'positive'))
  
  dmatrix <- create_matrix(
    df_sent$text,
    language = 'english',
    removeStopwords = F,
    removeNumbers = F,
    stemWords = F)
  
  mat <- as.matrix(dmatrix)
  
  classifier = naiveBayes(mat[1:2560,], as.factor(df_sent$sent[1:2560]))
  
  return(classifier)
  
}

nb_class <- doNB(bla_lex)



reddit_emp_4_c1b_lex <- load_nlp_data('Reddit', 'Employment', set = 4, level = 2, stage = '1b')

ids <- val_ids(reddit_emp_4_c1b_lex)

s <- reddit_emp_4_c1b_lex %>% 
  filter(id %in% ids) %>% 
  mutate(sent = ifelse(bing_score < 0, 'negative', 'positive'))

dtm <- create_matrix(
  s$text,
  language = 'english',
  removeStopwords = F,
  removeNumbers = F,
  stemWords = F)

freq <- findFreqTerms(dtm, lowfreq = 20)
length(freq)/dtm$ncol

sparse_dtm <- removeSparseTerms(dtm, 0.995)

sparse_df <- as.data.frame(as.matrix(sparse_dtm))
colnames(sparse_df) <- make.names(colnames(sparse_df))

sparse_df$sentiment <- as.factor(s$sent)

set.seed(13)

train_idx <- sample.split(sparse_df$sentiment, SplitRatio = 0.7)

train <- subset(sparse_df, train_idx == T)
test <- subset(sparse_df, train_idx == F)

classifier = naiveBayes(
  train[,!names(train) %in% 'sentiment'],
  train$sentiment)

train_pred = predict(classifier, train[,!names(train) %in% 'sentiment'])
table(train_pred, train$sentiment)

test_pred = predict(classifier, test[,!names(test) %in% 'sentiment'])
table(test_pred, test$sentiment)

train_cm <- confusionMatrix(train_pred, reference = train$sentiment)
test_cm <- confusionMatrix(test_pred, reference = test$sentiment)

set.seed(13)
o <- reddit_emp_4_c1b_lex[sample(nrow(reddit_emp_4_c1b_lex), 1000),] 

o <- o %>% 
  mutate(sent = ifelse(bing_score < 0, 'negative', 'positive'))


o_dtm <- DocumentTermMatrix(
  Corpus(VectorSource(o$text)), 
  control = list(dictionary = Terms(sparse_dtm))
)

o_df <- as.data.frame(as.matrix(o_dtm))

o_pred = predict(classifier, o_df)





 # 
# # test the validity
# predicted = predict(classifier, mat[2561:3200,])
# table(df_sent$sent[2561:3200], predicted)
# 
# table(t$bing_score[(train_size+1):s_size], predicted)
# recall_accuracy(t$bing_score[train_size+1:s_size], predicted)
# 
# 
# ids <- val_ids(bla_lex)
# 
# df_sent <- bla_lex %>% 
#   filter(id %in% ids) %>% 
#   mutate(sent = ifelse(bing_score < 0, 'negative', 'positive'))
# 
# dmatrix <- create_matrix(
#   df_sent$text,
#   language = 'english',
#   removeStopwords = F,
#   removeNumbers = F,
#   stemWords = F)
# 
# 
# dmatrix_ds <- removeSparseTerms(dmatrix, 0.99)
# mat <- as.matrix(dmatrix_ds)
# classifier = naiveBayes(mat[1:2560,], as.factor(df_sent$sent[1:2560]))
# 
# predicted = predict(classifier, mat[2561:3200,])
# table(df_sent$sent[2561:3200], predicted)
# recall_accuracy(df_sent$sent[2561:3200], predicted)
# 
# 
# log_classifier <- glm(as.factor(df_sent$sent[1:2560]) ~ mat[1:2560], family = 'binomial')
# log_predicted = predict(log_classifier, mat[2561:3200,])
# 
# 
# table(df_sent$sent)
# 
# table(df_sent$sent[1:2560])
# table(df_sent$sent[2561:3200])
# 
# 
# 
# 
# 
# reddit_emp_4_c1b <- load_nlp_data('Reddit', 'Employment', set = 4, level = 1, stage = '1b')
# 
# reddit_emp_4 <- load_nlp_data('Reddit', 'Employment', set = 4, level = 0)
# 
# pre_c1b <- clean_stage1b(reddit_emp_4, 'Reddit')
# post_c1b <- clean_stage1b(reddit_emp_4, 'Reddit')
# 
# pre_c2 <- clean_stage2(pre_c1b, 'Reddit')
# post_c2 <- clean_stage2(post_c1b, 'Reddit')
# alt_c2 <- clean_stage2(post_c1b, 'Reddit')
# 
# final_c1b <- clean_stage1b(reddit_emp_4, 'Reddit')
# final_c2 <- clean_stage2(final_c1b, 'Reddit')
# 
# 
# 
# 
# v2_reddit_emp_4_c1b <- clean_stage1b(reddit_emp_4, 'Reddit')
# 
# v2_reddit_emp_4_c2 <- clean_stage2(v2_reddit_emp_4_c1b, 'Reddit')
# 
# 
# reddit_emp_4_c1b %>% anti_join(v2_reddit_emp_4_c1b)
# 
# v2_reddit_emp_4_c1b %>% anti_join(reddit_emp_4_c1b)
# 
# t <- v2_reddit_emp_4_c1b[!(v2_reddit_emp_4_c1b$id %in% reddit_emp_4_c1b$id), ]
# 
# t_id <- 'ji2a42'
# reddit_emp_4$title[reddit_emp_4$id == t_id]
# removePunctuation(reddit_emp_4$title[reddit_emp_4$id == t_id])
# 
# reddit_emp_4$selftext[reddit_emp_4$id == t_id]
# removePunctuation(reddit_emp_4$selftext[reddit_emp_4$id == t_id], 
#                   preserve_intra_word_contractions = T,
#                   preserve_intra_word_dashes = T)
# gsub("[[:punct:]]", " ", reddit_emp_4$selftext[reddit_emp_4$id == t_id])
# 
# 
# un <- v2_reddit_emp_4_c1b %>% 
#   unnest_tokens(word, text)
# 
# 
# w_count <- reddit_emp_4_c1b %>% 
#   unnest_tokens(word, text) %>% 
#   count(word)
# 
# v2_w_count <- v2_reddit_emp_4_c1b %>% 
#   unnest_tokens(word, text) %>% 
#   count(word) %>% 
#   filter(n < 10)
# 
# v2_w_count_c2 <- v2_reddit_emp_4_c2 %>% 
#   unnest_tokens(word, text) %>% 
#   count(word)
# 
# v2_w_count_c2_f1 <- v2_w_count_c2 %>% 
#   filter(n > 10)
# 
# 
# w_count_f10 <- w_count %>% 
#   filter(n > 10)
# 
# v2_w_count_f10 <- v2_w_count %>% 
#   filter(n > 10)
# 
# bing_stem <- get_sentiments('bing') %>% 
#   mutate(word = wordStem(word)) %>%
#   distinct()
# 
# c_words <- v2_w_count_c2 %>% inner_join(bing_stem)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
