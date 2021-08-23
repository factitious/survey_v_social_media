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

list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

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
  } else if(level == 3){
    data_path <- glue('Methods/Data/{source}/NLP/ML/Subsets')
    df_name <- glue('{topic_short}_{set}.rds')
  }
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  print(glue("Loading {df_path}"))
  
  if(level == 0){
    df <- read.csv(df_path) %>%
      select(-any_of("X")) %>%
      suppressWarnings()
  } else {
    df <- readRDS(df_path) %>%
      select(-any_of("X")) %>%
      suppressWarnings()
  }
  
  return(df)
  
} 

val_ids <- function(df, neutral = F){
  # Choose a random sample of observations, at least n from each day/week.
  
  
  sent_count <- df %>% 
    group_by(day = as.Date(date), sentiment) %>% 
    summarise(obs = n()) %>% 
    pivot_wider(.,
                names_from = sentiment,
                values_from = obs) %>% 
    mutate(s = min(min(negative, neutral, positive),5)) %>% 
    select(day, s)
  
  set.seed(13)
  
  
  pos_ids <- df %>%
    filter(sentiment == 'positive') %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    inner_join(sent_count) %>%
    sample_n(., size = s) %>%
    ungroup()


  neg_ids <- df %>%
    filter(sentiment == 'negative') %>%
    mutate(day = as.Date(date)) %>%
    group_by(day) %>%
    inner_join(sent_count) %>%
    sample_n(., size = s) %>%
    ungroup()

  if(neutral){

    neutral_ids <- df %>%
      filter(sentiment == 'neutral') %>%
      mutate(day = as.Date(date)) %>%
      group_by(day) %>%
      inner_join(sent_count) %>%
      sample_n(., size = s) %>%
      ungroup()

    ids <- rbind(pos_ids, neutral_ids, neg_ids)
  } else{
    ids <- rbind(pos_ids, neg_ids)
  }

  return(ids$id)
  # return(sent_count)
}

temp_name <- function(source, topic, set = 1){
  
  df_orig <- load_nlp_data(source, 
                           topic,
                           set)
  df_lex_c1b <- load_nlp_data(source, 
                              topic, 
                              set, 
                              level = 2, 
                              stage = '1b')
  df_lex_c2 <- load_nlp_data(source,
                             topic,
                             set,
                             level = 2,
                             stage = '2')
  
  lex_agreement  <- df_lex_c2 %>% 
    mutate(bing_score = ifelse(bing_score<0, 'negative', 
                               ifelse(bing_score == 0, 'neutral','positive')),
           afinn_score = ifelse(afinn_score<0, 'negative', 
                                ifelse(afinn_score==0,'neutral','positive')),
           nrc_score = ifelse(nrc_score<0, 'negative', 
                              ifelse(nrc_score == 0,'neutral','positive'))) %>% 
    filter(bing_score == afinn_score) %>% 
    mutate(sentiment = bing_score) %>% 
    select(id, date, sentiment)
  
  ids <- val_ids(lex_agreement, neutral = T)

  df_orig <- df_orig %>%
    filter(id %in% ids)

  df_lex_c2 <- df_lex_c2 %>%
    filter(id %in% ids)

  df_lex_c1b <- df_lex_c1b %>%
    filter(id %in% ids)

  return(df_lex_c2)
  # return(lex_agreement)
  
}

get_subset <- function(source, topic, set = 1){
  
  set.seed(123)
  
  df_orig <- load_nlp_data(source, topic, set)
  
  df_c2 <- load_nlp_data(source, topic, set = set, level = 1, stage = '2')
  
  obs_count <- df_c2 %>% 
    group_by(day = as.Date(date)) %>% 
    summarise(obs_n = n())
  
  df_c2 <- df_c2 %>% 
    group_by(day = as.Date(date)) %>% 
    inner_join(obs_count) %>% 
    sample_n(., size = min(5, obs_n)) %>% 
    select(id)
  
  df <- df_orig[df_orig$id %in% df_c2$id,]
  
  if(source == 'Reddit'){
    
    df <- df %>% 
      mutate(orig_text = glue("{title} {selftext}")) %>% 
      select(-title, -selftext)
  } else if(source == 'Twitter'){
    
    df <- df %>% 
      mutate(orig_text = text) %>% 
      select(-text)
  }
  
  df <- df %>% 
    add_column(man_sentiment = NA)
  
  return(df)
  
}

save_subset <- function(df, source, topic, set){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/NLP/ML/Subsets')
  
  df_name <- glue('{topic_short}_{set}.rds')
  
  df_path <- file.path(root_dir,
                       data_path,
                       df_name)
  
  saveRDS(df, df_path)
  
}

MLannotate <- function(df_annotated){

  completed <- is.na(df_annotated$man_sentiment)
  cat(completed)
  
  if(all(completed == T)){
    start_idx <- 1
  } else {
    start_idx <- min(which(is.na(df_annotated$man_sentiment) == T))
  }

  for(idx in start_idx:nrow(df_annotated)){
    cat("\014")
    print(idx)
    print(df_annotated$orig_text[idx])
    input <- readline(prompt = "Sentiment: ")
    input <- ifelse(input == 'p', 1,
                    ifelse(input == 'n', -1,
                           ifelse(input == 'i', -99,
                                  ifelse(input == 'break', -1000, 0))))
    if(input == -1000){
      break
    }

    df_annotated$man_sentiment[idx] <- input
  }

  return(df_annotated)
}

change_scores <- function(df){
  
  df <- df %>% 
    mutate(bing_score = bing_score/bing_words,
           afinn_score = afinn_score/afinn_words,
           nrc_score = nrc_score/nrc_words) %>% 
    mutate(bing_score = ifelse(is.na(bing_score), 0,
                               ifelse(bing_score < 0, -1, 
                                      1)),
           afinn_score = ifelse(is.na(afinn_score), 0,
                               ifelse(afinn_score < 0, -1, 
                                      1)),
           nrc_score = ifelse(is.na(nrc_score), 0,
                               ifelse(nrc_score < 0, -1, 
                                      1))) %>% 
    select(-bing_words,
           -afinn_words,
           -nrc_words)
    
  return(df)
    
}

get_sparse_df <- function(df, train_dtm = NA, useMetrics = FALSE){
  
  
  if(is.na(train_dtm)){
    
    dtm <- create_matrix(
      df$text,
      language = 'english',
      removeStopwords = F,
      removeNumbers = F,
      stemWords = F)
    
    train_dtm <- removeSparseTerms(dtm, 0.995)
    
    sparse_df <- as.data.frame(as.matrix(train_dtm)) %>% 
      add_column(man_sentiment = df$man_sentiment) %>% 
      mutate_if(is.numeric, as.factor)
    

  } else {
    
    dtm <- DocumentTermMatrix(
      Corpus(VectorSource(df$text)),
      control = list(dictionary = Terms(train_dtm) )
    )
    
    sparse_df <- as.data.frame(as.matrix(dtm))

  }
  
  
  if(useMetrics){
    
    sparse_df <- sparse_df %>% 
      add_column(text = rownames(sparse_df)) %>% 
      inner_join(df %>% 
                   mutate(text = gsub(' ', '.', text))) %>% 
      select(
             -orig_text,
             -id,
             -date,
             -text,
             -retweet_count,
             -reply_count,
             -like_count,
             -quote_count)
  }
  
  return(list(train_dtm,sparse_df))
  
}

get_split <- function(df){
  
  set.seed(13)
  
  train_idx <- sample.split(df$man_sentiment, SplitRatio = 0.8)
  
  train <- subset(df, train_idx ==T)
  val <- subset(df, train_idx == F)
  
  return(list(train, val))
  
}

train_classifier <- function(train, val){
  
  classifier = naiveBayes(
    train[,!names(train) %in% 'man_sentiment'],
    train$man_sentiment)
  
  return(classifier)
}

do_pred_val <- function(classifier, train, val){
  
  train_pred = predict(classifier, train[,!names(train) %in% 'man_sentiment'])
  val_pred = predict(classifier, val[,!names(val) %in% 'man_sentiment'])
    
  train_cm <- confusionMatrix(train_pred, reference = train$man_sentiment)
  test_cm <- confusionMatrix(val_pred, reference = val$man_sentiment)
  
  return(list(train_cm, test_cm))

}

do_pred_all <- function(classifier, df, train_dtm, useMetrics = F){
  
  df <- df[sample(nrow(df), 100),]
  
  sparse_df <- get_sparse_df(df, train_dtm, useMetrics = useMetrics)
  
  sparse_df <- df %>% 
    add_column(man_sentiment = NA)
  
  sparse_df$man_sentiment = predict(classifier, sparse_df)
  
  return(sparse_df)
  
}

# sparse_c1b <- get_sparse_df(c1b, useMetrics = T)
# sparse_c2  <- get_sparse_df(c2, useMetrics = T)
# 
# list[c1b_train_cm, c1b_test_cm] <- do_nb(sparse_c1b)
# list[c2_train_cm, c2_test_cm] <- do_nb(sparse_c2)

# o <- reddit_emp_4_c1b_lex[sample(nrow(reddit_emp_4_c1b_lex), 10000),] 
# 
# o <- o %>% 
#   mutate(sent = as.factor(ifelse(bing_score < 0, 'negative', 'positive')))
# 
# o_dtm <- DocumentTermMatrix(
#   Corpus(VectorSource(o$text)), 
#   control = list(dictionary = Terms(sparse_dtm))
# )
# 
# o_df <- as.data.frame(as.matrix(o_dtm))
# 
# o_pred = predict(classifier, o_df)
# 
# o_cm <- confusionMatrix(o_pred, reference = o$sent)




# set.seed(13)
# 
# train_idx <- sample.split(df$man_sentiment, SplitRatio = 0.8)
# 
# train <- subset(df, train_idx ==T)
# test <- subset(df, train_idx == F)
# 
# classifier = naiveBayes(
#   train[,!names(train) %in% 'man_sentiment'],
#   train$man_sentiment)
# 
# train_pred = predict(classifier, train[,!names(train) %in% 'man_sentiment'])
# test_pred = predict(classifier, test[,!names(test) %in% 'man_sentiment'])
# 
# train_cm <- confusionMatrix(train_pred, reference = train$man_sentiment)
# test_cm <- confusionMatrix(test_pred, reference = test$man_sentiment)


# colnames(sparse_df) <- make.names(colnames(sparse_df))
#
# sparse_df$sentiment <- as.factor(s$sent)




# t_emp <- get_subset('Twitter', 'Employment')
# t_emp<- MLannotate(t_emp)
# save_subset(t_emp, 'Twitter', 'Employment', set = 1)
# 

# t_vac <- MLannotate('Twitter', 'Vaccine')
# t_vac <- MLannotate(t_vac)
# save_subset(t_vac, 'Twitter', 'Vaccine', set = 1)
# 
# 
# r_emp_1 <- MLannotate('Reddit', 'Employment', set = 1)
# r_emp_2 <- MLannotate('Reddit', 'Employment', set = 2)
# r_emp_3 <- MLannotate('Reddit', 'Employment', set = 3)
# r_emp_4 <- MLannotate('Reddit', 'Employment', set = 4)
# r_vac_1 <- MLannotate('Reddit', 'Vaccine', set = 1)


####### Tests

# man_t_emp <- load_nlp_data('Twitter','Employment',level = 3,set = 1)
# man_t_vac <- load_nlp_data('Twitter','Vaccine',level = 3,set = 1)
# 
# man_r_emp_1 <- load_nlp_data('Reddit','Employment',level = 3, set = 1)
# man_r_emp_2 <- load_nlp_data('Reddit','Employment',level = 3, set = 2)
# man_r_emp_3 <- load_nlp_data('Reddit','Employment',level = 3, set = 3)
# man_r_emp_4 <- load_nlp_data('Reddit','Employment',level = 3, set = 4)
# man_r_vac_1 <- load_nlp_data('Reddit','Vaccine',level = 3, set = 1)
# 
# all(t_emp == man_t_emp, na.rm = T)
# all(t_vac == man_t_vac, na.rm = T)
# all(r_emp_1 == man_r_emp_1, na.rm = T)
# all(r_emp_2 == man_r_emp_2, na.rm = T)
# all(r_emp_3 == man_r_emp_3, na.rm = T)
# all(r_emp_4 == man_r_emp_4, na.rm = T)
# all(r_vac_1 == man_r_vac_1, na.rm = T)

# df_c2 <- load_nlp_data('Reddit', 'Vaccine', set = 1, level = 1, stage = '2')
  




# 
# df_c2 <- load_nlp_data('Twitter', 'Employment', level = 1, stage = '2')
# 
# set.seed(13)
# 
# df_c2 <- df_c2 %>% 
#   group_by(day = as.Date(date)) %>% 
#   sample_n(., size = 30) %>% 
#   select(id)
# 
# df_orig <- load_nlp_data('Twitter', 'Employment')
# 
# df <- df_orig[df_orig$id %in% df_c2$id,]
# 
# df_annotated <- df_c2 %>% 
#   add_column(sentiment = NA)
#   
# 
# 
# for(idx in 1:2){
#   print(df$text[idx])
#   print(df$id[idx])
#   
#   df_annotated$sentiment[idx] <- readline(prompt = "Sentiment: ")
#   cat("\014")
#   
# }
# 
# df_final <- df_orig %>% 
#   inner_join(df_annotated)
# 
# 
# 
# 
# 
# 
# 
# # Code dump
# 
# df_agg <- temp_name('Twitter', 'Employment')
# 
# df_agg <- df_agg %>% 
#   mutate(bing_score = ifelse(bing_score<0, 'negative', 
#                              ifelse(bing_score == 0, 'neutral','positive')),
#          afinn_score = ifelse(afinn_score<0, 'negative', 
#                               ifelse(afinn_score==0,'neutral','positive')),
#          nrc_score = ifelse(nrc_score<0, 'negative', 
#                             ifelse(nrc_score == 0,'neutral','positive')))
# 
# df_agg %>% 
#   filter(bing_score == afinn_score & afinn_score == nrc_score)
# 
# 
# 
# 
# 
# count(n_pos = n(bing_score == 'positive'))
# 
# table(df_agg$bing_score)
# table(df_agg$afinn_score)
# table(df_agg$nrc_score)
#   
# t_ids <- temp_name('Twitter', 'Employment')
# 
# 
# 
# 
# # Load lex_c2 data
# df <- load_nlp_data('Twitter', 'Employment', level = 2, stage = '2')
# 
# df_dis <- df %>% 
#   mutate(bing_score = ifelse(bing_score<0, 'negative', 
#                              ifelse(bing_score == 0, 'neutral','positive')),
#          afinn_score = ifelse(afinn_score<0, 'negative', 
#                               ifelse(afinn_score==0,'neutral','positive')),
#          nrc_score = ifelse(nrc_score<0, 'negative', 
#                                ifelse(nrc_score == 0,'neutral','positive'))) %>% 
#   filter(bing_score == afinn_score & afinn_score == nrc_score)
# 
# 
# 
# table(df_dis2$bing_score)
# table(df_dis2$afinn_score)
# table(df_dis2$nrc_score)
# 
# prop.table(table(Bing = df_dis2$bing_score, 
#                  Afinn = df_dis2$afinn_score),2)*100
# 
# 
# 
# nrow(df_dis)/nrow(df)
# 
# 
# 
# check_lex_disagreement <- function(df){
#   
#   
#   
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# # This below needs to be worked into functions.
# # Plus testing + other classifiers + a way to integrate across the different datasets and analysis decisions.
# 
# reddit_emp_4_c1b_lex <- load_nlp_data('Reddit', 'Employment', set = 4, level = 2, stage = '1b')
# 
# ids <- val_ids(reddit_emp_4_c1b_lex)
# 
# 
# create_dtm <- function(df, ids, score){
#   
#   # Score 
#   
# 
#   df <- df %>% 
#     filter(id %in% ids) %>% 
#     mutate(sentiment = ifelse(score < 0, 'negative', 'positive'))
#   
#   
#   dtm <- create_matrix(
#       df$text,
#       language = 'english',
#       removeStopwords = F,
#       removeNumbers = F,
#       stemWords = F
#     ) %>% 
#     removeSparseTerms(., 0.995)
#   
#   sparse_df <- as.data.frame(as.matrix(dtm))
#   colnames(sparse_df) <- make.names(colnames(sparse_df))
#   sparse_df$sentiment <- as.factor(df$sentiment)
#   
#   
#   
#   
# }
# 
# 
# do_nb <- function(df){
#   
#   set.seed(13)
#   
#   train_idx <- sample.split(df$sentiment, SplitRatio = 0.8)
#   
#   train <- subset(df, train_idx ==T)
#   test <- subset(df, train_idx == F)
#   
#   classifier = naiveBayes(
#     train[,!names(train) %in% 'sentiment'],
#     train$sentiment)
#   
#   train_pred = predict(classifier, train[,!names(train) %in% 'sentiment'])
#   test_pred = predict(classifier, test[,!names(test) %in% 'sentiment'])
#   
#   train_cm <- confusionMatrix(train_pred, reference = train$sentiment)
#   test_cm <- confusionMatrix(test_pred, reference = test$sentiment)
#   
#   
#   return(c(train_pred, test_pred))
# }
# 
# 
# get_cm <- function(train_pred, test_pred){
#   
#   
#   
# }
# 
# 
# 
# 
# s <- reddit_emp_4_c1b_lex %>%
#   filter(id %in% ids) %>%
#   mutate(sent = ifelse(bing_score < 0, 'negative', 'positive'))
#
# dtm <- create_matrix(
#   s$text,
#   language = 'english',
#   removeStopwords = F,
#   removeNumbers = F,
#   stemWords = F)
#
# freq <- findFreqTerms(dtm, lowfreq = 20)
# length(freq)/dtm$ncol
#
# sparse_dtm <- removeSparseTerms(dtm, 0.995)
#
# sparse_df <- as.data.frame(as.matrix(sparse_dtm))
# colnames(sparse_df) <- make.names(colnames(sparse_df))
#
# sparse_df$sentiment <- as.factor(s$sent)
# 
# 
# 
# set.seed(13)
# 
# train_idx <- sample.split(sparse_df$sentiment, SplitRatio = 0.7)
# 
# train <- subset(sparse_df, train_idx == T)
# test <- subset(sparse_df, train_idx == F)
# 
# classifier = naiveBayes(
#   train[,!names(train) %in% 'sentiment'],
#   train$sentiment)
# 
# train_pred = predict(classifier, train[,!names(train) %in% 'sentiment'])
# table(train_pred, train$sentiment)
# 
# test_pred = predict(classifier, test[,!names(test) %in% 'sentiment'])
# table(test_pred, test$sentiment)
# 
# train_cm <- confusionMatrix(train_pred, reference = train$sentiment)
# test_cm <- confusionMatrix(test_pred, reference = test$sentiment)
# 
# set.seed(13)
# o <- reddit_emp_4_c1b_lex[sample(nrow(reddit_emp_4_c1b_lex), 10000),] 
# 
# o <- o %>% 
#   mutate(sent = as.factor(ifelse(bing_score < 0, 'negative', 'positive')))
# 
# o_dtm <- DocumentTermMatrix(
#   Corpus(VectorSource(o$text)), 
#   control = list(dictionary = Terms(sparse_dtm))
# )
# 
# o_df <- as.data.frame(as.matrix(o_dtm))
# 
# o_pred = predict(classifier, o_df)
# 
# o_cm <- confusionMatrix(o_pred, reference = o$sent)
# 
