library(tidyverse)
library(tidytext)
library(glue)
library(SnowballC)
library(gsubfn)
library(data.table)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

load_cdata <- function(source, topic, set, stage){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/Preprocessed/c{stage}')
  df_name <- glue('{topic_short}_{set}.rds')
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  df <- readRDS(df_path) %>%
    select(-any_of("X")) %>%
    suppressWarnings()
  
  return(df)
  
}

untidy_text <- function(df){
  
  df <- df %>% 
    group_by(across(c(-word))) %>% 
    summarize(text = str_c(word, collapse = " ")) %>% 
    ungroup()
  
  return(df)
}

get_sentiment_score <- function(df, lexicon, stemmed = FALSE){
  
  lex <- get_sentiments(lexicon)
  
  if(stemmed){
    lex <- lex %>% 
      mutate(word = wordStem(word))%>% 
      distinct()
  }
  if(lexicon == 'bing'){
    
    scored_df <- df %>% 
      unnest_tokens(word, text) %>% 
      inner_join(lex) %>%
      mutate(sentiment = ifelse(sentiment == 'positive', 1, -1)) %>% 
      group_by(id) %>% 
      mutate(
        bing_words = n(),
        bing_score = sum(sentiment)
      ) %>% 
      select(-sentiment) %>% 
      untidy_text() %>% 
      select(-text) %>% 
      right_join(df) %>% 
      mutate(bing_words = ifelse(is.na(bing_words), 0, bing_words),
             bing_score = ifelse(is.na(bing_score), 0, bing_score))
  }
  
  else if(lexicon == 'afinn'){
      
    
    scored_df <- df %>% 
      unnest_tokens(word, text) %>% 
      inner_join(lex) %>% 
      mutate(sentiment = value) %>% 
      select(-value) %>% 
      group_by(id) %>% 
      mutate(
        afinn_words = n(),
        afinn_score = sum(sentiment)
      ) %>% 
      select(-sentiment) %>% 
      untidy_text() %>% 
      select(-text) %>% 
      right_join(df) %>% 
      mutate(afinn_words = ifelse(is.na(afinn_words), 0, afinn_words),
             afinn_score = ifelse(is.na(afinn_score), 0, afinn_score))
  }
  
  else if(lexicon == 'nrc'){
    
    scored_df <- df %>%
      unnest_tokens(word, text) %>%
      inner_join(lex %>%
                   filter(sentiment %in% c('positive',
                                           'negative'))
                 ) %>%
      mutate(sentiment = ifelse(sentiment == 'positive', 1, -1)) %>% 
      group_by(id) %>% 
      mutate(
        nrc_words = n(),
        nrc_score = sum(sentiment)
      ) %>% 
      select(-sentiment) %>% 
      untidy_text() %>% 
      select(-text) %>% 
      right_join(df) %>% 
      mutate(nrc_words = ifelse(is.na(nrc_words), 0, nrc_words),
             nrc_score = ifelse(is.na(nrc_score), 0, nrc_score))
  }

  rm(df)
  
  return(scored_df)
}

gsc_all_lexicons <- function(df, stemmed = F){
  
  l1 <- get_sentiment_score(df, lexicon = 'bing', stemmed = stemmed)
  l2 <- get_sentiment_score(df, lexicon = 'afinn', stemmed = stemmed)
  l3 <- get_sentiment_score(df, lexicon = 'nrc', stemmed = stemmed)

  lex_df <- l1 %>%
    inner_join(l2) %>%
    inner_join(l3)
  
  # # Remove observations where all sentiments are 0. 
  # lex_df <- lex_df %>% 
  #   filter(
  #       bing_score != 0 &
  #       afinn_score != 0 &
  #       nrc_score != 0
  #     )
   
  return(lex_df)
}

save_lex <- function(df, source, topic, set, stage){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/NLP/Lexicon')
  
  df_name <- glue('{topic_short}_{set}_c{stage}.rds')
  
  df_path <- file.path(root_dir,
                       data_path,
                       df_name)
  
  saveRDS(df, df_path)
}

# start.time <- Sys.time()
# df <- load_cdata('Reddit', 'Employment', 1, '1b')
# emp_3_lex<- gsc_all_lexicons(
#   df,
#   stemmed = F
# )
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)

# bt <- t %>%
#   unnest_tokens(word, text) %>%
#   inner_join(bing) %>%
#   mutate(sentiment =
#            ifelse(sentiment == 'positive',
#                   1,
#                   -1)) %>%
#   group_by(id) %>%
#   mutate(
#     sent_words = n(),
#     sent_score = sum(sentiment)
#     ) %>%
#   select(-sentiment) %>%
#   untidy_text() %>%
#   select(-text) %>%
#   right_join(t) %>%
#   mutate(sent_words = ifelse(is.na(sent_words), 0, sent_words),
#          sent_score = ifelse(is.na(sent_score), 0, sent_score))
# # 
# at <- t %>%
#   unnest_tokens(word, text) %>%
#   inner_join(afinn) %>%
#   group_by(id) %>%
#   mutate(
#     sent_words = n(),
#     sent_score = sum(value)
#   ) %>%
#   select(-value) %>%
#   untidy_text() %>%
#   select(-text) %>%
#   right_join(t) %>%
#   mutate(sent_words = ifelse(is.na(sent_words), 0, sent_words),
#          sent_score = ifelse(is.na(sent_score), 0, sent_score))
# # 
# nt <- t %>%
#   unnest_tokens(word, text) %>%
#   inner_join(nrc %>%
#                filter(sentiment %in% c('positive',
#                                        'negative'))
#              ) %>%
#   mutate(sentiment = ifelse(sentiment == 'positive', 1, -1)) %>%
#   group_by(id) %>%
#   mutate(
#     sent_words = n(),
#     sent_score = sum(sentiment)
#   ) %>%
#   select(-sentiment) %>%
#   untidy_text() %>%
#   select(-text) %>%
#   right_join(t) %>%
#   mutate(sent_words = ifelse(is.na(sent_words), 0, sent_words),
#          sent_score = ifelse(is.na(sent_score), 0, sent_score))
