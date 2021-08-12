library(lubridate)
library(tidytext)
library(tm)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(data.table)
library(glue)
library(gsubfn)
library(stringr)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)
# source(file.path(root_dir,
#                  'Methods/Scripts/Preprocessing/Cleaning/spelling.R'))

load_data <- function(source, topic, set){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
    )
  
  data_path <- glue('Methods/Data/{source}/Raw/Aggregate')
  df_name <- glue('{topic_short}_{set}.csv')
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  df <- read.csv(df_path)
  
  return(df)
  
}

untidy_text <- function(df){
  
  df <- df %>% 
    group_by(across(c(-word))) %>% 
    summarize(text = str_c(word, collapse = " ")) %>% 
    ungroup()
  
  return(df)
}


clean_stage1b <- function(df, source){
  
  stop_words_nopunct <- stop_words %>% 
    mutate(word = removePunctuation(word)) ##%>% 
  # rbind(., c("word", "Custom"))
  
  if(source == 'Reddit'){
    
    df <- df %>% 
      select(
        -full_link,
        -retrieved_on, 
        -subreddit_id) %>%
      filter(!str_detect(subreddit, '^u_')) %>% 
      filter(selftext != '[removed]'&
               selftext != '[deleted]') %>% 
      mutate(date = ymd_hms(date,
                            tz = 'UTC')) %>% 
      mutate(posttitle = title) %>% 
      mutate(text = glue('{posttitle} {selftext}')) %>%
      mutate(text = tolower(text)) %>%
      select(
        -title,
        -posttitle,
        -selftext,
        -is_robot_indexable,
        -is_reddit_media_domain
      )
  }
  
  if(source == "Twitter"){
    
    df <- df %>% 
      select(
        -source,
        -location
      ) %>% 
      filter(
        !is.na(text) &
          text != '' &
          text != ' ' &
          !is.na(id) &
          id != '' &
          id != ' ' &
          str_detect(created_at, 'Z$')
      ) %>% 
      mutate(created_at = 
               ymd_hms(created_at,
                       tz = 'UTC')) %>% 
      mutate(
        text = tolower(text)
      )
    
  }
  
  # Remove mentions (@etc)
  df$text <- gsub("@\\w+", "", df$text)
  
  # Remove html sequences (&gt)
  df$text <- gsub("&\\w+", "", df$text)
  
  # Remove urls
  df$text <- gsub("https?://.+", "", df$text)
  
  # Remove hashtags
  df$text <- gsub("#\\w+", "", df$text)
  
  # Remove '
  df$text <- gsub("’", "", df$text)
  
  # Remove anything but standard ASCII just in case
  # Gets rid of emoticons at least.
  df$text <- gsub("[^\x01-\x7F]", "", df$text)
  
  df$text <- removePunctuation(df$text)
  df$text <- removeNumbers(df$text)
  df$text <- stripWhitespace(df$text)
  
  df <- df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words_nopunct) %>% 
    anti_join(stop_words) %>% 
    filter(!nchar(word) < 3)
  
  # Get rid of posts that have less than n (here 3) words.
  # Get data back into original format
  df <- df %>% 
    group_by(id) %>% 
    filter(n()>3) %>% 
    untidy_text()
  
  
  return(df)
  
}

clean_stage2 <- function(df, source){
  
  # N.B.: When doing lexicon-based analysis later on
  # the lexicons will need to be stemmed as well. 
  # e.g.:
  # bing_stem <- get_sentiments('bing') %>% 
  #   mutate(word = wordStem(word))%>% 
  #   distinct()
  
  df <- df %>%
    unnest_tokens(word, text) %>%
    mutate(word = wordStem(word)) %>%
    filter(!nchar(word) < 3) %>%
    group_by(id) %>%
    filter(n()>3) %>% 
    untidy_text(.)
  
  return(df)
  
}

topic_id <- function(df, source){
  
  df_dtm <- df %>% 
    unnest_tokens(word, text) %>% 
    select(word) %>% 
    group_by(word) %>% 
    summarize(count = n()) %>% 
    mutate(document = "Current") %>% 
    cast_dtm(.,
             document,
             term  = word,
             count)
  
  df_lda <- LDA(df_dtm, k = 3, control = list(seed = 13))
  
  df_topics <- tidy(df_lda, matrix = "beta")
    
  
  return(df_topics)
  
}

topic_top_terms <- function(df, source, plotit = FALSE){
  
  df_top_terms <- df %>% 
    group_by(topic) %>%
    slice_max(beta, n = 10) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  if(plotit){
    
    tt_plot <- df_top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered()
    
    print(tt_plot)
    
  }
  
  return(df_top_terms)
}

save_c <- function(df, source, topic, set, stage){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/Preprocessed/c{stage}')
  
  df_name <- glue('{topic_short}_{set}.rds')
  
  df_path <- file.path(root_dir,
                       data_path,
                       df_name)
  
  saveRDS(df, df_path)
}

  
  