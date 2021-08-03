library(lubridate)
library(tidytext)
library(tm)

library(tidyverse)
library(data.table)
library(glue)
library(gsubfn)
library(stringr)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

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
    summarize(text = str_c(word, collapse = " "))
  
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
      mutate(created_at = 
               ymd_hms(created_at,
                       tz = 'UTC')) %>% 
      filter(
        !is.na(text) |
          text == '' |
          text == ' '
      ) %>% 
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
  
  # Get rid of posts that have less than n (here 10) words.
  df <- df %>% 
    group_by(id) %>% 
    filter(n()>10)
  
  # Get data back into original format
  df <- untidy_text(df)
  
  return(df)
  
}

save_1b <- function(df, source, topic, set){
  
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/Preprocessed')
  
  df_name <- glue('{topic_short}_{set}_c1b.rds')
  
  df_path <- file.path(root_dir,
                       data_path,
                       df_name)
  
  saveRDS(df, df_path)
}