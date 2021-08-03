library(tidyverse)
library(data.table)
library(glue)
library(lubridate)
library(gsubfn)
library(tidytext)
library(qdap)
library(tm)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

load_data <- function(source, topic, set){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
    )
  
  df_path <- glue('Methods/Data/
                    {source}/Raw/Aggregate/
                    {topic_short}_{set}.csv')
  
  logs_path <- glue('Methods/Data/
                    {source}/Raw/Aggregate/
                    {topic_short}_{set}_logs.csv')
  
  df <- read.csv(df_path)
  logs <- read.csv(logs_path)
  
  # if(source == "Twitter" | 
  #    (source == "Reddit" & source2 == "Submissions")){
  #   
  #     if(topic == 'Employment'){
  #       df_name <- 'emp_1.csv'
  #       logs_name <- 'emp_1_logs.csv'
  #     }
  #     
  #     else if(topic == 'Vaccination'){
  #       df_name <- 'vac_1_df.csv'
  #       logs_name <- 'vac_1_logs.csv'
  #     }
  # }
  # 
  # if(source == "Reddit" & source2 == "Subreddits"){
  #   
  #     if(topic == 'Employment'){
  #       df_name <- 'emp_4_df.csv'
  #       logs_name <- 'emp_4_logs.csv'
  #     }
  #     
  #     else if(topic == 'Vaccination'){
  #       df_name <- 'vac_4_df.csv'
  #       logs_name <- 'vac_4_logs.csv'
  #     }
  #     
  # }
  # 
  # full_path_df <- file.path(root_dir, 
  #                           data_path,
  #                           df_name)
  # full_path_logs <- file.path(root_dir,
  #                             data_path,
  #                             logs_name)
  # 
  # df <- read.csv(full_path_df, header = T)
  # logs <- read.csv(full_path_logs, header = T)
  
  return(list(df, logs))
  
}


load_all_data <- function(){
  
  
}

change_dates <- function(df, logs, source){
  
  if(source == "Reddit"){
    
    df <- df %>% 
      mutate(retrieved_on = as.POSIXct(retrieved_on,
                                       origin = '1970-01-01',
                                       tz = 'UTC'),
             date = ymd_hms(date,
                            tz = 'UTC'))
    
    logs <- logs %>% 
      mutate(mostRecent = ymd_hms(mostRecent,
                                  tz = 'UTC'),
             oldest = ymd_hms(oldest,
                              tz = 'UTC')) %>% 
      mutate(periodCovered = mostRecent - oldest)
  }
  
  if(source == "Twitter"){
    
    df <- df %>% 
      mutate(created_at = ymd_hms(created_at,
                                  tz = 'UTC'))
    
    logs <- logs %>% 
      mutate(
        weekStart = ymd(weekStart, 
                        tz = 'UTC'),
        weekEnd = ymd(weekEnd,
                      tz = 'UTC'),
        mostRecent = ymd_hms(mostRecent,
                                  tz = 'UTC'),
        oldest = ymd_hms(oldest,
                              tz = 'UTC'))
  }
  
  return(list(df, logs))
  
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
  
  # Get rid of posts that have less than n (here 1) words.
  df <- df %>% 
    group_by(id) %>% 
    filter(n()>1)
  
  return(df)
  
}

untidy_text <- function(df){
  
  df <- df %>% 
    group_by(across(c(-word))) %>% 
    summarize(text = str_c(word, collapse = " "))
  
  return(df)
}


# Load data
list[reddit_emp_df, reddit_emp_logs] <- load_data('Reddit', 'Employment')
list[twitter_emp_df, twitter_emp_logs] <- load_data('Twitter', 'Employment')

# Get dates in the right format
list[reddit_emp_df, reddit_emp_logs] <- change_dates(reddit_emp_df, reddit_emp_logs, 'Reddit')
list[twitter_emp_df, twitter_emp_logs] <- change_dates(twitter_emp_df, twitter_emp_logs, 'Twitter')

# Clean - stage1a
c1b_reddit_emp <- clean_stage1b(reddit_emp_df, 'Reddit')
c1b_twitter_emp <- clean_stage1b(twitter_emp_df, 'Twitter')

# Untidy
c1b_untidy_reddit_emp <- untidy_text(c1b_reddit_emp)
c1b_untidy_twitter_emp <- untidy_text(c1b_twitter_emp)