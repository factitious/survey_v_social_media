library(tidyverse)
library(data.table)
library(glue)
library(lubridate)
library(gsubfn)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)
raw_data_path <- 'Methods/Data/Reddit/Raw/Employment/Aggregate/'


load_data <- function(source, topic){
  
  result <- list()
  
  data_path <- glue('Methods/Data/{source}/Raw/{topic}/Aggregate/')
  
  if(topic == 'Employment'){
    df_name <- 'emp_df.csv'
    logs_name <- 'emp_logs.csv'
  }
  
  else if(topic == 'Vaccination'){
    df_name <- 'vacc_df.csv'
    logs_name <- 'vacc_logs.csv'
  }
  
  full_path_df <- file.path(root_dir, 
                         data_path,
                         df_name)
  full_path_logs <- file.path(root_dir,
                              data_path,
                              logs_name)
  
  df <- read.csv(full_path_df, header = T)
  logs <- read.csv(full_path_logs, header = T)
  
  return(list(df, logs))
  
}

change_dates <- function(df, logs, source){
  
  if(source == "Reddit"){
  
    df <- df %>% 
      mutate(retrieved_on = as.POSIXct(retrieved_on,
                                       origin = '1970-01-01',
                                       tz = 'UTC'),
             date = ymd_hms(date))
    
    logs <- logs %>% 
      mutate(mostRecent = ymd_hms(mostRecent),
             oldest = ymd_hms(oldest)) %>% 
      mutate(periodCovered = mostRecent - oldest)
  }
  
  return(list(df, logs))
  
}

clean_stage1a <- function(df, source){
  
  if(source == 'Reddit'){
    
    df <- df %>% 
      select(
        -full_link,
        -retrieved_on, 
        -subreddit_id) %>%
      mutate(is_reddit_media_domain = 
               ifelse(is_reddit_media_domain == "True", 
                      T, F)) %>% 
      mutate(is_robot_indexable = 
               ifelse(is_robot_indexable == "True", 
                      T, F)) %>% 
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
    
    df$text <- removePunctuation(df$text)
    df$text <- stripWhitespace(df$text)
    
  }
  
  return(df)
  
}


# Load data
list[emp_df, emp_logs] <- load_data('Reddit', 'Employment')

# Get dates in the right format
list[emp_df, emp_logs] <- change_dates(emp_df, emp_logs, 'Reddit')

# Clean - stage1a
emp_df_1a <- clean_stage1a(emp_df, 'Reddit')
