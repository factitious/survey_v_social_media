library(tidyverse)
library(lubridate)
library(glue)
library(gsubfn)
library(caTools)
library(caret)
library(data.table)
library(formattable)

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
oj_data_path <- 'Methods/Data/Objective'
ai_data_path <- 'Methods/Data/Surveys/Axios-Ipsos/Aggregate'
hps_data_path <- 'Methods/Data/Surveys/HPS/Aggregate'
setwd(root_dir)

################################################
###Objective measures

# S&P data from https://www.nasdaq.com/market-activity/index/spx/historical.
# Downloaded at:
sp500 <- read.csv(
  file.path(
    root_dir,
    oj_data_path,
    'sp500.csv'
  )
) %>% 
  select(-Volume) %>% 
  mutate(Date = mdy(Date),
  ) %>% 
  filter(Date > ymd('2020-10-26'))

# UoM Index of Consumer Sentiment from http://www.sca.isr.umich.edu/tables.html
# Downloaded at:
ics <- read.csv('http://www.sca.isr.umich.edu/files/tbcics.csv',
                skip = 3, 
                header = TRUE) %>% 
  Filter(function(x)!all(is.na(x)), .) %>% 
  mutate(Date = glue('{X} {DATE.OF.SURVEY}'),
         Index = `INDEX.OF.CONSUMER.SENTIMENT`) %>% 
  select(Date, Index) %>% 
  slice(2:14) %>% 
  mutate(Date = ym(Date))


# CDC Vaccination statistics 
# (https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc)
cdc_vac <- read.csv(
  file.path(
    root_dir,
    oj_data_path,
    'COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv'
  )
) %>% 
  select(
    Date,
    Distributed,
    Administered,
    Dist_Per_100K,
    Admin_Per_100K,
    ) %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(
    Date < ymd('2021-05-24')
    ) %>% 
  group_by(Date) %>% 
  summarize(
    uptake = sum(Administered)/sum(Distributed)
  )
  


################################################
### Load survey data

ai <- list()
hps <- list()

ai$emp <- readRDS(
  file.path(
    root_dir, 
    ai_data_path,
    'emp.rds'
    )
)
  
ai$vac <- readRDS(
  file.path(
    root_dir, 
    ai_data_path,
    'vac.rds'
  )
)

hps$emp <- readRDS(
  file.path(
    root_dir, 
    hps_data_path,
    'emp.rds'
  )
)

hps$vac_s1 <- readRDS(
  file.path(
    root_dir, 
    hps_data_path,
    'vac_s1.rds'
  )
)

hps$vac_s2 <- readRDS(
  file.path(
    root_dir, 
    hps_data_path,
    'vac_s2.rds'
  )
)

################################################
### Load social media data.

load_nlp_data <- function(
  source, 
  topic, 
  set = 1, 
  level = 0, 
  stage = '1b'){
  
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

change_scores <- function(df, source){
  
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
  
  if(source == "Reddit"){
  
    df <- df %>% 
      mutate(
          nc_bing = num_comments * bing_score,
          nc_afinn = num_comments * afinn_score,
          nc_nrc = num_comments * nrc_score,
          sc_bing = score * bing_score,
          sc_afinn = score * afinn_score,
          sc_nrc = score * nrc_score,
          nc_sc_bing = num_comments * score * bing_score,
          nc_sc_afinn = num_comments * score * afinn_score,
          nc_sc_nrc = num_comments * score * nrc_score
        )
  
  } else if(source == "Twitter"){
    
    df <- df %>% 
      mutate(
         rt_bing = (retweet_count + quote_count) * bing_score,
         rt_afinn = (retweet_count + quote_count) * afinn_score,
         rt_nrc = (retweet_count + quote_count) * nrc_score,
         l_bing = (reply_count + like_count) * bing_score,
         l_afinn = (reply_count + like_count) * afinn_score,
         l_nrc = (reply_count + like_count) * nrc_score,
         rt_l_bing = (retweet_count + quote_count) * (reply_count + like_count) * bing_score,
         rt_l_afinn = (retweet_count + quote_count) * (reply_count + like_count) * afinn_score,
         rt_l_nrc = (retweet_count + quote_count) * (reply_count + like_count) * nrc_score
      ) 
      
    
  }
  
  return(df)
  
}


summarize_days <- function(df, source){
  
  if(source == "Reddit"){
  
      df <- df %>% 
        mutate(date = as.Date(date)) %>% 
        mutate(score = score -1) %>% 
        group_by(date) %>% 
        summarize(
          obs = as.character(n()),
          bing = sum(bing_score),
          nc_bing = sum(nc_bing),
          sc_bing = sum(sc_bing),
          nc_sc_bing = sum(nc_sc_bing),
          afinn = sum(afinn_score),
          nc_afinn = sum(nc_afinn),
          sc_afinn = sum(sc_afinn),
          nc_sc_afinn = sum(nc_sc_afinn),
          nrc = sum(nrc_score),
          nc_nrc = sum(nc_nrc),
          sc_nrc = sum(sc_nrc),
          nc_sc_nrc = sum(nc_sc_nrc)
        ) %>% 
        mutate(
          across(where(is.numeric), scale)
        ) %>% 
        mutate(obs = as.double(obs))
  
  } else if(source == "Twitter"){
    
      df <- df %>% 
        mutate(date = as.Date(date)) %>% 
        group_by(date) %>% 
        summarize(
          obs = as.character(n()),
          bing = sum(bing_score),
          afinn = sum(afinn_score),
          nrc = sum(nrc_score),
          rt_bing = sum(rt_bing),
          rt_afinn = sum(rt_afinn),
          rt_nrc = sum(rt_nrc),
          l_bing = sum(l_bing),
          l_afinn = sum(l_afinn),
          l_nrc = sum(l_nrc),
          rt_l_bing = sum(rt_l_bing),
          rt_l_afinn = sum(rt_l_afinn),
          rt_l_nrc = sum(rt_l_nrc)
        ) %>% 
        mutate(
          across(where(is.numeric), scale)
        ) %>% 
        mutate(obs = as.double(obs))
        
    
  }
      
  return(df)
}




load_all_reddit <- function(){
  
  reddit_emp <- list()
  reddit_vac <- list()
  
  # Employment
  # Set 1
  reddit_emp$c1b$set_1 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 1,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  reddit_emp$c2$set_1 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 1,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  # Set 2
  reddit_emp$c1b$set_2 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 2,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  reddit_emp$c2$set_2 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 2,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  # Set 3
  reddit_emp$c1b$set_3 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 3,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  reddit_emp$c2$set_3 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 3,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  # Set 4
  reddit_emp$c1b$set_4 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 4,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  reddit_emp$c2$set_4 <- load_nlp_data(
    source = "Reddit",
    topic = "Employment",
    set = 4,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  # Vaccination
  reddit_vac$c1b <- load_nlp_data(
    source = "Reddit",
    topic = "Vaccination",
    set = 1,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  # Vaccination
  reddit_vac$c2 <- load_nlp_data(
    source = "Reddit",
    topic = "Vaccination",
    set = 1,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Reddit") %>% 
    summarize_days(., source = "Reddit")
  
  return(list(reddit_emp, reddit_vac))
}

load_all_twitter <- function(){
  
  twitter_emp <- list()
  twitter_vac <- list()
  
  twitter_emp$c1b <- load_nlp_data(
    source = "Twitter",
    topic = "Employment",
    set = 1,
    level = 2,
    stage = '1b'
  ) %>% 
    change_scores(., source = "Twitter") %>% 
    summarize_days(., source = "Twitter")
  
  twitter_emp$c2 <- load_nlp_data(
    source = "Twitter",
    topic = "Employment",
    set = 1,
    level = 2,
    stage = '2'
  ) %>% 
    change_scores(., source = "Twitter") %>% 
    summarize_days(., source = "Twitter")
  
  twitter_vac$c1b <- load_nlp_data(
    source = "Twitter",
    topic = "Vaccination",
    set = 1,
    level = 2,
    stage = '1b'
  ) %>% 
    mutate(like_count = as.numeric(like_count)) %>% 
    change_scores(., source = "Twitter") %>% 
    summarize_days(., source = "Twitter")
  
  twitter_vac$c2 <- load_nlp_data(
    source = "Twitter",
    topic = "Vaccination",
    set = 1,
    level = 2,
    stage = '2'
  ) %>% 
    mutate(like_count = as.numeric(like_count)) %>% 
    change_scores(., source = "Twitter") %>% 
    summarize_days(., source = "Twitter")
  
  return(list(twitter_emp, twitter_vac))
  
}

list[reddit_emp, reddit_vac] <- load_all_reddit()
list[twitter_emp, twitter_vac] <- load_all_twitter()

