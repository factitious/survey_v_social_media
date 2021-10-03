library(tidyverse)
library(lubridate)
library(glue)
library(gsubfn)
library(caTools)
library(caret)
library(data.table)
library(formattable)
library(ggplot2)
library(readxl)
library(evolqg)
library(ggcorrplot)

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


## FUNC: Loading data ###############################

root_dir <- '/Volumes/Survey_Social_Media_Compare'
oj_data_path <- 'Methods/Data/Objective'
ai_data_path <- 'Methods/Data/Surveys/Axios-Ipsos/Aggregate'
hps_data_path <- 'Methods/Data/Surveys/HPS/Aggregate'
overall_path <- 'Methods/Data/Overall'
survey_periods_path <- 'Methods/Scripts/Surveys/table_details'
viz_path <- '/Users/munchausend/GoogleDrive/MSc/Dissertation/Write-up/Report/9247111/viz'
setwd(root_dir)

start_date <- '2020-10-22'
vac_start_date <- '2021-01-07'
vac_start_date_lag <- '2021-01-06'
end_date <- '2021-05-24'


###Objective measures

load_objective <- function(){
  
  objective <- list()
  
  # S&P data from https://www.nasdaq.com/market-activity/index/spx/historical.
  # Downloaded at:
  objective$raw$sp500 <- read.csv(
    file.path(
      root_dir,
      oj_data_path,
      'sp500.csv'
    )
  ) %>% 
    select(-Volume) %>% 
    dplyr::mutate(Day = mdy(Date),
    ) %>% 
    filter(Day >= ymd(start_date)) %>% 
    filter(Day < ymd(end_date)) %>% 
    select(Day, Close.Last) %>% 
    dplyr::mutate(sp500_Close = scale(Close.Last)) %>% 
    select(-Close.Last)
  
  # UoM Index of Consumer Sentiment from http://www.sca.isr.umich.edu/tables.html
  # Downloaded at:
  objective$raw$ics <- read.csv('http://www.sca.isr.umich.edu/files/tbcics.csv',
                                skip = 3, 
                                header = TRUE) %>% 
    Filter(function(x)!all(is.na(x)), .) %>% 
    dplyr::mutate(Day = glue('{X} {DATE.OF.SURVEY}'),
                  Index = `INDEX.OF.CONSUMER.SENTIMENT`) %>% 
    select(Day, Index) %>% 
    slice(2:14) %>% 
    dplyr::mutate(Day = ym(Day)) %>% 
    filter(Day >= ym(glue("{year(start_date)}-{month(start_date)}"))) %>% 
    filter(Day < ym(glue("{year(end_date)}-{month(end_date)}")))
  
  
  # CDC Vaccination statistics 
  # (https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc)
  objective$raw$cdc_vac <- read.csv(
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
    dplyr::mutate(Day = mdy(Date)) %>% 
    filter(
      Day < ymd(end_date)
    ) %>% 
    filter(
      Day > ymd(vac_start_date_lag)
    ) %>% 
    group_by(Day) %>% 
    dplyr::summarise(
      administered = sum(Administered)
    ) %>% 
    dplyr::mutate(administered = scale(administered - lag(administered))) %>% 
    filter(!is.na(administered))
  
  return(objective)
  
}

### Survey data

# Survey periods
load_sp <- function(root_dir, survey_periods_path){
  
  # "/Volumes/Survey_Social_Media_Compare/Methods/Scripts/Surveys/table_details/surveyPeriods.xlsx", 
  
  sp <- list()
  
  sp$raw <- read_excel(
    file.path(
      root_dir,
      survey_periods_path,
      'surveyPeriods.xlsx'
    ),
    sheet = "AI+HPS", col_types = c("text", 
                                    "date", "date", "text", "date", "date", 
                                    "text", "text", "text"))
  
  sp$proc <- sp$raw %>% 
    select(-A_I_week, -HPS_Week, -A_I_topic, -HPS_topic) %>% 
    dplyr::mutate(ai_start = A_I_start_date,
                  ai_end = A_I_end_date,
                  hps_start = HPS_start_date,
                  hps_end = HPS_end_date) %>% 
    select(Period, ai_start, ai_end,
           hps_start, hps_end) %>% 
    dplyr::mutate(Period = 
                    as.numeric(
                      str_extract(
                        Period, "[[:digit:]]+"
                      )
                    )
    )
  
  sp$hps <- sp$proc %>% 
    select(Period, hps_start, hps_end) %>% 
    distinct()%>%
    group_by(Period) %>% 
    dplyr::summarise(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(hps_start, tz = "UTC"),
        as.POSIXct(hps_end),
        by = "day")))
  
  ai_hps <- sp$proc %>% 
    dplyr::mutate(
      d = sp$proc %>% select(Period) %>% duplicated(.)) %>% 
    dplyr::mutate(
      Period = ifelse(d == T, glue("{Period}b"), Period)) %>% 
    select(Period, ai_start, ai_end) %>% 
    group_by(Period) %>% 
    dplyr::summarise(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(ai_start, tz = "UTC"),
        as.POSIXct(ai_end),
        by = "day"))
    ) %>% 
    dplyr::mutate(Period = 
                    as.numeric(
                      str_extract(
                        Period, "[[:digit:]]+")
                    )) %>% arrange(., Period)
  
  ai <- sp$proc %>% 
    dplyr::mutate(ai_Period = (1:nrow(.))) %>% 
    select(ai_Period, ai_start, ai_end) %>% 
    group_by(ai_Period) %>% 
    dplyr::summarise(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(ai_start, tz = "UTC"),
        as.POSIXct(ai_end),
        by = "day")))
  
  sp$ai <- ai_hps %>% 
    left_join(ai, by = "Day")
  
  return(sp)
  
}

load_surveys <- function(){
  
  surveys <- list()
  
  surveys$ai$emp <- readRDS(
    file.path(
      root_dir, 
      ai_data_path,
      'emp.rds'
    )
  ) %>% 
    add_column(
      ai_Period = (1:nrow(.)), 
      .after= "Period")
  
  setnames(
    surveys$ai$emp,
    old = c("18-49", "Total"),
    new = c("ai_18-49", "ai_Total")
  )
  
  surveys$ai$vac <- readRDS(
    file.path(
      root_dir, 
      ai_data_path,
      'vac.rds'
    )
  ) %>% 
    add_column(
      ai_Period = (1:nrow(.)), 
      .after= "Period") %>% 
    filter(Period >= 5)
  
  setnames(
    surveys$ai$vac,
    old = c("18-49", "Total"),
    new = c("ai_18-49", "ai_Total")
  )
  
  surveys$hps$emp <- readRDS(
    file.path(
      root_dir, 
      hps_data_path,
      'emp.rds'
    )
  ) %>% 
    dplyr::mutate(Period = 
                    as.numeric(
                      str_extract(
                        Period, "[[:digit:]]+"
                      )
                    )
    )
  
  setnames(
    surveys$hps$emp,
    old = c("18-54", "Total"),
    new = c("hps_18-54", "hps_Total")
  )
  
  hps_vac_s1 <- readRDS(
    file.path(
      root_dir, 
      hps_data_path,
      'vac_s1.rds'
    )
  ) %>% 
    dplyr::mutate(Period = 
                    as.numeric(
                      str_extract(
                        Period, "[[:digit:]]+"
                      )
                    )
    )
  
  setnames(
    hps_vac_s1,
    old = c("18-54", "Total"),
    new = c("hps_18-54", "hps_Total")
  )
  
  hps_vac_s2 <- readRDS(
    file.path(
      root_dir, 
      hps_data_path,
      'vac_s2.rds'
    )
  ) %>% 
    dplyr::mutate(Period = 
                    as.numeric(
                      str_extract(
                        Period, "[[:digit:]]+"
                      )
                    )
    )
  
  # setnames(
  #   hps_vac_s2,
  #   old = c("18-54", "Total"),
  #   new = c("hps_s2_18-54", "hps_s2_Total")
  # )
  # 
  # 
  # surveys$hps$vac <- hps_vac_s1 %>% 
  #   inner_join(hps_vac_s2, by = "Period") %>% 
  #   filter(Period >= 5)
  
  setnames(
    hps_vac_s2,
    old = c("18-54", "Total"),
    new = c("hps_18-54", "hps_Total")
  )
  
  
  surveys$hps$vac <- hps_vac_s1 %>% 
    filter(Period >=5)
  
  
  return(surveys)
}

### Social media data.

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
    data_path <- glue('Methods/Data/{source}/NLP/ML/c{stage}')
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
    dplyr::mutate(bing_score = bing_score/bing_words,
                  afinn_score = afinn_score/afinn_words,
                  nrc_score = nrc_score/nrc_words) %>% 
    dplyr::mutate(bing_score = ifelse(is.na(bing_score), 0.5,
                                      ifelse(bing_score < 0, 0,
                                             1)),
                  afinn_score = ifelse(is.na(afinn_score), 0.5,
                                       ifelse(afinn_score < 0, 0,
                                              1)),
                  nrc_score = ifelse(is.na(nrc_score), 0.5,
                                     ifelse(nrc_score < 0, 0,
                                            1))) %>% 
    select(-bing_words,
           -afinn_words,
           -nrc_words)
  
  if(source == "Reddit"){
    
    df <- df %>% 
      dplyr::mutate(Day = as.Date(date)) %>% select(-date) %>% 
      group_by(Day) %>% 
      dplyr::mutate(
        comm_weight = num_comments/sum(num_comments),
        score_weight = score/sum(score)
      ) %>% 
      dplyr::mutate(
        nc_bing = bing_score*comm_weight,
        nc_afinn = afinn_score*comm_weight,
        nc_nrc = nrc_score*comm_weight,
        sc_bing = bing_score*score_weight,
        sc_afinn = afinn_score*score_weight,
        sc_nrc = nrc_score*score_weight
      )
    
  } else if(source == "Twitter"){
    
    df <- df %>% 
      dplyr::mutate(Day = as.Date(date)) %>% 
      group_by(Day) %>% 
      dplyr::mutate(
        rtl_score = retweet_count + like_count,
        rtl_weight = rtl_score/sum(rtl_score),
        reply_weight = reply_count/sum(reply_count),
      ) %>% 
      dplyr::mutate(
        rtl_bing = bing_score*rtl_weight,
        rtl_afinn = afinn_score*rtl_weight,
        rtl_nrc = nrc_score*rtl_weight,
        re_bing = bing_score*reply_weight,
        re_afinn = afinn_score*reply_weight,
        re_nrc = nrc_score*reply_weight
      )
    
    
  }
  
  return(df)
  
}

summarize_days <- function(df, source){
  
  if(source == "Reddit"){
    
    df <- df %>% 
      group_by(Day) %>%
      dplyr::summarise(
        obs = n(),
        m_comm = mean(num_comments),
        m_score = mean(score),
        bing = mean(bing_score),
        nc_bing = sum(nc_bing),
        sc_bing = sum(sc_bing),
        afinn = mean(afinn_score),
        nc_afinn = sum(nc_afinn),
        sc_afinn = sum(sc_afinn),
        nrc = mean(nrc_score),
        nc_nrc = sum(nc_nrc),
        sc_nrc = sum(sc_nrc)
      )
    
  } else if(source == "Twitter"){
    
    df <- df %>% 
      group_by(Day) %>% 
      dplyr::summarise(
        obs = n(),
        m_rtl = mean(retweet_count + like_count),
        m_re = mean(reply_count),
        bing = mean(bing_score),
        afinn = mean(afinn_score),
        nrc = mean(nrc_score),
        rtl_bing = sum(rtl_bing),
        rtl_afinn = sum(rtl_afinn),
        rtl_nrc = sum(rtl_nrc),
        re_bing = sum(re_bing),
        re_afinn = sum(re_afinn),
        re_nrc = sum(re_nrc)
      )
    
    
  }
  
  return(df)
}

load_all_reddit <- function(level = 2, proc = F, change_sc = T, summ = T){
  
  if(proc == F){
    
    reddit <- list()
    
    # Employment
    # Set 1
    reddit$emp$c1_set1 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 1,
      level = level,
      stage = '1b'
    ) 
    
    reddit$emp$c2_set1 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 1,
      level = level,
      stage = '2'
    )       
    
    
    # Set 2
    reddit$emp$c1_set2 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 2,
      level = level,
      stage = '1b'
    )
    
    reddit$emp$c2_set2 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 2,
      level = level,
      stage = '2'
    )
    
    
    # Set 3
    reddit$emp$c1_set3 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 3,
      level = level,
      stage = '1b'
    ) 
    
    reddit$emp$c2_set3 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 3,
      level = level,
      stage = '2'
    ) 
    
    sr_list <- unique(reddit$emp$c1_set3$subreddit)
    
    # Set 4
    reddit$emp$c1_set4 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 4,
      level = level,
      stage = '1b'
    ) %>% 
      filter(subreddit %in% sr_list)
    
    reddit$emp$c2_set4 <- load_nlp_data(
      source = "Reddit",
      topic = "Employment",
      set = 4,
      level = level,
      stage = '2'
    ) %>% 
      filter(subreddit %in% sr_list)
    
    # Vaccination
    reddit$vac$c1 <- load_nlp_data(
      source = "Reddit",
      topic = "Vaccination",
      set = 1,
      level = level,
      stage = '1b'
    ) 
    
    # Vaccination
    reddit$vac$c2 <- load_nlp_data(
      source = "Reddit",
      topic = "Vaccination",
      set = 1,
      level = level,
      stage = '2'
    ) 
    
    
    if(change_sc){
      
      reddit$emp$c1_set1 <- reddit$emp$c1_set1%>% 
        change_scores(., source = "Reddit")
      
      reddit$emp$c1_set2 <- reddit$emp$c1_set2%>% 
        change_scores(., source = "Reddit") 
      
      reddit$emp$c1_set3 <- reddit$emp$c1_set3%>% 
        change_scores(., source = "Reddit")
      
      reddit$emp$c1_set4 <- reddit$emp$c1_set4%>% 
        change_scores(., source = "Reddit")
      
      reddit$emp$c2_set1 <- reddit$emp$c2_set1%>% 
        change_scores(., source = "Reddit")
      
      reddit$emp$c2_set2 <- reddit$emp$c2_set2%>% 
        change_scores(., source = "Reddit") 
      
      reddit$emp$c2_set3 <- reddit$emp$c2_set3%>% 
        change_scores(., source = "Reddit") 
      
      reddit$emp$c2_set4 <- reddit$emp$c2_set4%>% 
        change_scores(., source = "Reddit") 
      
      reddit$vac$c1 <- reddit$vac$c1%>% 
        change_scores(., source = "Reddit")
      
      reddit$vac$c2 <- reddit$vac$c2%>% 
        change_scores(., source = "Reddit")
      
    }
    
    if(summ){
      
      reddit$emp$c1_set1 <- reddit$emp$c1_set1%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c1_set2 <- reddit$emp$c1_set2%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c1_set3 <- reddit$emp$c1_set3%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c1_set4 <- reddit$emp$c1_set4%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2_set1 <- reddit$emp$c2_set1%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2_set2 <- reddit$emp$c2_set2%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2_set3 <- reddit$emp$c2_set3%>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2_set4 <- reddit$emp$c2_set4%>% 
        summarize_days(., source = "Reddit")
      
      reddit$vac$c1 <- reddit$vac$c1%>% 
        summarize_days(., source = "Reddit")
      
      reddit$vac$c2 <- reddit$vac$c2%>% 
        summarize_days(., source = "Reddit")
      
    }
    
    
  } else if(proc == T){
    
    reddit <- readRDS(
      file.path(
        root_dir, 
        overall_path,
        'reddit.rds'
      )
    )
    
  }
  
  return(reddit)
}

load_all_twitter <- function(level = 2, proc = F, change_sc = T, summ = T){
  
  if(proc == F){
    
    twitter <- list()
    
    twitter$emp$c1 <- load_nlp_data(
      source = "Twitter",
      topic = "Employment",
      set = 1,
      level = level,
      stage = '1b'
    ) 
    
    twitter$emp$c2 <- load_nlp_data(
      source = "Twitter",
      topic = "Employment",
      set = 1,
      level = level,
      stage = '2'
    )
    
    twitter$vac$c1 <- load_nlp_data(
      source = "Twitter",
      topic = "Vaccination",
      set = 1,
      level = level,
      stage = '1b'
    ) 
    
    twitter$vac$c2 <- load_nlp_data(
      source = "Twitter",
      topic = "Vaccination",
      set = 1,
      level = level,
      stage = '2'
    ) 
    
    if(change_sc){
      
      twitter$emp$c1 <- twitter$emp$c1 %>% 
        change_scores(., source = "Twitter")
      
      twitter$emp$c2 <- twitter$emp$c2 %>% 
        change_scores(., source = "Twitter")
      
      twitter$vac$c1 <- twitter$vac$c1 %>% 
        dplyr::mutate(like_count = as.numeric(like_count)) %>% 
        change_scores(., source = "Twitter")
      
      twitter$vac$c2 <- twitter$vac$c2 %>% 
        dplyr::mutate(like_count = as.numeric(like_count)) %>% 
        change_scores(., source = "Twitter")
      
    }
    
    if(summ){
      
      twitter$emp$c1 <- twitter$emp$c1 %>% 
        summarize_days(., source = "Twitter")
      
      twitter$emp$c2 <- twitter$emp$c2 %>% 
        summarize_days(., source = "Twitter")
      
      twitter$vac$c1 <- twitter$vac$c1 %>% 
        summarize_days(., source = "Twitter")
      
      twitter$vac$c2 <- twitter$vac$c2 %>% 
        summarize_days(., source = "Twitter")
      
    }
    
  } else if(proc == T){
    
    twitter <- readRDS(
      file.path(
        root_dir, 
        overall_path,
        'twitter.rds'
      )
    )
    
  }
  
  return(twitter)
  
}

# # To re-run:
# sm <- list()
# 
# sm$reddit <- load_all_reddit(proc = F)
# sm$twitter <- load_all_twitter(proc = F)
# saveRDS(
#   sm$reddit,
#   file.path(
#     root_dir,
#     overall_path,
#     'reddit.rds'
#   ))
# 
# saveRDS(
#   sm$twitter,
#   file.path(
#     root_dir,
#     overall_path,
#     'twitter.rds'
#   ))



## FUNC: Pre-processing ###############################

# Combine objective measures
c_objective <- function(objective_df){
  ts <- as.Date(seq.POSIXt(
    as.POSIXct(start_date, tz = "UTC"),
    as.POSIXct(end_date),
    by = "day"))
  
  ts <- as.data.frame(ts) %>% 
    dplyr::mutate(Day = ts) %>%  
    select(Day)
  
  objective_df$emp <- ts %>% 
    full_join(objective$raw$sp500, by = "Day") %>% 
    full_join(objective$raw$ics, by = "Day") %>% 
    dplyr::mutate(ics = Index) %>% 
    select(-Index) %>% 
    arrange(., Day) %>% 
    fill(ics) %>% 
    fill(sp500_Close) %>% 
    filter(Day >= ymd('2020-10-22'))
  
  objective_df$vac <- objective$raw$cdc_vac %>% 
    left_join(ts, by = "Day")
  
  return(objective_df)
}

# Combining objective measures with sm data.
c_sm_obj <- function(df, do_trim = F){
  
  clean_func <- function(df, topic, do_trim){
    
    cdf <- df %>% 
      full_join(objective[[topic]], by = "Day") %>%
      filter(Day >= ymd(start_date)) %>% 
      filter(!is.na(bing)) %>% 
      select(-contains('ics'))
    
    if(topic == 'vac'){
      
      cdf <- cdf %>%
        filter(Day > ymd(vac_start_date))
    }
    
    if(do_trim){
      cdf <- cdf %>% 
        filter(obs > 100)
    }
    
    return(cdf)
  }
  
  sm_obj <- list()
  
  sm_obj$reddit$emp <- lapply(sm$reddit$emp, clean_func, topic = 'emp', do_trim = do_trim) 
  sm_obj$reddit$vac <- lapply(sm$reddit$vac, clean_func, topic = 'vac', do_trim = do_trim) 
  
  sm_obj$twitter$emp <- lapply(sm$twitter$emp, clean_func, topic = 'emp', do_trim = do_trim) 
  sm_obj$twitter$vac <- lapply(sm$twitter$vac, clean_func, topic = 'vac', do_trim = do_trim) 
  
  return(sm_obj)
  
}

# Removing reddit datasets from the list resulting from c_sm_obj
rm_subsets <- function(sm, ds){
  
  if(ds == 'obj'){
    
    sm$reddit$emp <- sm$reddit$emp[1:2]
    names(sm$reddit$emp) <- c('c1', 'c2')
    
  } else if(ds == 'all'){
    
    sm$reddit$hps$emp <- sm$reddit$hps$emp[1:2]
    sm$reddit$ai$emp <- sm$reddit$ai$emp[1:2]
    sm$reddit$hps_ai$emp <- sm$reddit$hps_ai$emp[1:2]
    
    names(sm$reddit$hps$emp) <- c('c1', 'c2')
    names(sm$reddit$ai$emp) <- c('c1', 'c2')
    names(sm$reddit$hps_ai$emp) <- c('c1', 'c2')
  }
  
  return(sm)
}

# Combining surveys with sm data and objective measures.
c_all <- function(sp_df = sp, objective_df = objective, survey_df = surveys){
  
  c_all_func <- function(
    df,
    ss,
    topic){
    
    # cdf <- df %>% 
    #   inner_join(sp_df$hps, by = "Day") %>% 
    #   inner_join(objective_df[[topic]], by = "Day") %>% 
    #   inner_join(survey_df$hps[[topic]], by = "Period") %>%
    #   left_join(
    #     surveys$ai[[topic]] %>% inner_join(sp$hps, by = "Period"), 
    #     by = "Day") %>% 
    #   dplyr::mutate(
    #     Period = Period.x,
    #     Period.x = NULL,
    #     Period.y = NULL)
    # 
    # return(cdf)
    
    if(ss == 'hps'){
      cdf <- df %>%
        inner_join(sp_df[[ss]], by = "Day") %>%
        inner_join(objective_df[[topic]], by = "Day") %>%
        inner_join(survey_df[[ss]][[topic]], by = "Period")
    } else if(ss == 'ai'){
      cdf <- df %>%
        inner_join(sp_df[[ss]], by = "Day") %>%
        inner_join(objective_df[[topic]], by = "Day") %>%
        inner_join(survey_df[[ss]][[topic]], by = "ai_Period") %>%
        dplyr::mutate(
          Period = Period.x,
          Period.x = NULL,
          Period.y = NULL)
    } else if(ss == 'hps_ai'){
      
      cdf <- df %>% 
        inner_join(sp_df$hps, by = "Day") %>% 
        inner_join(objective_df[[topic]], by = "Day") %>% 
        inner_join(surveys$hps[[topic]], by = "Period") 
      
      # %>% 
      #   inner_join(surveys$ai[[topic]], by = "Period") %>% 
      #   select(-Day)
      
      # h3 <- h1 %>% 
      #   left_join(
      #     h2 %>% select(
      #       Day,
      #       `ai_18-49`,
      #       `ai_Total`
      #     ), 
      #     by = 'Day') %>% 
      # fill(
      #   `ai_18-49`,
      #   `ai_Total`,
      #   .direction = 'up'
      # )
      
    }
    
    return(cdf)
  }
  
  sm_all <- list()
  
  sm_all$reddit$hps$emp <- lapply(sm$reddit$emp, c_all_func, ss='hps', topic='emp')
  sm_all$reddit$hps$vac <- lapply(sm$reddit$vac, c_all_func, ss='hps', topic='vac')
  sm_all$reddit$ai$emp <- lapply(sm$reddit$emp, c_all_func, ss='ai', topic='emp')
  sm_all$reddit$ai$vac <- lapply(sm$reddit$vac, c_all_func, ss='ai', topic='vac')
  
  sm_all$twitter$hps$emp <- lapply(sm$twitter$emp, c_all_func, ss='hps', topic='emp')
  sm_all$twitter$hps$vac <- lapply(sm$twitter$vac, c_all_func, ss='hps', topic='vac')
  sm_all$twitter$ai$emp <- lapply(sm$twitter$emp, c_all_func, ss='ai', topic='emp')
  sm_all$twitter$ai$vac <- lapply(sm$twitter$vac, c_all_func, ss='ai', topic='vac')
  
  sm_all$reddit$hps_ai$emp <- lapply(sm$reddit$emp, c_all_func, ss='hps_ai', topic = 'emp')
  sm_all$reddit$hps_ai$vac <- lapply(sm$reddit$vac, c_all_func, ss='hps_ai', topic = 'vac')
  sm_all$twitter$hps_ai$emp <- lapply(sm$twitter$emp, c_all_func, ss='hps_ai', topic = 'emp')
  sm_all$twitter$hps_ai$vac <- lapply(sm$twitter$vac, c_all_func, ss='hps_ai', topic = 'vac')
  
  return(sm_all)
}

## EXEC: Loading + pre-processing #####

# Load data
# Objective
objective <- load_objective()
sp <- load_sp(root_dir, survey_periods_path)

# Surveys
surveys <- load_surveys()

# Social media data
sm <- list()
sm_full <- list()
sm$reddit <- load_all_reddit(proc = T)
sm$twitter <- load_all_twitter(proc = T)

sm_full$reddit <- load_all_reddit(proc = F, change_sc = T, summ = F)
sm_full$twitter <- load_all_twitter(proc = F, change_sc = T,  summ = F)

# Pre-process
objective <- c_objective(objective)
sm_obj <- c_sm_obj(do_trim=T)
sm_obj_nss <- rm_subsets(sm_obj, 'obj')
sm_all <- c_all()
sm_all_nss <- rm_subsets(sm_all, 'all')

create_hps_ai <- function(sm, source, topic){
  
  sm[[source]]$hps_ai[[topic]]$c1 <-
    sm[[source]]$hps[[topic]]$c1 %>% 
    left_join(
      sm[[source]]$ai[[topic]]$c1 %>% 
        select(
          Day,
          `ai_18-49`,
          `ai_Total`
        ), by = 'Day') %>% 
    fill(
      `ai_18-49`,
      `ai_Total`,
      .direction = 'updown'
    ) 
  
  sm[[source]]$hps_ai[[topic]]$c2 <-
    sm[[source]]$hps[[topic]]$c2 %>% 
    left_join(
      sm[[source]]$ai[[topic]]$c2 %>% 
        select(
          Day,
          `ai_18-49`,
          `ai_Total`
        ), by = 'Day') %>% 
    fill(
      `ai_18-49`,
      `ai_Total`,
      .direction = 'updown'
    )
  
  return(sm)
  
}

sm_all_nss <- create_hps_ai(sm_all_nss, 'reddit', 'emp')
sm_all_nss <- create_hps_ai(sm_all_nss, 'reddit', 'vac')
sm_all_nss <- create_hps_ai(sm_all_nss, 'twitter', 'emp')
sm_all_nss <- create_hps_ai(sm_all_nss, 'twitter', 'vac')

## Descriptives #######

# Summary of number of observations in all datasets (before pre-processing)
pptable <- function(summ_df){
  
  for(n in names(summ_df)){
    
    summ_df[[n]] <- as.character(summ_df[[n]])
    
  }
  
  return(summ_df)
  
}

summ_full <- function(sm_full){
  
  source_name <- names(sm_full)
  
  reddit_df <- data.frame(
    matrix(NA,
           nrow = 12,
           ncol = 0),
    row.names = 
      c(
        'n',
        'Subreddits',
        'Comments (Mean)',
        'Comments (% = 0)',
        'Score (Mean)',
        'Score (% = 1)',
        'BING score (Mean)',
        'BING score (% = 0.5)',
        'AFINN score (Mean)',
        'AFINN score (% = 0.5)',
        'NRC score (Mean)',
        'NRC score (% = 0.5)'
      )
  )
  
  twitter_df <- data.frame(
    matrix(NA,
           nrow = 13,
           ncol = 0),
    row.names = 
      c(
        'n',
        'RT (Mean)',
        'RT (% = 0)',
        'Likes (Mean)',
        'Likes (% = 0)',
        'Replies (Mean)',
        'Replies (% = 0)',
        'BING score (Mean)',
        'BING score (% = 0)',
        'AFINN score (Mean)',
        'AFINN score (% = 0)',
        'NRC score (Mean)',
        'NRC score (% = 0)'
      )
  )
  
  
  for(sn in source_name){
    
    
    topic_names <- names(sm_full[[sn]])
    
    for(tn in topic_names){
      
      d_names <- names(sm_full[[sn]][[tn]])
      
      for(dn in d_names){
        
        tdf <- sm_full[[sn]][[tn]][[dn]]
        
        n_obs <- as.numeric(nrow(tdf))
        
        bing_avg <- mean(tdf$bing_score)
        bing_perc_zero <- nrow(tdf[tdf$bing_score == 0.5,])/nrow(tdf)
        
        afinn_avg<- mean(tdf$afinn_score)
        afinn_perc_zero <- nrow(tdf[tdf$afinn_score == 0.5,])/nrow(tdf)
        
        nrc_avg <- mean(tdf$nrc_score)
        nrc_perc_zero <- nrow(tdf[tdf$nrc_score == 0.5,])/nrow(tdf)
        
        vn <- glue("{tn}_{dn}")
        
        if(sn=='reddit'){
          
          n_sr <- as.numeric(length(unique(tdf$subreddit)))
          
          nc_avg_comm <- mean(tdf$num_comments)
          nc_perc_zero <- nrow(tdf[tdf$num_comments == 0,])/nrow(tdf)
          
          sc_avg_comm <- mean(tdf$score)
          sc_perc_zero <- nrow(tdf[tdf$score == 1,])/nrow(tdf)
          
          m_list <- c(
            n_obs, 
            n_sr, 
            nc_avg_comm, 
            nc_perc_zero, 
            sc_avg_comm, 
            sc_perc_zero,
            bing_avg, 
            bing_perc_zero,
            afinn_avg, 
            afinn_perc_zero,
            nrc_avg, 
            nrc_perc_zero
          )
          
          
          reddit_df <- reddit_df %>% 
            add_column(bla = m_list)
          
          names(reddit_df)[names(reddit_df)=='bla'] <- vn
          
        } else if(sn=='twitter'){
          
          tdf <- tdf %>% 
            dplyr::mutate(like_count = as.numeric(like_count))
          
          rt_avg_comm <- mean(tdf$retweet_count + tdf$quote_count)
          rt_perc_zero <- nrow(tdf[tdf$retweet_count == 0,])/nrow(tdf)
          
          l_avg_comm <- mean(tdf$like_count)
          l_perc_zero <- nrow(tdf[tdf$like_count == 0,])/nrow(tdf)
          
          re_avg_comm <- mean(tdf$reply_count)
          re_perc_zero <- nrow(tdf[tdf$reply_count == 0,])/nrow(tdf)
          
          m_list <- c(
            n_obs, 
            rt_avg_comm, 
            rt_perc_zero, 
            l_avg_comm, 
            l_perc_zero,
            re_avg_comm, 
            re_perc_zero,
            bing_avg, 
            bing_perc_zero,
            afinn_avg, 
            afinn_perc_zero,
            nrc_avg, 
            nrc_perc_zero
          )
          
          
          twitter_df <- twitter_df %>% 
            add_column(bla = m_list)
          
          names(twitter_df)[names(twitter_df)=='bla'] <- vn
        }
        
      }}}
  
  reddit_df <- round(reddit_df,2)
  
  reddit_df[2, 3] <- 100
  reddit_df[2, 4] <- 100
  
  twitter_df <- round(twitter_df,2)
  
  return(list(
    reddit = reddit_df,
    twitter = twitter_df
  ))
  
}

full_desc <- summ_full(sm_full)
reddit_alldata_before_agg <- pptable(full_desc$reddit)
twitter_alldata_before_agg <- pptable(full_desc$twitter)


save_tab <- function(folder, df){
  
  saveRDS(
    df,
    file.path(
      viz_path,
      folder,
      glue("{substitute(df)}.rds")
    )
  )
  
}

save_tab('comparisons', reddit_alldata_before_agg)
save_tab('comparisons', twitter_alldata_before_agg)


# Summary of number of observations in all datasets (after pre-processing) 
summ_obj <- function(sm_obj){
  
  source_name <- names(sm_obj)
  
  reddit_df <- data.frame(
    matrix(NA,
           nrow = 13,
           ncol = 0),
    row.names = 
      c(
        'Days',
        'n (Mean)',
        'Comments',
        'Score',
        'BING',
        'NC_BING',
        'SC_BING',
        'AFINN',
        'NC_AFINN',
        'SC_AFINN',
        'NRC',
        'NC_NRC',
        'SC_NRC'
      )
  )
  
  twitter_df <- data.frame(
    matrix(NA,
           nrow = 13,
           ncol = 0),
    row.names = 
      c(
        'Days',
        'n (Mean)',
        'Retweets + likes',
        'Replies',
        'BING',
        'RTL_BING',
        'RE_BING',
        'AFINN',
        'RTL_AFINN',
        'RE_AFINN',
        'NRC',
        'RTL_NRC',
        'RE_NRC'
      )
  )
  
  
  for(sn in source_name){
    
    
    topic_names <- names(sm_obj[[sn]])
    
    for(tn in topic_names){
      
      d_names <- names(sm_obj[[sn]][[tn]])
      
      for(dn in d_names){
        
        tdf <- sm_obj[[sn]][[tn]][[dn]]
        
        n_days <- as.numeric(nrow(tdf))
        n_obs <- mean(tdf$obs)
        
        bing <- mean(tdf$bing)
        afinn <- mean(tdf$afinn)
        nrc <- mean(tdf$nrc)
        
        vn <- glue("{tn}_{dn}")
        
        if(sn=='reddit'){
          
          # tdf <- tdf %>% 
          #   mutate(m_comm = as.numeric(m_comm))
          # 
          m_comm_t <- mean(tdf$m_comm)
          
          m_score <- mean(tdf$m_score)
          
          nc_bing <- mean(tdf$nc_bing)
          sc_bing <- mean(tdf$sc_bing)
          
          nc_afinn <- mean(tdf$nc_afinn)
          sc_afinn <- mean(tdf$sc_afinn)
          
          nc_nrc <- mean(tdf$nc_nrc)
          sc_nrc <- mean(tdf$sc_nrc)
          
          m_list <- c(
            n_days, 
            n_obs,
            m_comm_t, 
            m_score,
            bing,
            nc_bing,
            sc_bing,
            afinn,
            nc_afinn,
            sc_afinn,
            nrc,
            nc_nrc,
            sc_nrc
          )
          
          
          reddit_df <- reddit_df %>% 
            add_column(bla = m_list)
          
          names(reddit_df)[names(reddit_df)=='bla'] <- vn
          
        } else if(sn=='twitter'){
          
          m_rtl <- mean(tdf$m_rtl)
          m_re <- mean(tdf$m_re)
          
          rtl_bing <- mean(tdf$rtl_bing)
          re_bing <- mean(tdf$re_bing)
          
          rtl_afinn <- mean(tdf$rtl_afinn)
          re_afinn <- mean(tdf$re_afinn)
          
          rtl_nrc <- mean(tdf$rtl_nrc)
          re_nrc <- mean(tdf$re_nrc)
          
          m_list <- c(
            n_days, 
            n_obs,
            m_rtl, 
            m_re,
            bing,
            rtl_bing,
            re_bing,
            afinn,
            rtl_afinn,
            re_afinn,
            nrc,
            rtl_nrc,
            re_nrc
          )
          
          
          twitter_df <- twitter_df %>% 
            add_column(bla = m_list)
          
          names(twitter_df)[names(twitter_df)=='bla'] <- vn
        }
        
      }}}
  
  reddit_df <- round(reddit_df,2)
  twitter_df <- round(twitter_df,2)
  
  return(list(
    reddit = reddit_df,
    twitter = twitter_df
  ))
  
}

obj_desc <- summ_obj(sm_obj)

reddit_alldata_after_agg <- pptable(obj_desc$reddit)
twitter_alldata_after_agg <- pptable(obj_desc$twitter)


save_viz('comparisons', reddit_alldata_after_agg)
save_viz('comparisons', twitter_alldata_after_agg)

### SM v Survey ####

cor_ss <- function(sm_all, topic){
  
  # All correlations at the periodical aggregation level. 
  
  ss_cor <- list()
  
  summ_per <- function(r_df, t_df, c, topic){
    
    
    if(topic == 'emp'){
      
      df <- r_df %>% 
        inner_join(t_df, by = "Day", suffix = c('_r', '_t')) %>%
        select(-Day)%>% 
        mutate(
          obs = obs_r,
          obs_r = NULL,
          obs_t = NULL) %>% 
        group_by(Period) %>% 
        dplyr::summarise(
          across(bing_r:re_nrc,
                 ~sum(obs*.x)/sum(obs)),
          hps_SMA = mean(`hps_18-54`),
          hps_Total = mean(hps_Total),
          ai_SMA = mean(`ai_18-49`),
          ai_Total= mean(ai_Total),
          sp500 = mean(sp500_Close)
        )
      
    } else if(topic == 'vac'){
      
      df <- r_df %>% 
        inner_join(t_df, by = "Day", suffix = c('_r', '_t')) %>% 
        select(-Day) %>% 
        mutate(obs = obs_r) %>% 
        mutate(obs_r = NULL,
               obs_t = NULL) %>% 
        group_by(Period) %>% 
        dplyr::summarize(
          across(bing_r:re_nrc,
                 ~sum(obs*.x)/sum(obs)),
          hps_SMA = mean(`hps_18-54`),
          hps_Total = mean(hps_Total),
          ai_SMA = mean(`ai_18-49`),
          ai_Total= mean(ai_Total),
          administered = mean(administered)
        )
      
    }
    
    return(df)
    
  }
  
  reddit_c1 <- sm_all$reddit$hps_ai[[topic]][['c1']] %>% 
    select(-m_comm, -m_score) %>% 
    select( -contains('ics'),  
            -contains('uptake'),
            -contains('administered')) %>% 
    select(-contains('sp500_Close')) %>% 
    select(-contains('hps_18-54')) %>% 
    select(-contains('hps_Total')) %>% 
    select(-contains('ai_18-49')) %>% 
    select(-contains('ai_Total')) 
  # select(-nc_bing, -nc_afinn, -nc_nrc,
  #        -sc_bing, -sc_afinn, -sc_nrc)
  
  reddit_c2 <- sm_all$reddit$hps_ai[[topic]][['c2']] %>% 
    select(-m_comm, -m_score) %>% 
    select( -contains('ics'),  -contains('uptake'),
            -contains('administered')) %>% 
    select(-contains('sp500_Close')) %>% 
    select(-contains('hps_18-54')) %>% 
    select(-contains('hps_Total')) %>% 
    select(-contains('ai_18-49')) %>% 
    select(-contains('ai_Total')) 
  # select(-nc_bing, -nc_afinn, -nc_nrc,
  #        -sc_bing, -sc_afinn, -sc_nrc)
  
  twitter_c1 <- sm_all$twitter$hps_ai[[topic]][['c1']] %>% 
    select(-m_rtl, -m_re, -contains('ics')) %>% 
    select(-contains('uptake')) %>% 
    select(-contains('Period'))
  # select(-re_bing, -re_afinn, -re_nrc,
  #        -rtl_bing, -rtl_afinn, -rtl_nrc)
  
  twitter_c2 <- sm_all$twitter$hps_ai[[topic]][['c2']] %>% 
    select(-m_rtl, -m_re, -contains('ics')) %>% 
    select(-contains('uptake')) %>% 
    select(-contains('Period'))
  # select(-re_bing, -re_afinn, -re_nrc,
  #        -rtl_bing, -rtl_afinn, -rtl_nrc)
  
  
  all_combined <- reddit_c1 %>% 
    inner_join(twitter_c1, by = "Day", suffix = c('_r', '_t')) %>% 
    select(-Day) %>% 
    mutate(
      obs = obs_r,
      obs_r = NULL,
      obs_t = NULL)
  
  ss_cor$df_c1<- summ_per(reddit_c1, twitter_c1, 'c1', topic = topic)
  ss_cor$df_c2 <- summ_per(reddit_c2, twitter_c2, 'c2', topic = topic)
  
  full_cor <- list()
  
  full_cor$c1$cor <- as.data.frame(cor(ss_cor$df_c1 %>% select(-Period)))
  full_cor$c1$pmat <- as.data.frame(cor_pmat(ss_cor$df_c1 %>% select(-Period)))
  
  full_cor$c2$cor <- as.data.frame(cor(ss_cor$df_c2 %>% select(-Period)))
  full_cor$c2$pmat <- as.data.frame(cor_pmat(ss_cor$df_c2 %>% select(-Period)))
  
  # return(full_cor)
  
  # Everything except the objective measures
  ss_cor$sm_survey$c1$cor <- full_cor$c1$cor[-23, 19:22]
  ss_cor$sm_survey$c2$cor <- full_cor$c2$cor[-23, 19:22]
  
  ss_cor$sm_survey$c1$pmat <- full_cor$c1$pmat[-23, 19:22]
  ss_cor$sm_survey$c2$pmat <- full_cor$c2$pmat[-23, 19:22]
  
  # Objective measures only
  ss_cor$obj$c1$cor <- full_cor$c1$cor %>% 
    select(contains('sp500') | contains('administered')) 
  ss_cor$obj$c2$cor <- full_cor$c2$cor %>% 
    select(contains('sp500') | contains('administered')) 
  
  ss_cor$obj$c1$pmat <- full_cor$c1$pmat%>% 
    select(contains('sp500') | contains('administered')) 
  ss_cor$obj$c2$pmat <- full_cor$c2$pmat%>% 
    select(contains('sp500') | contains('administered')) 
  
  return(ss_cor)
  
}

ss_all <- list()

ss_all$emp <- cor_ss(sm_all_nss, 'emp')
ss_all$vac <- cor_ss(sm_all_nss, 'vac')


save_sm_survey_plot <- function(folder, df, topic){
  
  
  c1 <- ggcorrplot(
    as.matrix(df$sm_survey$c1$cor),
    p.mat = as.matrix(df$sm_survey$c1$pmat),
    sig.level = 0.05,
    lab = T,
    show.legend = F
  ) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 14))
    # theme(axis.text = element_text(size = 20))
  
  c2 <- ggcorrplot(
    as.matrix(df$sm_survey$c2$cor),
    p.mat = as.matrix(df$sm_survey$c2$pmat),
    sig.level = 0.05,
    lab = T,
    show.legend = F
  ) +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 14))
  
  n <- glue("sm_survey_{topic}.png")
  
  fpath <- file.path(
    viz_path,
    folder,
    n
  )
  
  ggsave(
    fpath,
    arrangeGrob(c1, c2),
    width = 7,
    height = 5,
    units = 'in',
    dpi = 'retina'
  )
  
}

grid.arrange(c1, c2)

save_sm_survey_plot('comparisons', ss_all$emp, 'emp')
save_sm_survey_plot('comparisons', ss_all$vac, 'vac')


## Survey descriptives.
save_survey_desc_plot <- function(){
  
  ss_all$emp$df_c1 %>% 
    select(Period, 
           contains('hps'),
           contains('ai')) %>% 
    mutate(
      HPS_Total = hps_Total,
      HPS_SMA = hps_SMA,
      AI_Total = ai_Total,
      AI_SMA = ai_SMA,
      hps_Total = NULL,
      ai_Total = NULL,
      hps_SMA = NULL,
      ai_SMA = NULL) %>% 
    pivot_longer(
      cols= !Period,
      names_to = "M", 
      values_to = "Index"
    ) %>% 
    separate(
      M,
      into = c("Survey", "Age"),
      sep = '_'
    ) %>%
    arrange(., Period) %>%
    mutate(Period = as.factor(Period)) %>% 
    ggplot(.) +
    geom_bar(
      aes(x=Period, y=Index, fill=Survey),
      stat = 'identity',
      position = position_dodge()
    ) + theme_classic() + 
    facet_wrap(~Age) +
    labs(y = "Attitudes towards employment")
  
  
  ggsave(
    file.path(
      viz_path,
      'comparisons/surveys_emp_barplot.png'
    ),
    dpi = 'retina',
    width = 6.5,
    height = 5,
    units = 'in'
  )
  
  ss_all$vac$df_c1 %>% 
    select(Period, 
           contains('hps'),
           contains('ai')) %>% 
    mutate(
      HPS_Total = hps_Total,
      HPS_SMA = hps_SMA,
      AI_Total = ai_Total,
      AI_SMA = ai_SMA,
      hps_Total = NULL,
      ai_Total = NULL,
      hps_SMA = NULL,
      ai_SMA = NULL) %>% 
    pivot_longer(
      cols= !Period,
      names_to = "M", 
      values_to = "Index"
    ) %>% 
    separate(
      M,
      into = c("Survey", "Age"),
      sep = '_'
    ) %>%
    arrange(., Period) %>%
    mutate(Period = as.factor(Period)) %>% 
    ggplot(.) +
    geom_bar(
      aes(x=Period, y=Index, fill=Survey),
      stat = 'identity',
      position = position_dodge()
    ) + theme_classic() + 
    facet_wrap(~Age) +
    labs(y = "Attitudes towards vaccination")
  
  ggsave(
    file.path(
      viz_path,
      'comparisons/surveys_vac_barplot.png'
    ),
    dpi = 'retina',
    width = 6.5,
    height = 5,
    units = 'in'
  )
  
} 

save_survey_desc_plot()

vac_period_summaries_c1 <- round(ss_all$vac$df_c1,2)
vac_period_summaries_c2 <- round(ss_all$vac$df_c2,2)

emp_period_summaries_c1 <- round(ss_all$emp$df_c1,2)
emp_period_summaries_c2 <- round(ss_all$emp$df_c2,2)

save_tab('comparisons', vac_period_summaries_c1)
save_tab('comparisons', vac_period_summaries_c2)

save_tab('comparisons', emp_period_summaries_c1)
save_tab('comparisons', emp_period_summaries_c2)



## SM + SM v Objective #####

### Correlations
cor_matrix <- function(df_list, topic){
  
  
  df_names <- names(df_list)
  
  obj_list <- list()
  cov_list <- list()
  cor_list <- list()
  
  
  for(df_name in df_names){
    
    n <- df_name #glue("{topic}_{df_name}")
    
    df <- df_list[[df_name]] %>% 
      select(-obs, -Day) %>% 
      select(-contains('m_comm'), -contains('m_score')) %>% 
      select(-contains('m_rtl'), -contains('m_re'))
    
    if(topic=='emp'){
      
      obj_list[[n]]$cor <- df %>% 
        # mutate(sp500_lag = sp500_Close - lag(sp500_Close)) %>% 
        # filter(!is.na(sp500_lag)) %>% 
        cor(.) %>% 
        as.data.frame(.) %>% 
        select(sp500_Close)
      
      obj_list[[n]]$p <- df %>% 
        # mutate(sp500_lag = sp500_Close - lag(sp500_Close)) %>% 
        # filter(!is.na(sp500_lag)) %>% 
        cor_pmat(.) %>% 
        as.data.frame(.) %>% 
        select(sp500_Close)
      
      cov_list[[n]] <- df %>% 
        select(-sp500_Close) %>%
        cov(.)
      
      cor_list[[n]] <- cov2cor(cov_list[[n]])
      
      
      
    } else if(topic=='vac'){
      
      obj_list[[n]]$cor <- df %>% 
        cor(.) %>% 
        as.data.frame(.) %>% 
        select(administered)
      
      obj_list[[n]]$p <- df %>% 
        cor_pmat(.) %>% 
        as.data.frame(.) %>% 
        select(administered)
      
      
      cov_list[[n]] <- df_list[[df_name]] %>% 
        select(-obs, -Day) %>% 
        select(-contains('m_comm'), -contains('m_score')) %>% 
        select(-contains('m_rtl'), -contains('m_re')) %>% 
        select(-contains('uptake'),-contains('administered')) %>% 
        cov(.)
      
      cor_list[[n]] <- cov2cor(cov_list[[n]])
      
    }
    
    
  }
  
  cor_measures <- list(cov_list = cov_list, 
                       cor_list = cor_list, 
                       obj_list = obj_list)
  
  return(cor_measures)
  
}

reddit_emp_cor <- cor_matrix(sm_obj$reddit$emp, 'emp')
reddit_vac_cor <- cor_matrix(sm_obj$reddit$vac, 'vac')

twitter_emp_cor <- cor_matrix(sm_obj$twitter$emp, 'emp')
twitter_vac_cor <- cor_matrix(sm_obj$twitter$vac, 'vac')

# reddit_emp_cor$Mantel <- MantelCor(reddit_emp_cor$cor_list, permutations = 10000)
# reddit_vac_cor$Mantel <- MantelCor(reddit_vac_cor$cor_list, permutations = 10000)
# 
# twitter_emp_cor$Mantel <- MantelCor(twitter_emp_cor$cor_list, permutations = 10000)
# twitter_vac_cor$Mantel <- MantelCor(twitter_vac_cor$cor_list, permutations = 10000)

# emp_cor <- list()
# emp_cor$cor_list <- c(reddit_emp_cor$cor_list, twitter_emp_cor$cor_list)
# emp_cor$Mantel <- MantelCor(emp_cor$cor_list, permutations = 10000)

reddit_cor <- list()
names(reddit_emp_cor$cor_list) <- paste0('emp_', names(reddit_emp_cor$cor_list))
names(reddit_vac_cor$cor_list) <- paste0('vac_', names(reddit_vac_cor$cor_list))
reddit_cor$cor_list <- c(reddit_emp_cor$cor_list, reddit_vac_cor$cor_list)
reddit_cor$Mantel <- MantelCor(reddit_cor$cor_list, permutation = 10000)

twitter_cor <- list()
names(twitter_emp_cor$cor_list) <- paste0('emp_', names(twitter_emp_cor$cor_list))
names(twitter_vac_cor$cor_list) <- paste0('vac_', names(twitter_vac_cor$cor_list))
twitter_cor$cor_list <- c(twitter_emp_cor$cor_list, twitter_vac_cor$cor_list)
twitter_cor$Mantel <- MantelCor(twitter_cor$cor_list, permutations = 10000)


save_mantel_plot <- function(folder, df){
  
  pname <- str_split(substitute(df), '_')[[1]][1]
  
  ggcorrplot(df$Mantel$correlations,
             p.mat = df$Mantel$probabilities,
             lab = T,
             type = 'lower') +
    theme(axis.text.y = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 14))
  
  ggsave(
    file.path(
      viz_path,
      folder,
      glue("{pname}_mantel.png")
    ),
    width = 10,
    height = 9,
    units = 'in',
    dpi = 'retina'
  )
  
}

save_mantel_plot('comparisons', reddit_cor)
save_mantel_plot('comparisons', twitter_cor)


cor_obj_reddit_subsets <- function(obj_list){
  
  rs_cor <- list()
  
  full_obj <- list()
  
  full_obj$cor <- data.frame(
    matrix(NA, 
           nrow = nrow(obj_list[[1]]$cor),
           ncol = 0
    )
  )  
  
  rownames(full_obj$cor) <- rownames(obj_list[[1]]$cor)
  
  full_obj$p <- data.frame(
    matrix(NA, 
           nrow = nrow(obj_list[[1]]$p),
           ncol = 0
    )
  )  
  
  rownames(full_obj$p) <- rownames(obj_list[[1]]$cor)
  
  
  for(n in names(obj_list)){
    
    cor_df <- obj_list[[n]]$cor 
    colnames(cor_df) <- n
    
    p_df <- obj_list[[n]]$p 
    colnames(p_df) <- n
    
    full_obj$cor[[n]] <- cbind(cor_df)
    
    full_obj$p[[n]] <- cbind(p_df)
    
  }
  
  full_obj$cor <- full_obj$cor[-nrow(full_obj$cor),]
  full_obj$p <- full_obj$p[-nrow(full_obj$p),]
  
  return(full_obj)
  
}

r <- cor_obj_reddit_subsets(reddit_emp_cor$obj_list)

ggcorrplot(
  as.matrix(r$cor),
  p.mat = as.matrix(r$p),
  lab = T
) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14))

cor_sm <- function(sm_obj, topic){
  
  sm_cor <- list()
  
  reddit_c1 <- sm_obj[['reddit']][[topic]][['c1']] %>% 
    select(-obs, -m_comm, -m_score, -contains('ics')) %>%  
    select(-contains('sp500_Close')) %>% 
    select(-contains('uptake')) %>% 
    select(-contains('administered'))
  
  reddit_c2 <- sm_obj[['reddit']][[topic]][['c2']] %>% 
    select(-obs, -m_comm, -m_score, -contains('ics')) %>%  
    select(-contains('sp500_Close')) %>% 
    select(-contains('uptake')) %>% 
    select(-contains('administered'))
  
  twitter_c1 <- sm_obj[['twitter']][[topic]][['c1']] %>% 
    select(-obs, -m_rtl, -m_re, -contains('ics'))%>% 
    select(-contains('uptake'))
  
  twitter_c2 <- sm_obj[['twitter']][[topic]][['c2']] %>% 
    select(-obs, -m_rtl, -m_re, -contains('ics'))%>% 
    select(-contains('uptake'))
  
  c1 <- reddit_c1 %>% 
    inner_join(twitter_c1, by = "Day", suffix = c('_r', '_t')) %>% 
    select(-Day)
  
  c2 <- reddit_c2 %>% 
    inner_join(twitter_c2, by = "Day", suffix = c('_r', '_t')) %>% 
    select(-Day)
  
  full_cor_c1 <- cor(c1) %>% round(., 2) %>% as.data.frame(.)
  full_pmat_c1 <- cor_pmat(c1) %>% round(., 2) %>% as.data.frame(.)
  
  full_cor_c2 <- cor(c2)%>% round(., 2) %>% as.data.frame(.)
  full_pmat_c2 <- cor_pmat(c2) %>% round(., 2) %>% as.data.frame(.)
  
  # return(full_cor_c1)
  
  sm_cor$sm$c1$cor <- full_cor_c1[-nrow(full_cor_c1),] %>% 
    select(-contains("sp500_Close")) %>% 
    select(-contains("administered"))
  sm_cor$sm$c1$pmat <- full_pmat_c1[-nrow(full_pmat_c1),]%>% 
    select(-contains("sp500_Close")) %>% 
    select(-contains("administered"))
  
  sm_cor$sm$c2$cor <- full_cor_c2[-nrow(full_cor_c2),] %>% 
    select(-contains("sp500_Close")) %>% 
    select(-contains("administered"))
  sm_cor$sm$c2$pmat <- full_pmat_c2[-nrow(full_pmat_c2),] %>% 
    select(-contains("sp500_Close")) %>% 
    select(-contains("administered"))
  
  sm_cor$obj$c1$cor <- full_cor_c1[-nrow(full_cor_c1),] %>% 
    select(contains("sp500_Close") | contains("administered"))
  sm_cor$obj$c1$pmat <- full_pmat_c1[-nrow(full_pmat_c1),] %>% 
    select(contains("sp500_Close") | contains("administered"))
  
  sm_cor$obj$c2$cor <- full_cor_c2[-nrow(full_cor_c2),] %>%
    select(contains("sp500_Close") | contains("administered"))
  sm_cor$obj$c2$pmat <- full_pmat_c2[-nrow(full_pmat_c2),] %>% 
    select(contains("sp500_Close") | contains("administered"))
  
  return(sm_cor)
  
}

all_cor <- list()
all_cor$emp <- cor_sm(sm_obj_nss, 'emp')
all_cor$vac <- cor_sm(sm_obj_nss, 'vac')


save_allcor_plot<- function(df, topic, c){
  
  n <- glue("allcor_{topic}_{c}.png")
  fpath <- file.path(viz_path, 'comparisons', n)
  
  ggcorrplot(as.matrix(df[[topic]]$sm[[c]]$cor), 
             p.mat = as.matrix(df[[topic]]$sm[[c]]$pmat),
             type = 'lower', 
             sig.level = 0.05,
             lab = T)
  
  ggsave(
    fpath,
    width = 10,
    height = 10,
    units = 'in',
    dpi = 'retina'
  )
  
}


save_allcor_plot(all_cor, 'emp', 'c1')
save_allcor_plot(all_cor, 'emp', 'c2')
save_allcor_plot(all_cor, 'vac', 'c1')
save_allcor_plot(all_cor, 'vac', 'c2')


# Objective measures

p_emp <- sm_obj$reddit$emp$c1_set1 %>% 
  mutate(
    sp500 = scales::rescale(sp500_Close, to=c(0,1))) %>% 
  select(Day, sp500)


p_emp %>%
  ggplot(.) +
  geom_line(aes(x=Day, y=sp500)) +
  theme_minimal() +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 month"),
               date_labels = "%m-%Y") +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
    )


ggsave(
  file.path(
    viz_path,
    'comparisons',
    'sp500.png'
  ),
  dpi = 'retina'
)


p_vac <- sm_obj$reddit$vac$c1 %>% 
  mutate(
    Doses = scales::rescale(administered, to=c(0,1))
  ) %>% 
  select(Day, Doses)



p_vac %>% 
  ggplot(.)+
  geom_line(aes(x=Day, y=Index, color=Doses)) +
  theme_classic()+
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 month"),
               date_labels = "%m-%Y") +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )

ggsave(
  file.path(
    viz_path,
    'comparisons',
    'doses.png'
  ),
  dpi = 'retina'
)


p <- p_emp %>% 
  full_join(p_vac) 

p %>% 
  pivot_longer(col=!Day, names_to="Measure", values_to="Index") %>% 
  ggplot(.) +
  geom_line(aes(x=Day,y=Index,color=Measure)) +
  theme_minimal()+
  scale_x_date(breaks = function(x) seq.Date(from = min(x), 
                                             to = max(x), 
                                             by = "1 month"),
               date_labels = "%m-%Y") +
  theme(
    axis.text.x=element_text(angle=60, hjust=1),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )  +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14))


ggsave(
  file.path(
    viz_path,
    'comparisons',
    'objective.png'
  ),
  dpi = 'retina'
)

mean(p_emp$sp500) #0.5848342
sd(p_emp$sp500) #0.2458853

mean(p_vac$Doses) #0.3717237
sd(p_vac$Doses) #0.215651

p %>% 
  pivot_longer(col=!Day, names_to="Measure", values_to="Index") %>% 
  ggplot(.) +
  geom_density(aes(x=Index, color=Measure)) +
  theme_classic()+
  theme(
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  ) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14))

ggsave(
  file.path(
    viz_path,
    'appendix',
    'objective_density.png'
  ),
  dpi = 'retina'
)


# Twitter and reddit against objective measures (daily aggregation)
cor_obj_all <- function(allcor, rm_last_row = F){
  
  cor <- allcor$emp$obj$c1$cor %>% 
    mutate(sp500_Close = NULL) %>% 
    mutate(sp500 = NULL)
  
  pmat <- cor
  
  topics <- names(allcor)
  
  for(topic in topics){
    
    
    for(c in c('c1', 'c2')){
      
      dfc <- allcor[[topic]]$obj[[c]]$cor
      names(dfc) <- glue("{topic}_{c}")
      
      dfp <- allcor[[topic]]$obj[[c]]$pmat
      names(dfp) <- glue("{topic}_{c}")
      
      cor <- cbind(cor, dfc)
      pmat <- cbind(pmat, dfp)
      
    }
    
  }
  
  if(rm_last_row){
    
    cor <- cor[-nrow(cor),]
    pmat <- pmat[-nrow(pmat),]
    
  }
  
  all <- list(
    cor = round(cor,2), 
    pmat = round(pmat,2))
  
  return(all)
  
  
}

all_daily <- cor_obj_all(all_cor)

# Twitter and reddit + surveys against objective measures (periodical aggregation)
all_per <- cor_obj_all(ss_all, T)


#TODO: These figures might need re-doing, as I'm not sure how visible they'll be when printed#

fpath <- file.path(
  viz_path,
  'comparisons'
)


all_daily_plot <- ggcorrplot(
  as.matrix(all_daily$cor),
  p.mat = as.matrix(all_daily$pmat),
  lab = T,
  show.legend = F
) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14))

# ggsave(
#   file.path(
#     fpath,
#     'against_objective_daily.png'),
#     width = 15,
#     height = 12,
#     units = 'in',
#     dpi = 'retina'
#   )

all_per_plot <- ggcorrplot(
  as.matrix(all_per$cor),
  p.mat = as.matrix(all_per$pmat),
  lab = T,
  show.legend = F
) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 14))

grid.arrange(all_daily_plot, all_per_plot)

ggsave(
  file.path(
    fpath,
    'against_objective_per.png'),
    width = 15,
    height = 12,
    units = 'in',
    dpi = 'retina'
  )



ggsave(
  file.path(
    fpath, 
    'against_objective.png'
  ),
  arrangeGrob(all_daily_plot, all_per_plot),
  width = 15,
  height = 12,
  units = 'in',
  dpi = 'retina'
)


### Twitter + Reddit v Objective
merge_sm <- function(obj_nss){
  
  merge_one <- function(obj_nss, topic, c){
  
  mdf <- obj_nss$reddit[[topic]][[c]] %>% 
    select(Day, obs, bing, afinn, nrc, contains('sp500') | contains('administered')) %>% 
    inner_join(
      obj_nss$twitter[[topic]][[c]] %>% 
        select(Day, bing, afinn, nrc), 
      by = 'Day', suffix = c('_r', '_t'))
  
  if(topic == 'emp'){
    
    mdf <- mdf %>% 
      mutate(sp500 = scale(sp500_Close),
             sp500_Close = NULL)
  } else if(topic == 'vac'){
    
    mdf <- mdf %>% 
      mutate(administered = scale(administered))
    
  }
  
  return(mdf)
  
  }
  
  sm <- list()
  
  sm$emp$c1 <- merge_one(obj_nss, 'emp', 'c1')
  sm$emp$c2 <- merge_one(obj_nss, 'emp', 'c2')
  
  sm$vac$c1 <- merge_one(obj_nss, 'vac', 'c1')
  sm$vac$c2 <- merge_one(obj_nss, 'vac', 'c2')
  
  return(sm)
  
}

sm_tr <- merge_sm(sm_obj_nss)

fit_tr_obj_models <- function(sm_merged) {
  models <- list()
  
  fit_one <- function(sm_merged, topic) {
    m <- list()
    
    aic_t <- list()
    aic_r <- list()
    aic_tr <- list()
    
    rsq_t <- list()
    rsq_r <- list()
    rsq_tr <- list()
    
    mse_t <- list()
    mse_r <- list()
    mse_tr <- list()
    
    
    
    for (c in c('c1', 'c2')) {
      for (score in c('bing', 'afinn', 'nrc')) {
        if (topic == 'emp') {
          dv <- 'sp500'
        } else if (topic == 'vac') {
          dv <- 'administered'
        }
        
        f_t <- as.formula(paste(dv, '~', glue("{score}_t")))
        f_r <- as.formula(paste(dv, '~', glue("{score}_r")))
        f_tr <-as.formula(paste(dv, '~', glue("{score}_t"), " + ", glue("{score}_r")))
      
        m[[c]][[score]]$t <- lm(f_t, data = sm_merged[[topic]][[c]])
        m[[c]][[score]]$r <- lm(f_r, data = sm_merged[[topic]][[c]])
        m[[c]][[score]]$tr <-lm(f_tr, data = sm_merged[[topic]][[c]])
        
        aic_t <- append(aic_t, AIC(m[[c]][[score]]$t))
        aic_r <- append(aic_r, AIC(m[[c]][[score]]$r))
        aic_tr <- append(aic_tr, AIC(m[[c]][[score]]$tr))
        
        mse_t <- append(mse_t, MSE(predict(m[[c]][[score]]$t), sm_merged[[topic]][[c]][[dv]]))
        mse_r <- append(mse_r, MSE(predict(m[[c]][[score]]$r), sm_merged[[topic]][[c]][[dv]]))
        mse_tr <- append(mse_tr, MSE(predict(m[[c]][[score]]$tr), sm_merged[[topic]][[c]][[dv]]))
        
        rsq_t <- append(rsq_t, summary(m[[c]][[score]]$t)$adj.r.squared)
        rsq_r <- append(rsq_r, summary(m[[c]][[score]]$r)$adj.r.squared)
        rsq_tr <- append(rsq_tr, summary(m[[c]][[score]]$tr)$adj.r.squared)
        
  
      }
    }
    
    m$aic_df <- data.frame(
      matrix(NA,
             nrow = 6,
             ncol = 3),
      row.names = 
        c(
          "c1_bing",
          "c1_afinn",
          "c1_nrc",
          "c2_bing",
          "c2_afinn",
          "c2_nrc"
        )
    ) %>% mutate(
      Twitter = as.numeric(aic_t), 
      Reddit = as.numeric(aic_r), 
      Both = as.numeric(aic_tr),
                 X1 = NULL, X2 = NULL, X3 = NULL) %>% 
      round(., 2)
    
    m$mse_df <- data.frame(
      matrix(NA,
             nrow = 6,
             ncol = 3),
      row.names = 
        c(
          "c1_bing",
          "c1_afinn",
          "c1_nrc",
          "c2_bing",
          "c2_afinn",
          "c2_nrc"
        )
    ) %>% mutate(
      Twitter = as.numeric(mse_t), 
      Reddit = as.numeric(mse_r), 
      Both = as.numeric(mse_tr),
      X1 = NULL, X2 = NULL, X3 = NULL) %>% 
      round(., 2)
    
    m$rsq_df <- data.frame(
      matrix(NA,
             nrow = 6,
             ncol = 4),
      row.names = 
        c(
          "c1_bing",
          "c1_afinn",
          "c1_nrc",
          "c2_bing",
          "c2_afinn",
          "c2_nrc"
        )
    ) %>% mutate(
      Twitter = as.numeric(rsq_t),
      Reddit = as.numeric(rsq_r), 
      Both = as.numeric(rsq_tr),
      X1 = NULL, X2 = NULL, X3 = NULL) %>% 
      round(., 2)
    
    
    return(m)
    
  }
  
  models$emp <- fit_one(sm_merged, topic = 'emp')
  models$vac <- fit_one(sm_merged, topic = 'vac')
  
  return(models)
  
}

tr_obj_models <- fit_tr_obj_models(sm_tr)


fit_tr_sm_obj_models_per <- function(sm_merged, agg) {
  models <- list()
  
  fit_one <- function(sm_merged, topic) {
    m <- list()
    aic <- list()
    rsq <- list()
    mse <- list()
    f <- list()
    
    n <- c('t', 'r', 'tr',
           'hps', 'ai', 
           'hps_t','hps_r', 'hps_tr',
           'ai_t', 'ai_r', 'ai_tr',
           'hps_ai_t', 'hps_ai_r', 'hps_ai_tr')
    
    for(i in c('df_c1', 'df_c2')){
      
      aic[[i]] <- list()
      rsq[[i]] <- list()
      
    }
    
    
    if (topic == 'emp') {
      dv <- 'sp500'
    } else if (topic == 'vac') {
      dv <- 'administered'
    }
    
    f$t <- as.formula(paste(dv, '~ nrc_t'))
    f$r <- as.formula(paste(dv, '~ afinn_r'))
    f$tr <-as.formula(paste(dv, '~ nrc_t + afinn_r'))
    f$hps <- as.formula(paste(dv, '~ hps_SMA'))
    f$ai <- as.formula(paste(dv, '~ ai_SMA'))
    f$hps_t <- as.formula(paste(dv, '~ hps_SMA + nrc_t'))
    f$hps_r <- as.formula(paste(dv, '~ hps_SMA + afinn_r'))
    f$hps_tr <- as.formula(paste(dv, '~ hps_SMA + nrc_t + afinn_r'))
    f$ai_t <- as.formula(paste(dv, '~ ai_SMA + nrc_t'))
    f$ai_r <- as.formula(paste(dv, '~ ai_SMA + afinn_r'))
    f$ai_tr <- as.formula(paste(dv, '~ ai_SMA + nrc_t + afinn_r'))
    f$hps_ai_t <- as.formula(paste(dv, '~ hps_SMA + ai_SMA + nrc_t'))
    f$hps_ai_r <- as.formula(paste(dv, '~ hps_SMA + ai_SMA + afinn_r'))
    f$hps_ai_tr <- as.formula(paste(dv, '~ hps_SMA + ai_SMA + nrc_t + afinn_r'))
    
    
    for (c in c('df_c1', 'df_c2')) {
      
      for(i in n){
        
        m[[c]][[i]] <- lm(f[[i]], data = sm_merged[[topic]][[c]])
        
        aic[[c]] <- append(aic[[c]], AIC(m[[c]][[i]]))
        
        rsq[[c]] <- append(rsq[[c]], summary(m[[c]][[i]])$adj.r.squared)
        
        mse[[c]] <- append(mse[[c]], MSE(predict(m[[c]][[i]]) , sm_merged[[topic]][[c]][[dv]]))
        
      }
      
    }
    
    m$aic_df <- data.frame(
      matrix(NA,
             nrow = 14,
             ncol = 2),
      row.names = 
        c(
          't', 'r', 'tr',
          'hps', 'ai', 
          'hps_t','hps_r', 'hps_tr',
          'ai_t', 'ai_r', 'ai_tr',
          'hps_ai_t', 'hps_ai_r', 'hps_ai_tr'
        )
    ) %>% mutate(
      c1 = as.numeric(aic$df_c1), 
      c2 = as.numeric(aic$df_c2), 
      X1 = NULL, X2 = NULL) %>% 
      round(., 2)
    
    m$mse_df <- data.frame(
      matrix(NA,
             nrow = 14,
             ncol = 2),
      row.names = 
        c(
          't', 'r', 'tr',
          'hps', 'ai', 
          'hps_t','hps_r', 'hps_tr',
          'ai_t', 'ai_r', 'ai_tr',
          'hps_ai_t', 'hps_ai_r', 'hps_ai_tr'
        )
    ) %>% mutate(
      c1 = as.numeric(mse$df_c1), 
      c2 = as.numeric(mse$df_c2), 
      X1 = NULL, X2 = NULL) %>% 
      round(., 2)
    
    m$rsq_df <- data.frame(
      matrix(NA,
             nrow = 14,
             ncol = 2),
      row.names = 
        c(
          't', 'r', 'tr',
          'hps', 'ai', 
          'hps_t','hps_r', 'hps_tr',
          'ai_t', 'ai_r', 'ai_tr',
          'hps_ai_t', 'hps_ai_r', 'hps_ai_tr'
        )
    ) %>% mutate(
      c1 = as.numeric(rsq$df_c1), 
      c2 = as.numeric(rsq$df_c2), 
      X1 = NULL, X2 = NULL) %>% 
      round(., 2)
    
    
    
    return(m)
    
  }
  
  models$emp <- fit_one(sm_merged, topic = 'emp')
  models$vac <- fit_one(sm_merged, topic = 'vac')
  
  return(models)
  
}

tr_obj_models_per <- fit_tr_sm_obj_models_per(ss_all)


ppmodels <- function(df, agg){
  
  
  if(agg=="daily"){
    
    emp <- df$emp$mse_df %>% 
      merge(df$emp$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
      merge(df$emp$aic_df %>% mutate(Row.names = rownames(.))) %>% 
      mutate(Twitter_aic = Twitter,
             Reddit_aic = Reddit,
             Both_aic = Both,
             Both = NULL, Twitter = NULL, Reddit = NULL)
    
    rownames(emp) <- emp$Row.names
    
    emp$Row.names <- NULL
    
    emp <- emp[c(2,5,1,4,3,6),c(1,4,7,2,5,8,3,6,9)] #c(2,6,1,4,3,5)
    
    
    vac <- df$vac$mse_df %>% 
      merge(df$vac$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
      merge(df$vac$aic_df %>% mutate(Row.names = rownames(.))) %>% 
      mutate(Twitter_aic = Twitter,
             Reddit_aic = Reddit,
             Both_aic = Both,
             Both = NULL, Twitter = NULL, Reddit = NULL)
    
    rownames(vac) <- vac$Row.names
    vac$Row.names <- NULL
    
    vac <- vac[c(2,5,1,4,3,6),c(1,4,7,2,5,8,3,6,9)]
    
    am <- emp %>% 
      merge(vac, by = 'row.names', suffixes = c('_emp', '_vac'))
    
    rownames(am) <- am$Row.names
    
    am$Row.names <- NULL
    
    
    emp$M <- rownames(emp)
  
    rownames(emp) <- NULL
    
    emp <- emp %>% 
      separate(M, into=c("Pipeline", "Score"))
    
    emp <- emp[,c(11,10,1,2,3,4,5,6,7,8,9)]
    
    vac$M <- rownames(vac)
    
    rownames(vac) <- NULL
    
    vac <- vac %>% 
      separate(M, into=c("Pipeline", "Score"))
    
    vac <- vac[,c(11,10,1,2,3,4,5,6,7,8,9)]
    
    
  } else if(agg=="per"){
    
    emp <- df$emp$mse_df %>% 
      merge(df$emp$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
      merge(df$emp$aic_df %>% mutate(Row.names = rownames(.))) %>% 
      mutate(c1_aic = c1,
             c2_aic = c2,
             c1 = NULL, c2 = NULL)
    
    rownames(emp) <- emp$Row.names
    
    emp$Row.names <- NULL
    
    emp <- emp[c(13,12,14,5,1,10,9,11,3,2,4,7,6,8), c(1,3,5,2,4,6)] #c(13,12,14,5,9,10,11,2,3,4,6,7,8)
    
    vac <- df$vac$mse_df %>% 
      merge(df$vac$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
      merge(df$vac$aic_df %>% mutate(Row.names = rownames(.))) %>% 
      mutate(c1_aic = c1,
             c2_aic = c2,
             c1 = NULL, c2 = NULL)
    
    rownames(vac) <- vac$Row.names
    
    vac$Row.names <- NULL
    
    vac <- vac[c(13,12,14,5,1,10,9,11,3,2,4,7,6,8), c(1,3,5,2,4,6)] #c(13,12,14,5,9,10,11,2,3,4,6,7,8)
    
    
    vac <- df$vac$mse_df %>% 
      merge(df$vac$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
      merge(df$vac$aic_df %>% mutate(Row.names = rownames(.))) %>% 
      mutate(c1_aic = c1,
             c2_aic = c2,
             c1 = NULL, c2 = NULL)
    
    rownames(vac) <- vac$Row.names
    
    vac$Row.names <- NULL
    
    
    am <- emp %>% 
      merge(vac, by = 'row.names', suffixes = c('_emp', '_vac'))
    
    rownames(am) <- am$Row.names
    
    am$Row.names <- NULL
    
    am <- am[c(13,12,14,5,1,10,9,11,3,2,4,7,6,8),c(1,2,3,4,5,6,7,9,11,8,10,12) ] #c(13,12,14,5,9,10,11,2,3,4,6,7,8)
    
    
  }
  
  m <- list(emp=emp, vac=vac, am=am)
  
  return(m)
  
}

daily_m <- ppmodels(tr_obj_models, 'daily')
per_m <- ppmodels(tr_obj_models_per, 'per')


saveRDS(
  daily_m,
  file.path(
    viz_path,
    'comparisons',
    'm_daily.rds'
  )
)

saveRDS(
  per_m,
  file.path(
    viz_path,
    'comparisons',
    'm_per.rds'
  )
)



##### Code Dump #######

# # Changing score + summarizing days
# r2 <- load_nlp_data(
#   source = "Reddit",
#   topic = "Employment",
#   set = 2,
#   level = 2,
#   stage = '1b'
# )
# 
# r3 <- load_nlp_data(
#   source = "Reddit",
#   topic = "Employment",
#   set = 3,
#   level = 2,
#   stage = '1b'
# )
# 


# 
# bla <- load_nlp_data(
#   source = "Reddit",
#   topic = "Employment",
#   set = 4,
#   level = 2,
#   stage = '1b'
# )
# 
# 
# bla <- bla %>%
#   dplyr::mutate(bing_score = bing_score/bing_words,
#          afinn_score = afinn_score/afinn_words,
#          nrc_score = nrc_score/nrc_words) %>%
#   dplyr::mutate(bing_score = ifelse(is.na(bing_score), 0.5,
#                              ifelse(bing_score < 0, 0,
#                                     1)),
#          afinn_score = ifelse(is.na(afinn_score), 0.5,
#                               ifelse(afinn_score < 0, 0,
#                                      1)),
#          nrc_score = ifelse(is.na(nrc_score), 0.5,
#                             ifelse(nrc_score < 0, 0,
#                                    1)))
# 
# 
# bla2 <- bla %>%
#   select(num_comments, score, date, bing_score, nrc_score) %>%
#   dplyr::mutate(Day = as.Date(date)) %>% select(-date) %>% 
#   group_by(Day) %>% 
#   dplyr::mutate(
#     comm_weight = num_comments/sum(num_comments),
#     score_weight = score/sum(score),
#     i_score = num_comments + score,
#     i_weight = i_score/sum(i_score)) %>% 
#   dplyr::mutate(
#     nc_bing = bing_score*comm_weight,
#     sc_bing = bing_score*score_weight,
#     i_bing = bing_score*i_weight,
#     nc_nrc = nrc_score*comm_weight,
#     sc_nrc = nrc_score*score_weight,
#     i_nrc = nrc_score*i_weight
#   )
# 
# bla3 <- bla2 %>% 
#   group_by(Day) %>%
#   dplyr::summarise(
#     bing = mean(bing_score),
#     nc_bing = sum(nc_bing),
#     sc_bing = sum(sc_bing),
#     i_bing = sum(i_bing),
#     nrc = mean(nrc_score),
#     nc_nrc = sum(nc_nrc),
#     sc_nrc = sum(sc_nrc),
#     i_nrc = sum(i_nrc)
#   )
# 
# 
# b <- bla2 %>% filter(Day == ymd("2021-02-23")) 
# 
# 
# t <- load_nlp_data(
#   source = "Twitter",
#   topic = "Employment",
#   set = 1,
#   level = 2,
#   stage = '1b'
# )
# 
# t <- t %>% 
#   dplyr::mutate(bing_score = bing_score/bing_words,
#          afinn_score = afinn_score/afinn_words,
#          nrc_score = nrc_score/nrc_words) %>%
#   dplyr::mutate(bing_score = ifelse(is.na(bing_score), 0.5,
#                              ifelse(bing_score < 0, 0,
#                                     1)),
#          afinn_score = ifelse(is.na(afinn_score), 0.5,
#                               ifelse(afinn_score < 0, 0,
#                                      1)),
#          nrc_score = ifelse(is.na(nrc_score), 0.5,
#                             ifelse(nrc_score < 0, 0,
#                                    1)))
# 
# 
# t2 <- t %>% 
#   select(-id, -text) %>% 
#   dplyr::mutate(Day = as.Date(date)) %>% 
#   group_by(Day) %>% 
#   dplyr::mutate(
#     like_weight = like_count/sum(like_count),
#     reply_weight = reply_count/sum(reply_count),
#     rt_score = retweet_count + quote_count,
#     i_score = retweet_count + quote_count + like_count,
#     rt_weight = rt_score/sum(rt_score),
#     i_weight = i_score/sum(i_score),
#   ) %>% 
#   dplyr::mutate(
#     l_bing = bing_score*like_weight,
#     l_afinn = afinn_score*like_weight,
#     l_nrc = nrc_score*like_weight,
#     re_bing = bing_score*reply_weight,
#     re_afinn = afinn_score*reply_weight,
#     re_nrc = nrc_score*reply_weight,
#     rt_bing = bing_score*rt_weight,
#     rt_afinn = afinn_score*rt_weight,
#     rt_nrc = nrc_score*rt_weight,
#     i_bing = bing_score*i_weight,
#     i_afinn = afinn_score*i_weight,
#     i_nrc = nrc_score*i_weight
#   )
# 
# t3 <- t2 %>% 
#   group_by(Day) %>% 
#   dplyr::summarise(
#     bing = mean(bing_score),
#     l_bing = sum(l_bing),
#     l_afinn = sum(l_afinn),
#     l_nrc = sum(l_nrc),
#     re_bing = sum(re_bing),
#     re_afinn = sum(re_afinn),
#     re_nrc = sum(re_nrc),
#     rt_bing = sum(rt_bing),
#     rt_afinn = sum(rt_afinn),
#     rt_nrc = sum(rt_nrc),
#     i_bing = sum(i_bing),
#     i_afinn = sum(i_afinn),
#     i_nrc = sum(i_nrc)
#   )

# # Combining surveys with sm data and objective measures.
# # e.g.
# eg_reddit$emp_obj_surveys_hps$df 
# 
# x <- sm$reddit$emp$c1b_set4 %>% 
#   inner_join(sp$hps, by = "Day") %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   inner_join(surveys$hps$emp, by = "Period") %>% 
#   inner_join(surveys$ai$emp, by = "Period")
# 
# 
# y <- surveys$ai$emp %>% inner_join(sp$hps, by = "Period") %>% select(-Day, -ai_Period)
# 
# z <- x %>% inner_join(y, by = "Period")
# 
# View(eg_reddit$emp_obj_surveys_hps$df)
# 
# 
# eg_reddit$emp_obj_surveys_ai$df <- sm$reddit$emp$c1b_set1 %>% 
#   inner_join(sp$ai, by = "Day") %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   inner_join(surveys$ai$emp, by = "ai_Period")
# 
# eg_twitter$emp_obj_surveys_hps$df <- sm$twitter$emp$c1b %>% 
#   inner_join(sp$hps, by = "Day") %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   inner_join(surveys$hps$emp, by = "Period")
# 
# 
# eg_twitter$emp_obj_surveys_ai$df <- sm$twitter$emp$c2 %>% 
#   inner_join(sp$ai, by = "Day") %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   inner_join(surveys$ai$emp, by = "ai_Period")
# 
# View(eg_reddit$emp_obj_surveys_hps$df)
# View(eg_reddit$emp_obj_surveys_ai$df)
# 
# View(eg_twitter$emp_obj_surveys_hps$df)
# View(eg_twitter$emp_obj_surveys_ai$df)
# 
# bla <- sm$reddit$emp$c1b$set1 %>% 
#   inner_join(sp$ai, by = "Day") %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   inner_join(surveys$hps$emp, by = "Period") 
# 
# 
# 
# bla2 <- bla %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     sp500 = mean(sp500_Close),
#     ics = mean(ics),
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs))
#   )
# 
# 
# bla3 <- bla %>% 
#   group_by(ai_Period) %>% 
#   dplyr::summarise(
#     sp500 = mean(sp500_Close),
#     ics = mean(ics),
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs))
#   )
# 
# 
# View(eg_reddit$emp_obj_surveys_ai$df)
# 
# eg_reddit$emp_obj_surveys$df <- eg_reddit$emp_obj_surveys$df %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     obs = sum(obs),
#     sp500 = mean(sp500_Close),
#     ics = mean(ics),
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs))
#   )
# 
# View(eg_reddit$emp_obj_surveys_hps$df)
# 
# 
# # Other
# t <- sm$reddit$emp$c1b$set1 %>% 
#   inner_join(sp$ai, by = "Day") %>% 
#   dplyr::mutate(Period = as.character(Period),
#          obs = as.character(obs)) %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     across(where(is.numeric), mean)
#   ) %>% 
#   dplyr::mutate(Period = as.numeric(Period)) %>% 
#   inner_join(
#     surveys$ai$emp, 
#     by = "Period") 
# 
# 
# 
# t <- objective$sp500 %>% 
#   arrange(., Day) %>% 
#   dplyr::mutate(r_sp500 = sp500_Close - lag(sp500_Close),
#          r_sp500 = scale(r_sp500))
# 
# 
# t %>% 
#   ggplot(., aes(x = Day,y = r)) + 
#   geom_line()
# 
# 
# bla <- sm$reddit$emp$c1b$set1 %>% 
#   select(Day, bing) %>% 
#   arrange(., Day) %>% 
#   dplyr::mutate(r_bing = bing - lag(bing),
#          r_bing = scale(r_bing))
# 
# 
# 
# blat <- bla %>% 
#   inner_join(t, by = "Day") %>% 
#   filter(!is.na(r_sp500)) 
# 
# 
# 
# blat %>% 
#   select(Day, r_bing, r_sp500) %>% 
#   pivot_longer(
#     ., 
#     cols = contains("r"),
#     names_to = "Measure",
#     values_to = "Index"
#   ) %>% 
#   ggplot(., aes(x = Day, y = Index, color = Measure)) +
#   geom_line()
# 
# 
# cor(blat$bing, blat$sp500_Close)
# 
# cor(blat$r_bing, blat$r_sp500)
# 
# mse(blat$bing, blat$sp500_Close %>% scales::rescale())
# 
# mse(blat$r_bing, blat$r_sp500)

# # Combining objective measures with sm data.
# # e.g.
# eg_reddit <- list()
# eg_twitter <- list()
# 
# eg_reddit$emp_obj_df <- sm$reddit$emp_c1b_set1 %>%
#   full_join(objective$emp, by = "Day") %>%
#   filter(Day > ymd(start_date))
# 
# eg_reddit$emp_obj$df2 <-sm$reddit$emp_c1b_set1 %>% 
#   comb_sm_obj(.)
# 
# 
# eg_reddit$vac_obj$df <- sm$reddit$vac_c1b %>%
#   inner_join(objective$vac, by = "Day") %>%
#   filter()
# 
# eg_twitter$emp_obj$df <- sm$twitter$emp_c1b %>% 
#   inner_join(objective$emp, by = "Day") %>% 
#   filter(Day > ymd(start_date))
# 
# eg_twitter$vac_obj$df <- sm$twitter$vac_c1b %>% 
#   inner_join(objective$vac, by = "Day") %>% 
#   filter(Day > ymd(start_date))
# 
# View(eg_twitter$vac_obj$df)
# 
# # Transformation (i.e. pivoting) for plotting.
# eg_reddit$emp_obj$toplot <- eg_reddit$emp_obj$df %>% 
#   select(Day, contains("nrc"), obs) %>% #bing, nc_bing, sc_bing, nc_sc_bing, obs) %>%
#   pivot_longer(.,
#                cols = contains("nrc"),
#                names_to = "Metric",
#                values_to = "Index")

# eg_reddit$emp_obj$toplot %>% 
#   ggplot(.) +
#   geom_line(aes(x = Day, y = Index, color = Metric))

# Rolling average
# geom_line(aes(x=Day, y=rollmean(bing, 5)))
# 
# lines(rollmean(m2$bing, 5))
# 
# 
# geom_line(aes(x=Day, y = frollmean(bing, 7, align="left")), color='red') +
# theme_classic()
# 
# 
# ggplot(., aes(x = Day)) +
# geom_line(aes(y = bing)) + 
# geom_line(aes(y = nc_bing))

# # Combining surveys with sm data.
# eg_reddit$emp_surveys$df <- sm$reddit$emp_c1b_set1 %>% 
#   inner_join(sp$hps, by = "Day")
# 
# 
# eg_reddit$emp_surveys$df <- eg$emp_surveys$df %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     across(bing:i_nrc,
#            ~sum(.x*obs)/sum(obs)),
#     obs = sum(obs)
#   )
#   select(Day, r_bing, r_sp500) %>% 
#   pivot_longer(
#     ., 
#     cols = contains("r"),
#     names_to = "Measure",
#     values_to = "Index"
#   ) %>% 






# bla2 <- bla %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     sp500 = mean(sp500_Close),
#     ics = mean(ics),
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs))
#   )
# 
# n_obs <- function(sm_obj){
#   
#   source_name <- names(sm_obj)
#   reddit_df <- data.frame(
#     matrix(NA,
#            nrow = 7,
#            ncol = 0)
#   )
#   
#   twitter_df <- data.frame(
#     matrix(NA,
#            nrow = 7,
#            ncol = 0)
#   )
#   
#   for(sn in source_name){
#     
#     
#     topic_names <- names(sm_obj[[sn]])
#     
#     for(tn in topic_names){
#       
#       d_names <- names(sm_obj[[sn]][[tn]])
#       
#       for(dn in d_names){
#         
#         tvar <- summary(sm_obj[[sn]][[tn]][[dn]]$obs)
#         
#         vn <- glue("{tn}_{dn}")
#         
#         tdf <- data.frame(
#           temp_col = round(unclass(tvar),2),
#           check.names = F
#         )
#         
#         
#         tdf <- rbind(
#           Days = length(unique(sm_obj[[sn]][[tn]][[dn]]$Day)),
#           tdf)
#         
#         
#         # add_row(tdf, length(unique(sm_obj[[sn]][[tn]][[dn]]$Day)))
#         
#         names(tdf) <- vn
#         
#         if(sn=='reddit'){
#           reddit_df <- cbind(reddit_df, tdf)
#         } else if(sn=='twitter'){
#           twitter_df <- cbind(twitter_df, tdf) 
#         }
#         
#       }
#       
#     }
#     
#   }
#   
#   return(list(reddit = reddit_df, twitter = twitter_df))
#   
#   
# }
# 
# n_all <- n_obs(sm_obj)
# n_all_trim <- n_obs(sm_obj_trim)
# 
# pptable(n_all$reddit)
# formattable(n_all$twitter)
# formattable(n_all_trim$reddit)
# formattable(n_all_trim$twitter)
# 


## WIP ####
# 
# sm_obj$reddit$vac$c1 %>% 
#   select(Day,obs, bing) %>%
#   dplyr::mutate(obs = scales::rescale(obs, to=c(0,1))) %>% 
#   ggplot(.) +
#   geom_line(aes(x = Day, y = obs, color = "Observations")) +
#   geom_line(aes(x = Day, y = bing, color = "Bing_score")) 
# 
# 
# cor(sm_obj$reddit$vac$c1$bing, sm_obj$reddit$vac$c1$obs)
# 
# 
# sm_obj$reddit$vac$c1 %>% 
#   filter(Day > ymd('2021-02-07')) %>% 
#   filter(Day < ymd('2021-02-22')) %>% 
#   select(Day,uptake) %>% 
#   ggplot(., aes(x=Day, y=uptake)) +
#   geom_line()
# 
# 
# h <- sm_all$reddit$hps$vac$c1 %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs)),
#     across(
#       contains('hps'),
#       ~mean(.x)
#     ),
#     uptake = mean(uptake))
# 
# 
# cor(h$`hps_s2_18-54`, h$uptake)
# 
# h %>% 
#   select(Period,
#          contains('hps'),
#          uptake) %>% 
#   cor(.)
# 
# 
# 
# pivot_longer(
#   .,
#   cols = !Period,
#   names_to = "Measure",
#   values_to = "Index"
# ) %>% 
#   ggplot(.) + 
#   geom_bar(
#     aes(x=Period, y=Index, fill=Measure),
#     stat='identity',
#     position=position_dodge()
#   )
# 
# 
# ggplot(.) +
#   geom_bar(aes(x=Period, y=uptake, color = "Uptake"),
#            stat = 'identity',
#            position = position_dodge()) +
#   geom_bar(aes(x=Period, y=`hps_s2_18-54`, color = "s2 (18-54)"), stat = 'identity',
#            position = position_dodge()) +
#   geom_bar(aes(x=Period, y=`hps_s1_18-54`, color = "s1 (18-54)"), stat = 'identity',
#            position = position_dodge()) +
#   geom_bar(aes(x=Period, y=`hps_s2_Total`, color = "s2 (Total)"), stat = 'identity',
#            position = position_dodge()) +
#   geom_bar(aes(x=Period, y=`hps_s1_Total`, color = "s1 (Total)"), stat = 'identity',
#            position = position_dodge()) 
# 
# #     across(
# #       bing:i_nrc,
# #       ~sum(.x*obs)/sum(obs))
# 
# 
# c %>% 
#   dplyr::mutate(admin_lag = scales::rescale(admin_lag, to = c(0,1))) %>% 
#   filter(dow != 6) %>%
#   filter(dow != 7) %>%
#   ggplot(.) + 
#   geom_line(aes(x=Day, y=admin_lag))
# 
# 
# # Comparison of trends across periods.
# View(sm_all$reddit$hps_ai$emp$c1_set1)
# 
# t <- sm_all$reddit$hps_ai$emp$c1_set1
# 
# t2 <- t %>% 
#   dplyr::mutate(Period = as.factor(Period)) %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     sp500 = mean(sp500_Close),
#     ics = mean(ics),
#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs)
#     ),
#     across(
#       c("hps_18-54", "hps_Total"),
#       ~mean(.x)
#     ),
#     across(
#       c("ai_18-49", "ai_Total"),
#       ~mean(.x)
#     )
#   )
# 
# 
# # Reddit: Bing | AFINN | NRC
# 
# r_lex <- t2 %>% 
#   select(Period, contains("bing"), contains("afinn")) %>% #contains('nrc')
#   pivot_longer(
#     .,
#     cols = !Period,
#     names_to = "Measure",
#     values_to = "Index"
#   ) %>% 
#   separate(
#     Measure,
#     into = c("Weighting", "Measure"),
#     fill = "left"
#   ) %>% 
#   dplyr::mutate_at(vars(Weighting), ~replace_na(., 'none')) %>% 
#   dplyr::mutate(Weighting = ifelse(Weighting == 'nc', 'comments',
#                                    ifelse(Weighting == 'sc', 'score',
#                                           ifelse(Weighting == 'i', 'all_interactions', 'none'))))
# 
# r_lex %>% 
#   ggplot(.) +
#   geom_bar(
#     aes(x=Period, y=Index, fill=Measure),
#     stat = "identity",
#     position = position_dodge(),
#     alpha = 0.8
#   ) + theme_classic() +
#   geom_line(
#     aes(x=as.numeric(Period), y=Index, color=Measure)
#   ) +
#   facet_wrap(~Weighting)
# 
# 
# 
# # AI vs HPS
# t2 %>% 
#   select(Period, contains("ai_"), contains("hps_")) %>% 
#   pivot_longer(
#     .,
#     cols = !Period,
#     names_to = "Measure",
#     values_to = "Index"
#   ) %>% 
#   ggplot(.) +
#   geom_bar(
#     aes(x = Period, y = Index, fill = Measure),
#     stat = "identity",
#     position = position_dodge(),
#     alpha = 0.7
#   ) +
#   geom_line(aes(x = as.numeric(Period), y = Index, color = Measure)) +
#   theme_classic()
# 
# 
# # Within measures
# 
# t2 %>% 
#   # select(Period, contains("ai_"), contains("hps_")) %>% 
#   pivot_longer(
#     .,
#     cols = !Period,
#     names_to = "Measure",
#     values_to = "Index"
#   ) %>% 
#   ggplot(.) + 
#   geom_bar(aes(x = Period, 
#                y = Index, 
#                fill = Measure),
#            stat = "identity",
#            position=position_dodge())
# 
# 
# 
# 
# t2 <- t %>% 
#   group_by(Period) %>% 
#   dplyr::summarize(
#     hps = mean(`hps_s1_18-54`), ai = mean(`ai_18-49`),admin = mean(administered),
#     bing = mean(bing)
#   )
# 
# emp <- tr_obj_models_per$emp$mse_df %>% 
#   merge(tr_obj_models_per$emp$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
#   merge(tr_obj_models_per$emp$aic_df %>% mutate(Row.names = rownames(.))) %>% 
#   mutate(c1_aic = c1,
#          c2_aic = c2,
#          c1 = NULL, c2 = NULL)
# 
# rownames(emp) <- emp$Row.names
# 
# emp$Row.names <- NULL
# 
# emp <- emp[, c(1,3,5,2,4,6)]
# 
# vac <- tr_obj_models_per$vac$mse_df %>% 
#   merge(tr_obj_models_per$vac$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
#   merge(tr_obj_models_per$vac$aic_df %>% mutate(Row.names = rownames(.))) %>% 
#   mutate(c1_aic = c1,
#          c2_aic = c2,
#          c1 = NULL, c2 = NULL)
# 
# rownames(vac) <- vac$Row.names
# 
# vac$Row.names <- NULL
# 
# vac <- vac[, c(1,3,5,2,4,6)]
# 
# 
# 
# vac <- tr_obj_models$vac$mse_df %>% 
#   merge(tr_obj_models$vac$rsq_df, by = "row.names", suffixes = c('_mse', '_rsq')) %>% 
#   merge(tr_obj_models$vac$aic_df %>% mutate(Row.names = rownames(.))) %>% 
#   mutate(Twitter_aic = Twitter,
#          Reddit_aic = Reddit,
#          Both_aic = Both,
#          Both = NULL, Twitter = NULL, Reddit = NULL)
# 
# rownames(vac) <- vac$Row.names
# 
# vac$Row.names <- NULL
# 
# 
# am <- emp %>% 
#   merge(vac, by = 'row.names', suffixes = c('_emp', '_vac'))
# 
# rownames(am) <- am$Row.names
# 
# am$Row.names <- NULL
# 
