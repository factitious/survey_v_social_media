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
setwd(root_dir)

start_date <- '2020-10-22'
vac_start_date <- '2021-01-07'
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
    mutate(Day = mdy(Date),
    ) %>% 
    filter(Day > ymd(start_date)) %>% 
    filter(Day < ymd(end_date)) %>% 
    select(Day, Close.Last) %>% 
    mutate(sp500_Close = Close.Last) %>% 
    select(-Close.Last)
  
  # UoM Index of Consumer Sentiment from http://www.sca.isr.umich.edu/tables.html
  # Downloaded at:
  objective$raw$ics <- read.csv('http://www.sca.isr.umich.edu/files/tbcics.csv',
                                skip = 3, 
                                header = TRUE) %>% 
    Filter(function(x)!all(is.na(x)), .) %>% 
    mutate(Day = glue('{X} {DATE.OF.SURVEY}'),
           Index = `INDEX.OF.CONSUMER.SENTIMENT`) %>% 
    select(Day, Index) %>% 
    slice(2:14) %>% 
    mutate(Day = ym(Day)) %>% 
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
    mutate(Day = mdy(Date)) %>% 
    filter(
      Day < ymd(end_date)
    ) %>% 
    group_by(Day) %>% 
    dplyr::summarise(
      uptake = sum(Administered)/sum(Distributed),
      administered = sum(Administered)
    )
  
  return(objective)
  
}

### Survey data

# Survey periods
load_sp <- function(){
  
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
    mutate(ai_start = A_I_start_date,
           ai_end = A_I_end_date,
           hps_start = HPS_start_date,
           hps_end = HPS_end_date) %>% 
    select(Period, ai_start, ai_end,
           hps_start, hps_end) %>% 
    mutate(Period = 
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
    mutate(
      d = sp$proc %>% select(Period) %>% duplicated(.)) %>% 
    mutate(
      Period = ifelse(d == T, glue("{Period}b"), Period)) %>% 
    select(Period, ai_start, ai_end) %>% 
    group_by(Period) %>% 
    dplyr::summarise(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(ai_start, tz = "UTC"),
        as.POSIXct(ai_end),
        by = "day"))
    ) %>% 
    mutate(Period = 
             as.numeric(
               str_extract(
                 Period, "[[:digit:]]+")
             )) %>% arrange(., Period)
  
  ai <- sp$proc %>% 
    mutate(ai_Period = (1:nrow(.))) %>% 
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
      
      # Changed period after data was processed.
      # Changing the second P12 to P13 manually here
      # instead of re-running the pre-processing of the survey data.
      surveys$ai$emp$Period[nrow(surveys$ai$emp)] <- 13
      surveys$ai$vac$Period[nrow(surveys$ai$vac)] <- 13
      
      surveys$hps$emp <- readRDS(
        file.path(
          root_dir, 
          hps_data_path,
          'emp.rds'
        )
      ) %>% 
        mutate(Period = 
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
        mutate(Period = 
                 as.numeric(
                   str_extract(
                     Period, "[[:digit:]]+"
                   )
                 )
        )
      
      setnames(
        hps_vac_s1,
        old = c("18-54", "Total"),
        new = c("hps_s1_18-54", "hps_s1_Total")
      )
      
      hps_vac_s2 <- readRDS(
        file.path(
          root_dir, 
          hps_data_path,
          'vac_s2.rds'
        )
      ) %>% 
        mutate(Period = 
                 as.numeric(
                   str_extract(
                     Period, "[[:digit:]]+"
                   )
                 )
        )
      
      setnames(
        hps_vac_s2,
        old = c("18-54", "Total"),
        new = c("hps_s2_18-54", "hps_s2_Total")
      )
      
      
      surveys$hps$vac <- hps_vac_s1 %>% 
        inner_join(hps_vac_s2, by = "Period") %>% 
        filter(Period >= 5)
        

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
    mutate(bing_score = bing_score/bing_words,
           afinn_score = afinn_score/afinn_words,
           nrc_score = nrc_score/nrc_words) %>% 
    mutate(bing_score = ifelse(is.na(bing_score), 0.5,
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
      mutate(Day = as.Date(date)) %>% select(-date) %>% 
      group_by(Day) %>% 
      mutate(
        comm_weight = num_comments/sum(num_comments),
        score_weight = score/sum(score),
        i_score = num_comments + score,
        i_weight = i_score/sum(i_score)
      ) %>% 
      mutate(
        nc_bing = bing_score*comm_weight,
        nc_afinn = afinn_score*comm_weight,
        nc_nrc = nrc_score*comm_weight,
        sc_bing = bing_score*score_weight,
        sc_afinn = afinn_score*score_weight,
        sc_nrc = nrc_score*score_weight,
        i_bing = bing_score*i_weight,
        i_afinn = afinn_score*i_weight,
        i_nrc = nrc_score*i_weight
      )
  
  } else if(source == "Twitter"){
    
    df <- df %>% 
      mutate(Day = as.Date(date)) %>% 
      group_by(Day) %>% 
      mutate(
        like_weight = like_count/sum(like_count),
        reply_weight = reply_count/sum(reply_count),
        rt_score = retweet_count + quote_count,
        i_score = retweet_count + quote_count + like_count,
        rt_weight = rt_score/sum(rt_score),
        i_weight = i_score/sum(i_score),
      ) %>% 
      mutate(
        l_bing = bing_score*like_weight,
        l_afinn = afinn_score*like_weight,
        l_nrc = nrc_score*like_weight,
        re_bing = bing_score*reply_weight,
        re_afinn = afinn_score*reply_weight,
        re_nrc = nrc_score*reply_weight,
        rt_bing = bing_score*rt_weight,
        rt_afinn = afinn_score*rt_weight,
        rt_nrc = nrc_score*rt_weight,
        i_bing = bing_score*i_weight,
        i_afinn = afinn_score*i_weight,
        i_nrc = nrc_score*i_weight
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
          bing = mean(bing_score),
          nc_bing = sum(nc_bing),
          sc_bing = sum(sc_bing),
          i_bing = sum(i_bing),
          afinn = mean(afinn_score),
          nc_afinn = sum(nc_afinn),
          sc_afinn = sum(sc_afinn),
          i_afinn = sum(i_afinn),
          nrc = mean(nrc_score),
          nc_nrc = sum(nc_nrc),
          sc_nrc = sum(sc_nrc),
          i_nrc = sum(i_nrc)
        )
  
  } else if(source == "Twitter"){
    
      df <- df %>% 
        group_by(Day) %>% 
        dplyr::summarise(
          obs = n(),
          bing = mean(bing_score),
          l_bing = sum(l_bing),
          l_afinn = sum(l_afinn),
          l_nrc = sum(l_nrc),
          re_bing = sum(re_bing),
          re_afinn = sum(re_afinn),
          re_nrc = sum(re_nrc),
          rt_bing = sum(rt_bing),
          rt_afinn = sum(rt_afinn),
          rt_nrc = sum(rt_nrc),
          i_bing = sum(i_bing),
          i_afinn = sum(i_afinn),
          i_nrc = sum(i_nrc)
        )
        
    
  }
      
  return(df)
}

load_all_reddit <- function(proc = F, summ = T){
  
  if(proc == F){
  
      reddit <- list()
      
      # Employment
      # Set 1
      reddit$emp$c1_set1 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '1b'
      ) 
      
      reddit$emp$c2_set1 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '2'
      )       
    
      
      # Set 2
      reddit$emp$c1_set2 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 2,
        level = 2,
        stage = '1b'
      )
      
      reddit$emp$c2_set2 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 2,
        level = 2,
        stage = '2'
      )
      
      
      # Set 3
      reddit$emp$c1_set3 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 3,
        level = 2,
        stage = '1b'
      ) 
      
      reddit$emp$c2_set3 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 3,
        level = 2,
        stage = '2'
      ) 
      
      # Set 4
      reddit$emp$c1_set4 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 4,
        level = 2,
        stage = '1b'
      ) 
      
      reddit$emp$c2_set4 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 4,
        level = 2,
        stage = '2'
      ) 

      # Vaccination
      reddit$vac$c1 <- load_nlp_data(
        source = "Reddit",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '1b'
      ) 
      
      # Vaccination
      reddit$vac$c2 <- load_nlp_data(
        source = "Reddit",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '2'
      ) 
      
      
      if(summ){
        
        reddit$emp$c1_set1 <- reddit$emp$c1_set1%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c1_set2 <- reddit$emp$c1_set2%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c1_set3 <- reddit$emp$c1_set3%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c1_set4 <- reddit$emp$c1_set4%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c2_set1 <- reddit$emp$c2_set1%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c2_set2 <- reddit$emp$c2_set2%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$emp$c2_set3 <- reddit$emp$c2_set3%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        
        reddit$emp$c2_set4 <- reddit$emp$c2_set4%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$vac$c1 <- reddit$vac$c1%>% 
          change_scores(., source = "Reddit") %>% 
          summarize_days(., source = "Reddit")
        
        reddit$vac$c2 <- reddit$vac$c2%>% 
          change_scores(., source = "Reddit") %>% 
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

load_all_twitter <- function(proc = F, summ = T){
  
  if(proc == F){
      
      twitter <- list()
      
      twitter$emp$c1 <- load_nlp_data(
        source = "Twitter",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '1b'
      ) 

      twitter$emp$c2 <- load_nlp_data(
        source = "Twitter",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '2'
      )
      
      twitter$vac$c1 <- load_nlp_data(
        source = "Twitter",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '1b'
      ) 
      
      twitter$vac$c2 <- load_nlp_data(
        source = "Twitter",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '2'
      ) 
      
      if(summ){
        
        twitter$emp$c1 <- twitter$emp$c1 %>% 
          change_scores(., source = "Twitter") %>% 
          summarize_days(., source = "Twitter")
        
        twitter$emp$c2 <- twitter$emp$c2 %>% 
          change_scores(., source = "Twitter") %>% 
          summarize_days(., source = "Twitter")
        
        twitter$vac$c1 <- twitter$vac$c1 %>% 
          mutate(like_count = as.numeric(like_count)) %>% 
          change_scores(., source = "Twitter") %>% 
          summarize_days(., source = "Twitter")
        
        twitter$vac$c2 <- twitter$vac$c2 %>% 
          mutate(like_count = as.numeric(like_count)) %>% 
          change_scores(., source = "Twitter") %>% 
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
    mutate(Day = ts) %>%  
    select(Day)
  
  objective_df$emp <- ts %>% 
    full_join(objective$raw$sp500, by = "Day") %>% 
    full_join(objective$raw$ics, by = "Day") %>% 
    mutate(ics = Index) %>% 
    select(-Index) %>% 
    arrange(., Day) %>% 
    fill(ics) %>% 
    fill(sp500_Close) %>% 
    filter(Day > ymd('2020-10-22'))
  
  objective_df$vac <- objective$raw$cdc_vac %>% 
    left_join(ts, by = "Day")
  
  return(objective_df)
}

# Combining objective measures with sm data.
c_sm_obj <- function(df, do_trim = F){
  
  clean_func <- function(df, topic, do_trim){
    
    cdf <- df %>% 
      full_join(objective[[topic]], by = "Day") %>%
      filter(Day > ymd(start_date)) %>% 
      filter(!is.na(bing)) 
    
    if(topic == 'vac'){
      
      cdf <- cdf %>%
        filter(!is.na(uptake))
        # filter(Day > ymd(vac_start_date))
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
  
# Combining surveys with sm data and objective measures.
c_all <- function(sp_df = sp, objective_df = objective,survey_df = surveys){
  
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
    #   mutate(
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
        mutate(
          Period = Period.x,
          Period.x = NULL,
          Period.y = NULL)
    } else if(ss == 'hps_ai'){
      
      cdf <- df %>% 
        inner_join(sp$hps, by = "Day") %>% 
        inner_join(objective[[topic]], by = "Day") %>% 
        inner_join(surveys$hps[[topic]], by = "Period") %>% 
        inner_join(surveys$ai[[topic]], by = "Period") %>% 
        select(-Day)
      
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
sp <- load_sp()

# Surveys
surveys <- load_surveys()

# Social media data
sm <- list()
sm_full <- list()
sm$reddit <- load_all_reddit(proc = T)
sm$twitter <- load_all_twitter(proc = T)

sm_full$reddit <- load_all_reddit(proc = F, summ = F)
sm_full$twitter <- load_all_twitter(proc = F, summ = F)

# Pre-process
objective <- c_objective(objective)
sm_obj <- c_sm_obj()
sm_obj_trim <- c_sm_obj(do_trim=T)
sm_all <- c_all()


## Descriptives #######

# Comparison of trends across periods.
View(sm_all$reddit$hps_ai$emp$c1_set1)

t <- sm_all$reddit$hps_ai$emp$c1_set1

t2 <- t %>% 
  mutate(Period = as.factor(Period)) %>% 
  group_by(Period) %>% 
  dplyr::summarise(
    sp500 = mean(sp500_Close),
    ics = mean(ics),
    across(
      bing:i_nrc,
      ~sum(.x*obs)/sum(obs)
    ),
    across(
      c("hps_18-54", "hps_Total"),
      ~mean(.x)
    ),
    across(
      c("ai_18-49", "ai_Total"),
      ~mean(.x)
    )
  )

 
# Reddit: Bing | AFINN | NRC

r_lex <- t2 %>% 
  select(Period, contains("bing"), contains("afinn")) %>% #contains('nrc')
  pivot_longer(
    .,
    cols = !Period,
    names_to = "Measure",
    values_to = "Index"
  ) %>% 
  separate(
    Measure,
    into = c("Weighting", "Measure"),
    fill = "left"
  ) %>% 
  mutate_at(vars(Weighting), ~replace_na(., 'none')) %>% 
  mutate(Weighting = ifelse(Weighting == 'nc', 'comments',
                            ifelse(Weighting == 'sc', 'score',
                                   ifelse(Weighting == 'i', 'all_interactions', 'none'))))

r_lex %>% 
  ggplot(.) +
  geom_bar(
    aes(x=Period, y=Index, fill=Measure),
    stat = "identity",
    position = position_dodge(),
    alpha = 0.8
  ) + theme_classic() +
  geom_line(
    aes(x=as.numeric(Period), y=Index, color=Measure)
  ) +
  facet_wrap(~Weighting)
  


# AI vs HPS
t2 %>% 
  select(Period, contains("ai_"), contains("hps_")) %>% 
  pivot_longer(
    .,
    cols = !Period,
    names_to = "Measure",
    values_to = "Index"
  ) %>% 
  ggplot(.) +
  geom_bar(
    aes(x = Period, y = Index, fill = Measure),
    stat = "identity",
    position = position_dodge(),
    alpha = 0.7
  ) +
  geom_line(aes(x = as.numeric(Period), y = Index, color = Measure)) +
  theme_classic()
 
 
# Within measures

t2 %>% 
  # select(Period, contains("ai_"), contains("hps_")) %>% 
  pivot_longer(
    .,
    cols = !Period,
    names_to = "Measure",
    values_to = "Index"
  ) %>% 
  ggplot(.) + 
  geom_bar(aes(x = Period, 
               y = Index, 
               fill = Measure),
           stat = "identity",
           position=position_dodge())




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



fdf <- data.frame(
  matrix(NA,
         nrow = 12,
         ncol = 0),
  row.names = 
    c(
      'n',
      'Subreddits',
      'Comments (Mean)',
      'Comments (% == 0)',
      'Score (Mean)',
      'Score (% == 0)',
      'BING score (Mean)',
      'BING score (% == 0)',
      'AFINN score (Mean)',
      'AFINN score (% == 0)',
      'NRC score (Mean)',
      'NRC score (% == 0)'
    )
)


## Analysis #####

# Summary of number of observations in all datasets (after pre-processing)
summ_full <- function(sm_full){
  
  source_name <- names(sm_obj)
  
  reddit_df <- data.frame(
    matrix(NA,
           nrow = 12,
           ncol = 0),
    row.names = 
      c(
        'n',
        'Subreddits',
        'Comments (Mean)',
        'Comments (% == 0)',
        'Score (Mean)',
        'Score (% == 0)',
        'BING score (Mean)',
        'BING score (% == 0)',
        'AFINN score (Mean)',
        'AFINN score (% == 0)',
        'NRC score (Mean)',
        'NRC score (% == 0)'
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
        'RT (% == 0)',
        'Likes (Mean)',
        'Likes (% == 0)',
        'Replies (Mean)',
        'Replies (% == 0)',
        'BING score (Mean)',
        'BING score (% == 0)',
        'AFINN score (Mean)',
        'AFINN score (% == 0)',
        'NRC score (Mean)',
        'NRC score (% == 0)'
      )
  )
  
  
  for(sn in source_name){
    
    
    topic_names <- names(sm_obj[[sn]])
    
    for(tn in topic_names){
      
      d_names <- names(sm_obj[[sn]][[tn]])
      
      for(dn in d_names){
        
        tdf <- sm_full[[sn]][[tn]][[dn]]
        
        n_obs <- as.numeric(nrow(tdf))
        
        bing_avg <- mean(tdf$bing_score)
        bing_perc_zero <- nrow(tdf[tdf$bing_score == 0,])/nrow(tdf)
        
        afinn_avg<- mean(tdf$afinn_score)
        afinn_perc_zero <- nrow(tdf[tdf$afinn_score == 0,])/nrow(tdf)
        
        nrc_avg <- mean(tdf$nrc_score)
        nrc_perc_zero <- nrow(tdf[tdf$nrc_score == 0,])/nrow(tdf)
        
        vn <- glue("{tn}_{dn}")
        
        if(sn=='reddit'){
            
            n_obs <- as.numeric(nrow(tdf))
            
            n_sr <- as.numeric(length(unique(tdf)))
            
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
  
  reddit_df <- round(reddit_df,1)
  twitter_df <- round(twitter_df,1)
  
  return(list(
    reddit = reddit_df,
    twitter = twitter_df
  ))
  
}

full_desc <- summ_full(sm_full)




# Summary of number of observations in all datasets (after pre-processing) 
n_obs <- function(sm_obj){
  
  source_name <- names(sm_obj)
  reddit_df <- data.frame(
    matrix(NA,
           nrow = 7,
           ncol = 0)
  )
  
  twitter_df <- data.frame(
    matrix(NA,
           nrow = 7,
           ncol = 0)
  )
  
  for(sn in source_name){
    
    
    topic_names <- names(sm_obj[[sn]])
    
    for(tn in topic_names){
      
      d_names <- names(sm_obj[[sn]][[tn]])
      
      for(dn in d_names){
        
        tvar <- summary(sm_obj[[sn]][[tn]][[dn]]$obs)
        
        vn <- glue("{tn}_{dn}")
        
        tdf <- data.frame(
          temp_col = round(unclass(tvar),2),
          check.names = F
        )
        
        
        tdf <- rbind(
          Days = length(unique(sm_obj[[sn]][[tn]][[dn]]$Day)),
          tdf)
        
        
        # add_row(tdf, length(unique(sm_obj[[sn]][[tn]][[dn]]$Day)))
        
        names(tdf) <- vn
        
        if(sn=='reddit'){
          reddit_df <- cbind(reddit_df, tdf)
        } else if(sn=='twitter'){
          twitter_df <- cbind(twitter_df, tdf) 
        }
        
      }
      
    }
    
  }
  
  return(list(reddit = reddit_df, twitter = twitter_df))
  
  
}

n_all <- n_obs(sm_obj)
n_all_trim <- n_obs(sm_obj_trim)

formattable(n_all$reddit)
formattable(n_all$twitter)
formattable(n_all_trim$reddit)
formattable(n_all_trim$twitter)

# Reddit

## Employment

### Correlations
cor_matrix <- function(df_list, topic){
  
  
  df_names <- names(df_list)
  
  obj_list <- list()
  cov_list <- list()
  cor_list <- list()
  
  
  for(df_name in df_names){
    
    df <- df_list[[df_name]] %>% 
      select(-obs, -Day)
    
    if(topic=='emp'){
      
      obj_list[[df_name]] <- df %>% 
        cor(.) %>% 
        as.data.frame(.) %>% 
        select(sp500_Close, ics)
    
      cov_list[[df_name]] <- df %>% 
        select(-sp500_Close, -ics) %>%
        cov(.)
      
      cor_list[[df_name]] <- cov2cor(cov_list[[df_name]])
      
      
      
      } else if(topic=='vac'){
        
        obj_list[[df_name]] <- df %>% 
          cor(.) %>% 
          as.data.frame(.) %>% 
          select(uptake)
        
        cov_list[[df_name]] <- df_list[[df_name]] %>% 
          select(-obs, -Day) %>% 
          select(-uptake) %>%
          cov(.)
      
      cor_list[[df_name]] <- cov2cor(cov_list[[df_name]])
      
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

reddit_emp_cor$Mantel <- MantelCor(reddit_emp_cor$cor_list, permutations = 1000)
reddit_vac_cor$Mantel <- MantelCor(reddit_vac_cor$cor_list, permutations = 1000)

twitter_emp_cor$Mantel <- MantelCor(twitter_emp_cor$cor_list, permutations = 1000)
twitter_vac_cor$Mantel <- MantelCor(twitter_vac_cor$cor_list, permutations = 1000)


draw_mantel <- function(cmat){
  
  df <- as.data.frame(round(cmat$Mantel$correlations, 3))
  df[df==0] <- '-'
  
  formattable(df)
  
}

draw_mantel(reddit_emp_cor)
draw_mantel(reddit_vac_cor)
draw_mantel(twitter_emp_cor)
draw_mantel(twitter_vac_cor)


cor_obj_reddit_subsets <- function(obj_list){
  
  rs_cor <- list()
 
  te <- obj_list
  
  full_te <- data.frame(
    matrix(NA, 
           nrow = nrow(te[[1]]),
           ncol = 0
    )
  ) 
  
  for(n in names(te)){
    
    colnames(te[[n]]) <- paste(n, colnames(te[[n]]), sep="_")
    
    full_te <- cbind(full_te, te[[n]])  
    
  }
  
  rs_cor$c1$sp500 <- full_te %>% 
    mutate(Measure = rownames(full_te)) %>% 
    filter(Measure != "sp500_Close") %>% 
    filter(Measure != "ics") %>%
    select(contains('c1')) %>% 
    select(contains("sp500_Close")) %>% 
    mutate(
      `Set 1` = c1_set1_sp500_Close,
      `Set 2` = c1_set2_sp500_Close,
      `Set 3` = c1_set3_sp500_Close,
      `Set 4` = c1_set4_sp500_Close
    ) %>% 
    select(
      `Set 1`,
      `Set 2`,
      `Set 3`,
      `Set 4`
    )
  
  rs_cor$c1$ics <- full_te %>% 
    mutate(Measure = rownames(full_te)) %>% 
    filter(Measure != "sp500_Close") %>% 
    filter(Measure != "ics") %>%
    select(contains('c1')) %>% 
    select(contains("ics")) %>% 
    mutate(
      `Set 1` = c1_set1_ics,
      `Set 2` = c1_set2_ics,
      `Set 3` = c1_set3_ics,
      `Set 4` = c1_set4_ics
    ) %>% 
    select(
      `Set 1`,
      `Set 2`,
      `Set 3`,
      `Set 4`
    )
  
  
  rs_cor$c2$sp500 <- full_te %>% 
    mutate(Measure = rownames(full_te)) %>% 
    filter(Measure != "sp500_Close") %>% 
    filter(Measure != "ics") %>%
    select(contains('c2')) %>% 
    select(contains("sp500_Close")) %>% 
    mutate(
      `Set 1` = c2_set1_sp500_Close,
      `Set 2` = c2_set2_sp500_Close,
      `Set 3` = c2_set3_sp500_Close,
      `Set 4` = c2_set4_sp500_Close
    ) %>% 
    select(
      `Set 1`,
      `Set 2`,
      `Set 3`,
      `Set 4`
    )
  
  rs_cor$c2$ics <- full_te %>% 
    mutate(Measure = rownames(full_te)) %>% 
    filter(Measure != "sp500_Close") %>% 
    filter(Measure != "ics") %>%
    select(contains('c2')) %>% 
    select(contains("ics")) %>% 
    mutate(
      `Set 1` = c2_set1_ics,
      `Set 2` = c2_set2_ics,
      `Set 3` = c2_set3_ics,
      `Set 4` = c2_set4_ics
    ) %>% 
    select(
      `Set 1`,
      `Set 2`,
      `Set 3`,
      `Set 4`
    )
  
  return(rs_cor)
  
}

rs_cor <- cor_obj_reddit_subsets(reddit_emp_cor$obj_list)

draw_hm <- function(cm){
  
  heatmap.2(
    x = as.matrix(cm),
    Rowv = F,
    Colv = F,
    dendrogram = 'none',
    cellnote = as.matrix(cm),
    notecol = 'black',
    notecex = 1.8,
    trace = 'none',
    col = "cm.colors",
    key = F
    # margins = c(7,11)
  )
  
}

draw_hm(rs_cor$c1$sp500)
draw_hm(rs_cor$c1$ics)

draw_hm(rs_cor$c2$sp500)
draw_hm(rs_cor$c2$ics)






full_t_long <- full_t %>% 
  mutate(Measure = rownames(full_t)) %>%
  pivot_longer(
    .,
    cols = everything(),
    names_to = "CSet",
    values_to = "Correlation"
  ) %>% 
  separate(
    CSet,
    into = c("Clean", "Delete1", "Set", "Measure", "Delete2"),
    fill = "right"
  ) %>% 
  select(!contains("Delete"))


full_t_long





c1 <- RandomMatrix(10, 1, 1, 10)
c2 <- RandomMatrix(10, 1, 1, 10)
c3 <- RandomMatrix(10, 1, 1, 10)
MantelCor(cov2cor(c1), cov2cor(c2))

cov.list <- list(c1, c2, c3)
cor.list <- llply(list(c1, c2, c3), cov2cor)

MantelCor(cor.list)

reps <- unlist(lapply(cor.list, MonteCarloRep, 10, correlation = FALSE))

MantelCor(cor.list, repeat.vector = reps)


c1 <- cov(sm_obj$reddit$emp$c1_set1 %>% 
                          select(-obs, -Day, -sp500_close, -ics))

c2 <- cov(sm_obj$reddit$emp$c1_set1 %>% 
            select(-obs, -Day))

c3 <- cov(sm_obj$reddit$emp$c1_set3 %>% 
            select(-obs, -Day))

c4 <- cov(sm_obj$reddit$emp$c1_set4 %>% 
            select(-obs, -Day))





c5_mat <- RandomMatrix(14, 1, 1, 14)
c5 <- cov(c5_mat)

cov.list <- list(c1, c2, c3, c4, c5)
cor.list <- llply(list(c1, c2, c3, c4, c5), cov2cor)

reps <- unlist(lapply(cov.list, MonteCarloRep, 10, MatrixCor, correlation = TRUE))
MantelCor(cor.list, repeat.vector = reps)

MantelCor(cor.list, permutations = 1000)



blab <- corrplot(as.matrix(c), method = 'color')

cp <- recordPlot()

c <- c[apply(c, 1, function(x) any(x > 0.99)), apply(c, 1, function(x) any(x > 0.99))] 
### Subsets.

##### Code Dump #######

# # Changing score + summarizing days
r2 <- load_nlp_data(
  source = "Reddit",
  topic = "Employment",
  set = 2,
  level = 2,
  stage = '1b'
)

r3 <- load_nlp_data(
  source = "Reddit",
  topic = "Employment",
  set = 3,
  level = 2,
  stage = '1b'
)



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
#   mutate(bing_score = bing_score/bing_words,
#          afinn_score = afinn_score/afinn_words,
#          nrc_score = nrc_score/nrc_words) %>%
#   mutate(bing_score = ifelse(is.na(bing_score), 0.5,
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
#   mutate(Day = as.Date(date)) %>% select(-date) %>% 
#   group_by(Day) %>% 
#   mutate(
#     comm_weight = num_comments/sum(num_comments),
#     score_weight = score/sum(score),
#     i_score = num_comments + score,
#     i_weight = i_score/sum(i_score)) %>% 
#   mutate(
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
#   mutate(bing_score = bing_score/bing_words,
#          afinn_score = afinn_score/afinn_words,
#          nrc_score = nrc_score/nrc_words) %>%
#   mutate(bing_score = ifelse(is.na(bing_score), 0.5,
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
#   mutate(Day = as.Date(date)) %>% 
#   group_by(Day) %>% 
#   mutate(
#     like_weight = like_count/sum(like_count),
#     reply_weight = reply_count/sum(reply_count),
#     rt_score = retweet_count + quote_count,
#     i_score = retweet_count + quote_count + like_count,
#     rt_weight = rt_score/sum(rt_score),
#     i_weight = i_score/sum(i_score),
#   ) %>% 
#   mutate(
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
#   mutate(Period = as.character(Period),
#          obs = as.character(obs)) %>% 
#   group_by(Period) %>% 
#   dplyr::summarise(
#     across(where(is.numeric), mean)
#   ) %>% 
#   mutate(Period = as.numeric(Period)) %>% 
#   inner_join(
#     surveys$ai$emp, 
#     by = "Period") 
# 
# 
# 
# t <- objective$sp500 %>% 
#   arrange(., Day) %>% 
#   mutate(r_sp500 = sp500_Close - lag(sp500_Close),
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
#   mutate(r_bing = bing - lag(bing),
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

sm_obj$reddit$vac$c1 %>% 
  select(Day,obs, bing) %>%
  mutate(obs = scales::rescale(obs, to=c(0,1))) %>% 
  ggplot(.) +
  geom_line(aes(x = Day, y = obs, color = "Observations")) +
  geom_line(aes(x = Day, y = bing, color = "Bing_score")) 


cor(sm_obj$reddit$vac$c1$bing, sm_obj$reddit$vac$c1$obs)


sm_obj$reddit$vac$c1 %>% 
  filter(Day > ymd('2021-02-07')) %>% 
  filter(Day < ymd('2021-02-22')) %>% 
  select(Day,uptake) %>% 
  ggplot(., aes(x=Day, y=uptake)) +
  geom_line()


h <- sm_all$reddit$hps$vac$c1 %>% 
  group_by(Period) %>% 
  dplyr::summarise(
        across(
          bing:i_nrc,
          ~sum(.x*obs)/sum(obs)),
        across(
          contains('hps'),
          ~mean(.x)
        ),
        uptake = mean(uptake))


cor(h$`hps_s2_18-54`, h$uptake)

h %>% 
  select(Period,
         contains('hps'),
         uptake) %>% 
  cor(.)
  
  
  
  pivot_longer(
    .,
    cols = !Period,
    names_to = "Measure",
    values_to = "Index"
  ) %>% 
  ggplot(.) + 
  geom_bar(
    aes(x=Period, y=Index, fill=Measure),
    stat='identity',
    position=position_dodge()
  )
  
  
  ggplot(.) +
  geom_bar(aes(x=Period, y=uptake, color = "Uptake"),
           stat = 'identity',
           position = position_dodge()) +
  geom_bar(aes(x=Period, y=`hps_s2_18-54`, color = "s2 (18-54)"), stat = 'identity',
           position = position_dodge()) +
  geom_bar(aes(x=Period, y=`hps_s1_18-54`, color = "s1 (18-54)"), stat = 'identity',
           position = position_dodge()) +
  geom_bar(aes(x=Period, y=`hps_s2_Total`, color = "s2 (Total)"), stat = 'identity',
           position = position_dodge()) +
  geom_bar(aes(x=Period, y=`hps_s1_Total`, color = "s1 (Total)"), stat = 'identity',
           position = position_dodge()) 

#     across(
#       bing:i_nrc,
#       ~sum(.x*obs)/sum(obs))


cdv_p <- read.csv(
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
  mutate(Day = mdy(Date)) %>% 
  filter(
    Day < ymd(end_date)
  ) %>% 
  filter(
    Day > ymd(vac_start_date)
  ) %>% 
  group_by(Day) %>% 
  dplyr::summarise(
    uptake = sum(Administered)/sum(Distributed),
    administered = sum(Administered)
  ) %>% 
  mutate(admin_lag = administered - lag(administered),
         dow = wday(Day)) 

cdv_p %>% 
  mutate(admin_lag = scales::rescale(admin_lag, to = c(0,1))) %>% 
  filter(dow != 6) %>%
  filter(dow != 7) %>%
  ggplot(.) + 
  geom_line(aes(x=Day, y=admin_lag))
  

