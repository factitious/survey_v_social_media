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


## Loading data ###############################

root_dir <- '/Volumes/Survey_Social_Media_Compare'
oj_data_path <- 'Methods/Data/Objective'
ai_data_path <- 'Methods/Data/Surveys/Axios-Ipsos/Aggregate'
hps_data_path <- 'Methods/Data/Surveys/HPS/Aggregate'
overall_path <- 'Methods/Data/Overall'
survey_periods_path <- 'Methods/Scripts/Surveys/table_details'
setwd(root_dir)

start_date <- '2020-10-22'
end_date <- '2021-05-24'


###Objective measures

objective <- list()

# S&P data from https://www.nasdaq.com/market-activity/index/spx/historical.
# Downloaded at:
objective$sp500 <- read.csv(
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
objective$ics <- read.csv('http://www.sca.isr.umich.edu/files/tbcics.csv',
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
objective$cdc_vac <- read.csv(
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
  summarize(
    uptake = sum(Administered)/sum(Distributed)
  )
  



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
    summarize(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(hps_start, tz = "UTC"),
        as.POSIXct(hps_end),
        by = "day"))
    )
  
  sp$ai <- sp$proc %>% 
    mutate(
      d = sp$proc %>% select(Period) %>% duplicated(.)) %>% 
    mutate(
      Period = ifelse(d == T, glue("{Period}b"), Period)
    ) %>% 
    select(Period, ai_start, ai_end) %>% 
    group_by(Period) %>% 
    summarize(
      Day = as.Date(seq.POSIXt(
        as.POSIXct(ai_start, tz = "UTC"),
        as.POSIXct(ai_end),
        by = "day"))
    ) %>% 
    mutate(Period = 
             as.numeric(
               str_extract(
                 Period, "[[:digit:]]+")
             )) %>% 
    arrange(., Period)
  
  return(sp)
  
}

sp <- load_sp()


surveys <- list()

surveys$ai$emp <- readRDS(
  file.path(
    root_dir, 
    ai_data_path,
    'emp.rds'
    )
) %>% 
  select(
    Period,
    `18-29`,
    `30-49`,
    `50-64`,
    `Male`,
    `Female`,
    `Total`
         )
  
surveys$ai$vac <- readRDS(
  file.path(
    root_dir, 
    ai_data_path,
    'vac.rds'
  )
) %>% 
  select(
    Period,
    `18-29`,
    `30-49`,
    `50-64`,
    `Male`,
    `Female`,
    `Total`
  )

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

surveys$hps$vac_s1 <- readRDS(
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

surveys$hps$vac_s2 <- readRDS(
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


### Social media data.

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
        summarise(
          obs = n(),
          bing = mean(bing_score),
          nc_bing = sum(nc_bing),
          sc_bing = sum(sc_bing),
          i_bing = sum(i_bing),
          nrc = mean(nrc_score),
          nc_nrc = sum(nc_nrc),
          sc_nrc = sum(sc_nrc),
          i_nrc = sum(i_nrc)
        )
  
  } else if(source == "Twitter"){
    
      df <- df %>% 
        group_by(Day) %>% 
        summarise(
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


load_all_reddit <- function(proc = F){
  
  if(proc == F){
  
      reddit <- list()
      
      # Employment
      # Set 1
      reddit$emp$c1b$set_1 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2$set_1 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      # Set 2
      reddit$emp$c1b$set_2 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 2,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2$set_2 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 2,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      # Set 3
      reddit$emp$c1b$set_3 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 3,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2$set_3 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 3,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      # Set 4
      reddit$emp$c1b$set_4 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 4,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      reddit$emp$c2$set_4 <- load_nlp_data(
        source = "Reddit",
        topic = "Employment",
        set = 4,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      # Vaccination
      reddit$vac$c1b <- load_nlp_data(
        source = "Reddit",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
      # Vaccination
      reddit$vac$c2 <- load_nlp_data(
        source = "Reddit",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Reddit") %>% 
        summarize_days(., source = "Reddit")
      
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

load_all_twitter <- function(proc = F){
  
  if(proc == F){
  
      twitter_emp <- list()
      twitter_vac <- list()
      
      twitter <- list()
      
      twitter$emp$c1b <- load_nlp_data(
        source = "Twitter",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '1b'
      ) %>% 
        change_scores(., source = "Twitter") %>% 
        summarize_days(., source = "Twitter")
      
      twitter$emp$c2 <- load_nlp_data(
        source = "Twitter",
        topic = "Employment",
        set = 1,
        level = 2,
        stage = '2'
      ) %>% 
        change_scores(., source = "Twitter") %>% 
        summarize_days(., source = "Twitter")
      
      twitter$emp$ml <- load_nlp_data(
        source = "Twitter",
        topic = "Employment",
        set = 1,
        level = 3,
        stage = '2'
      ) %>% 
        change_scores(., source = "Twitter") %>% 
        summarize_days(., source = "Twitter")
      
      twitter$vac$c1b <- load_nlp_data(
        source = "Twitter",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '1b'
      ) %>% 
        mutate(like_count = as.numeric(like_count)) %>% 
        change_scores(., source = "Twitter") %>% 
        summarize_days(., source = "Twitter")
      
      twitter$vac$c2 <- load_nlp_data(
        source = "Twitter",
        topic = "Vaccination",
        set = 1,
        level = 2,
        stage = '2'
      ) %>% 
        mutate(like_count = as.numeric(like_count)) %>% 
        change_scores(., source = "Twitter") %>% 
        summarize_days(., source = "Twitter")
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


sm <- list()

sm$reddit <- load_all_reddit(proc = T)
sm$twitter <- load_all_twitter(proc = T)

# # To re-run:
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



## Descriptives ###############################

# EMPLOYMENT: Combine objective measures
ts <- as.Date(seq.POSIXt(
  as.POSIXct(start_date, tz = "UTC"),
  as.POSIXct(end_date),
  by = "day"))

ts <- as.data.frame(ts) %>% 
  mutate(Day = ts) %>% 
  select(Day)

obj <- ts %>% 
  full_join(objective$sp500, by = "Day") %>% 
  full_join(objective$ics, by = "Day") %>% 
  mutate(ics = Index) %>% 
  select(-Index) %>% 
  arrange(., Day) %>% 
  fill(ics) %>% 
  fill(sp500_Close) %>% 
  filter(Day > ymd('2020-10-22')) %>% 
  mutate(
    across(where(is.numeric), scale)
  )


# obj %>% 
#   ggplot(.) + 
#   geom_line(aes(x = Date, y = sp500_Close)) +
#   geom_line(aes(x = Date, y = ics))


obj_t <- obj %>% 
  mutate(Month = ym(glue("{year(Day)}-{month(Day)}"))) %>% 
  group_by(Month) %>% 
  summarize(
    m_avg_sp = mean(sp500_Close),
    m_avg_ics = mean(ics))

# obj_t %>% 
#   ggplot(.) + 
#   geom_line(aes(x = Month, y = m_avg_sp)) +
#   geom_line(aes(x = Month, y = m_avg_ics))



m1 <- sm$reddit$emp$c1b$set_1 %>% 
  full_join(obj, by = "Day") %>% 
  filter(obs > 100) %>% 
  filter(Day > ymd(start_date))
  


m2 <- m1 %>%
  select(Day, contains("nrc"), obs) %>% #bing, nc_bing, sc_bing, nc_sc_bing, obs) %>% 
  pivot_longer(., 
               cols = contains("nrc"),
               names_to = "Metric",
               values_to = "Index")


m2 %>% 
  ggplot(.)+
  geom_line(aes(x=Day, y=bing), color='blue') + 
  geom_line(aes(x=Day, y=bing_smooth), color='red')
  
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
  
  
  
# EMPLOYMENT: Comparing surveys.

names(surveys$ai$emp[!(names(surveys$ai$emp) %in% "Period")]) <- paste0("ai_", names(surveys$ai$emp[!(names(surveys$ai$emp) %in% "Period")]))
names(surveys$ai$emp) <- paste0("ai_", names(surveys$ai$vac))


t <- sm$reddit$emp$c1b$set_1 %>% 
  inner_join(sp$ai, by = "Day") %>% 
  mutate(Period = as.character(Period),
         obs = as.character(obs)) %>% 
  group_by(Period) %>% 
  summarise(
    across(where(is.numeric), mean)
  ) %>% 
  mutate(Period = as.numeric(Period)) %>% 
  inner_join(
    surveys$ai$emp, 
    by = "Period") 
  






%>% 
  inner_join(sp$ai, by = "Day") 

%>% 
  group_by(Period) %>% 
  summarize(
    bing = sum(bing)/sum(obs)
  )



##### Code Dump #######


# # Changing score + summarizing days
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
#   summarise(
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
#   summarise(
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


