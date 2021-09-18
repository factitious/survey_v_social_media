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
)

sp500 <- sp500 %>% 
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
    # Date > ymd('2021-01-04'),
    Date < ymd('2021-05-24'))

cdc_vac_s <- cdc_vac %>% 
  mutate(
    day_uptake_per_100 = Admin_Per_100K/Dist_Per_100K
  ) %>% 
  group_by(Date) %>% 
  summarize(
    uptake = sum(Administered)/sum(Distributed),
    uptake_per_100 = mean(day_uptake_per_100, na.rm = T)
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


