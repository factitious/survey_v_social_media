library(lubridate)
library(tidyverse)
library(glue)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)
data_path <- 'Methods/Data/Objective'

# S&P data from https://www.nasdaq.com/market-activity/index/spx/historical.
# Downloaded at:
sp500 <- read.csv(
  file.path(
    root_dir,
    data_path,
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