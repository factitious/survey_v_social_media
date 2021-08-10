library(tidyverse)
library(tidytext)
library(glue)
library(SnowballC)
library(gsubfn)
library(data.table)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

# list <- structure(NA,class="result")
# "[<-.result" <- function(x,...,value) {
#   args <- as.list(match.call())
#   args <- args[-c(1:2,length(args))]
#   length(value) <- length(args)
#   for(i in seq(along=args)) {
#     a <- args[[i]]
#     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
#   }
#   x
# }

load_cdata <- function(source, topic, set, stage){
  
  topic_short <- substr(
    tolower(topic),
    1,
    3
  )
  
  data_path <- glue('Methods/Data/{source}/Preprocessed')
  df_name <- glue('{topic_short}_{set}_c{stage}.rds')
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  df <- readRDS(df_path) %>% 
    select(-X)
  
  return(df)
  
}

load_all_sentiments <- function(){
  
  bing <- get_sentiments('bing')
  bing_stem <- bing %>% 
    mutate(word = wordStem(word))%>% 
    distinct()
  
  afinn <- get_sentiments('afinn')
  afinn_stem <- afinn %>% 
    mutate(word = wordStem(word)) %>% 
    distinct()
  
  nrc <- get_sentiments('nrc')
  nrc_stem <- nrc %>% 
    mutate(word = wordStem(word)) %>% 
    distinct()
  
  return(list(
    bing,
    bing_stem,
    afinn,
    afinn_stem,
    nrc,
    nrc_stem
  ))
  
  
}

untidy_text <- function(df){
  
  df <- df %>% 
    group_by(across(c(-word))) %>% 
    summarize(text = str_c(word, collapse = " ")) %>% 
    ungroup()
  
  return(df)
}

emp_3_c1b <- load_cdata('Reddit', 'Employment', 3, '1b')

list[
  bing, bing_stem,
  afinn, afinn_stem,
  nrc, nrc_stem
  ] <- load_all_sentiments()

get_sentiment_score <- function(df, lexicon, stemmed = FALSE) {
  
  if(lexicon == 'bing'){
    
    scored_df <- unnest_tokens(word, text) %>% 
      inner_join(bing) %>% 
      mutate(sentiment = ifelse(sentiment == 'positive', 1, -1)) %>% 
      group_by(id) %>% 
      mutate(
        sent_word = n(),
        sent_score = sum(sentiment)
      ) %>% 
      select(-sentiment) %>% 
      untidy_text() %>% 
      select(-text) %>% 
      right_join(df) %>% 
      mutate(sent_words = ifelse(is.na(sent_words), 0, sent_words),
             sent_score = ifelse(is.na(sent_score), 0, sent_score))
    
    
  }
  
  else if(lexicon == 'afinn'){
    
    
    
  }
  
  else if(lexicon == 'nrc'){
    
    
    
  }
    
    
  }
}


X<-1
Y<-T

X %>% add(1) %>% { ifelse(Y ,add(.,1), . ) }


bt <- t %>% 
  unnest_tokens(word, text) %>% 
  inner_join(bing) %>% 
  mutate(sentiment = 
           ifelse(sentiment == 'positive', 
                  1, 
                  -1)) %>% 
  group_by(id) %>% 
  mutate(
    sent_words = n(),
    sent_score = sum(sentiment)
    ) %>% 
  select(-sentiment) %>% 
  untidy_text() %>% 
  select(-text) %>% 
  right_join(t) %>% 
  mutate(sent_words = ifelse(is.na(sent_words), 0, sent_words),
         sent_score = ifelse(is.na(sent_score), 0, sent_score))

bt2 <- t %>% 
  select(-text) %>% 
  left_join(., bt)
  

  mutate(score = sum(sentiment))

  summarise(score = sum(sentiment))
  
  
  
  
  # group_by(across(-word, sentiment)) %>% 
  # mutate()



%>% 
  count(word, sentiment, sort = TRUE) %>% 
  mutate(
    score = case_when(
      sentiment == 'negative' ~ n*(-1),
      sentiment == 'positive' ~ n*1
    )
  )
