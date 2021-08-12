# For testing purposes

# t <- load_data('Twitter', 'Employment', 1) %>%
#   mutate(sentiment = ifelse(runif(nrow(.)) > 0.5, 'Positive', 'Negative'))
# 
# t_sent <- t %>% 
#   select(text, sentiment) %>% 
#   slice(1:15000)

library(tidyverse)
library(glue)
library(RTextTools)
library(e1071)



root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

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
  }
  
  df_path <- file.path(
    root_dir,
    data_path,
    df_name
  )
  
  if(level == 0){
    df <- read.csv(df_path)
  } else {
    df <- readRDS(df_path) %>%
      select(-any_of("X")) %>%
      suppressWarnings()
  }

  return(df)
  
}

bla <- load_nlp_data(
  source = 'Reddit', 
  topic = 'Vaccination', 
  level = 2,
  stage = '1b')

s_size <- 1500

t <- bla %>% 
  slice(1:1500) %>% 
  mutate(bing_score = ifelse(bing_score <0, 'negative', 'positive'))

train_size <- round(s_size/3)


matrix_df = create_matrix(
  t$text, 
  language = 'english',
  removeStopwords = F,
  removeNumbers = F,
  stemWords = T)
mat = as.matrix(matrix_df)


# train the model
classifier = naiveBayes(mat[1:train_size,], as.factor(t$bing_score[1:train_size]) )

# test the validity
predicted = predict(classifier, mat[train_size+1:s_size,]); predicted
table(t$bing_score[train_size+1:s_size], predicted)
recall_accuracy(t$bing_score[train_size+1:s_size], predicted)
