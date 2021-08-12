## Reddit data (Employment):
# 1. Query: all
# 2. Query: top 100 subreddits.
# 3. Query: likely-to-be-relevant subreddits.
# 4. No-query: all data from likely-to-be-relevant subreddits.

# 1 - 'Aggregate/emp_df.csv'
# 4 - 'Aggregate/emp_sr_df.csv'

library(tidyverse)
library(data.table)
library(glue)
library(gsubfn)
library(stringr)

root_dir <- '/Volumes/Survey_Social_Media_Compare'
setwd(root_dir)

load_data <- function(source, topic, source2 = 'Submissions'){

  data_path <- glue('Methods/Data/{source}/Raw/Aggregate/')

  if(source == "Twitter" |
     (source == "Reddit" & source2 == "Submissions")){

    if(topic == 'Employment'){
      df_name <- 'emp_1.csv'
      logs_name <- 'emp_1_logs.csv'
    }

    else if(topic == 'Vaccination'){
      df_name <- 'vac_1_df.csv'
      logs_name <- 'vac_1_logs.csv'
    }
  }

  if(source == "Reddit" & source2 == "Subreddits"){

    if(topic == 'Employment'){
      df_name <- 'emp_4_df.csv'
      logs_name <- 'emp_4_logs.csv'
    }

    else if(topic == 'Vaccination'){
      df_name <- 'vac_4_df.csv'
      logs_name <- 'vac_4_logs.csv'
    }

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

top_sr <- function(df){
  
  sorted_sr <- df %>% 
    select(subreddit) %>% 
    group_by(subreddit) %>% 
    summarize(count_sr = n()) %>% 
    arrange(desc(count_sr))
  
  return(sorted_sr)
  
}

filter_topn <- function(df, n){
  
  topn_sr <- top_sr(df) %>% 
    slice(1:n) 
  
  filtered_df <- df %>% 
    filter(subreddit %in% topn_sr$subreddit)
  
  return(filtered_df)
  
}

filter_relevant <- function(df){
  
  # List of terms for finding likely-to-be-relevant subreddits
  # Contains
  SR_TERMS1 = c(
    'job',
    'work',
    'employ',
    'career',
    'resume'
  )
  
  # Does not contain
  SR_TERMS2 = list(
    'uk',
    'europe',
    'eu',
    'china',
    'alberta',
    'ontario',
    'aldi',
    'u_',
    'network',
    '75thrangerrecruiting',
    'ashesrecruitment',
    'beachbodyworkouts',
    'woodwork',
    'brawlrecruit',
    'homework',
    'Homework',
    'canada',
    'india',
    'cscareerquestionsIN',
    'bdjobsbarta',
    'gta',
    'glasgow',
    'german',
    'cscareerquestionssea',
    'careerblogzino',
    'dogeworkers', 
    'dubai_jobs',
    'edmontonjobs',
    'entitledcoworkers',
    'fastworkers',
    'fifacareers',
    'guildwarsdyejob',
    'howhumanbeingswork',
    'idoworkherelady',
    'idontworkhere',
    'idontworkherelady',
    'iceland',
    'workshop',
    'jobsims',
    'joblessreincarnation',
    'philippine',
    'jobstobedone',
    'jobswales',
    'jobs_to_be_done',
    'justnocoworker',
    'lawjobssydney',
    'framework',
    'phjobs',
    'pakjobsearch',
    'perthjobs',
    'qtframework',
    'racistslosetheirjobs',
    'radicalsocialwork',
    'saskatchewanjobs',
    'sexworkersanonymous',
    'sgworkplace',
    'shadowwork',
    'solidworks',
    'steve',
    'swiss',
    'thaijobs',
    'vancouverislandjobs',
    'vancouverjobs',
    'victoriajobs',
    'ottawa',
    'workinginottawa',
    'workingk9',
    'workingout',
    'worktogame',
    'workout',
    'workingwithautismnj',
    'auscscareerquestions',
    'bananojobs',
    'berlinjobs',
    'cscareerquestionscad',
    'cscareerquestionsoce',
    'cscareerquestionseu',
    'cscareerquestionssea',
    'cscareerquestionsin',
    'cscareerquestionsjp',
    'cscareerquestionsoce',
    'de_jobs_jobsuche',
    'dogswithoutjobs',
    'ireland',
    'ghostswithjobs',
    'jobb',
    'jobbit',
    'jobcorps',
    'jobmania',
    'japan',
    'london',
    'mikew_reddit_work',
    'nijobs',
    'nzjobs',
    'olyjobs',
    'onejob',
    'sex',
    'artwork',
    'fifa',
    'stopworking',
    'thisismyjob',
    'trailwork',
    'turtleswithjobs',
    'work_at_nothing',
    'workaholics',
    'workandtravel',
    'workplace_bullying',
    'worksmarternotharder',
    'ziojobs',
    'careerblogzino',
    'dogs',
    'britishcolumbiajobs',
    'cats',
    'dejobs',
    'govtjobnotification',
    'guildwarsdyejob',
    'hrisafunnycareer',
    'horriblecoworkers',
    'howhumanbeingswork',
    'idoworkherelady',
    'joebidenhatesjobs',
    'learnhowitworks',
    'mathnasiumemployees',
    'nothowgirlswork',
    'peoplewhoworkat',
    'timeworkssubmissions',
    'toowoombajobs',
    'workingonitastrology',
    'workplacesafety',
    'workspaces',
    'bathandbodyworks',
    'dirtyjobs',
    'dogswithjobs',
    'employergore',
    'energy_work',
    'evejobs',
    'funnyworkstories',
    'mspaintatwork',
    'regularjobz',
    'theideologyofwork'
  )
  
  ss1 <- reddit_emp_df %>% 
    filter(
      str_detect(
        tolower(subreddit),
        str_c(SR_TERMS1, collapse = "|")
      ) &
        !str_detect(
          tolower(subreddit),
          str_c(SR_TERMS2, collapse = "|")
        )
    )
  
  return(ss1)
}

# Load data
list[reddit_emp_df, reddit_emp_logs] <- load_data('Reddit', 'Employment')

# Filter top 100 subreddits
reddit_emp_df_top100 <- filter_topn(reddit_emp_df, 100)

# Filter likely-to-be-relevant subreddits
# Get all subreddits and exclude those 
# that are unlikely to be relevant.
reddit_emp_df_relevant <- filter_relevant(reddit_emp_df)


# Save the files

data_path <- glue('Methods/Data/Reddit/Raw/Aggregate/')
df2_name <- 'emp_2.csv'
df3_name <- 'emp_3.csv'

full_path_df2 <- file.path(root_dir, 
                          data_path,
                          df2_name)

full_path_df3 <- file.path(root_dir,
                           data_path,
                           df3_name) 

write.csv(reddit_emp_df_top100,
          file = full_path_df2)

write.csv(reddit_emp_df_relevant,
          file = full_path_df3)

rm(list = ls())
