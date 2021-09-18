rm(list = ls())
library(readxl)
library(tidyverse)
library(data.table)
library(glue)
 
rootDir <- '/Volumes/Survey_Social_Media_Compare'
data_path <- file.path(rootDir, 'Methods/Data/Surveys/HPS/Raw/')

first_week <- 18
last_week <- 28

hps_weeks <- seq(from = first_week, to = last_week, by = 1)
# hps_periods <- seq(from = 1, to = (last_week - first_week)+1, by = 1)

url_emp1 <- vector(mode = "list", length = length(hps_weeks))
dest_emp1 <- vector(mode = "list", length = length(hps_weeks))
vars_emp1 <- character(length(hps_weeks))

url_emp2 <- vector(mode = "list", length = length(hps_weeks))
dest_emp2 <- vector(mode = "list", length = length(hps_weeks))
vars_emp2 <- character(length(hps_weeks)) 

url_vacc <- vector(mode = "list", length = length(hps_weeks) - 4)
dest_vacc <- vector(mode = "list", length = length(hps_weeks) - 4)
vars_vacc <- character(length(hps_weeks)-4)


idx <- 1
idx_vac <- 1

for (week in hps_weeks){
  
  if (week <= 21){
    year <- "2020"
  } else {
    year <- "2021"
  }
  
  url_emp1[[idx]] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/employ1_week{week}.xlsx")
  
  url_emp2[[idx]] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/employ2_week{week}.xlsx")
  
  dest_emp1[[idx]] <- glue("{data_path}P{idx}_emp1.xlsx")
  vars_emp1[idx] <- glue("P{idx}_emp1")
  
  dest_emp2[[idx]] <- glue("{data_path}P{idx}_emp2.xlsx")
  vars_emp2[idx] <- glue("P{idx}_emp2")
  
  if (week > 21){
    url_vacc[[idx_vac]] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/health5_week{week}.xlsx")
    dest_vacc[[idx_vac]] <- glue("{data_path}P{idx}_vacc.xlsx")
    vars_vacc[idx_vac] <- glue("P{idx}_vacc")
    idx_vac <- idx_vac + 1
  }
  
  idx <- idx + 1
}

getFiles <- function(urls, dest_files, var_names){
  
  dataList <- vector("list", length = length(dest_files))
  
  for (idx in 1:length(dest_files)){
    if (!file.exists(dest_files[[idx]])){
      curl::curl_download(urls[[idx]], dest_files[[idx]])
    }
    
    suppressMessages(dataList[[idx]] <- read_excel(dest_files[[idx]],
                     sheet = 1,
                     skip = 5))
    
    # assign(var_names[[idx]], df, envir = parent.frame())
    
  }
  
  names(dataList) <- var_names
  
  return(dataList)
  
  }

hps_vaccData <- getFiles(url_vacc, dest_vacc, vars_vacc)
hps_emp1Data <- getFiles(url_emp1, dest_emp1, vars_emp1)
hps_emp2Data <- getFiles(url_emp2, dest_emp2, vars_emp2)

# Where to save the data? 
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/HPS/Proc")
fname <- "HPS_data.RData"
save(hps_vaccData, hps_emp1Data,hps_emp2Data, file = file.path(proc_data_path, fname))
