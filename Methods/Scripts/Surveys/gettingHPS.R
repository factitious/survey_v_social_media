rm(list = ls())
library(readxl)
library(tidyverse)
library(data.table)
library(glue)

data_path <- '/Users/munchausend/GoogleDrive/MSc/Dissertation/Methods/Data/HPS/Raw/'

first_week <- 18
last_week <- 28

hps_weeks <- seq(from = first_week, to = last_week, by = 1)
# hps_periods <- seq(from = 1, to = (last_week - first_week)+1, by = 1)

url_emp1 <- vector(mode = "list", length = length(hps_weeks))
dest_emp1 <- vector(mode = "list", length = length(hps_weeks))
vars_emp1 <- vector(mode = "list", length = length(hps_weeks))

url_emp2 <- vector(mode = "list", length = length(hps_weeks))
dest_emp2 <- vector(mode = "list", length = length(hps_weeks))
vars_emp2 <- vector(mode = "list", length = length(hps_weeks))

url_vacc <- vector(mode = "list", length = length(hps_weeks) - 4)
dest_vacc <- vector(mode = "list", length = length(hps_weeks) - 4)
vars_vacc <- vector(mode = "list", length = length(hps_weeks) - 4)


idx <- 1
idx_vac <- 1

for (week in hps_weeks){
  
  if (week <= 21){
    year <- "2020"
  } else {
    year <- "2021"
  }
  
  url_emp1[idx] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/employ1_week{week}.xlsx")
  
  url_emp2[idx] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/employ2_week{week}.xlsx")
  
  dest_emp1[idx] <- glue("{data_path}P{idx}_emp1.xlsx")
  vars_emp1[idx] <- glue("P{idx}_emp1")
  
  dest_emp2[idx] <- glue("{data_path}P{idx}_emp2.xlsx")
  vars_emp2[idx] <- glue("P{idx}_emp2")
  
  if (week > 21){
    url_vacc[idx_vac] <- glue("https://www2.census.gov/programs-surveys/demo/tables/hhp/{year}/wk{week}/health5_week{week}.xlsx")
    dest_vacc[idx_vac] <- glue("{data_path}P{idx}_vacc.xlsx")
    vars_vacc[idx_vac] <- glue("P{idx}_vacc")
    idx_vac <- idx_vac + 1
  }
  
  idx <- idx + 1
}

getFiles <- function(urls, dest_files, var_names){
  
  for (idx in 1:length(dest_files)){
    if (!file.exists(dest_files[[idx]])){
      curl::curl_download(urls[[idx]], dest_files[[idx]])
    }
    
    df <- read_excel(dest_files[[idx]],
                     sheet = 1,
                     skip = 5)
    
    assign(var_names[[idx]], df, envir = parent.frame())
  }}



cleanEmp1 <- function(df){
  
  # Remove all rows that are completely empty:
  df <- df %>% 
    filter_all(any_vars(complete.cases(.)))
  
  # Change column names and select only the ones we're interested in (i.e. total/yes/no)
  df <- df %>% 
    rename(demog = "...1",
           income_loss_total = "...2",
           income_loss_yes = "Yes...3",
           income_loss_no = "No...4",
           income_loss_nr = "Did not report...5") %>% 
    select(demog, income_loss_total, income_loss_yes, income_loss_no)
  
  # Select only the rows we're interested in (i.e. the first 10) and remove "Age" and "Sex" columns.
  # These will include total counts, as well as counts based on age and sex.
  df <- df %>% 
    slice(1:10) %>% 
    filter(demog != "Age" & demog != "Sex")
  
  # Calculate the ratio we're interested in based on total counts, as well as for each age group and sex in part
  # Ratio: Proportion of individuals that experienced loss of income out of all respondents (that reported on this measure)
  df <- df %>% 
    mutate(income_loss = income_loss_yes/(income_loss_yes + income_loss_no)) %>% 
    select(demog, income_loss)
  
  # Convert to wide (this makes more sense for combining all the periods)
  df_wide <- spread(df, demog, income_loss)
  
  return (df_wide)
}

cleanEmp2 <- function(df){
  
  # Remove all rows that are completely empty:
  df <- df %>% 
    filter_all(any_vars(complete.cases(.)))
  
  
  df <- df %>%
    slice(2:11) %>% # Select only rows we're interested in (i.e. 2 - 11)
    filter(...1 != "Age" & ...1 != "Sex") %>% # and remove "Age" and "Sex" columns.
    mutate(across(contains("Yes"),as.double)) %>% # change column type from 'char' to 'double'
    mutate(Yes = rowSums(select(., contains("Yes")))) %>% # sum up all the "Yes" columns (we don't care about sector)
    # Change var names
    mutate(demog = ...1, 
           emp_loss_total = ...2,
           emp_loss_yes = Yes,
           emp_loss_no = No) %>%
    mutate(unemployed = emp_loss_yes/(emp_loss_yes + emp_loss_no)) %>% # Calculate ratio
    select(demog, unemployed)
  
  # # Convert to wide (this makes more sense for combining all the periods)
  df_wide <- spread(df, demog, emp)
  
  return (df_wide)
}

cleanVacc <- function(df, phase){
  
  df <- df %>% 
    filter_all(any_vars(complete.cases(.))) 
  
  df <- df %>% 
    slice(3:11) %>% 
    filter(...1 != "Age" & ...1 != "Sex")
  
  if (phase == "3.0"){
    df <- df %>% 
      rename(demog = ...1,
             r_total = ...2,
             yes_total = Yes...3,
             yes_all = Yes...4,
             yes_some = Yes...5,
             yes_nr = Yes...6,
             no_total = No...7,
             no_def_will = No...8,
             no_prob_will = No...9,
             no_prob_not = No...10,
             no_def_not = No...11,
             no_nr = No...12,
             total_nr = `Did not report`
      )
    
  }
  
  if (phase == "3.1"){
    
    df <- df %>% 
      rename(demog = ...1,
             r_total = ...2,
             yes_total = Yes...3,
             yes_all = Yes...4,
             yes_some = Yes...5,
             yes_nr = Yes...6,
             no_total = No...7,
             no_def_will = No...8,
             no_prob_will = No...9,
             no_unsure = No...10,
             no_prob_not = No...11,
             no_def_not = No...12,
             no_nr = No...13,
             total_nr = `Did not report`
      )
  }  
  
  
  df <- df %>% 
    mutate(across(r_total:total_nr, as.double))
  
  return (df)
}

allPeriods <- function(var_names, table){
  
  
  for (i in 1:length(var_names)){
    
    # Clean df corresponding to each period.
    
    if (table == "emp1"){
      df_i = cleanEmp1(get(var_names[[i]]))
    }
    
    if (table == "emp2"){
      df_i = cleanEmp2(get(var_names[[i]]))
    }
    
    if (table == "vacc"){
      if (i > 6){
        df_i = cleanVcc(get(var_names[[i]]), phase = "3.0")
      } else {
        df_i = cleanVcc(get(var_names[[i]]), phase = "3.1")
      }
      
    }
    
    
    # Get period from name.
    p <- sub("\\_.*", "", var_names[[i]])
    
    # Add a column corresponding to the period
    df_i <- df_i %>% 
      add_column(Period = p, .before = 1)
    
    # Initialize df for storing all periods on the first iteration, bind rows on subsequent iterations.
    if (i == 1){
      df_all_periods <- df_i
    } else {
      df_all_periods <- bind_rows(df_all_periods, df_i)
    }
    
  }
  
  return(df_all_periods)
  
}

getFiles(url_vacc, dest_vacc, vars_vacc)
getFiles(url_emp1, dest_emp1, vars_emp1)
getFiles(url_emp2, dest_emp2, vars_emp2)