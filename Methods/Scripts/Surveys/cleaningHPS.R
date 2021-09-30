library(readxl)
library(tidyverse)
library(data.table)
library(glue)
 

rootDir <- '/Volumes/Survey_Social_Media_Compare'
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/HPS/Proc")
fname <- "HPS_data.RData"

# Load the data (if it isn't already)

if(!exists('hps_emp1Data') && !exists('hps_emp2Data') && !exists('hps_vaccData')){
  
  load(file.path(proc_data_path, fname))
  
}


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
    filter(demog != "Age" & demog != "Sex")=
  
  # Calculate the ratio we're interested in based on total counts, as well as for each age group and sex in part
  # Ratio: Proportion of individuals that experienced loss of income out of all respondents (that reported on this measure)
  # df <- df %>%
  #   mutate(income_loss = income_loss_yes/(income_loss_yes + income_loss_no)) %>%
  #   select(demog, income_loss)
  
  # Convert to wide (this makes more sense for combining all the periods)
  # df_wide <- spread(df, demog, income_loss)
  
  return (df)
  
  # NB: No longer using this one, so not worth aggregating.
}

cleanEmp2 <- function(df, type = 1){
  
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
           employed = Yes,
           unemployed = No) %>% 
    select(demog, emp_loss_total, employed, unemployed) 
  
  #### Insert type 1 to got back to the original (full demographic data)
  if(type == 2){
      cDF <- as.data.frame(t(as.matrix(df)))
      
      colnames(cDF) <- colnames(cDF) <- cDF[1,]
      
      cDF <- cDF %>% 
        slice(2:nrow(cDF))
      
      cDF <- cDF %>% mutate(across(.cols = everything(), as.numeric))
      
      cDF <- cDF %>% mutate(`18-54` = `18 - 24` + `25 - 39` + `40 - 54`) %>% 
        select(Total, `18-54`)
      
      df <- as.data.frame(t(as.matrix(cDF)))
      
      df$demog <- rownames(df)
      
      rownames(df) <- NULL
  }
  #####
  
  df_wide <- df %>% 
    mutate(perc_employed = employed/(employed + unemployed)) %>% 
    select(demog, perc_employed) 
  
  df_wide <- spread(df_wide, demog, perc_employed)
  
  return (df_wide)
}

cleanVacc <- function(df, phase, type = 1){
  
  # Type 1 returns a list of dataframes with clean data for each period.
  # Type 2 returns a list of lists, with each element containing a tibble with
  #   with demographic characteristics in each column and a single row, i.e. score.
  
  df <- df %>% 
    filter_all(any_vars(complete.cases(.))) 
  
  df <- df %>% 
    slice(2:11) %>% 
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
      ) %>% 
      mutate(across(r_total:total_nr, as.double))
    
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
      ) %>% 
      mutate(across(r_total:total_nr, as.double))
    
    
      
  }      
  
  if(type == 1){
    return(df)
  } else if(type ==2){
    
    ### Remove this bit to got back to original (full demog) df.
    cDF <- as.data.frame(t(as.matrix(df)))
    
    colnames(cDF) <- colnames(cDF) <- cDF[1,]
    
    cDF <- cDF %>% 
      slice(2:nrow(cDF))
    
    cDF <- cDF %>% mutate(across(.cols = everything(), as.numeric))
    
    cDF <- cDF %>% mutate(`18-54` = `18 - 24` + `25 - 39` + `40 - 54`) %>% 
      select(Total, `18-54`)
    
    df <- as.data.frame(t(as.matrix(cDF)))
    
    df$demog <- rownames(df)
    
    rownames(df) <- NULL
    ###
    
    df_s1 <- df %>% 
      mutate(
        s1 = (
          1  *  no_def_will +
         0.75 * no_prob_will +
        0.25 * no_prob_not +
         0 * no_def_not)/( no_def_will + no_prob_not + no_def_not)
      ) %>% 
      select(demog, s1)
    
    
    df_s2 <- df %>% 
      mutate(
        s2 = yes_total/(yes_total + no_total)
        ) %>% 
      select(demog, s2)
    
    df_s1 <- spread(df_s1, demog, s1)
    df_s2 <- spread(df_s2, demog, s2)
    return(list(s1 = df_s1, s2 = df_s2))
  }
}

all_periods <- function(dataList, table, bind = "False", vac_ctype = 2, emp_ctype = 2){
  
  cleanData <- vector('list', length = length(dataList))
  
  for (i in 1:length(dataList)){
    
    # Clean df corresponding to each period.
    
    if (table == "emp1"){
      cleanData[[i]] <- cleanEmp1(dataList[[i]])
    }
    
    if (table == "emp2"){
      cleanData[[i]] <- cleanEmp2(dataList[[i]], type = emp_ctype)
    }
    
    if (table == "vacc"){
      if (i > 6){
        cleanData[[i]] <- cleanVacc(dataList[[i]], phase = "3.1", type = vac_ctype)
      } else {
        cleanData[[i]] <- cleanVacc(dataList[[i]], phase = "3.0", type = vac_ctype)
      }
      
    }
     
    names(cleanData) <- names(dataList)
    
  }
  
  return(cleanData)
  
}

agg_all_periods <- function(cdl, table = 'emp'){
  
  p_names <- names(cdl)
  
  for(i in 1:length(cdl)){
    
    p <- str_extract(
      sub('\\_.*', '', p_names[i]),
      "[[:digit:]]+")
    
    if(table == "emp"){
        
          df_i <- cdl[[i]] %>% 
            add_column(Period = p, .before = 1)
          
          if(i==1){
            agg_df <- df_i
          } else{
             agg_df <- bind_rows(agg_df, df_i)
             }
    } else if(table == "vac"){
      
          p <- sub('\\_.*', '', p_names[i])
          
          df_i_s1 <- cdl[[i]]$s1 %>% 
            add_column(Period = p, .before = 1)
          
          df_i_s2 <- cdl[[i]]$s2 %>% 
            add_column(Period = p, .before = 1)
          
          if(i==1){
            agg_df_s1 <- df_i_s1
            agg_df_s2 <- df_i_s2
          } else{
            agg_df_s1 <- bind_rows(agg_df_s1, df_i_s1)
            agg_df_s2 <- bind_rows(agg_df_s2, df_i_s2)
          }
          
        }
  }
  
  if(table == 'emp'){
    return(agg_df)
  } else if(table == 'vac'){
    return(list(s1 = agg_df_s1, s2 = agg_df_s2))
  }
  
}

hps_cleanEmp1 <- all_periods(hps_emp1Data, "emp1")
hps_cleanEmp2 <- all_periods(hps_emp2Data, "emp2", emp_ctype = 2)
hps_cleanVacc <- all_periods(hps_vaccData, "vacc", vac_ctype = 1)
hps_cleanVacc2 <- all_periods(hps_vaccData, "vacc", vac_ctype = 2)

hps_cleanEmp2_agg <- agg_all_periods(hps_cleanEmp2)
hps_cleanVacc_agg <- agg_all_periods(hps_cleanVacc2, 'vac')


# Save the data.
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/HPS/Proc")
fname_clean <- "clean_HPS_data.RData"
save(hps_cleanEmp1, hps_cleanEmp2, hps_cleanVacc, file = file.path(proc_data_path, fname_clean))

agg_data_path <- file.path(rootDir, 'Methods/Data/Surveys/HPS/Aggregate')
saveRDS(hps_cleanEmp2_agg,
        file.path(
          agg_data_path,
          'emp.rds'
        ))

saveRDS(hps_cleanVacc_agg$s1,
        file.path(
          agg_data_path,
          'vac_s1.rds'
        ))

saveRDS(hps_cleanVacc_agg$s2,
        file.path(
          agg_data_path,
          'vac_s2.rds'
        ))




