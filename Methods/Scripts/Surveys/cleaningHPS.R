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
    filter(demog != "Age" & demog != "Sex")
  
  # Calculate the ratio we're interested in based on total counts, as well as for each age group and sex in part
  # Ratio: Proportion of individuals that experienced loss of income out of all respondents (that reported on this measure)
  # df <- df %>% 
  #   mutate(income_loss = income_loss_yes/(income_loss_yes + income_loss_no)) %>% 
  #   select(demog, income_loss)
  
  # # Convert to wide (this makes more sense for combining all the periods)
  # df_wide <- spread(df, demog, income_loss)
  
  return (df)
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
           emp_loss_no = No) 
  
  # %>%
  #   mutate(unemployed = emp_loss_yes/(emp_loss_yes + emp_loss_no)) %>% # Calculate ratio
  #   select(demog, unemployed)
  
  # # # Convert to wide (this makes more sense for combining all the periods)
  # df_wide <- spread(df, demog, emp)
  
  return (df)
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

allPeriods <- function(dataList, table, bind = "False"){
  
  cleanData <- vector('list', length = length(dataList))
  
  for (i in 1:length(dataList)){
    
    # Clean df corresponding to each period.
    
    if (table == "emp1"){
      cleanData[[i]] <- cleanEmp1(dataList[[i]])
    }
    
    if (table == "emp2"){
      cleanData[[i]] <- cleanEmp2(dataList[[i]])
    }
    
    if (table == "vacc"){
      if (i > 6){
        cleanData[[i]] <- cleanVacc(dataList[[i]], phase = "3.1")
      } else {
        cleanData[[i]] <- cleanVacc(dataList[[i]], phase = "3.0")
      }
      
    }
     
    names(cleanData) <- names(dataList)
    
  }
  
  return(cleanData)
  
}

hps_cleanEmp1 <- allPeriods(hps_emp1Data, "emp1")
hps_cleanEmp2 <- allPeriods(hps_emp2Data, "emp2")
hps_cleanVacc <- allPeriods(hps_vaccData, "vacc")

# Where to save the data? 
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/HPS/Proc")
fname_clean <- "clean_HPS_data.RData"
save(hps_cleanEmp1, hps_cleanEmp2, hps_cleanVacc, file = file.path(proc_data_path, fname_clean))


