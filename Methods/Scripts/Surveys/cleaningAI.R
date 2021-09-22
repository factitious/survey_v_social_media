rootDir <- '/Volumes/Survey_Social_Media_Compare'
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/Axios-Ipsos/Proc")
fname <- "AI_data.RData"

# Load the data (if it isn't already)

if(!exists('empData') && !exists('vaccData')){
  
  load(file.path(proc_data_path, fname))
  
}


cleanEmp <- function(df, period){
  #' Clean employment dataframe (for one A/I week).
  #' NB: Reporting changes after period 8 (i.e. March 1st); 
  #'     'Not working' becomes a single category, whereas before it was 5 (temp layoff, looking, retired, disabled)
  #'     Since we don't the more granular information starting March, it makes sense to combine them for when they are available.
  
  # Remove "*" characters from the data.
  df[] <- lapply(df, gsub, pattern = '\\*', replacement = '')
  
  df <- df %>% 
    filter(!is.na(...1)) %>% # Remove rows where the first col is na 
    rename(Status = ...1,
           Total = ...2) %>% # Rename columns
    select(!(contains("Yes") | contains("No"))) %>%  # Select only the columns that don't contain "Yes" or "No", we're not really interested in this.
    mutate(across(!contains("Status"),as.double)) 
  
  cNames <- df$Status
  df$Status <- NULL
  
  return(df)
  
  # Transpose the df for calculations (working with rows is an absolute pain in R)
  cDF <- as.data.frame(t(as.matrix(df)))
  colnames(cDF) <- cNames
  
  
  if (period < 9){
    
    cDF <- cDF %>% 
      mutate(Working = `Working - self-employed` + `Working - as a paid employee`,
             `Not working` = `Not working - on temporary layoff from a job` + 
               `Not working - looking for work` +
               `Not working - retired` +
               `Not working - disabled` +
               `Not working - other`) %>% 
      select(Working, `Not working`) 
    
  } else{
    
    cDF <- cDF %>% 
      mutate(Working = `Working full-time` + `Working part-time`) %>% 
      select(Working, `Not working`) 
  } 
  
  cDF <- cDF %>% 
    add_column(Demog = rownames(cDF), .before = 1)
  
  rownames(cDF) <- NULL
  
  cDF_wide <- cDF %>% 
    mutate(perc_employed = Working/(Working + `Not working`)) %>% 
    select(Demog, perc_employed)
  
  cDF_wide <- spread(cDF_wide, Demog, perc_employed)
  
  
  return(cDF_wide)
  
}

cleanEmp2 <- function(df, period){
  #' Clean employment dataframe (for one A/I week).
  #' NB: Reporting changes after period 8 (i.e. March 1st); 
  #'     'Not working' becomes a single category, whereas before it was 5 (temp layoff, looking, retired, disabled)
  #'     Since we don't the more granular information starting March, it makes sense to combine them for when they are available.
  
  # Remove "*" characters from the data.
  df[] <- lapply(df, gsub, pattern = '\\*', replacement = '')
  
  df <- df %>% 
    filter(!is.na(...1)) %>% # Remove rows where the first col is na 
    rename(Status = ...1,
           Total = ...2) %>% # Rename columns
    select(!(contains("Yes") | contains("No"))) %>%  # Select only the columns that don't contain "Yes" or "No", we're not really interested in this.
    mutate(across(!contains("Status"),as.double)) 
  
  cNames <- df$Status
  df$Status <- NULL
  
  df <- df %>% 
    select(Total, `18-29`, `30-49`) %>% 
    mutate(`18-49` = `18-29` + `30-49`) %>% 
    select(Total, `18-49`)
  
  
  # Transpose the df for calculations (working with rows is an absolute pain in R)
  cDF <- as.data.frame(t(as.matrix(df)))
  colnames(cDF) <- cNames
  
  
  if (period < 9){
    
    cDF <- cDF %>% 
      mutate(Working = `Working - self-employed` + `Working - as a paid employee`,
             `Not working` = `Not working - on temporary layoff from a job` + 
               `Not working - looking for work` +
               `Not working - retired` +
               `Not working - disabled` +
               `Not working - other`) %>% 
      select(Working, `Not working`) 
    
  } else{
    
    cDF <- cDF %>% 
      mutate(Working = `Working full-time` + `Working part-time`) %>% 
      select(Working, `Not working`) 
  } 
  
  cDF <- cDF %>% 
    add_column(Demog = rownames(cDF), .before = 1)
  
  rownames(cDF) <- NULL
  
  cDF_wide <- cDF %>% 
    mutate(perc_employed = Working/(Working + `Not working`)) %>% 
    select(Demog, perc_employed)
  
  cDF_wide <- spread(cDF_wide, Demog, perc_employed)
  
  
  return(cDF_wide)
  
}

cleanVacc <- function(df, period){
  #' Clean vaccination dataframe
  
  df[] <- lapply(df, gsub, pattern='\\*', replacement='')
  
  df <- df %>% 
    filter(!is.na(...1)) %>% 
    rename(Status = ...1,
           Total = ...2) %>% 
    select(!(contains(c("Yes", "No")))) %>% 
    mutate(across(!contains('status'), as.double))
  
  cNames <- df$Status
  df$Status <- NULL
  cDF <- as.data.frame(t(as.matrix(df)))
  colnames(cDF) <- cNames
  cDF <- cDF %>% 
    select(!contains(c("Mentions", "Skipped", "Top", "Base", "(net)")))
  
  cDF <- cDF %>%
    add_column(demog = rownames(cDF), .before = 1)

  rownames(cDF) <- NULL
  
  
  if('Received vaccine' %in% names(cDF)){
    cDF_wide <- cDF %>%
      mutate(across(`Received vaccine`:`Not at all likely`, as.double)) %>% 
      mutate(
        s = (
          1  * `Received vaccine` +
          1  * `Very likely` +
         0.5 * `Somewhat likely` +
        -0.5 * `Not very likely` +
         -1  * `Not at all likely`
        ) /  rowSums(cDF %>% select(-demog)) 
      ) %>% 
      select(demog, s)
  } else {
    cDF_wide <- cDF %>%
      mutate(across(`Very likely`:`Not at all likely`, as.double)) %>% 
      mutate(
        s = (
            1  * `Very likely` +
           0.5 * `Somewhat likely` +
          -0.5 * `Not very likely` +
           -1  * `Not at all likely`
        ) /  rowSums(cDF %>% select(-demog))
      ) %>% 
      select(demog, s)
  }


  cDF_wide <- spread(cDF_wide, demog, s)
  
  return(cDF_wide)
  
}

cleanVacc2 <- function(df, period){
  #' Clean vaccination dataframe
  
  df[] <- lapply(df, gsub, pattern='\\*', replacement='')
  
  df <- df %>% 
    filter(!is.na(...1)) %>% 
    rename(Status = ...1,
           Total = ...2) %>% 
    select(!(contains(c("Yes", "No")))) %>% 
    mutate(across(!contains('status'), as.double))
  
  cNames <- df$Status
  df$Status <- NULL
  
  df <- df %>%
    select(Total, `18-29`, `30-49`) %>%
    mutate(`18-49` = `18-29` + `30-49`) %>%
    select(Total, `18-49`)
  
  
  cDF <- as.data.frame(t(as.matrix(df)))
  # colnames(cDF) <- cNames
  cDF <- cDF %>% 
    select(!contains(c("Mentions", "Skipped", "Top", "Base", "(net)")))
  
  cDF <- cDF %>%
    add_column(demog = rownames(cDF), .before = 1)
  
  rownames(cDF) <- NULL
  
  
  if('Received vaccine' %in% names(cDF)){
    cDF_wide <- cDF %>%
      mutate(across(`Received vaccine`:`Not at all likely`, as.double)) %>% 
      mutate(
        s = (
          1  * `Received vaccine` +
            1  * `Very likely` +
            0.5 * `Somewhat likely` +
            -0.5 * `Not very likely` +
            -1  * `Not at all likely`
        ) /  rowSums(cDF %>% select(-demog)) 
      ) %>% 
      select(demog, s)
  } else {
    cDF_wide <- cDF %>%
      mutate(across(`Very likely`:`Not at all likely`, as.double)) %>% 
      mutate(
        s = (
          1  * `Very likely` +
            0.5 * `Somewhat likely` +
            -0.5 * `Not very likely` +
            -1  * `Not at all likely`
        ) /  rowSums(cDF %>% select(-demog))
      ) %>% 
      select(demog, s)
  }
  
  
  cDF_wide <- spread(cDF_wide, demog, s)
  
  return(cDF_wide)
  
}


cleanAI <- function(empData, vaccData){
  
  cleanEmpData <- vector("list", length = length(empData))
  cleanVaccData <- vector("list", length = length(vaccData))
  empPeriods <- as.numeric(str_extract(names(empData), pattern = "\\d+"))
  vaccPeriods <- as.numeric(str_extract(names(vaccData), pattern = "\\d+"))
  
  for (idx in 1:length(empData)){
    
    try(cleanEmpData[[idx]] <- cleanEmp2(empData[[idx]], empPeriods[idx]))
    
  }
  
  for (idx in 1:length(vaccData)){
    
    try(cleanVaccData[[idx]] <- cleanVacc2(vaccData[[idx]], vaccPeriods[idx]))
    
  }
  
  names(cleanEmpData) <- names(empData)
  names(cleanVaccData) <- names(vaccData)
  
  
  cleanDataAll <- list(cleanEmpData, cleanVaccData)
  names(cleanDataAll) <- c("empData", "vaccData")
  
  return(cleanDataAll)
  
}


agg_all_periods <- function(cdl){
  
  p_names <- names(cdl)
  
  for(i in 1:length(cdl)){
    
    p <- str_extract(
      sub('\\_.*', '', p_names[i]),
      "[[:digit:]]+"
    )
    
    df_i <- cdl[[i]] %>% 
      add_column(Period = p, .before = 1)
    
    if(i==1){
      agg_df <- df_i
    } else{
      agg_df <- bind_rows(agg_df, df_i)
    }
    
  } 
  
  agg_df$Period <- as.double(agg_df$Period)
  
  agg_df <- agg_df[order(agg_df$Period), ]
  
  return(agg_df)
  
}

allDataClean <- cleanAI(empData, vaccData)
cleanEmpData <- allDataClean[['empData']]
cleanVaccData <- allDataClean[['vaccData']]

ai_cleanEmp_agg <- agg_all_periods(cleanEmpData)
ai_cleanVac_agg <- agg_all_periods(cleanVaccData)

fname_clean <- "clean_AI_data.RData"
save(cleanEmpData, cleanVaccData, file = file.path(proc_data_path,fname_clean))

agg_data_path <- file.path(rootDir,"Methods/Data/Surveys/Axios-Ipsos/Aggregate")

saveRDS(ai_cleanEmp_agg,
        file.path(
          agg_data_path,
          'emp.rds'
        ))

saveRDS(ai_cleanVac_agg,
        file.path(
          agg_data_path,
          'vac.rds'
        ))

