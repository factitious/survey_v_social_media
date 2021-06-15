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
    
    # %>% 
    #   mutate(unemployed = `Not working`/(`Not working` + Working)) %>% 
    #   select(unemployed)
    
  } else{
    
    cDF <- cDF %>% 
      mutate(Working = `Working full-time` + `Working part-time`) %>% 
      select(Working, `Not working`) 
    
    # %>% # Calculate unemployment rate.
    #   mutate(unemployed = `Not working`/(`Not working` + Working)) %>% 
    #   select(unemployed)
    
  } 
  
  cDF <- cDF %>% 
    add_column(Demog = rownames(cDF), .before = 1)
  
  rownames(cDF) <- NULL
  
  return(cDF)
  
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
  
  return(cDF)
  
}



cleanAI <- function(empData, vaccData){
  
  cleanEmpData <- vector("list", length = length(empData))
  cleanVaccData <- vector("list", length = length(vaccData))
  empPeriods <- as.numeric(str_extract(names(empData), pattern = "\\d+"))
  vaccPeriods <- as.numeric(str_extract(names(vaccData), pattern = "\\d+"))
  
  for (idx in 1:length(empData)){
    
    try(cleanEmpData[[idx]] <- cleanEmp(empData[[idx]], empPeriods[idx]))
    
  }
  
  for (idx in 1:length(vaccData)){
    
    try(cleanVaccData[[idx]] <- cleanVacc(vaccData[[idx]], vaccPeriods[idx]))
    
  }
  
  names(cleanEmpData) <- names(empData)
  names(cleanVaccData) <- names(vaccData)
  
  
  cleanDataAll <- list(cleanEmpData, cleanVaccData)
  names(cleanDataAll) <- c("empData", "vaccData")
  
  return(cleanDataAll)
  
}

allDataClean <- cleanAI(empData, vaccData)
cleanEmpData <- allDataClean[['empData']]
cleanVaccData <- allDataClean[['vaccData']]

fname_clean <- "clean_AI_data.RData"
save(cleanEmpData, cleanVaccData, file = file.path(proc_data_path,fname_clean))
