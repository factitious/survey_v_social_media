
# Setup 

# Clear env
rm(list = ls())

# Load packages
library(readxl)
library(tidyverse)
library(data.table)
library(glue)
library(lubridate)
library(docstring)
library(glue)

# Set root directory.
rootDir <- '/Volumes/Survey_Social_Media_Compare'

# Load sheet holding survey periods
s_periods_fname <- "Methods/Scripts/Surveys/table_details/surveyPeriods.xlsx"

surveyPeriods <- read_excel(file.path(rootDir, s_periods_fname), 
                            sheet = "AI+HPS", 
                            col_types = c("text", 
                                          "date", 
                                          "date", 
                                          "text", 
                                          "date", 
                                          "date", 
                                          "text",
                                          "text",
                                          "text"))

# Get month and day when of the A/I period end (this is how files are named)
ai_dates <- paste0(month(surveyPeriods$A_I_end_date,label = TRUE, abbr = FALSE), " " ,day(surveyPeriods$A_I_end_date))

# Make df with the corresponding HPS period + the date identified above.
# Will use this to find the correct files.
ai_periods <- data.frame(Period = surveyPeriods$Period, 
                         Date = ai_dates)

# Get the correct sheets from the A/I survey data

# Functions.
# Geting the employment data.
getEmp <- function(dir_path, file){
  #' Getting the employment sheet from the Axios-Ipsos survey data for a specific A/I "week"
  #' Input: dir_path (chr), path to the data.
  #'        file (chr), file name.
  #' Output: df (data.frame), dataframe of the sheet containing the employment data from one A/I "week" 
  
  # Specify full path.
  fpath <- file.path(dir_path, file)
  
  # Read the correct sheet.
  # The naming (for the relevant sheet) changed from "PPWORK" to "PPEMPLOY", early on.
  # Only one of these will be present, so 2 try's seems like the most straightforward way to handle it.
  # TODO: re-write using purr's possibly()?
  try(df <- read_excel(fpath,
                       sheet = "PPEMPLOY", 
                       skip = 9, 
                       n_max = 12), silent = TRUE)
  
  try(df <- read_excel(fpath,
                       sheet = "PPWORK", 
                       skip = 9, 
                       n_max = 24), silent = TRUE)
  return(df)
  
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
      select(Working, `Not working`) %>% 
      mutate(unemployed = `Not working`/(`Not working` + Working)) %>% 
      select(unemployed)
    
  } else{
    
    cDF <- cDF %>% 
      mutate(Working = `Working full-time` + `Working part-time`) %>% 
      select(Working, `Not working`) %>% 
      mutate(unemployed = `Not working`/(`Not working` + Working)) %>% 
      select(unemployed)
    
  } 
  
  cDF <- cDF %>% 
    add_column(Status = rownames(cDF), .before = 1)
  
  rownames(cDF) <- NULL
  
  return(cDF)
  
}


# Getting vaccination data
getVacc <- function(dir_path, file){
  #' Getting the vaccination sheet from the Axios-Ipsos survey data for a specific A/I "week"
  #' Input: dir_path (chr), path to the data.
  #'        file (chr), file name.
  #' Output: df (data.frame), dataframe of the sheet containing the employment data from one A/I "week" 
  
  # Specify full path
  fpath <- file.path(dir_path, file)
  
  # Get the name of the tabs in the spreadsheet.
  tab_names <- excel_sheets(fpath)
  
  if ("Q73" %in% tab_names){
    
    # Read in relevant sheet.
    df <- read_excel(fpath,
                     sheet = "Q73",
                     skip = 9,
                     n_max = 34)
    
  } else {
    df <- NULL
    print(glue("No sheet Q73 for {file}"))
  }
  
  return(df)
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

getAllSheets <- function(dir_path, ai_files, ai_periods, table){
  #' 
  #' Input: dir_path (chr), path to the data.
  #'        ai_file (chr), all file names, e.g. ai_files[1] >> "Axios-Ipsos April 19 Crosstabs.xlsx"
  #'        ai_periods (data.frame): period and end date of each A/I "week".
  #'        table (chr): "emp" or "vacc", depending on which sheets you are getting. 
  #' Output: dataList (list), each element is a dataframe containing emp/vacc data extracted for an A/I week.

  # Initialize vector for holding df names
  dfNames <- character(length(ai_files))
  
  # Initialize empty list for holding variables.
  dataList <- vector("list", length(ai_periods$Period))#list() 
  
  # Which periods are unique
  unique_periods <- ai_periods$Period[!ai_periods$Period %in% ai_periods$Period[duplicated(ai_periods$Period)]]
  
  # File number
  fileNum <- 0
  
  for (file in ai_files){
    
    fileNum <- fileNum + 1
    
    date <- str_match(file, pattern = " .*\\d")
    date <- str_replace(date, " ", "")
    
    period <- ai_periods$Period[ai_periods$Date == date]
    
    # Determine name of file (based on period and whether the A/I week is unique within that period).
    
    temp_name <- paste0(period, "_", table, "_",1)
    
    if (temp_name %in% dfNames){
      dfNames[fileNum] <- paste0(period, "_", table, "_",2)
    } else{
      dfNames[fileNum] <- temp_name
    }
    
    
    # TODO: This is actually a bit inefficient, as we are opening the same file twice 
    # (once for employment and once for vaccination). The main reason for doing this was
    # that R functions can't really return more than 1 "object", but can be fixed by
    # using a list-of-lists, e.g. dataEmp and dataVacc each containing n dataframes 
    # (where n is the # of periods)
    
    # Get the right sheet
    if (table == "emp"){
      print(glue("Getting employment data from {file} ..."))
      suppressMessages(dataList[[fileNum]] <- getEmp(dir_path, file))
      # Clean df here
      # dataList[[fileNum]] <- cleanEmp(dataList[[fileNum]], period)
    }
    
    if (table == "vacc"){
      print(glue("Getting vaccination data from {file} ..."))
      suppressMessages(dataList[[fileNum]] <- getVacc(dir_path, file))
      # Clean df here
      #df <- cleanVacc(df)
    }
    
    print(glue("Done\n\n"))
    
  }
  
  names(dataList) <- dfNames
  
  dataList <- dataList[order(names(dataList))]
  
  return(dataList)
  
}


# Where is the data? (i.e. excel files)
data_path <- file.path(rootDir,"Methods/Data/Surveys/Axios-Ipsos/Raw")

# List all files
ai_files <- list.files(data_path, pattern = "^Axios")

empData <- getAllSheets(data_path, ai_files, ai_periods, "emp")
save(empData, file = 'AI_empData.RData')

vaccData <- getAllSheets(data_path, ai_files, ai_periods, "vacc")
save(vaccData, file = 'AI_empData.RData')

getIndices <- function(df, table){
  
  
}


