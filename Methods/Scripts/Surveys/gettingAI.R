
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
    print(glue("No sheet Q73 (i.e. vaccination data) for {file}"))
  }
  
  return(df)
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
  dataList <- vector("list", length = length(ai_periods$Period)) #list() 
  
  # File number
  fileNum <- 0
  
  for (file in ai_files){
    
    fileNum <- fileNum + 1
    
    date <- str_match(file, pattern = " .*\\d")
    date <- str_replace(date, " ", "")
    
    period <- as.numeric(str_extract(ai_periods$Period[ai_periods$Date == date], pattern = '\\d+'))
    
    # Determine name of file (based on period and whether the A/I week is unique within that period).
    temp_name <- glue("P{period}_{table}_1")
      
      paste0(period, "_", table, "_",1)
    
    if (temp_name %in% dfNames){
      dfNames[fileNum] <- glue("P{period}_{table}_2")
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
    }
    
    if (table == "vacc"){
      print(glue("Getting vaccination data from {file} ..."))
      suppressMessages(dataList[[fileNum]] <- getVacc(dir_path, file))
    }
      
    print(glue("Done\n\n"))
    
  }
  
  # There is no vaccination data in the first A/I week, 
  # so this needs to be removed from data list and dfNames
  if(table == "vacc"){
    dfNames <- dfNames[dfNames != "P1_vacc_1"]
    dataList[['P1_emp_1']] <- NULL
  }
  
  names(dataList) <- dfNames    
  
  return(dataList)
  
}


# Where is the data? (i.e. excel files)
data_path <- file.path(rootDir,"Methods/Data/Surveys/Axios-Ipsos/Raw")

# Get a list of all the files in the dir.
ai_files <- list.files(data_path, pattern = "^Axios")


# Get all the employment and vaccination sheets.
empData <- getAllSheets(data_path, ai_files, ai_periods, "emp")
vaccData <- getAllSheets(data_path, ai_files, ai_periods, "vacc")

# Where to save the data? 
proc_data_path <- file.path(rootDir,"Methods/Data/Surveys/Axios-Ipsos/Proc")
fname <- "AI_data.RData"
save(empData, vaccData, file = file.path(proc_data_path, fname))


