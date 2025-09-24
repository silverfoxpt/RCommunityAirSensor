qualtrics_create_monthly_survey <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, readr, readxl)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R") 
  source(file = sourcePath, echo = FALSE)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R") 
  source(file = sourcePath, echo = FALSE)
  
  # get date of this month
  current_date <- Sys.Date()
  start_of_current_month <- floor_date(current_date, unit = "month") 
  
  # Check if Log has already been collected
  logfile <- get_monthly_log()
  
  # check if survey already created
  if (check_exist_in_log(logfile, start_of_current_month, "CreateSurvey")) {
    return()
  }
  
  #readRenviron(".Rqualtrics")
  
  # get info
  qualtKey      <- Sys.getenv("QUALTRICS_API_KEY")
  templateId    <- Sys.getenv("QUALTRICS_MONTHLY_TEMPLATE_ID")
  directoryId   <- Sys.getenv("QUALTRICS_DIRECTORY_ID")
  libraryId     <- Sys.getenv("QUALTRICS_LIBRARY_ID")
  messageId     <- Sys.getenv("QUALTRICS_MESSAGE_ID")
  
  # get qsf template file
  qsfData <- get_survey_qsf(
    qualtKey, 
    templateId, 
    paste("Monthly Community Sensor Data Quality Assurance survey - Month of ", start_of_current_month, sep = "") #survey new name
  )
  
  # create survey
  newSurveyId <- create_survey_qsf(
    qualtKey,
    qsfData
  )
  
  # write to weekly log
  write_to_monthly_log(start_of_current_month, 'CreateSurvey', newSurveyId)
  
  # set survey as active
  set_qualtrics_survey_active(qualtKey, newSurveyId)
  
  # publish survey
  publishInfo <- publish_survey(qualtKey, newSurveyId)
}
qualtrics_create_monthly_survey()

# Finish testing: 17 Mar 2025