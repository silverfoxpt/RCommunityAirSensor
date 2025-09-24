qualtrics_create_weekly_survey <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R") 
  source(file = sourcePath, echo = FALSE)
  
  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R") 
  source(file = sourcePath, echo = FALSE)
  
  # get date of this week
  current_date <- Sys.Date()
  start_of_current_week <- floor_date(current_date, unit = "week") 
  
  # Check if Log has already been collected
  logfile <- get_weekly_log()
  
  # check if survey already created
  if (check_exist_in_log(logfile, start_of_current_week, "CreateSurvey")) {
    return()
  }
  
  # get info
  qualtKey      <- Sys.getenv("QUALTRICS_API_KEY")
  templateId    <- Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID")
  directoryId   <- Sys.getenv("QUALTRICS_DIRECTORY_ID")
  libraryId     <- Sys.getenv("QUALTRICS_LIBRARY_ID")
  messageId     <- Sys.getenv("QUALTRICS_MESSAGE_ID")
  
  # get qsf template file
  qsfData <- get_survey_qsf(
    qualtKey, 
    templateId, 
    paste("Weekly Community Sensor Health Check survey - Week of ", start_of_current_week, sep = "") #survey new name
  )
  
  # create survey
  newSurveyId <- create_survey_qsf(
    qualtKey,
    qsfData
  )
  
  # write to weekly log
  write_to_weekly_log(start_of_current_week, 'CreateSurvey', newSurveyId)
  
  # set survey as active
  set_qualtrics_survey_active(qualtKey, newSurveyId)
  
  # publish survey
  publishInfo <- publish_survey(qualtKey, newSurveyId)
}
qualtrics_create_weekly_survey()

# Tested: 20 Jan 2025