qualtrics_send_weekly_survey <- function() {
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
  
  # check if survey hasn't been published
  if (check_not_exist_in_log(logfile, start_of_current_week, "CreateSurvey")) {
    return()
  }
  
  # check if survey already send
  if (check_exist_in_log(logfile, start_of_current_week, "SendSurvey")) {
    return()
  }
  
  # get info
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  directoryId <- Sys.getenv("QUALTRICS_DIRECTORY_ID")
  libraryId <- Sys.getenv("QUALTRICS_LIBRARY_ID")
  messageId <- Sys.getenv("QUALTRICS_MESSAGE_ID")
  
  # retrieve survey id from logfile, through "CreateSurvey" tag
  surveyId <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")
  
  # retrieve participants from CSV file
  participants <- get_weekly_personnel_list() 
  
  # generate mailing list
  mailingId <- create_and_add_contact_from_personnel_list(
    qualtKey, directoryId,
    paste("Weekly mailing list - ", start_of_current_week, sep = ""),
    participants
  )
  
  distribute_qualtrics_survey(
    qualtKey,
    libraryId,
    messageId,
    mailingId,
    paste("Weekly monitor health check survey - week of ", start_of_current_week, sep = ""),
    surveyId,
    isMultiple = TRUE
  )

  # save to log
  write_to_weekly_log(start_of_current_week, "SendSurvey", surveyId)

  # delete mailing list - Proposal
}
qualtrics_send_weekly_survey()

# Tested: 19 Jan 2025