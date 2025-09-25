#' @title Qualtrics Weekly - send survey reminder through emails
#' @param None
#' @export
#' @concept role:qualtrics_weekly
#' @concept removedDependencies:false
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:false
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
qualtrics_send_reminder_email <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate)

  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R")
  # source(file = sourcePath, echo = FALSE)
  #
  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R")
  # source(file = sourcePath, echo = FALSE)

  # get date of this week
  current_date <- Sys.Date()
  start_of_current_week <- floor_date(current_date, unit = "week")

  # Check if Log has already been collected
  logfile <- get_weekly_log()

  # check if survey hasn't been retrieved
  responseData <- logfile %>% dplyr::filter(OriginDate == start_of_current_week & Action == "RetrieveResponse")
  if (responseData %>% nrow <= 0) {
    return()
  }

  # check if survey reminder already sent
  if (check_exist_in_log(logfile, start_of_current_week, "SendReminder")) {
    return()
  }

  # get info
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  directoryId <- Sys.getenv("QUALTRICS_DIRECTORY_ID")
  libraryId <- Sys.getenv("QUALTRICS_LIBRARY_ID")
  messageId <- Sys.getenv("QUALTRICS_REMINDER_MESSAGE_ID")

  # retrieve surveyID from logfile, through "CreateSurvey" tag
  surveyId <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")

  # get response data file name
  dataFileName <- responseData$SaveData #get response data file name
  if (length(dataFileName) > 1) { # if more than one response
    dataFileName <- dataFileName[[length(dataFileName)]] # get last response
  }

  # get unresponsed personnel
  unresponsedPersonel <- get_unresponsed_personnel_list(dataFileName)
  print(unresponsedPersonel)

  if (unresponsedPersonel %>% nrow() == 0) { # no unresponsed personnel -> great, just stop function
    write_to_weekly_log(start_of_current_week, "SendReminder", surveyId)
    return()
  }

  # generate mailing list
  mailingId <- create_and_add_contact_from_personnel_list(
    qualtKey, directoryId,
    paste("Reminder: Weekly mailing list - ", start_of_current_week, sep = ""),
    unresponsedPersonel
  )

  # distribute
  distribute_qualtrics_survey(
    qualtKey,
    libraryId,
    messageId,
    mailingId,
    paste("Reminder: Weekly monitor health check survey - week of ", start_of_current_week, sep = ""),
    surveyId,
    isMultiple = TRUE
  )

  # save to log
  write_to_weekly_log(start_of_current_week, "SendReminder", surveyId)
}
#qualtrics_send_reminder_email()

# Tested: 20 Jan 2025
