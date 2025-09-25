#' @title Send Monthly Survey via Qualtrics API
#' @param None
#' @return None
#' @export
#' @concept role:qualtrics_monthly
#' @concept removedDependencies:false
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:false
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
qualtrics_send_monthly_survey <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, readr, readxl)

  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R")
  # source(file = sourcePath, echo = FALSE)
  #
  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R")
  # source(file = sourcePath, echo = FALSE)

  #readRenviron(".Rqualtrics")

  # get date of this month
  current_date <- Sys.Date()
  start_of_current_month <- floor_date(current_date, unit = "month")

  # Check if Log has already been collected
  logfile <- get_monthly_log()

  # check if survey hasn't been published
  if (check_not_exist_in_log(logfile, start_of_current_month, "CreateSurvey")) {
    return()
  }

  # check if survey already send
  if (check_exist_in_log(logfile, start_of_current_month, "SendSurvey")) {
    return()
  }

  # get info
  qualtKey <- Sys.getenv("QUALTRICS_API_KEY")
  directoryId <- Sys.getenv("QUALTRICS_DIRECTORY_ID")
  libraryId <- Sys.getenv("QUALTRICS_LIBRARY_ID")
  messageId <- Sys.getenv("QUALTRICS_MESSAGE_ID")

  # retrieve survey id from logfile, through "CreateSurvey" tag
  surveyId <- get_first_save_data_from_monthly_log(logfile, start_of_current_month, "CreateSurvey")

  # retrieve Analyst(s) from CSV file
  # NOTE: Currently there should only be one analyst (!)
  participants <- get_main_personnel_list(role = "Analyst")
  #print(participants)

  # generate mailing list
  mailingId <- create_and_add_contact_from_personnel_list(
    qualtKey, directoryId,
    paste("Monthly mailing list - ", start_of_current_month, sep = ""),
    participants
  )

  distribute_qualtrics_survey(
    qualtKey,
    libraryId,
    messageId,
    mailingId,
    "Monthly CAMN monitor Quality Assurance survey",
    surveyId,
    isMultiple = TRUE
  )

  # save to log
  write_to_monthly_log(start_of_current_month, "SendSurvey", surveyId)

  # # delete mailing list
  # delete_mailing_list(qualtKey, directoryId, mailingId)
}
#qualtrics_send_monthly_survey()

# Finish testing: 17 Mar 2024
