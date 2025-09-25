#' @title Qualtrics Weekly - concentrate data from responses
#' @description This function concentrates data from survey responses for weekly report generation.
#' It checks if the report has already been sent for the current week, retrieves response data,
#' merges it with personnel information, and prepares the data for report generation.
#' @param None
#' @concept role:qualtrics_weekly_helper
#' @concept removedDependencies:false
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:false
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
qualtrics_concentrate_data_from_responses_weekly <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, here, rmarkdown,
                 data.table, formattable, tidyr, shiny, DT, knitr, kableExtra,
                 tinytex, flextable, officer, stringr, rlang, mailR, geometry,
                 readr, readxl)

  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R")
  # source(file = sourcePath, echo = FALSE)
  #
  # sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R")
  # source(file = sourcePath, echo = FALSE)
  #
  # sourcePath <- file.path(getwd(), "Code", "EmailUtil.R")
  # source(file = sourcePath, echo = FALSE)

  # get date of this week
  current_date <- Sys.Date()
  start_of_current_week <- floor_date(current_date, unit = "week")

  # Check if Log has already been collected
  logfile <- get_weekly_log()

  # check if at least one instance of RetrieveResponse has run
  responseData <- logfile %>% dplyr::filter(OriginDate == start_of_current_week & Action == "RetrieveResponse")
  if (responseData %>% nrow <= 0) {
    return()
  }
  descriptionData <- logfile %>% dplyr::filter(OriginDate == start_of_current_week & Action == "RetrieveQuestionDesc")

  # check if report sent
  if (check_exist_in_log(logfile, start_of_current_week, "SendReport")) {
    return()
  }

  # get full response data
  dataFileName <- responseData$SaveData #get response data file name
  if (length(dataFileName) > 1) { # if more than one response
    dataFileName <- dataFileName[[length(dataFileName)]] # get last response
  }

  # get question description data
  descriptionFileName <- descriptionData$SaveData #get description data file name
  if (length(descriptionFileName) > 1) { # if more than one response
    descriptionFileName <- descriptionFileName[[length(descriptionFileName)]] # get last response
  }

  # responsed & unresponsed
  unresponsedPersonel <- get_unresponsed_personnel_list(dataFileName)
  responsedPersonel <- get_responsed_personnel_list(dataFileName)

  # add info to personnelInfo
  personnelInfo <- get_merge_personnel_sensor_list() %>%
    dplyr::mutate(Submitted = if_else(DeviceID %in% responsedPersonel$DeviceID, "Yes", "No"))

  # get question description file
  questionDesc <- get_question_descriptions(descriptionFileName)

  # get responses file
  responses <- get_processed_responses_list(dataFileName)

  get_answer_based_on_deviceID_and_main_questionTag <- function(email, deviceID, questionMainTag) {
    filteredSingleQuestionTag <- questionDesc %>%
      dplyr::filter(DeviceID == deviceID & grepl(questionMainTag, qname)) %>%
      dplyr::pull(qname)

    # In case the device isn't managed by this tag e.g. a Clarity ID but a PA question tag
    if (rlang::is_empty(filteredSingleQuestionTag)) {
      return(NA)
    }

    answerBaseOnTag <- responses %>%
      dplyr::filter(RecipientEmail == !!email) %>%
      dplyr::pull(!!filteredSingleQuestionTag)
    return(answerBaseOnTag)
  }

  # merge responses INTO responsedPersonel
  responsedPersonel <- responsedPersonel %>%
    dplyr::rowwise() %>%
    dplyr::mutate(PAQuestion1 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion1")) %>%
    dplyr::mutate(PAQuestion2 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion2")) %>%
    dplyr::mutate(PAQuestion3 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion3")) %>%
    dplyr::mutate(ClarityQuestion1 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion1")) %>%
    dplyr::mutate(ClarityQuestion2 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion2")) %>%
    dplyr::mutate(ClarityQuestion3 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion3")) %>%
    dplyr::ungroup()

  # Get weekly update question
  weeklyShortcode <- get_weekly_question_shortlist()

  return(list(
    PersonnelInfo = personnelInfo,
    Responsed = responsedPersonel,
    Unresponsed = unresponsedPersonel,
    QuestionDesc = questionDesc,
    RawResponses = responses,
    WeeklyShortlist = weeklyShortcode
  ))
}

#TODO: Install tinytex beforehand manually
#' @title Qualtrics Weekly - concentrate data from responses
#' @description This function concentrates data from survey responses for weekly report generation.
#' It checks if the report has already been sent for the current week, retrieves response data,
#' merges it with personnel information, and prepares the data for report generation.
#' @param None
#' @export
#' @concept role:qualtrics_weekly
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:false
#' @concept cleanupComments:false
#' @concept addRoxygenComments:true
qualtrics_generate_weekly_report <- function() {
  # get date of this week
  current_date <- Sys.Date()
  start_of_current_week <- floor_date(current_date, unit = "week")

  # get data
  data <- qualtrics_concentrate_data_from_responses_weekly()
  if (is.null(data)) {
    return()
  }

  # generate markdown report - PDF
  # TODO: Consider HTML in the future?
  pdfFilePath <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Weekly", "Reports",
                           paste("WeeklyCommunitySensorReport - ", start_of_current_week, ".pdf", sep = ""))

  rmarkdown::render(
    file.path("Code", "QualtricsCode", "QualtricsWeekly", "QualtricsWeeklyReportTemplate.Rmd"),
    params = list(
      title = paste("Qualtrics Weekly Sensor Status Report - week of ", start_of_current_week, sep = ""),
      subtitle = paste("Week of ", start_of_current_week, sep = ""),
      myData = data
    ),
    output_file = pdfFilePath
  )

  # Send email to Maintainer and Admin
  personnelInfo <- get_main_personnel_list(role = c("Maintainer", "Admin"))
  send_email_gmail(
    Sender = "@gmail.com",
    Recipients = personnelInfo %>% dplyr::pull("Email"),
    Subject = paste("RWeather - Weekly Community Sensor Report - ", start_of_current_week),
    Body = "Please review the report for this week Community QA process.\nSent by automation system.",
    AttachmentPaths = c(pdfFilePath),
    AttachmentNames = c("WeeklyReport.pdf")
  )

  # Save to log - Weekly Action Log
  write_to_weekly_log(
    start_of_current_week,
    "SendReport",
    paste("Qualtrics Weekly Sensor Health Report - week of ", start_of_current_week, ".pdf", sep = "")
  )
}
#qualtrics_generate_weekly_report()

# Tested: 20 Jan 2025

# FOR TESTING ONLY ####
#data <- qualtrics_concentrate_data_from_responses()
#questions <- get_all_survey_questions(Sys.getenv("QUALTRICS_API_KEY"), Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID"))
