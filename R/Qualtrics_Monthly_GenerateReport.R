qualtrics_concentrate_monthly_data_from_responses <- function() {
  # Installation and loading
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, glue, httr2, jsonlite, lubridate, here, rmarkdown,
                 data.table, formattable, tidyr, shiny, DT, knitr, kableExtra,
                 tinytex, flextable, officer, stringr, rlang, mailR, geometry,
                 readr, readxl)

  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsUtil.R")
  source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "QualtricsCode", "QualtricsSecondaryUtils.R")
  source(file = sourcePath, echo = FALSE)

  sourcePath <- file.path(getwd(), "Code", "EmailUtil.R")
  source(file = sourcePath, echo = FALSE)

  # get date of this month
  current_date <- Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  # Check if Log has already been collected
  logfile <- get_monthly_log()

  # check if at least one instance of RetrieveResponse has run
  responseData <- logfile %>% dplyr::filter(OriginDate == start_of_current_month & Action == "RetrieveResponse")
  if (responseData %>% nrow <= 0) {
    return()
  }
  descriptionData <- logfile %>% dplyr::filter(OriginDate == start_of_current_month & Action == "RetrieveQuestionDesc")

  # check if report sent
  if (check_exist_in_log(logfile, start_of_current_month, "SendReport")) {
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
  unresponsedPersonel <- get_unresponsed_analyst_list(dataFileName)
  responsedPersonel <- get_responsed_analyst_list(dataFileName)

  # add info to personnelInfo
  personnelInfo <- get_main_personnel_list(role = "Analyst") %>%
    dplyr::mutate(Submitted = if_else(Email %in% responsedPersonel$Email, "Yes", "No"))

  # get question description file
  questionDesc <- get_monthly_question_descriptions(descriptionFileName)

  # get responses file
  responses <- get_monthly_responses_list(dataFileName)

  # get monitor sites
  monitors <- get_monitor_sites()

  get_answer_based_on_deviceID_and_main_questionTag <- function(deviceID, questionMainTag) {
    filteredSingleQuestionTag <- questionDesc %>%
      dplyr::filter(DeviceID == deviceID & grepl(questionMainTag, qname)) %>%
      dplyr::pull(qname)

    # In case the device isn't managed by this tag e.g. a Clarity ID but a PA question tag
    if (rlang::is_empty(filteredSingleQuestionTag) || length(filteredSingleQuestionTag) <= 0) {
      return(NA)
    }

    answerBaseOnTag <- responses %>%
      #dplyr::filter(RecipientEmail == !!email) %>%
      dplyr::pull(!!filteredSingleQuestionTag)
    return(answerBaseOnTag)
  }

  # get question tags
  questionTags <- get_monthly_question_info() %>%
    dplyr::pull("QuestionTag")

  # merge questions' answers with monitors' information
  monitors <- monitors %>%
    dplyr::rowwise() %>%
    purrr::reduce(questionTags, function(df, question_tag) {
      df %>%
        dplyr::mutate(!!question_tag := get_answer_based_on_deviceID_and_main_questionTag(DeviceID, question_tag))
    }, .init = .) %>%
    dplyr::ungroup()

  # get unresolved monitors
  newDesc <- get_monthly_question_descriptions(descriptionFileName, splitLikeUnresponsed = T)

  get_answer_for_unresolved_monitor <- function(myId, myReason, myDate, qTag) {
    filteredSingleQuestionTag <- newDesc %>%
      dplyr::filter(grepl(qTag, qname)) %>%
      dplyr::filter(DeviceID == myId & Reason == myReason & ErrorDate == myDate) %>%
      dplyr::pull(qname)

    if (length(filteredSingleQuestionTag) == 1) {
      answerBaseOnTag <- responses %>% dplyr::pull(!!filteredSingleQuestionTag)
    } else {
      return(NA)
    }

    answerBaseOnTag <- responses %>%
      dplyr::pull(!!filteredSingleQuestionTag)
    return(answerBaseOnTag)
  }

  unresolved <- get_unresolved_monitor_log() %>%
    dplyr::filter(Resolved == "No") %>% # Filter out already resolved monitors from previous months
    dplyr::rowwise() %>%
    dplyr::mutate(Resolved = get_answer_for_unresolved_monitor(DeviceID, Reason, OriginDate, "Unresolve_")) %>%
    dplyr::ungroup()

  unresolvedNotes <- unresolved %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Notes = get_answer_for_unresolved_monitor(DeviceID, Reason, OriginDate, "UnresolveNotes_")) %>%
    dplyr::ungroup()

  return(list(
    PersonnelInfo = personnelInfo,
    Responsed = responsedPersonel,
    Unresponsed = unresponsedPersonel,
    QuestionDesc = questionDesc,
    RawResponses = responses,
    Monitors = monitors,
    QuestionOtherInfo = get_monthly_question_info(),
    UnresolvedMonitor = unresolved,
    UnresolvedNotes = unresolvedNotes
  ))
}
#tmp <- qualtrics_concentrate_monthly_data_from_responses()

#TODO: Install tinytex beforehand manually
qualtrics_generate_monthly_report <- function() {
  # get data
  data <- qualtrics_concentrate_monthly_data_from_responses()
  if (is.null(data)) {
    return()
  }

  # get date of this week
  current_date <- Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  # generate markdown report - PDF
  # TODO: Consider HTML in the future?
  pdfFilePath <- file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Qualtrics", "Monthly", "Reports",
                           paste("DataQAReport - ", start_of_current_month, ".pdf", sep = ""))
  rmarkdown::render(
    file.path("Code", "QualtricsCode", "QualtricsMonthly", "QualtricsMonthlyReportTemplate.Rmd"),
    params = list(
      title = "Qualtrics Monthly Air Sensor Data Check Report",
      subtitle = paste("Month of ", month(start_of_current_month), " - ", year(start_of_current_month),
                       " / Recorded on ", data$RawResponses %>% dplyr::pull(EndDate),
                       sep = ""),
      myData = data
    ),
    output_file = pdfFilePath
  )

  # Add new unresolved
  newUnresolved <- concentrate_unresolved_monitor_qualtrics(data, data[["UnresolvedMonitor"]] %>%
                                                              dplyr::mutate(Resolved = as.character(Resolved))
                                                            )
  write.csv(newUnresolved,
            file.path(Sys.getenv("BOX_UPLOAD_ROOT_FOLDER"), "CSV", "Imports", "UnresolvedMonitor.csv"),
            row.names = FALSE
  )

  # Send email to Maintainer and Admin
  personnelInfo <- get_main_personnel_list(role = c("Maintainer", "Admin"))
  # send_email_gmail(
  #   Sender = "@gmail.com",
  #   Recipients = personnelInfo %>% dplyr::pull("Email"),
  #   Subject = paste("RWeather - Data Quality Assurance Report - ", start_of_current_month),
  #   Body = "Please review the report for this month Data Quality Assurance process.\nSent by automation system.",
  #   AttachmentPaths = c(pdfFilePath),
  #   AttachmentNames = c("MonthlyReport.pdf")
  # )

  send_email_smtp2go_attachment(
    api_key = Sys.getenv("SMTP_API"),
    sender = "@gmail.com",
    recipient = personnelInfo %>% dplyr::pull("Email"),
    subject = paste("RWeather - Data Quality Assurance Report - ", start_of_current_month),
    text_body = "Please review the report for this month Data Quality Assurance process.\nSent by automation system.",
    attachment_path = c(pdfFilePath)
  )

  # Save to log - Monthly Action Log
  write_to_monthly_log(
    start_of_current_month,
    "SendReport",
    paste("Qualtrics Monthly Data Assurance Report - month of ", start_of_current_month, ".pdf", sep = "")
  )
}
qualtrics_generate_monthly_report()

# Finish testing: 17 Mar 2024

# FOR TESTING ONLY ####
#data <- qualtrics_concentrate_data_from_responses()
#questions <- get_all_survey_questions(Sys.getenv("QUALTRICS_API_KEY"), Sys.getenv("QUALTRICS_WEEKLY_TEMPLATE_ID"))
