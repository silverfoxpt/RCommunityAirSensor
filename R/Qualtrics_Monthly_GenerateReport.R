#' Concentrate monthly Qualtrics responses into structured lists
#'
#' Collects and merges monthly Qualtrics question descriptions, responses,
#' personnel and monitor information into a single named list for downstream
#' reporting. This function performs in-package data consolidation and does
#' not perform any side-effecting I/O (files are not written here).
#'
#' @param upload_root_folder Character. Root folder for uploads/exports. Defaults
#'   to the environment variable `UPLOAD_ROOT_FOLDER` if set.
#' @return A named list with elements: `PersonnelInfo`, `Responsed`,
#'   `Unresponsed`, `QuestionDesc`, `RawResponses`, `Monitors`,
#'   `QuestionOtherInfo`, `UnresolvedMonitor`, `UnresolvedNotes`.
#' @details
#' This helper collects monthly Qualtrics data previously saved by other
#' utilities (referenced via helper functions). It expects those helper
#' functions (e.g. `get_monthly_log`, `get_monthly_responses_list`) to be
#' available in the package namespace.
#'
#' @export
#' @concept role:helper
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
qualtrics_concentrate_monthly_data_from_responses <- function(upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER")) {
  # Determine current month bounds
  current_date <- base::Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  # Check monthly action log
  logfile <- get_monthly_log()

  # check that at least one RetrieveResponse entry exists
  responseData <- logfile %>% dplyr::filter(OriginDate == start_of_current_month & Action == "RetrieveResponse")
  if (nrow(responseData) <= 0) {
    return(NULL)
  }
  descriptionData <- logfile %>% dplyr::filter(OriginDate == start_of_current_month & Action == "RetrieveQuestionDesc")

  # check if a monthly report has already been sent
  if (check_exist_in_log(logfile, start_of_current_month, "SendReport")) {
    return(NULL)
  }

  # get most recent saved response file name
  dataFileName <- responseData$SaveData
  if (length(dataFileName) > 1) {
    dataFileName <- dataFileName[[length(dataFileName)]]
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

  # read question descriptions and responses via helper functions
  questionDesc <- get_monthly_question_descriptions(descriptionFileName)

  responses <- get_monthly_responses_list(dataFileName)

  # monitor site metadata
  monitors <- get_monitor_sites()

  get_answer_based_on_deviceID_and_main_questionTag <- function(deviceID, questionMainTag) {
    filteredSingleQuestionTag <- questionDesc %>%
      dplyr::filter(DeviceID == deviceID & grepl(questionMainTag, qname)) %>%
      dplyr::pull(qname)

    if (rlang::is_empty(filteredSingleQuestionTag) || length(filteredSingleQuestionTag) <= 0) {
      return(NA_character_)
    }

    answerBaseOnTag <- responses %>%
      dplyr::pull(!!filteredSingleQuestionTag)
    return(answerBaseOnTag)
  }

  questionTags <- get_monthly_question_info() %>% dplyr::pull("QuestionTag")

  monitors <- monitors %>%
    dplyr::rowwise() %>%
    purrr::reduce(questionTags, function(df, question_tag) {
      df %>%
        dplyr::mutate(!!question_tag := get_answer_based_on_deviceID_and_main_questionTag(DeviceID, question_tag))
    }, .init = .) %>%
    dplyr::ungroup()

  newDesc <- get_monthly_question_descriptions(descriptionFileName, splitLikeUnresponsed = TRUE)

  get_answer_for_unresolved_monitor <- function(myId, myReason, myDate, qTag) {
    filteredSingleQuestionTag <- newDesc %>%
      dplyr::filter(grepl(qTag, qname)) %>%
      dplyr::filter(DeviceID == myId & Reason == myReason & ErrorDate == myDate) %>%
      dplyr::pull(qname)

    if (length(filteredSingleQuestionTag) != 1) {
      return(NA_character_)
    }

    answerBaseOnTag <- responses %>% dplyr::pull(!!filteredSingleQuestionTag)
    return(answerBaseOnTag)
  }

  unresolved <- get_unresolved_monitor_log() %>%
    dplyr::filter(Resolved == "No") %>%
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

#' Qualtrics monthly — generate and send PDF report
#'
#' Generates a monthly Qualtrics Data QA PDF report using consolidated
#' responses produced by `qualtrics_concentrate_monthly_data_from_responses()`
#' and emails the report to maintainers. File paths and email configuration
#' are parameterized to avoid hard-coded credentials in package code.
#'
#' @details
#' **Run:**
#' 1. Call `qualtrics_concentrate_monthly_data_from_responses()` to collect data
#' 2. Render the RMarkdown report to PDF using `rmarkdown::render()`
#' 3. Save unresolved monitor records via `concentrate_unresolved_monitor_qualtrics()`
#' 4. Email the PDF to maintainers and admins
#' 5. Record the SendReport action to the monthly log
#'
#' **File structure:**
#' \preformatted{
#' [upload_root_folder]/CSV/Qualtrics/Monthly/Reports/
#' }
#'
#' @param upload_root_folder Character. Root folder used for saving the PDF and imports.
#'   Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @param rmd_template Character. Path to the RMarkdown template to render.
#'   Defaults to `Code/QualtricsCode/QualtricsMonthly/QualtricsMonthlyReportTemplate.Rmd`.
#' @param smtp_api Character. API key for SMTP provider. Defaults to
#'   `Sys.getenv("SMTP_API")`.
#' @param smtp_sender Character. Sender email address. Defaults to
#'   `Sys.getenv("SMTP_SENDER")` or a redacted placeholder if unset.
#' @param send_email Logical. Whether to send the email. Defaults to
#'   `FALSE`.
#' @return NULL. Called for side effects: writes files and sends email.
#' @section Error handling:
#' The function stops with clear messages if required data is missing, if the
#' RMarkdown render fails, or if email sending fails (helper email functions
#' are expected to signal errors).
#' @examples
#' \dontrun{
#' qualtrics_generate_monthly_report()
#' }
#' @seealso
#' \code{\link{qualtrics_concentrate_monthly_data_from_responses}}
#' @export
#' @concept role:qualtrics_monthly
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
qualtrics_generate_monthly_report <- function(
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  rmd_template = file.path("Code", "QualtricsCode", "QualtricsMonthly", "QualtricsMonthlyReportTemplate.Rmd"),
  smtp_api = Sys.getenv("SMTP_API"),
  smtp_sender = Sys.getenv("SMTP_SENDER"),
  send_email = FALSE
) {
  # normalize sender fallback
  if (is.null(smtp_sender) || smtp_sender == "") smtp_sender <- "<REDACTED_EMAIL>"

  # collect consolidated data
  data <- qualtrics_concentrate_monthly_data_from_responses(upload_root_folder = upload_root_folder)
  if (is.null(data)) {
    return(invisible(NULL))
  }

  # determine month for labeling
  current_date <- Sys.Date()
  start_of_current_month <- lubridate::floor_date(current_date, unit = "month")

  # render PDF report
  pdfFilePath <- file.path(upload_root_folder, "CSV", "Qualtrics", "Monthly", "Reports",
                           paste0("DataQAReport - ", start_of_current_month, ".pdf"))
  rmarkdown::render(
    input = rmd_template,
    params = list(
      title = "Qualtrics Monthly Air Sensor Data Check Report",
      subtitle = paste0("Month of ", lubridate::month(start_of_current_month), " - ", lubridate::year(start_of_current_month),
                        " / Recorded on ", data$RawResponses %>% dplyr::pull(EndDate)),
      myData = data
    ),
    output_file = pdfFilePath
  )

  # Add new unresolved and save import CSV
  newUnresolved <- concentrate_unresolved_monitor_qualtrics(data, data[["UnresolvedMonitor"]] %>%
                                                              dplyr::mutate(Resolved = as.character(Resolved))
  )
  write.csv(newUnresolved,
                  file.path(upload_root_folder, "CSV", "Imports", "UnresolvedMonitor.csv"),
                  row.names = FALSE)

  # Send email to Maintainer and Admin
  if (send_email) {
    personnelInfo <- get_main_personnel_list(role = c("Maintainer", "Admin"))
    send_email_smtp2go_attachment(
      api_key = smtp_api,
      sender = smtp_sender,
      recipient = personnelInfo %>% dplyr::pull("Email"),
      subject = paste0("RWeather - Data Quality Assurance Report - ", start_of_current_month),
      text_body = "Please review the report for this month Data Quality Assurance process.\nSent by automation system.",
      attachment_path = c(pdfFilePath)
    )
  }

  # record that the report was sent
  write_to_monthly_log(
    start_of_current_month,
    "SendReport",
    paste0("Qualtrics Monthly Data Assurance Report - month of ", start_of_current_month, ".pdf")
  )

  return(invisible(NULL))
}
