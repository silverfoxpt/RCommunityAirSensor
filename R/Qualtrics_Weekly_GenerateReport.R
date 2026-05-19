
#' Concentrate weekly Qualtrics responses into structured lists
#'
#' This function collects weekly Qualtrics response artifacts recorded in the
#' weekly log, loads the associated question descriptions and processed
#' responses, and returns a named list containing personnel and response
#' summaries used by downstream reporting functions.
#'
#' @details
#' **Run:**
#' 1. Determine the week start using `current_date`.
#' 2. Read the weekly log and select the latest response/description artifacts.
#' 3. Load processed responses, question descriptions and personnel lists.
#' 4. Join answers back to personnel by `DeviceID` and `Email`.
#'
#' **Data processing details:**
#' - Expects helper functions such as `get_weekly_log()`,
#'   `get_unresponsed_personnel_list()`, `get_responsed_personnel_list()`,
#'   `get_merge_personnel_sensor_list()`, `get_question_descriptions()` and
#'   `get_processed_responses_list()` to be available in the package namespace.
#'
#' @param current_date Date object used to determine the start of the week. Defaults to `Sys.Date()`.
#'
#' @return A named list with elements: `PersonnelInfo`, `Responsed`, `Unresponsed`,
#' `QuestionDesc`, `RawResponses`, `WeeklyShortlist`.
#'
#' @section Error handling:
#' The function returns `NULL` (invisibly) when there are no responses for the
#' requested week, and raises errors when required helper functions or expected
#' artifacts are missing.
#'
#' @examples
#' \dontrun{
#' qualtrics_concentrate_data_from_responses_weekly(Sys.Date())
#' }
#'
#' @seealso
#' \code{\link{qualtrics_generate_weekly_report}}
#'
#' @export
#' @concept role:process
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
qualtrics_concentrate_data_from_responses_weekly <- function(current_date = Sys.Date()) {
  start_of_current_week <- lubridate::floor_date(current_date, unit = "week")

  # Check weekly log for response artifacts
  logfile <- get_weekly_log()

  # select response and description entries for this week
  responseData <- logfile %>% dplyr::filter(OriginDate == start_of_current_week & Action == "RetrieveResponse")
  if (nrow(responseData) <= 0) {
    return(invisible(NULL))
  }
  descriptionData <- logfile %>% dplyr::filter(OriginDate == start_of_current_week & Action == "RetrieveQuestionDesc")

  # if report already sent, nothing to do
  if (check_exist_in_log(logfile, start_of_current_week, "SendReport")) {
    return(invisible(NULL))
  }

  # get last saved artifact names
  dataFileName <- responseData$SaveData
  if (length(dataFileName) > 1) {
    dataFileName <- dataFileName[[length(dataFileName)]]
  }

  descriptionFileName <- descriptionData$SaveData
  if (length(descriptionFileName) > 1) {
    descriptionFileName <- descriptionFileName[[length(descriptionFileName)]]
  }

  # responsed & unresponsed lists 
  unresponsedPersonel <- get_unresponsed_personnel_list(dataFileName)
  responsedPersonel <- get_responsed_personnel_list(dataFileName)

  personnelInfo <- get_merge_personnel_sensor_list() %>%
    dplyr::mutate(Submitted = dplyr::if_else(DeviceID %in% responsedPersonel$DeviceID, "Yes", "No"))

  questionDesc <- get_question_descriptions(descriptionFileName)
  responses <- get_processed_responses_list(dataFileName)

  get_answer_based_on_deviceID_and_main_questionTag <- function(email, deviceID, questionMainTag) {
    filteredSingleQuestionTag <- questionDesc %>%
      dplyr::filter(DeviceID == deviceID & grepl(questionMainTag, qname)) %>%
      dplyr::pull(qname)

    # In case the device isn't managed by this tag e.g. a Clarity ID but a PA question tag
    if (rlang::is_empty(filteredSingleQuestionTag)) {
      return(NA)
    }

    # dynamic column access: use dplyr and rlang helpers
    tag_sym <- rlang::sym(filteredSingleQuestionTag)
    answerBaseOnTag <- responses %>%
      dplyr::filter(RecipientEmail == email) %>%
      dplyr::pull(!!tag_sym)

    return(answerBaseOnTag)
  }

  responsedPersonel <- responsedPersonel %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      PAQuestion1 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion1"),
      PAQuestion2 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion2"),
      PAQuestion3 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "PAQuestion3"),
      ClarityQuestion1 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion1"),
      ClarityQuestion2 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion2"),
      ClarityQuestion3 = get_answer_based_on_deviceID_and_main_questionTag(Email, DeviceID, "ClarityQuestion3")
    ) %>%
    dplyr::ungroup()

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

#' Generate and send the weekly Qualtrics sensor report (PDF)
#'
#' This workflow collects the weekly consolidated Qualtrics data (via
#' `qualtrics_concentrate_data_from_responses_weekly()`), renders a PDF report from
#' an R Markdown template, optionally emails the PDF to maintainers/admins, and
#' writes an entry to the weekly action log to avoid duplicate sends.
#'
#' @details
#' **Run:**
#' 1. Calls `qualtrics_concentrate_data_from_responses_weekly()` to obtain a data bundle.
#' 2. Renders the report from `template_path` using `rmarkdown::render()`.
#' 3. Sends the generated PDF to responsible personnel (unless `recipients` is provided).
#' 4. Records the `SendReport` action in the weekly log to prevent re-send.
#'
#' **File structure:**
#' \preformatted{
#' [upload_root_folder]/CSV/Qualtrics/Weekly/Reports/
#'   WeeklyCommunitySensorReport - YYYY-MM-DD.pdf
#' }
#'
#' @param current_date Date used to determine week bounds. Defaults to `Sys.Date()`.
#' @param upload_root_folder Character string. Root folder where reports will be written. Defaults to the `UPLOAD_ROOT_FOLDER` environment variable.
#' @param smtp_api Character string. SMTP API key used for sending email when `send_email = TRUE`. Defaults to `SMTP_API` environment variable.
#' @param smtp_sender Character string. Sender email address used by the SMTP service. Defaults to `SMTP_SENDER` environment variable.
#' @param rmd_template Character string. Path to the R Markdown template used to render the report. Defaults to `data-raw/Qualtrics_Weekly_SurveyReportTemplate.Rmd`.
#' @param send_email Logical. If `TRUE`, the rendered PDF will be sent via SMTP using `smtp_api`/`smtp_sender`. Defaults to `FALSE`.
#'
#' @return NULL. Called for side effects: writes a PDF, sends email, and logs the action.
#'
#' @section Error handling:
#' Stops with informative messages when required inputs or folders are missing. Creates target folders when necessary.
#'
#' @examples
#' \dontrun{
#' qualtrics_generate_weekly_report()
#' }
#'
#' @seealso
#' \code{\link{qualtrics_concentrate_data_from_responses_weekly}}
#'
#' @export
#' @concept role:report
#' @concept removedDependencies:true
#' @concept removedRawFunctionCalls:true
#' @concept removedSensitiveInfo:true
#' @concept cleanupParameters:true
#' @concept cleanupComments:true
#' @concept cleanupDependenciesNamespace:true
#' @concept addRoxygenComments:true
#' @concept addCheckSetupFolder:true
qualtrics_generate_weekly_report <- function(
  current_date = Sys.Date(),
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  smtp_api = Sys.getenv("SMTP_API"),
  smtp_sender = Sys.getenv("SMTP_SENDER"),
  rmd_template = file.path("data-raw", "Qualtrics_Weekly_SurveyReportTemplate.Rmd"),
  send_email = FALSE
) {
  start_of_current_week <- lubridate::floor_date(current_date, unit = "week")

  data <- qualtrics_concentrate_data_from_responses_weekly(current_date = current_date)
  if (is.null(data)) {
    return(invisible(NULL))
  }

  if (is.null(upload_root_folder) || identical(upload_root_folder, "") || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided (or set via UPLOAD_ROOT_FOLDER environment variable)")
  }

  pdfFilePath <- file.path(upload_root_folder, "CSV", "Qualtrics", "Weekly", "Reports",
                           paste("WeeklyCommunitySensorReport - ", start_of_current_week, ".pdf", sep = ""))

  # ensure directory exists
  dir.create(dirname(pdfFilePath), recursive = TRUE, showWarnings = FALSE)

  rmarkdown::render(
    rmd_template,
    params = list(
      title = paste("Qualtrics Weekly Sensor Status Report - week of ", start_of_current_week, sep = ""),
      subtitle = paste("Week of ", start_of_current_week, sep = ""),
      myData = data
    ),
    output_file = pdfFilePath
  )

  personnelInfo <- get_main_personnel_list(role = c("Maintainer", "Admin"))

  if (send_email) {
    personnelInfo <- get_main_personnel_list(role = c("Maintainer", "Admin"))
    send_email_smtp2go_attachment(
      api_key = smtp_api,
      sender = smtp_sender,
      recipient = personnelInfo %>% dplyr::pull("Email"),
      subject = paste0("RWeather - Weekly Health Check - ", start_of_current_week),
      text_body = "Please review the report for this week Health Check process.\nSent by automation system.",
      attachment_path = c(pdfFilePath)
    )
  }

  write_to_weekly_log(
    start_of_current_week,
    "SendReport",
    paste("Qualtrics Weekly Sensor Health Report - week of ", start_of_current_week, ".pdf", sep = "")
  )
}
