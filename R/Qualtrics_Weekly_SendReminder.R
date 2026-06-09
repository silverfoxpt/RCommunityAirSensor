#' Send weekly Qualtrics reminder email to unresponsive personnel
#'
#' Sends a reminder distribution for the current weekly survey to personnel who
#' have not yet responded. The workflow checks the weekly log, builds a mailing
#' list from the unresponsive participants, sends the reminder message, and
#' records completion in the weekly log.
#'
#' @details
#' **Run:**
#' 1. **Validation**: Checks the upload folder structure and required Qualtrics credentials.
#' 2. **Log check**: Reads the weekly log and skips execution if the survey response has not been retrieved or if the reminder was already sent.
#' 3. **Survey lookup**: Retrieves the survey id from the weekly `CreateSurvey` log entry.
#' 4. **Participant filtering**: Loads the processed response file and derives the unresponsive personnel list.
#' 5. **Mailing list creation**: Creates a Qualtrics mailing list for the unresponsive participants.
#' 6. **Distribution**: Sends the reminder distribution using the configured message and survey.
#' 7. **Logging**: Writes a `SendReminder` entry to the weekly log.
#'
#' **Data processing details:**
#' - Uses the latest retrieved weekly response file recorded in the weekly log.
#' - Derives the reminder recipient list from `get_unresponsed_personnel_list()`.
#' - Keeps the reminder distribution aligned to the same weekly survey identifier used for creation.
#'
#' **File structure:**
#' \preformatted{
#' [upload_root_folder]/CSV/Exports/QualtricsWeeklyLog.csv
#' [upload_root_folder]/CSV/Imports/MainPersonnel.csv
#' [upload_root_folder]/CSV/Qualtrics/Weekly/
#' }
#'
#' @param current_date Date used to determine the start of the week. Defaults to `Sys.Date()`.
#' @param upload_root_folder Character string specifying the root upload folder. Defaults to `Sys.getenv("UPLOAD_ROOT_FOLDER")`.
#' @param qualtrics_api_key Character string containing the Qualtrics API key. Defaults to `Sys.getenv("QUALTRICS_API_KEY")`.
#' @param directory_id Character string containing the Qualtrics directory ID. Defaults to `Sys.getenv("QUALTRICS_DIRECTORY_ID")`.
#' @param library_id Character string containing the Qualtrics library ID. Defaults to `Sys.getenv("QUALTRICS_LIBRARY_ID")`.
#' @param message_id Character string containing the reminder message ID. Defaults to `Sys.getenv("QUALTRICS_REMINDER_MESSAGE_ID")`.
#' @param mailing_list_prefix Character string used to name the reminder mailing list. Defaults to `"Reminder: Weekly mailing list - "`.
#' @param email_subject_prefix Character string used as the reminder email subject prefix. Defaults to `"Reminder: Weekly monitor health check survey - week of "`.
#'
#' @return NULL. Called for side effects: sends a reminder and writes the weekly log entry.
#'
#' @section Error handling:
#' The function stops with informative messages if required credentials are missing, the upload folder structure is invalid, the weekly survey creation log entry cannot be found, or the response file name is missing from the weekly log.
#'
#' @examples
#' \dontrun{
#' qualtrics_send_reminder_email()
#' qualtrics_send_reminder_email(
#'   qualtrics_api_key = "<REDACTED_API_KEY>",
#'   directory_id = "<REDACTED_DIRECTORY_ID>",
#'   library_id = "<REDACTED_LIBRARY_ID>",
#'   message_id = "<REDACTED_MESSAGE_ID>"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_weekly_log}}, \code{\link{get_first_save_data_from_weekly_log}},
#' \code{\link{get_unresponsed_personnel_list}}, \code{\link{create_and_add_contact_from_personnel_list}},
#' \code{\link{distribute_qualtrics_survey}}, \code{\link{write_to_weekly_log}}
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
qualtrics_send_reminder_email <- function(
  current_date = Sys.Date(),
  upload_root_folder = Sys.getenv("UPLOAD_ROOT_FOLDER"),
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  directory_id = Sys.getenv("QUALTRICS_DIRECTORY_ID"),
  library_id = Sys.getenv("QUALTRICS_LIBRARY_ID"),
  message_id = Sys.getenv("QUALTRICS_REMINDER_MESSAGE_ID"),
  mailing_list_prefix = "Reminder: Weekly mailing list - ",
  email_subject_prefix = "Reminder: Weekly monitor health check survey - week of "
) {
  if (is.null(upload_root_folder) || !nzchar(upload_root_folder)) {
    stop("upload_root_folder must be provided or set via the UPLOAD_ROOT_FOLDER environment variable.")
  }
  if (!check_folder_and_file_structure(upload_root_folder)) {
    stop("upload_root_folder does not contain the required weekly Qualtrics folder and file structure.")
  }

  if (is.null(qualtrics_api_key) || !nzchar(qualtrics_api_key)) {
    stop("qualtrics_api_key must be provided or set via the QUALTRICS_API_KEY environment variable.")
  }
  if (is.null(directory_id) || !nzchar(directory_id)) {
    stop("directory_id must be provided or set via the QUALTRICS_DIRECTORY_ID environment variable.")
  }
  if (is.null(library_id) || !nzchar(library_id)) {
    stop("library_id must be provided or set via the QUALTRICS_LIBRARY_ID environment variable.")
  }
  if (is.null(message_id) || !nzchar(message_id)) {
    stop("message_id must be provided or set via the QUALTRICS_REMINDER_MESSAGE_ID environment variable.")
  }

  start_of_current_week <- lubridate::floor_date(current_date, unit = "week")
  logfile <- get_weekly_log(root_folder = upload_root_folder)

  response_data <- logfile[
    logfile$OriginDate == start_of_current_week & logfile$Action == "RetrieveResponse",
    drop = FALSE
  ]
  if (nrow(response_data) <= 0) {
    return(invisible(NULL))
  }

  if (check_exist_in_log(logfile, start_of_current_week, "SendReminder")) {
    return(invisible(NULL))
  }

  survey_id <- get_first_save_data_from_weekly_log(logfile, start_of_current_week, "CreateSurvey")
  if (is.null(survey_id) || !nzchar(survey_id)) {
    stop("No survey id was found in the weekly log for the current week.")
  }

  response_file_name <- utils::tail(response_data$SaveData, 1)
  if (is.null(response_file_name) || !nzchar(response_file_name)) {
    stop("No response file name was found in the weekly log for the current week.")
  }

  unresponsive_personnel <- get_unresponsed_personnel_list(
    response_file_name,
    root_folder = upload_root_folder
  )

  if (nrow(unresponsive_personnel) <= 0) {
    write_to_weekly_log(start_of_current_week, "SendReminder", survey_id, root_folder = upload_root_folder)
    return(invisible(NULL))
  }

  mailing_id <- create_and_add_contact_from_personnel_list(
    qualtrics_api_key,
    directory_id,
    paste0(mailing_list_prefix, start_of_current_week),
    unresponsive_personnel
  )

  distribute_qualtrics_survey(
    qualtrics_api_key,
    library_id,
    message_id,
    mailing_id,
    paste0(email_subject_prefix, start_of_current_week),
    survey_id,
    isMultiple = TRUE
  )

  write_to_weekly_log(start_of_current_week, "SendReminder", survey_id, root_folder = upload_root_folder)

  invisible(NULL)
}
