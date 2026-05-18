#' Send Monthly Survey via Qualtrics API
#'
#' Sends the monthly CAMN monitor Quality Assurance survey to the analyst mailing
#' list using the Qualtrics API. This is a workflow wrapper that checks the
#' monthly log, ensures the survey has been created, generates a mailing list
#' from personnel, distributes the survey, and writes an entry to the monthly
#' log to prevent duplicate sends.
#'
#' @details
#' **Run:**
#' 1. Validation: ensures required parameters or environment variables exist
#' 2. Log check: verifies that a "CreateSurvey" entry exists for the current month
#' 3. Duplicate check: skips if a "SendSurvey" entry already exists
#' 4. Generates a mailing list from the analyst personnel list
#' 5. Distributes the survey and records the send in the monthly log
#'
#' **Data processing details:**
#' - Uses the package's monthly log functions to read and write state
#' - Delegates contact creation and distribution to helper functions
#'
#' @param current_date Date used to determine the current month. Defaults to Sys.Date().
#' @param qualtrics_api_key Qualtrics API key. Defaults to environment variable `QUALTRICS_API_KEY`.
#' @param directory_id Qualtrics directory ID where contacts are created. Defaults to `QUALTRICS_DIRECTORY_ID` env var.
#' @param library_id Qualtrics library ID containing the survey message. Defaults to `QUALTRICS_LIBRARY_ID` env var.
#' @param message_id Qualtrics message ID to use for distribution. Defaults to `QUALTRICS_MESSAGE_ID` env var.
#'
#' @return NULL (invisibly). Side effects: creates mailing lists and triggers Qualtrics distribution; writes to monthly log.
#'
#' @section Error handling:
#' The function stops with a clear message when required parameters or environment
#' variables are missing. It returns silently (invisibly) when the send is not
#' applicable (survey not created yet or already sent for the month).
#'
#' @examples
#' \dontrun{
#' qualtrics_send_monthly_survey()
#' }
#'
#' @seealso
#' \code{\link{get_monthly_log}}, \code{\link{create_and_add_contact_from_personnel_list}}, \code{\link{distribute_qualtrics_survey}}
#'
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
qualtrics_send_monthly_survey <- function(
  current_date = Sys.Date(),
  qualtrics_api_key = Sys.getenv("QUALTRICS_API_KEY"),
  directory_id = Sys.getenv("QUALTRICS_DIRECTORY_ID"),
  library_id = Sys.getenv("QUALTRICS_LIBRARY_ID"),
  message_id = Sys.getenv("QUALTRICS_MESSAGE_ID")
) {
  if (is.null(current_date) || length(current_date) != 1) {
    stop("`current_date` must be a single Date-like value")
  }

  # Basic validation for required configuration
  if (is.null(qualtrics_api_key) || qualtrics_api_key == "") {
    stop("`qualtrics_api_key` is required (or set QUALTRICS_API_KEY environment variable)")
  }
  if (is.null(directory_id) || directory_id == "") {
    stop("`directory_id` is required (or set QUALTRICS_DIRECTORY_ID environment variable)")
  }
  if (is.null(library_id) || library_id == "") {
    stop("`library_id` is required (or set QUALTRICS_LIBRARY_ID environment variable)")
  }
  if (is.null(message_id) || message_id == "") {
    stop("`message_id` is required (or set QUALTRICS_MESSAGE_ID environment variable)")
  }

  start_of_current_month <- lubridate::floor_date(as.Date(current_date), unit = "month")

  # Read monthly log using package helper
  logfile <- get_monthly_log()

  # If the survey has not been created yet, nothing to send
  if (check_not_exist_in_log(logfile, start_of_current_month, "CreateSurvey")) {
    return(invisible(NULL))
  }

  # If already sent, skip
  if (check_exist_in_log(logfile, start_of_current_month, "SendSurvey")) {
    return(invisible(NULL))
  }

  # Retrieve survey id that was saved when survey was created
  surveyId <- get_first_save_data_from_monthly_log(logfile, start_of_current_month, "CreateSurvey")

  # Get analyst(s) from project personnel list
  participants <- get_main_personnel_list(role = "Analyst")

  # Create mailing list and add contacts
  mailingId <- create_and_add_contact_from_personnel_list(
    qualtrics_api_key,
    directory_id,
    paste0("Monthly mailing list - ", start_of_current_month),
    participants
  )

  # Distribute the survey
  distribute_qualtrics_survey(
    qualtrics_api_key,
    library_id,
    message_id,
    mailingId,
    "Monthly CAMN monitor Quality Assurance survey",
    surveyId,
    isMultiple = TRUE
  )

  # Save to monthly log to prevent duplicate sends
  write_to_monthly_log(start_of_current_month, "SendSurvey", surveyId)

  invisible(NULL)
}
